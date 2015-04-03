/* *******************************************************************
 * Rocstar Simulation Suite                                          *
 * Copyright@2015, Illinois Rocstar LLC. All rights reserved.        *
 *                                                                   *
 * Illinois Rocstar LLC                                              *
 * Champaign, IL                                                     *
 * www.illinoisrocstar.com                                           *
 * sales@illinoisrocstar.com                                         *
 *                                                                   *
 * License: See LICENSE file in top level of distribution package or *
 * http://opensource.org/licenses/NCSA                               *
 *********************************************************************/
/* *******************************************************************
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   *
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES   *
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND          *
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE CONTRIBUTORS OR           *
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   *
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE    *
 * USE OR OTHER DEALINGS WITH THE SOFTWARE.                          *
 *********************************************************************/
// $Id: Overlay_primitives.C,v 1.15 2008/12/06 08:43:28 mtcampbe Exp $

//==========================================================
// The implementation of the two basic primitive: intersection and projection.
// This file uses the HDS accessor.
//
// Author: Xiangmin Jiao
// Date:   12/13/2000
//==========================================================

#include "Overlay_primitives.h"
#include <cmath>
#include <algorithm>
#include <CGAL/solve.h>

RFC_BEGIN_NAME_SPACE

HDS_accessor<Tag_true> acc;

Vertex_set::Vertex_set( const Halfedge* h) 
  : _pane(acc.get_pane(h)), _ne(count_edges(h)) 
{
  RFC_precondition ( _ne==3 || _ne==4);
  
  Element_node_enumerator ene( _pane->base(), _pane->get_lid( h->facet()));
  
  Size nn=ene.size_of_nodes();
  int r=-1;
  Value v = _pane->get_lid(h->vertex());
  _nodes.resize(nn);

  for ( Size i=0; i<nn; ++i) {
    if ((_nodes[i] = ene[i])==v) r=(i+_ne-1)%_ne;
  }
  RFC_assertion( r>=0 && r<int(_ne));
  if (r>0) {
    std::rotate( &_nodes[0], &_nodes[r], &_nodes[_ne]);
    if (nn>_ne) std::rotate( &_nodes[_ne], &_nodes[_ne+r], &_nodes[_ne+_ne]);
  }
}

Vertex_set::Vertex_set( const Halfedge* h, Size)
  : _pane(acc.get_pane(h)), _ne(3) 
{ 
  _nodes.resize(_ne, -1);

  _nodes[0] = _pane->get_lid( h->origin());
  _nodes[1] = _pane->get_lid( h->destination());
}

// This function computes the intersection of the edge [b->org,b->dst]
//   with [g->org,g->dst]. It assumes that the two edges are not
//   parallel. The output c1 and c2 are parameterization of 
//   the locations of the intersection. It snaps the intersections
//   onto vertices if their parameterizations are close to 0 or 1.
// Returns true iff both |*c1| and |*c2| are in [0,1].
bool Overlay_primitives::
intersect( const Halfedge *hb,
	   const Halfedge *hg,
	   Real start,
	   Real *c1,
	   Real *c2,
	   Real *orien_match,
	   Real *normal_match,
	   bool is_opposite,
	   Real eps_e) const {
  RFC_assertion( !hg->is_border());

  // Switch blue edge if it is a border edge to use Point_set.
  bool is_b_border = acc.is_border(hb);
  if ( is_b_border) hb = acc.get_opposite( hb);

  const Point_set pbs(hb);
  const Point_set pgs(hg);
  const Normal_set ngs(hg);

  *c1 = start; // At input, set *c1 to start
  intersect( pbs, pgs, ngs, c1, c2, is_b_border);

  if ( std::fabs( *c1) < 1/eps_e && std::fabs( *c2) < 1/eps_e) {
    Vector_3 v2 = acc.get_normal( hg);
    Vector_3 v1 = acc.get_normal( hb);
    double prod1 = v1 * v2;

    v1 = acc.get_normal( acc.get_opposite(hb));
    double prod2 = v1 * v2;

    // Reverse the sign if the two projdirs having different directions.
    if ( is_opposite)
    { prod1 = -prod1; prod2 = -prod2; }

    *normal_match = std::max(prod1, prod2);

    // Finally, we determine c1 and c2.
    if ( std::fabs(*c1-1) <= eps_e) *c1 = 1.;
    else if ( std::fabs(*c1) <= eps_e) *c1 = 0.;

    if ( std::fabs(*c2-1) <= eps_e) *c2 = 1.;
    else if ( std::fabs(*c2) <= eps_e) *c2 = 0.;

    if ( orien_match) {
      Vector_3 J[3];
      J[0] = get_tangent( hb, *c1); 
      J[1] = get_tangent( hg, *c2); 
      J[2] = get_projdir( hg, *c2);
      *orien_match = Vector_3::cross_product( J[0], J[1]) * J[2] / 
	std::sqrt( (J[0]*J[0]) * (J[1]*J[1]) * (J[2]*J[2]));

      if (is_b_border) *orien_match = -*orien_match;
    }

    // Switch edge back
    if ( is_b_border) { *c1 = 1-*c1; }

    return *c1 >= 0. && *c1 <= 1. && *c2 >= 0. && *c2 <= 1.;
  }
  else
    return false;
}

// The intersection is a quadratic function and in general it has two
// solutions. We pick the solution based on s (the parameterization on blue
// edge) and t (the parameterization on green edge) as follows:
//   If any t is far less than 0 or far greater than 1, ignore this solution.
//   If both s1 and s2 are greater than start, then take smaller one.
//   If both are smaller than the start, then no intersection.
//   If one is greater and the other is smaller, then return the greater one.
// In addition, it safeguards errors by comparing the residual against 
// the longer of the green and blue edges.
void Overlay_primitives::
intersect( const Point_set &pbs,
	   const Point_set &pgs,
	   const Normal_set &ngs,
	   Real *c1,
	   Real *c2, bool reversed) const {

  Vector_3 vb = pbs[1]-pbs[0], vg=pgs[1]-pgs[0], vn=ngs[1]-ngs[0];
  Vector_3 vd = pgs[0]-pbs[0];

  Real c = Vector_3::cross_product( vb, ngs[0])*vd;
  Real d = Vector_3::cross_product( vb, ngs[1])*( pgs[1]-pbs[0]);
  Real a = Vector_3::cross_product( vb, vn) * vg;
  Real b = d - c - a;

  // We now need to solve a*t^2 + b*t + c =0 to get t. First scale a, b, and c.
  Real max_abc = std::max( std::max(std::fabs(a), std::fabs(b)), std::fabs(c));
  if ( max_abc > 0) 
  { a /= max_abc; b /= max_abc; c /= max_abc; d /= max_abc; }
  
  Real s, t;
  Real eps = 0.1, tol_fea = 1;

  if ( a == 0) {
    // The two edges are parallel.
    if ( b == 0) 
    { *c1 = *c2 = HUGE_VAL; return; }

    t = -c/b;

    if ( t >= -tol_fea && t <= 1+tol_fea) {
      Real denominator = (1-t) * (Vector_3::cross_product( vg, ngs[0]) * vb) +
	t * (Vector_3::cross_product( vg, ngs[1]) * vb);
      Real nominator = (1-t) * (Vector_3::cross_product( vg, ngs[0]) * vd) +
	t * (Vector_3::cross_product( vg, ngs[1]) * vd);
      s = nominator / denominator;
    }
    else
    { *c1 = *c2 = HUGE_VAL; return; }
  }
  else {
    Real det = b*b - 4*a*c;
    if ( det < 0) 
    { *c1 = *c2 = HUGE_VAL; return; }
    
    Real sqrt_d = std::sqrt( det);
    Real s1, s2, t1, t2;

    if ( b>0) {
      Real temp = -b - sqrt_d;
      t1 = 2*c / temp;
      t2 = temp / 2/ a;
    }
    else {
      Real temp = -b + sqrt_d;
      t1 = temp / 2 / a;
      t2 = 2*c / temp;
    }

    if ( std::fabs(t1 -1.) < eps || std::fabs(t2 -1.) < eps ) {
      // Use alternative formula to compute intersection
      Real b = -(c - d - a);
      Real sqrt_d = std::sqrt( b*b - 4*a*d);
      if ( det < 0)
      { *c1 = *c2 = HUGE_VAL; return; }
      if ( b>0) {
	Real temp = -b - sqrt_d;
	if (std::fabs(t1 - 1.) < eps) t1 = 1 + 2*d / temp;
	if (std::fabs(t2 - 1.) < eps) t2 = 1 + temp / 2 / a;
      }
      else {
	Real temp = -b + sqrt_d;
	if (std::fabs(t1 - 1.) < eps) t1 = 1 + temp / 2 / a;
	if (std::fabs(t2 - 1.) < eps) t2 = 1 + 2*d / temp;
      }
    }

    if ( t1 >= -tol_fea && t1 <= 1+tol_fea) {
      Real denominator = (1-t1) * (Vector_3::cross_product( vg, ngs[0]) * vb) +
	t1 * (Vector_3::cross_product( vg, ngs[1]) * vb);
      Real numerator = (1-t1) * (Vector_3::cross_product( vg, ngs[0]) * vd) +
	t1 * (Vector_3::cross_product( vg, ngs[1]) * vd);
      s1 = numerator / denominator;
    }
    else
      s1 = reversed ? HUGE_VAL : -HUGE_VAL;

    if ( t2 >= -tol_fea && t2 <= 1+tol_fea) {
      Real denominator = (1-t2) * (Vector_3::cross_product( vg, ngs[0]) * vb) +
	t2 * (Vector_3::cross_product( vg, ngs[1]) * vb);
      Real numerator = (1-t2) * (Vector_3::cross_product( vg, ngs[0]) * vd) +
	t2 * (Vector_3::cross_product( vg, ngs[1]) * vd);
      s2 = numerator / denominator;
    }
    else
      s2 = reversed ? HUGE_VAL : -HUGE_VAL;
    
    double start = *c1-eps;

    if ( !reversed) {
      if ( s1 < start && s2 < start)
      { *c1 = *c2 = HUGE_VAL; return; }
      else if ( s2 < start || s1 >= start && s1 <= s2)
      { s = s1; t = t1; }
      else 
      { s = s2; t  = t2; }
    }
    else {
      if ( 1-s1 < start && 1-s2 < start)
      { *c1 = *c2 = -HUGE_VAL; return; }
      else if ( 1-s2 < start || 1-s1 >= start && 1-s1 <= 1-s2)
      { s = s1; t = t1; }
      else 
      { s = s2; t = t2; }
    }
  }

  // Check residule of the solution and make sure the direction between
  // the two points is reasonably close to the projection direction
  Generic_element e(3);
  Vector_3 v2 = e.interpolate( ngs, t);
  Vector_3 v3 = e.interpolate( pbs, s)-e.interpolate( pgs, t);
  Real error = (v3-(v3*v2)/(v2*v2)*v2).squared_norm() / std::max( vb*vb,vg*vg);
  if ( error >= _eps_p*_eps_p) {
#ifdef DEBUG
    std::cerr << "WARNING: Rejecting intersection due to too large error " 
	      << error << ". Solution was " << s << "," << t << std::endl;
#endif
    *c1 = *c2 = HUGE_VAL;
  }
  else 
  { *c1 = s; *c2 = t; }
}

// Project a point p onto an element along a given direction.
// The projection direction is given by the input parameter dir. 
// If the input is Vector_3(0,0,0), evaluate the direction by 
// sum N_i*n_i. Note that g and pt are in-out parameters.
bool
Overlay_primitives::
project_onto_element( const Point_3 &p,
		      Halfedge **g,
		      Parent_type *pt,
		      Vector_3 dir,
		      Point_2 *nc_out,
		      const Real eps_p,
		      Real eps_np) const {
  if ( eps_np < 0) eps_np = eps_p;
  RFC_assertion( eps_p>_eps_c);
  RFC_assertion( *pt != PARENT_VERTEX);

  // Construct two elements.
  Point_set  ps(*g);
  Normal_set ns(*g);

  Point_2 nc(0.,0.);
  Generic_element e(ps.size_of_edges()); // We ignore the midpoints
  const bool comp_dir = ( dir == Vector_3(0,0,0));

  // Solve the equation using Newton's method for nonlinear equations.
  Real t(0.);
  RFC_assertion_code( Size i=0);
  RFC_assertion_code( const Size max_nsteps = 20);
  double err = 1.e30, eps_c2=_eps_c*_eps_c;
  for ( ;;RFC_assertion(++i<max_nsteps)) {
    if ( comp_dir) e.interpolate( ns, nc, &dir);

    // evaluate f
    Vector_3 f = e.interpolate( ps, nc) - p + t*dir;
    double et = f*f / e.Jacobian_det( ps,nc);
    if ( et <= eps_c2 || et >= err) break;
    err = et;

    Vector_3 J[3];
    e.Jacobian( ps, nc, J);
    if ( comp_dir) {
      Vector_3 J2[2];
      e.Jacobian( ns, nc, J2);
      J[0] = J[0] + t*J2[0];
      J[1] = J[1] + t*J2[1];
    }
    J[2] = dir;

    // Solve the linear system Ju=f
    Real u[3];
    CGAL::solve( J[0][0], J[0][1], J[0][2], J[1][0], J[1][1], J[1][2],
		 J[2][0], J[2][1], J[2][2], f[0], f[1], f[2],
		 u[0], u[1], u[2]);
    
    nc = Point_2( nc[0] - u[0], nc[1] - u[1]); t -= u[2];

    if ( std::fabs( nc[0]) > 2 || std::fabs( nc[1]) > 2 ) {
      // System is diverging
      break;
    }
  }

  // Perturb nc by eps_p.
  nc = Point_2( ( nc[0] <= eps_p && nc[0] >= -eps_np)
		? 0. : ( ( nc[0]>=1.-eps_p && nc[0]<=1.+eps_np) ? 1 : nc[0]),
		( nc[1] <= eps_p && nc[1] >= -eps_np)
		? 0. : ( ( nc[1]>=1.-eps_p && nc[1]<=1.+eps_np) ? 1 : nc[1]));

  // if the projection is restricted onto the edge, return it now.
  if ( *pt == PARENT_EDGE) {
    if ( nc[0] < 0.) { nc[0] = nc[1] = 0; }
    else if ( nc[0] > 1.) { nc[0] = nc[1] = 0; *g = (*g)->next(); }
    else nc = Point_2( nc[0], 0.);

    if ( nc[0] == 0 || nc[0] == 1) *pt = PARENT_VERTEX;
    if ( nc_out) *nc_out = nc;
    return true;
  }

  // Otherwise, determine whether the projection is in the interior,
  //   on the edge, or at a vertex.
  switch ( ps.size_of_edges()) {
  case 3: {
    if ( (nc[0] + nc[1]) >= 1-eps_p && (nc[0] + nc[1]) <= 1+eps_np) {
      // Projects onto the diagonal edge
      if ( nc[0] < eps_p) { 
	*g = (*g)->prev(); nc[0] = nc[1] = 0; *pt = PARENT_VERTEX; 
      }
      else {
	*g = (*g)->next(); nc = Point_2( nc[1], 0.); 
	*pt = (nc[1] == 0.) ? PARENT_VERTEX : PARENT_EDGE; 
      }
    }
    else if ( nc[0] == 0.) {
      // Projects onto the horizontal edge
      if ( nc[1] == 0.) 
	{ nc[0] = nc[1] = 0; *pt = PARENT_VERTEX; }
      else 
	{ *g = (*g)->prev(); nc = Point_2( 1.-nc[1], 0.); *pt = PARENT_EDGE; }
    }
    else if ( nc[1] == 0.)
      // Projects onto the vertical edge
      { nc = Point_2( nc[0], 0.); *pt = PARENT_EDGE; }
    else
      { *pt = PARENT_FACE; }
    if ( nc_out) *nc_out = nc;

    return ( nc[0] >= 0. && nc[1] >= 0. && nc[0]+nc[1] <= 1.);
  }
  case 4: {
    // Projects onto the left edge
    if ( nc[0] == 0.) {
      if ( nc[1] == 0.) 
	{ *pt = PARENT_VERTEX; }
      else { 
	*g = (*g)->prev(); nc = Point_2( 1.-nc[1], 0.);
	*pt = (nc[1] == 1.) ? PARENT_VERTEX : PARENT_EDGE;
      }
    }
    else if ( nc[0] == 1.) {
      // Projects onto the right edge
      if ( nc[1] == 1.)
	{ *g = (*g)->next()->next(); nc[0] = nc[1] = 0; *pt = PARENT_VERTEX;}
      else {
	*g = (*g)->next(); nc = Point_2( nc[1], 0.);
	*pt = (nc[1] == 0.) ? PARENT_VERTEX : PARENT_EDGE;
      }
    }
    else if ( nc[1] == 0.)
      // Projects onto the bottom edge
      { *pt = PARENT_EDGE; }
    else if ( nc[1] == 1.) 
      // Projects onto the top edge
      { *g=(*g)->next()->next(); nc=Point_2(1.-nc[0],0.); *pt=PARENT_EDGE; }
    else 
      { *pt = PARENT_FACE; }

    if ( nc_out) *nc_out = nc;

    return ( nc[0] >= 0. && nc[0] <= 1. && nc[1] >= 0. && nc[1] <= 1.);
  }
  default:
    break;
  }

  return false;
}

// Solve the equation q=p1+s*(p2-p1)+u*n+v*b
// for s, u, and v and return s.
Real
Overlay_primitives::
project_green_feature( const Vector_3 &n,  // Normal direction
		       const Vector_3 &t,  // Tangential direction
		       const Point_3  &p1, // Source blue vertex
		       const Point_3  &p2, // Target blue vertex
		       const Point_3  &q,  // Green vertex
		       const Real eps_e) const
{
  Vector_3 f = q-p1;
  Vector_3 c1 = p2-p1;
  Vector_3 b = Vector_3::cross_product( n, t);

  Real s,u,v;
  CGAL::solve( c1[0], c1[1], c1[2], n[0], n[1], n[2],
	       b[0], b[1], b[2], f[0], f[1], f[2], s, u, v);
  if ( std::fabs(s) <= eps_e)
    s = 0;
  else if ( std::fabs(1.-s) <= eps_e)
    s = 1.;

  return s;
}

// Solve the equation p=q1+s*(q2-q1)+u*(n1+s*(n2-n1))+v*(b1+s*(b2-b1))
// for s, u, and v and return s;
Real
Overlay_primitives::
project_blue_feature( const Vector_3 &n1, const Vector_3 &n2,
		      const Vector_3 &t1, const Vector_3 &t2,
		      const Point_3  &q1, const Point_3  &q2,
		      const Point_3  &p, 
		      const Real eps_e) const {
  RFC_assertion( eps_e>_eps_c);
  Real s=0, u=0, v=0;
  Vector_3 b1 = Vector_3::cross_product( n1, t1);
  Vector_3 b2 = Vector_3::cross_product( n2, t2);
  Vector_3 qdiff=q2-q1, bdiff=b2-b1, ndiff=n2-n1;

  RFC_assertion_code( Size i=0);
  RFC_assertion_code( const Size max_nsteps = 20);
  double err = 1.e30, eps_c2=_eps_c*_eps_c;
  for ( ;;RFC_assertion(++i<max_nsteps)) {
    Vector_3 c2 = (s*ndiff += n1);
    Vector_3 c3 = (s*bdiff += b1);

    Vector_3 f = ((q1-p) += s*qdiff += u*c2 += v*c3);
    double et = f*f / (qdiff*qdiff);

    if ( et <= eps_c2 || et >= err) break;
    err = et;

    // Vector_3 c1 = (q2-q1) + u*(n2-n1) + v*(b2-b1);
    Vector_3 c1 = (u*ndiff += v*bdiff += qdiff);
    Real x[3];
    CGAL::solve( c1[0], c1[1], c1[2], c2[0], c2[1], c2[2],
		 c3[0], c3[1], c3[2], f[0], f[1], f[2], x[0], x[1], x[2]);

    s -= x[0]; u -= x[1]; v -= x[2];
  }

  if ( std::fabs(s) <= eps_e)
    s = 0;
  else if ( std::fabs(1.-s) <= eps_e)
    s = 1.;

  return s;
}

// Snap a blue ridge edge onto a green ridge vertex.
void Overlay_primitives::
snap_blue_ridge_edge( const Halfedge *b,
		      const Halfedge *g,
		      Real *cb,
		      Real *cg,
		      Parent_type *t, 
		      Real tol) const {
  const RFC_Pane_overlay *pn = acc.get_pane( g);
  bool is_border = acc.is_border(b) || acc.is_border(acc.get_opposite(b));

  // If the green edge is a ridge, then do nothing
  if ( pn->is_feature_1(g) && !is_border && *cg != 0 && *cg!=1) return;
  
  const Vertex *v=NULL;
  if ( *cg <= tol) {
    v = acc.get_origin(g);

    if (is_border && acc.is_border(v) || !is_border && acc.is_on_feature( v)) {
      *cg = 0; *t = PARENT_VERTEX;
    }
  }
  else if ( *cg >= 1-tol){
    v = acc.get_destination(g);

    if (is_border && acc.is_border(v) || !is_border && acc.is_on_feature( v)) {
      *cg = 1; *t = PARENT_VERTEX;
    }
  }
}

// Snap a ridge vertex onto a ridge edge
void Overlay_primitives::
snap_blue_ridge_vertex( const Vertex *v,
			Halfedge **g,
			Parent_type *pt,
			Point_2 *nc_p,
			Real tol) const {
  bool is_border = acc.is_border(v);
  COM_assertion( acc.is_on_feature( v) && !acc.is_border( *g));

  Point_2 &nc = *nc_p;
  
  Vertex_set vs(*g);
  RFC_Pane_overlay *pn = acc.get_pane(*g);

  // Otherwise, determine whether the projection is in the interior,
  //   on the edge, or at a vertex.
  switch ( vs.size_of_edges()) {
  case 3: {
    double meas[3] = { nc[0], nc[1], 1-nc[0]-nc[1]};
    Halfedge *hs[3] = { (*g)->prev(), *g, (*g)->next() };
    bool is_feas[3];

    if ( is_border)
      for ( int i=0; i<3; ++i) is_feas[i] = acc.is_border( hs[i]->opposite());
    else
      for ( int i=0; i<3; ++i) is_feas[i] = pn->is_feature_1( hs[i]);

    for ( int k=0; k<3; ++k) {
      // Snap to the closest feature edge
      if ( meas[k] <= tol && is_feas[k] && 
	   (meas[k] <= meas[(k+1)%3] || !is_feas[(k+1)%3]) &&
	   (meas[k] <= meas[(k+2)%3] || !is_feas[(k+2)%3])) {
	*g = hs[k];

	if ( k==0) nc[0] = 1-nc[1]; else if ( k==2) nc[0] = nc[1];
	nc[1] = 0;

	if ( nc[0]>=1) nc[0] = 1.; else if ( nc[0]<=0) nc[0] = 0.;
	if ( nc[0] == 0. || nc[0]==1.) *pt = PARENT_VERTEX;
	else *pt = PARENT_EDGE;

	return;
      }
    }

    const Vertex *org;
    for (int k=0; k<3; ++k) {
      // Snap to the closest feature vertex
      if ( 1-meas[(k+1)%3] <= tol && 
	   ( acc.is_border( org= acc.get_origin( hs[k])) ||
	     !is_border && acc.is_on_feature( org))) {
	*g = hs[k];
	nc = Point_2( 0, 0);
      
	*pt = PARENT_VERTEX;
	break;
      }
    }
    return;
  }
  case 4: {
    double meas[4] = { nc[0], nc[1], 1-nc[0], 1-nc[1]};
    Halfedge *hs[4] = { (*g)->prev(), *g, (*g)->next(), (*g)->next()->next() };
    bool is_feas[4];

    if ( is_border)
      for ( int i=0; i<4; ++i) is_feas[i] = acc.is_border( hs[i]->opposite());
    else
      for ( int i=0; i<4; ++i) is_feas[i] = pn->is_feature_1( hs[i]);

    for ( int k=0; k<4; ++k) {
      // Snap to the closest feature edge
      if ( meas[k] <= tol && is_feas[k] && 
	   (meas[k] <= meas[(k+1)%4] || !is_feas[(k+1)%4]) &&
	   (meas[k] <= meas[(k+2)%4] || !is_feas[(k+2)%4]) &&
	   (meas[k] <= meas[(k+3)%4] || !is_feas[(k+3)%4]) ) {
	*g = hs[k];

	nc[0] = meas[(k+3)%4];
	nc[1] = 0.;

	if ( nc[0]>=1) nc[0] = 1.; else if ( nc[0]<=0) nc[0] = 0.;
	if ( nc[0] == 0. || nc[0]==1.) *pt = PARENT_VERTEX;
	else *pt = PARENT_EDGE;

	return;
      }
    }

    const Vertex *org;
    for ( int k=0; k<4; ++k) {
      // Snap to the closest feature vertex
      if ( meas[ (k+3)%4] <= tol && meas[ k] <= tol &&
	   ( acc.is_border( org=acc.get_origin( hs[k])) ||
	     !is_border && acc.is_on_feature( org))) {
	*g = hs[k];
	nc = Point_2( 0, 0);
      
	*pt = PARENT_VERTEX;
	break;
      }
    }
    return;
  }
  default:
    break;
  }
}

Real Overlay_primitives::
normalmatch( const Halfedge *b, const Halfedge *g) const {
  RFC_assertion( acc.is_feature_1(b) && acc.is_feature_1(g));

  const Halfedge *bopp = acc.get_opposite(b), *gopp = acc.get_opposite(g);
  const Vector_3& nrm_b = acc.get_normal( b);
  const Vector_3& nrm_bopp = acc.get_normal( bopp);

  const Vector_3& nrm_g = acc.get_normal( g);
  const Vector_3& nrm_gopp = acc.get_normal( gopp);

  // First, figure out best matching.
  Real match[4] = { nrm_b*nrm_g, nrm_bopp*nrm_g, 
		    nrm_b*nrm_gopp, nrm_bopp*nrm_gopp};

  // Find the best match.
  Real best_match=-1; int m=0;
  for ( int i=0; i<4; ++i) {
    if ( match[i]>best_match) { best_match = match[m=i]; }
  }
  
  // Return the other match
  return match[3-m];
}

RFC_END_NAME_SPACE






