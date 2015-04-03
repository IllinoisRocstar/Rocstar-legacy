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
// $Id: Overlay_primitives.h,v 1.16 2008/12/06 08:43:27 mtcampbe Exp $

//==========================================================
// This file defines the class Overlay_primitives, which provides two
//   basic primitives: intersection of two edges, and projection of 
//   a point onto a face. In addition, it also provides subroutines
//   for computing weighted squared distance from a point to an object.
//
// Author: Xiangmin Jiao
// Last modified:   01/21/2001
//
// See also: Overlay_primitives.C.
//==========================================================

#ifndef RFC_OVERLAY_PRIMITIVES_H
#define RFC_OVERLAY_PRIMITIVES_H

#include <utility>
#include "HDS_overlay.h"
#include "HDS_accessor.h"

RFC_BEGIN_NAME_SPACE

// Collection of vertices of the incident facet of a given halfedge.
class Vertex_set {
public:
  typedef Vertex_set                     Self;
  typedef Vertex_overlay                 Vertex;
  typedef Halfedge_overlay               Halfedge;
  typedef int                            Value;
  typedef unsigned int                   Size;

  Vertex_set() : _pane(NULL), _ne(0) {}
  explicit Vertex_set( const Halfedge* h);
  Vertex_set( const Halfedge* h, Size);

  // Use the default copy constructor and copy assignment operator.
  //   Vertex_set( const Vertex_set &vs) : _nodes( vs._nodes), _ne( vs._ne) {}
  //   Vertex_set &operator=( const Vertex_set &vs) 
  //   { if (this!=&vs) { vs _nodes = vs._nodes; _ne = vs._ne;} return *this; }

  const RFC_Pane_overlay *pane() const { return _pane; }
  int operator[]( Size i) const { return _nodes[i]; }
  
  Size size_of_edges() const { return _ne; }
  Size size_of_nodes() const { return _nodes.size(); }

protected:
  Size count_edges( const Halfedge *e) const {
    Size i=0;
    const Halfedge *h=e;
    do { ++i; } while ( (h=h->next())!= e);
    return i;
  }

  const RFC_Pane_overlay *_pane;
  std::vector<int>  _nodes;
  Size              _ne;     // Number of edges and number of nodes.
};

// Wrapper for point set of the incident facet of a given halfedge.
class Point_set {
public:
  typedef Point_set                      Self;
  typedef Vertex_overlay                 Vertex;
  typedef Halfedge_overlay               Halfedge;
  typedef Point_3                        Value;
  typedef unsigned int                   Size;

  explicit Point_set( const Halfedge *h) : vs(h) {}
  Point_set( const Halfedge *h, Size) : vs(h,0) {}
  
  // Use the default copy constructor and copy assignment operator.
  //   Point_set( const Self &ps) : vs( ps.vs) {}
  //   Self &operator=( const Self &ps) 
  //   { if (this != ps) { vs = ps.vs; } return *this; }
  
  const Point_3& operator[]( Size i) const { return vs.pane()->get_point(vs[i]); }
  Size size_of_edges() const { return vs.size_of_edges(); }
  Size size_of_nodes() const { return vs.size_of_nodes(); }

protected:
  Vertex_set   vs;
};

// Wrapper for vertex normals of the incident facet of a given halfedge.
class Normal_set {
public:
  typedef Normal_set                     Self;   
  typedef Vertex_overlay                 Vertex;
  typedef Halfedge_overlay               Halfedge;
  typedef Facet_overlay                  Facet;
  typedef Vector_3                       Value;
  typedef unsigned int                   Size;

  explicit Normal_set( const Halfedge *h) : vs(h), hs(vs.size_of_nodes()) {
    for ( unsigned int i=0, s=hs.size(); i<s; ++i) 
    { hs[i] = h; h = h->next(); }
  }
  Normal_set( const Halfedge *h, Size) : vs(h,0), hs(vs.size_of_nodes()) {
    for ( unsigned int i=0, s=hs.size(); i<s; ++i) 
    { hs[i] = h; h = h->next(); }
  }

  // Use the default copy constructor and copy assignment operator.
  //   Normal_set( const Self &ns) : vs( ns.vs);
  //   Self &operator=( const Self &ns);

  const Value& operator[]( Size i) const 
  { return vs.pane()->get_normal(hs[i], vs[i]); }
  Size size_of_edges() const { return vs.size_of_edges(); }
  Size size_of_nodes() const { return vs.size_of_nodes(); }

protected:
  Vertex_set   vs;
  std::vector< const Halfedge*> hs;
};

class Overlay_primitives {
public:
  typedef Overlay_primitives             Self;
  typedef Vertex_overlay                 Vertex;
  typedef Halfedge_overlay               Halfedge;
  typedef Facet_overlay                  Facet;

  typedef unsigned int                   Size;

  // Indicates whether the two surfaces have opposite normal directions.
  // By default, they are opposite of each other.
  
  Overlay_primitives( Real ec=1.e-9, Real ep=1.e-6, Real ee=1.e-3) 
    : _eps_c(ec), _eps_p(ep), _eps_e(ee)
  { RFC_assertion( _eps_c<_eps_p && _eps_p<_eps_e); }

  // This function computes the intersection of the edge [b->org,b->dst]
  //   with [g->org,g->dst]. It assumes that the two edges are NOT
  //   colinear. The output c1 and c2 are parameterization of 
  //   the locations of the intersection.
  // Returns true iff both |*c1| and |*c2| are in [0,1].
  bool
  intersect( const Halfedge *b,
	     const Halfedge *g,
	     Real start,
	     Real *c1,
	     Real *c2,
	     Real *dir,
	     Real *dir_match,
	     bool is_opposite,
	     Real eps_e) const;

  void
  intersect( const Point_set &pbs,
	     const Point_set &pgs,
	     const Normal_set &ngs,
	     Real *c1,
	     Real *c2, bool revsersed) const;

  // Project a point p onto an element along a given direction. 
  //  The projection direction is given by the input parameter dir. 
  //  If the input is NULL_VECTOR, evaluate the direction by sum N_i*n_i.
  //  g and pt are both input and output.
  bool
  project_onto_element( const Point_3 &p,
			Halfedge **g,
			Parent_type *pt,
			Vector_3 dir,
			Point_2 *nc_out, 
			const Real eps_p,         // Tolerance for perturbation
			Real eps_np=-1.) const;

  Real
  project_green_feature( const Vector_3 &n,  // Normal direction
			 const Vector_3 &t,  // Tangential direction
			 const Point_3  &p1, // Source blue vertex
			 const Point_3  &p2, // Target blue vertex
			 const Point_3  &q,  // Green vertex
			 const Real eps_p) const;
  
  Real
  project_blue_feature( const Vector_3 &n1, const Vector_3 &n2,
			const Vector_3 &t1, const Vector_3 &t2,
			const Point_3  &q1, const Point_3  &q2,
			const Point_3  &p,
			const Real eps_p) const;

  Real normalmatch( const Halfedge *b, const Halfedge *g) const;

  // Helpers for evaluating physical coordinates, tangents and projdirs
  Point_3 get_point( const Halfedge *b, const Point_2S &nc) const {
    if ( b->is_border()) 
    { RFC_assertion( nc[1]==0); return get_point( b, nc[0]); }

    Point_set ps(b); Point_2 p(nc[0],nc[1]);
    return Generic_element(ps.size_of_edges()).interpolate(ps, p);
  }

  Point_3 get_point( const Halfedge *b, const Point_2 &nc) const{
    if ( b->is_border()) 
    { RFC_assertion( nc[1]==0); return get_point( b, nc[0]); }

    Point_set ps(b);
    return Generic_element(ps.size_of_edges()).interpolate(ps, nc);
  }

  Point_3 get_point( const Halfedge *b, Real a) const {
    // Here, we assume edge is straight line.
    if ( a==0) return b->origin()->point();
    return b->origin()->point() + a*(b->destination()->point()-
				     b->origin()->point());
  }

  Vector_3 get_projdir( const Halfedge *b, Real a) const {
    Normal_set ns(b,1);
    return Generic_element(ns.size_of_edges()).interpolate( ns, a);
  }

  Vector_3 get_tangent( const Halfedge *b, Real a) const {
    Point_set ps(b,1);
    Vector_3 v;
    Generic_element(ps.size_of_edges()).Jacobian( ps, a, v);
    return v;
  }

  enum { AREA_WEIGHTED=0, UNIT_WEIGHT=1, ANGLE_WEIGHTED=2 };

  // Note that the normal vector returned is not normalized.
  // Use area-weighted by default.
  Vector_3 get_face_normal( const Halfedge *b, const Point_2 &nc, 
			    int scheme=0) const {
    // Evaluate the normal as the cross product of the colume vectors
    // of its Jacobian matrix.
    Point_set ps(b);
    Size ne = ps.size_of_edges(), nv=ne;
    Vector_3   J[2];

    Generic_element( ne, nv).Jacobian( ps, nc, J);

    Vector_3  nrm = Vector_3::cross_product( J[0], J[1]);
    if ( scheme == AREA_WEIGHTED) return nrm;
    nrm.normalize();
    if ( scheme == UNIT_WEIGHT) return nrm;

    double cosa = J[0]*J[1] / std::sqrt((J[0]*J[0])*(J[1]*J[1]));
    if ( cosa>1) cosa = 1; else if ( cosa<-1) cosa = -1;

    return std::acos(cosa) * nrm;
  }

  // Snap a ridge edge.
  void snap_blue_ridge_edge( const Halfedge *b,
			     const Halfedge *g,
			     Real *cb,
			     Real *cg,
			     Parent_type *t, 
			     Real tol=0.1) const;

  // Snap a ridge vertex
  void snap_blue_ridge_vertex( const Vertex *v,
			       Halfedge **g,
			       Parent_type *t, 
			       Point_2 *nc,
			       Real tol=0.1) const;

protected:
  Real abs( const Real x) const {  return fabs(x); }

  int sign( const Real x) const 
  { return ( x == Real(0.)) ? 0 : ( x > Real(0.) ? 1 : -1); }

  Real squared_norm( const Vector_3 &v) const { return v*v; }

  Real _eps_c;
  Real _eps_p;
  Real _eps_e;

private:
  // This class is neither copy constuctible nor assignable.
  // Explicitly disallow implicit definition of these functions.
  Self &operator=( const Self&);
};

RFC_END_NAME_SPACE

#endif // RFC_OVERLAY_PRIMITIVES_H
// EOF






