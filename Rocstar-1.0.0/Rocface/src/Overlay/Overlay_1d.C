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
// $Id: Overlay_1d.C,v 1.17 2008/12/06 08:43:28 mtcampbe Exp $

#include "Overlay.h"
#include <cstdio>
#include <iterator>

RFC_BEGIN_NAME_SPACE

// Find the next vertex on b.
INode *Overlay::
project_next_vertex( Halfedge *b, Halfedge *g) {
  const RFC_Pane_overlay *gpane = acc.get_pane(g);
  const Vertex *borg = b->origin(), *bdst = b->destination();
  const Vertex *gorg = g->origin(), *gdst = g->destination();

  double eps_e;

  if ( B->snap_on_features()) eps_e = 5.e-3; // Increase the threshold for RSRM
  else eps_e = Overlay::eps_e;

  Real s = op.project_green_feature( gpane->get_normal( g, gdst), 
				     gpane->get_tangent( g, gdst),
				     borg->point(), bdst->point(), 
				     gdst->point(), eps_e);
  INode *x = new INode();
  if ( s < 1) {
    if ( s < 0) s = 0; // Adjust the origin of the blue edge.
    acc.set_parent( x, b, Point_2( s,0), BLUE);
    acc.set_parent( x, g, Point_2( 1,0), GREEN);
  }
  else if ( s>= 1) {
    // The blue parent is bdst, so we locate a non-border 
    // blue halfedge originated from b.
    acc.set_parent( x, b, Point_2( 1,0), BLUE);
    // Project green vertex onto blue edge
    Real t;
    if ( s > 1) {
      // Project the blue vertex onto the green halfedge
      t=op.project_blue_feature( gpane->get_normal(g, gorg), 
				 gpane->get_normal(g, gdst),
				 gpane->get_tangent(g, gorg),
				 gpane->get_tangent(g, gdst),
				 gorg->point(), gdst->point(),
				 bdst->point(), eps_e);
      if ( t>1) t=1;
    }
    else
      t = 1;
    acc.set_parent( x, g, Point_2(t,0), GREEN);

    if ( B->snap_on_features()) {
      // Update the coordinates for the blue point
      Point_3 &pnt = const_cast<Point_3&>(bdst->point());
      pnt = gorg->point()+t*(gdst->point()-gorg->point());
    }
  }
  
  // Insert the new edge into the blue edge.
  insert_node_in_blue_edge( *x, b); 

  return x;
}

// Compare whether two bounding boxes are the same within certain tolerance
static bool equal( const Bbox_3 &b1, const Bbox_3 &b2, Real tol_r) {
  Bbox_3 u = b1+b2;
  Real tol_a = std::max(std::max(u.xmax()-u.xmin(), u.ymax()-u.ymin()),
			u.zmax()-u.zmin()) * tol_r;
  return ( std::max( std::fabs(b1.xmin()-b2.xmin()), 
		     std::fabs(b1.xmax()-b2.xmax())) <= tol_a &&
	   std::max( std::fabs(b1.ymin()-b2.ymin()), 
		     std::fabs(b1.ymax()-b2.ymax())) <= tol_a &&
	   std::max( std::fabs(b1.zmin()-b2.zmin()), 
		     std::fabs(b1.zmax()-b2.zmax())) <= tol_a);
}

int Overlay::
overlay_features_1() {
  Feature_list_1 &lf_b = B->flist_1(), &lf_g = G->flist_1();

  bool determined_direction=false;
  int  is_opp=-1;
  const Real bbox_tol = 1.e-1, d2e_ratio_sq=0.5, d2e_ratio_sq_l=1.e-4;

  int dropped=0;

  // Loop through blue features
  for ( Feature_list_1::iterator it_f_b = lf_b.begin(), iend_f_b=lf_b.end();
	it_f_b != iend_f_b; ++it_f_b) {
    std::cout << "\nProcessing blue ridge with bounding box              "
	      << it_f_b->bbox << std::endl;
    Feature_1 *f_b = &*it_f_b;
    Feature_1::iterator it_b_mid = it_f_b->begin();
    // Find a vertex in middle of the blue feature
    std::advance( it_b_mid, (f_b->size()-1)/2);
    
    Halfedge *b = *it_b_mid;
    Vertex *bdst = acc.get_destination(b);
    const Point_3 &bpdst = bdst->point();
    INode *bnode=acc.get_inode( bdst);
    INode *bn_frt=acc.get_inode( acc.get_origin(it_f_b->front()));
    INode *bn_bck=acc.get_inode( acc.get_destination(it_f_b->back()));

    Feature_list_1::iterator  f_g = lf_g.end();
    Feature_1::iterator it_g_mid;
    Real  param=-HUGE_VAL, sqd=HUGE_VAL;

    // Loop through green 1-features
    for ( Feature_list_1::iterator it_f_g = lf_g.begin(), iend_f_g=lf_g.end();
	  it_f_g != iend_f_g; ++it_f_g) {
      // Pre-filter out severely mismatching ones
      if ( bn_frt && bn_frt!=acc.get_inode( acc.get_origin(it_f_g->front())) &&
	   bn_frt != acc.get_inode( acc.get_destination(it_f_g->back())) ||
	   bn_bck && bn_bck!=acc.get_inode( acc.get_origin(it_f_g->front())) &&
	   bn_bck != acc.get_inode( acc.get_destination(it_f_g->back())) ||
	   !equal( it_f_b->bbox, it_f_g->bbox, bbox_tol))
	continue;

      std::cout << "\tComparing with green ridge with bounding box "
		<< it_f_g->bbox << std::endl;
      if ( bnode != NULL) {
	f_g = it_f_g; RFC_assertion( bn_frt && bn_bck == bnode);
	if ( bnode == acc.get_inode( acc.get_origin(it_f_g->front()))) 
	{ it_g_mid = it_f_g->begin(); param = 0.; }
	else 
	{ it_g_mid = --it_f_g->end(); param = 1.; }
	break;
      }
      // Identify the green edge closest to the blue point
      for ( Feature_1::iterator tmp_it_g=it_f_g->begin(), iend=it_f_g->end();
	    tmp_it_g != iend; ++tmp_it_g) {
	Halfedge *g = *tmp_it_g;
	const Vertex *gorg = g->origin(), *gdst = g->destination();
	const Point_3 &gporg = gorg->point(), &gpdst = gdst->point();

	Real gsqlen = (gpdst-gporg).squared_norm(), sd=gsqlen*d2e_ratio_sq;
	if ( (bpdst-gporg).squared_norm()<sd || 
	     (bpdst-gpdst).squared_norm()<sd) {
	  const RFC_Pane_overlay *gpane = acc.get_pane(g);
	  Real t = op.project_blue_feature( gpane->get_normal(g, gorg),
					    gpane->get_normal(g, gdst),
					    gpane->get_tangent(g, gorg),
					    gpane->get_tangent(g, gdst),
					    gporg, gpdst, 
					    bpdst, eps_e);
	  if ( t>=0 && t <= 1) {
	    Point_3 gp = gporg+t*(gpdst-gporg);
	    Real sd = (gp-bpdst).squared_norm();
	    if (sd < d2e_ratio_sq*(bpdst-b->origin()->point()).squared_norm() && sd<sqd) {
	      f_g = it_f_g; it_g_mid = tmp_it_g; param=t; sqd = sd; 
	      if ( sqd < d2e_ratio_sq_l*gsqlen) break;
	    }
	  }
	}
      }
    }

    // If did not find a match, remove the blue feature.
    if ( f_g == lf_g.end()) {
      std::cout << "Warning: Could not find match for the blue ridge!" << std::endl;
 
       it_f_b = --lf_b.erase( it_f_b);
      ++dropped;
      continue;
    }
    else {
      std::cout << "\tMatched with green ridge with bounding box   "
		<< f_g->bbox << std::endl;
    }

    // Determine the orientation of the blue and green features and
    // reverse the green feature if they have different orientation
    Vector_3 dir_b = acc.get_pane(b)->get_tangent( b,b->destination());
    Halfedge *g=*it_g_mid;
    Vector_3 dir_g = acc.get_pane(g)->get_tangent( g,g->destination());
    bool dir_match = dir_b*dir_g > 0;

    // If no inode has yet been created at the blue vertex, create one now.
    if ( bnode == NULL) {
      // Create a new inode for the vertex
      bnode = new INode();
      acc.set_parent( bnode, b, Point_2(1,0), BLUE);
      acc.set_parent( bnode, *it_g_mid, Point_2( param, 0), GREEN);

      insert_node_in_blue_edge( *bnode, b);

      if ( !determined_direction) {
	const Halfedge *bopp = acc.get_opposite( b);
	const Halfedge *g=*it_g_mid, *gopp = acc.get_opposite( g);
	is_opp = ((acc.get_pane(b)->get_normal( b, b->destination())+
		   acc.get_pane(bopp)->get_normal( bopp, bopp->origin())) *
		  (acc.get_pane(g)->get_normal( g, g->destination()) +
		   acc.get_pane(gopp)->get_normal( gopp, gopp->origin())) < 0);
	determined_direction = true;
      }
    }

    // Project the blue vertex to the green 1-feature.
    // Starting from the blue vertex, move forward along the blue 1-feature
    // and intersect with the normal planes of the green 1-features to
    // locate all the inodes.
    INode *x = bnode;
    Feature_1::iterator it_g=it_g_mid;
    if ( param == 0. && dir_match) 
    { if ( it_g == f_g->begin()) it_g = f_g->end(); --it_g; }
    else if ( param == 1. && !dir_match) 
    { ++it_g; if ( it_g == f_g->end()) it_g = f_g->begin(); }

    for (Feature_1::iterator it_b=it_b_mid, iend=--f_b->begin();
	 it_b!=iend; --it_b) {
      // Loop until the target of the green halfedge projects beyond the
      //    halfedge.
      Halfedge *b=acc.get_opposite(*it_b);  
      RFC_assertion( acc.get_pane(b)->is_feature_1(b));
      INode *idst= acc.get_inode(acc.get_destination(b));

      for (;;) {
	Halfedge *g = dir_match?acc.get_opposite(*it_g):*it_g;
	RFC_assertion( acc.get_pane(g)->is_feature_1( g));

	if ( x->parent_type( BLUE)==PARENT_VERTEX &&
	     acc.get_origin(x->halfedge( BLUE))==acc.get_destination(b) ||
	     idst && get_edge_parent(*x,*idst,GREEN)!=Host_face(0,PARENT_NONE))
	  break;
	x = project_next_vertex(b, g);

	if ( x->nat_coor( GREEN)[0] == 0. && 
	     acc.get_origin(x->halfedge( GREEN)) != acc.get_origin(g)) { 
	  // Done with the green edge
	  if ( dir_match) 
	  { if ( it_g == f_g->begin()) it_g = f_g->end(); --it_g; }
	  else 
	  { ++it_g; if ( it_g == f_g->end()) it_g = f_g->begin(); }
	}
	else
	  break;
      }
    }
    x = bnode;
    it_g=it_g_mid;
    if ( param == 0. && !dir_match) 
    { if ( it_g == f_g->begin()) it_g = f_g->end(); --it_g; }
    else if ( param == 1. && dir_match) 
    { ++it_g; if ( it_g == f_g->end()) it_g = f_g->begin(); }

    for (Feature_1::iterator it_b=it_b_mid, iend=f_b->end(); ++it_b!=iend; ) {
      // Loop until the target of the green halfedge projects beyond the
      //    halfedge.
      Halfedge *b=*it_b;  RFC_assertion( acc.get_pane(b)->is_feature_1(b));
      INode *idst= acc.get_inode(acc.get_destination(b));

      for (;;) {
	Halfedge *g = dir_match?*it_g:acc.get_opposite(*it_g);
	RFC_assertion( acc.get_pane(g)->is_feature_1( g));

	if ( x->parent_type( BLUE)==PARENT_VERTEX &&
	     acc.get_origin(x->halfedge( BLUE))==acc.get_destination(b) ||
	     idst && get_edge_parent(*x,*idst,GREEN)!=Host_face(0,PARENT_NONE))
	  break;
	x = project_next_vertex(b, g);

	if ( x->nat_coor( GREEN)[0] == 0. && 
	     acc.get_origin(x->halfedge( GREEN)) != acc.get_origin(g)) { 
	  // Done with the green edge
	  if ( dir_match) 
	  { ++it_g; if ( it_g == f_g->end()) it_g = f_g->begin(); }
	  else 
	  { if ( it_g == f_g->begin()) it_g = f_g->end(); --it_g; }
	}
	else
	  break;
      }
    }
  }

  if ( dropped) 
    std::cout << "Ignored " << dropped << " 1-features in \"" << B->name() 
	      << "\" during matching" << std::endl;

  if ( (dropped=lf_g.size()-lf_b.size()))
    std::cout << "Ignored " << dropped << " 1-features in \"" << G->name() 
	      << "\" after matching" << std::endl;
  
  return is_opp;
}

RFC_END_NAME_SPACE






