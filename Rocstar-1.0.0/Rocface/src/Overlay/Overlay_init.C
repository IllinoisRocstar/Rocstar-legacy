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
// $Id: Overlay_init.C,v 1.12 2008/12/06 08:43:28 mtcampbe Exp $

//==========================================================
// The implementation of the overlay algorithm.
//
// Author: Xiangmin Jiao
// Last modified:   04/06/2001
//
//==========================================================

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <queue>
#include <set>
#include <utility>
#include "Timing.h"
#include "Overlay.h"

RFC_BEGIN_NAME_SPACE

// The initialization for the overlay algorithm. It locates the
//     green parent of a blue vertex and create an inode for it.
//     This initialization step takes linear time.
// Returns NULL if unsuccessful. Otherwise, creates a new INode.
INode *Overlay::overlay_init() {
  INode *v=NULL;

  // get an unmarked halfedge from the blue mesh and take its origin as x.
  Halfedge *b=B->get_an_unmarked_halfedge(), *g;
  if ( b==NULL) return NULL;

  Vertex *x = b->origin();   RFC_assertion( !acc.is_border(x));

  // locate the green parent of x.
  Parent_type t;
  Point_2    nc;
  get_green_parent( x, b, &g, &t, &nc);
  RFC_assertion ( t != PARENT_NONE && g != NULL);

  // create a new inode for x
  v = new INode();
  v->set_parent( b, Point_2(0,0), BLUE);
  v->set_parent( g, nc, GREEN);

  if ( verbose2) {
    std::cout << "\nFound a seed at blue vertex (" 
	      << acc.get_pane(x)->get_point(x) << ") \nin green ";
    const RFC_Pane_overlay *gpane=acc.get_pane(g);
    switch ( t) {
    case PARENT_VERTEX:
      std::cout << "vertex (" << gpane->get_point( g->origin()) << ")\n";
      break;
    case PARENT_EDGE:
      std::cout << "edge (" << gpane->get_point( g->origin()) << "), (" 
		<< gpane->get_point( g->destination()) << ")\n";
      break;
    case PARENT_FACE: {
      std::cout << "face ";
      Halfedge *gt = g;
      do {
	std::cout << "\t(" << gpane->get_point( gt->origin()) << "), ";
      }	while ( (gt = gt->next()) != g);
      std::cout << std::endl;
      break;
    }
    default:
      RFC_assertion( false);
    }
    std::cout << std::endl;
  }

  return v;
}

// Get the green parent of a vertex v. This subroutine takes
//   linear time in terms of the number of vertices in G.
void Overlay::
get_green_parent( const Vertex *v,
		  const Halfedge *b,
		  Halfedge     **h_out,
		  Parent_type  *t_out,
		  Point_2      *nc) 
{
  const Point_3 &p = acc.get_pane(v)->get_point(v);
  Real  sq_dist = 1.e30;
  Vertex *w = NULL;

  //=================================================================
  // Locate the closest green vertex
  //=================================================================
  std::vector<RFC_Pane_overlay*>   ps;
  G->panes( ps);
  // Loop through all the panes of G
  for ( std::vector<RFC_Pane_overlay*>::iterator 
	  pit=ps.begin(); pit != ps.end(); ++pit) {
    RFC_Pane_overlay &pane = **pit;
    // loop through all the green vertices that are complete
    for ( HDS::Vertex_iterator vit=pane.hds().vertices_begin(); 
	  vit!=pane.hds().vertices_end(); ++vit) 
      if ( vit->halfedge() && vit->halfedge()->destination() == &*vit) {
	// if the distance is closer than previous ones, save it
	Real sq_d = ( p- acc.get_pane(&*vit)->get_point(&*vit)).squared_norm();
	if ( sq_d < sq_dist) { sq_dist = sq_d;  w = &*vit; }
      }
  }

  if ( w == NULL) return; 

  RFC_assertion( acc.is_primary( w)); // Because we start from smaller ids.
  // Let h be a nonborder incident halfedge of w
  Halfedge *h = acc.get_halfedge( w);
  h = !acc.is_border( h) ? acc.get_next( h) : acc.get_opposite( h);

  // We perform breadth-first search starting from h
  std::queue< Halfedge*> q;
  std::list< Halfedge*>  hlist;

  q.push( h);
  // Mark the halfedges in the same face as h
  Halfedge *h0 = h;
  do { RFC_assertion( !acc.marked(h)); acc.mark( h); hlist.push_back( h); } 
  while ( (h=acc.get_next(h)) != h0);

  Vector_3 vec(0.,0.,0.);
  while ( !q.empty()) {
    h = q.front(); q.pop();
    
    *t_out = PARENT_NONE; *h_out=h;
    if ( op.project_onto_element( p, h_out, t_out, vec, nc, eps_e) &&
	 std::abs(acc.get_normal(b)* acc.get_normal(h)) >= 0.6)
      break;

    // Insert the incident faces of h into the queue
    h0 = h;
    do {
      Halfedge *hopp=acc.get_opposite(h);
      if ( !acc.is_border(hopp) && ! acc.marked( hopp)) {
	Halfedge *h1 = hopp;
	q.push( h1);
	do { 
	  acc.mark(h1); hlist.push_back( h1); 
	} while ((h1=acc.get_next(h1))!=hopp);
      }
    } while ( (h=acc.get_next(h)) != h0);
  }
  // Unmark the halfedges
  while ( !hlist.empty()) 
  { acc.unmark( hlist.front()); hlist.pop_front(); }

  // If v is too far from p_out, return NULL.
  vec = op.get_point( *h_out, *nc) - p;
  if ( vec * vec > sq_length( **h_out) + sq_length( *acc.get_halfedge(v)))  {
    *h_out = NULL; *t_out = PARENT_NONE;
  }
  else {
    // Determine whether the two surfaces are facing each other
    is_opposite = ( acc.get_pane(b)->get_normal( b, v) * 
		    op.get_face_normal( *h_out, *nc)) < 0;
  }
}

RFC_END_NAME_SPACE






