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
// $Id: Overlay_0d.C,v 1.15 2008/12/06 08:43:28 mtcampbe Exp $

#include "Overlay.h"
#include <cmath>
#include "../Rocmap/include/kdtree_d.h"

RFC_BEGIN_NAME_SPACE

class Point_3_ref : protected Point_3 {
public:
  Point_3_ref() : _v(NULL) {}
  Point_3_ref( const Point_3 &r, Overlay::Vertex *v=NULL) 
    : Point_3(r), _v(v) {}
  
  using Point_3::operator[];
  
  Overlay::Vertex *vertex() const { RFC_assertion( _v); return _v; }
  
private:
  Overlay::Vertex *_v;
};

typedef CGAL::Kdtree_interface<Point_3_ref>  KD_interface;
typedef CGAL::Kdtree_d<KD_interface>         KD_tree;
typedef KD_tree::Box                         box;

// Match 0-dimensional features.
void Overlay::
match_features_0() {
 
  const Real w2e_ratio = 1;
  const Real d2e_ratio_sq = 1;
  
  std::list<Feature_0>  &bf0 = B->flist_0();
  std::list<Feature_0>  &gf0 = G->flist_0();
  
  std::vector< Point_3_ref> gf0_1;
  gf0_1.reserve( gf0.size());
  for ( std::list<Feature_0>::iterator it=gf0.begin(); it!=gf0.end(); ++it)
    gf0_1.push_back( Point_3_ref( it->point(), it->vertex()));

  // Create a 3-dimensional KD-tree for the 0-features of green mesh
  KD_tree kdtree(3); kdtree.build( gf0_1);

  int dropped=0;
  // Query every 0-feature of the blue mesh in the KD-tree to find
  //      whether there is a unique corresponding one in the other mesh.
  for ( std::list<Feature_0>::iterator
	  it=bf0.begin(), iend=bf0.end(); it != iend; ++it) {
    Vertex *v = it->vertex();
    // Find the corresponding green feature of *it in the KD-tree
    const Point_3 &p = v->point();

    // Compute the tolerance.
    Real tol=HUGE_VAL;
    Halfedge *b=acc.get_halfedge(v), *b0=b;
    do {
      Real t = sq_length( *b);
      if ( t<tol) tol=t;
    } while ( (b=acc.get_next_around_destination( b))!=b0);
    tol = std::sqrt( tol);

    const Vector_3 vtol(tol*w2e_ratio, tol*w2e_ratio, tol*w2e_ratio);
    KD_tree::Box box( p-vtol, p+vtol, 3);

    std::vector< Point_3_ref>  outList; outList.reserve(2);
    kdtree.search( std::back_inserter( outList), box);

    // Find the closest corner in the list
    Real dist_min = HUGE_VAL;
    int k=-1;

    for ( int i=0, n = outList.size(); i<n; ++i) {
      Real d=(p-reinterpret_cast<const Point_3&>(outList[i])).squared_norm();
      if ( d <= dist_min) {
	dist_min = d; k=i;
      }
    }

    if ( dist_min<HUGE_VAL) {
      Halfedge *g = acc.get_halfedge( outList[k].vertex());
      if ( acc.is_border(g)) g = acc.get_opposite(g);
      else g = acc.get_next(g);

      Real s=tol*tol;
      Halfedge *h=g, *h0=h;
      do {
	Real t = sq_length( *h);
	if ( t<s) s=t;
      } while ( (h=acc.get_next_around_origin( h))!=h0);

      if ( dist_min < s*d2e_ratio_sq) {
	if ( verbose2) {
	  std::cout << "Matched\t blue vertex " 
		    << acc.get_pane(b)->get_index(b->origin())+1 
		    << " in pane " << acc.get_pane(b)->id() << " at " << p
		    << "\n  with\t green vertex " 
		    << acc.get_pane(g)->get_index(g->origin())+1 
		    << " in pane " << acc.get_pane(g)->id() << " at " 
		    << g->origin()->point() << std::endl;
	}

	Halfedge *b = acc.get_halfedge( v);
	if ( acc.is_border(b)) b = acc.get_opposite(b);
	else b = acc.get_next(b);
	
	// Create an inode for the o-feature
	INode *x = new INode();
	x->set_parent( b, Point_2(0,0), BLUE);
	x->set_parent( g, Point_2(0,0), GREEN);

	if ( B->snap_on_features()) {
	  // Update the coordinates for the blue point
	  Point_3 &pnt = const_cast<Point_3&>( acc.get_origin(b)->point());
	  pnt = acc.get_origin( g)->point();
	}

	insert_node_in_blue_edge( *x, b);
	continue;
      }
    }

    if ( verbose) {
      std::cout << "\nRocface Warning: Dropping blue corner vertex " 
		<< acc.get_pane(b)->get_index(b->origin())+1 
		<< " in pane " << acc.get_pane(b)->id() 
		<< " at " << p << std::endl;
    }

    // Remove the false blue 0-feature
    it = --B->remove_feature_0( it);
    ++dropped;
  }

  if ( verbose && dropped) {
    std::cout << "Dropped " << dropped << " corners in \"" << B->name() 
	      << "\" after feature matching" << std::endl;
  }

  dropped=0;
  // Remove the false green 0-features
  for ( std::list<Feature_0>::iterator
	  it=gf0.begin(), iend=gf0.end(); it != iend; ++it) {
    Vertex *v = it->vertex();
    if ( acc.get_inode( v) == NULL) {

      if ( verbose) {
	std::cout << "\nRocface Warning: Dropping green corner vertex " 
		  << acc.get_pane(v)->get_index(v)+1 
		  << " in pane " << acc.get_pane(v)->id() 
		  << " at " << v->point() << std::endl;
      }
      
      // Unmark the 0-features in the green window
      it = --G->remove_feature_0( it);
      ++dropped;
    }
  }

  if ( verbose && dropped) {
    std::cout << "Dropped " << dropped << " corners in \"" << G->name() 
	      << "\" after feature matching" << std::endl;
  }
}

RFC_END_NAME_SPACE






