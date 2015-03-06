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
// $Id: Triangulation.h,v 1.9 2008/12/06 08:43:27 mtcampbe Exp $

// This file implements the ear-removal algorithm for triangulating a
//   convex polygon described in "Computational Geometry in C".

#ifndef TRIANGULATION_H
#define TRIANGULATION_H

#include "rfc_basic.h"
#include "Tuple.h"
#include <CGAL/predicates_on_points_2.h>
#include <CGAL/Line_2_Line_2_intersection.h>
#include <CGAL/Segment_2_Segment_2_intersection.h>
#include <vector>
#include <list>

RFC_BEGIN_NAME_SPACE

//! Triangulating a convex polygon by ear-removal.
/*!
  This class implements the ear-removal algorithm described in the book 
  "Computational Geometry in C" for triangulating a convex polygon. 
  It triangulates a polygon by cutting off ears incrementally until only 
  one triangle is left. This method may generate triangles with poor quality,
  but quality is not a concern because we will compute integration instead 
  of differentiation.
*/
class Triangulation {
public:
  typedef CGAL::PointS2<Real>                       PointS2;
  typedef CGAL::Simple_cartesian<Real>::Segment_2   SegmentS2;

private:
  // A helper class for tracking information about a node of the polygon. 
  class Node {
    short int    _id;
    bool         _ear;
  public:
    //! A constructor.
    explicit Node( short int i) : _id(i), _ear(false) {}

    int  id()     const { return _id; }
    bool is_ear() const { return _ear; }
    void set_ear( bool b) { _ear = b; }
  };

  typedef std::list< Node>            Node_list;
  typedef Node_list::iterator         Node_iterator;
  typedef Node_list::const_iterator   Node_const_iterator;
public:
  typedef Three_tuple< int>           Triangle;    
                                      //!< Connectivity for a triangle.
  typedef std::vector< Triangle>      Connectivity;
                                      //!< Element connectivity table.

  //! A constructor.
  Triangulation() {}

  //! Main entry for triangulation.
  /*!
    \param ps the coordinates of the nodes.
    \param n  the number of nodes.
    \param tri the element connectivity for output.
   */
  void triangulate( const Point_2 *ps, int n, Connectivity *tri) {
    _pnts = ps;
    _nodes.clear();
    for ( int i=0; i<n; ++i) _nodes.push_back( Node(i));

    // Initialize ears
    for ( Node_iterator it=_nodes.begin(); it!=_nodes.end(); ++it) {
      it->set_ear( is_diagonal( get_prev(it), get_next(it)));
    }

    tri->resize(0); tri->reserve( n-2);
    // Loop through to remove ears.
    for ( ; n>3; --n) {
      // The inner loop searches for an ear
      for ( Node_iterator it=_nodes.begin(); it!=_nodes.end(); ++it) {
	if ( it->is_ear()) {
	  Node_iterator v1=get_prev( it), v0=get_prev( v1);
	  Node_iterator v3=get_next( it), v4=get_next( v3);

	  // Output the ear
	  tri->push_back( Triangle( v1->id(), it->id(), v3->id()));

	  // update neighbor vertices
	  v1->set_ear( is_diagonal( v0, v3));
	  v3->set_ear( is_diagonal( v1, v4));

	  // Cut off the ear
	  _nodes.erase( it);
	  break;
	}
      } 
    }
    RFC_assertion( n==3);

    tri->push_back( Triangle( _nodes.front().id(), (++_nodes.begin())->id(),
			      _nodes.back().id()));
    _nodes.clear();
  }

private:
  //============= Helper subroutines
  const PointS2 &get_point(const Node &v) const { return (PointS2&)_pnts[v.id()]; }

  Node_iterator get_next( Node_iterator i) {
    return ( ++i==_nodes.end()) ? _nodes.begin() : i;
  }
  Node_iterator get_prev(  Node_iterator i) {
    if ( i==_nodes.begin()) i=_nodes.end();
    return --i;
  }

  Node_const_iterator get_next( Node_const_iterator i) const {
    return ( ++i==_nodes.end()) ? _nodes.begin() : i;
  }
  Node_const_iterator get_prev( Node_const_iterator i) const {
    if ( i==_nodes.begin()) i=_nodes.end();
    return --i;
  }

  bool is_diagonal( Node_const_iterator u, Node_const_iterator v) const {
    return in_cone( u, v) && in_cone( v, u) && is_diagonalie( u, v);
  }

  bool in_cone( Node_const_iterator u, Node_const_iterator v) const {
    Node_const_iterator a0 = get_prev( u);
    Node_const_iterator a1 = get_next( u);
    
    if ( !CGAL::rightturn( get_point(*u), get_point(*a1), get_point(*a0))) {
      return ( CGAL::leftturn(get_point(*u), get_point(*v), get_point(*a0)) &&
	       CGAL::leftturn(get_point(*v), get_point(*u), get_point(*a1)));
    }
    
    return ( ! CGAL::rightturn(get_point(*v), get_point(*u), get_point(*a1)) &&
	     ! CGAL::rightturn(get_point(*a0), get_point(*u), get_point(*v)));
  }

  bool is_diagonalie( Node_const_iterator u, Node_const_iterator v) const {
    for ( Node_const_iterator i=_nodes.begin(); i!=_nodes.end(); ++i) {
      Node_const_iterator j = get_next(i);

      if ( u!=i && u!=j && v!=i && v!=j &&
	   CGAL::do_intersect( SegmentS2( get_point(*u), get_point(*v)),
			       SegmentS2( get_point(*i), get_point(*j))))
	return false;
    }
    return true;
  }

private: // Data members
  const Point_2 *_pnts;
  Node_list      _nodes;

};

RFC_END_NAME_SPACE

#endif






