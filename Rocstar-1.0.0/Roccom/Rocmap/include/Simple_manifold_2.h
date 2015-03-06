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
// $Id: Simple_manifold_2.h,v 1.9 2008/12/06 08:43:20 mtcampbe Exp $

//=======================================================================
//  This file defines a data structures for accessing a pane of a surface
//  mesh. It mimics the halfedge data structure.
//
//  Author: Xiangmin Jiao
//  Last modified: July 18, 2003
//=======================================================================

#ifndef __SIMPLE_MANIFOLD_2_H_
#define __SIMPLE_MANIFOLD_2_H_

#include "mapbasic.h"
#include "roccom_devel.h"
#include <cassert>

MAP_BEGIN_NAMESPACE

/** The ID of a facet (edge in 2D and face in 3D) encodes an element ID
 *  and the local facet's ID internal to the element. For internal facets,
 *  element ID starts with 1, and internal ID starts with 0 and must be 
 *  smaller than 15. For border facets, the element ID correspondes to the
 *  numbering of border facets between 1 and the number of border facets, and
 *  the local edge ID is BndID=15.
 */
class Facet_ID {
public:
  enum { nDigits=4, BndID=15 };

  Facet_ID( ) : _code(0) {}
  Facet_ID( int eid, char lid) : _code( (eid<<nDigits)+(BndID&lid)) {}

  /// Element ID of the halfedge.
  int  eid() const { return _code>>nDigits; }
  /// Local edge ID of the halfedge within its element.
  char lid() const { return _code&BndID; }

  /// Determines whether the facet is on border.
  bool is_border() const { return lid()==BndID; }

  /// Comparison operators
  bool operator==( const Facet_ID e) const 
  { return _code==e._code; }
  bool operator!=( const Facet_ID e) const 
  { return _code!=e._code; }
  bool operator<( const Facet_ID e) const 
  { return lid()<e.lid() || lid()==e.lid() && _code<e._code; }
private:
  int         _code;
};

/** Provides a data structure accessing nodes, elements, and edges in a 
 *  pane, in a manner similar to the halfedge data structure, assuming 
 *  the elements form a 2-manifold. It provides interfaces for accessing
 *  the real part, the ghost part, or the whole pane.
 */
class Simple_manifold_2 {
public:
  typedef Facet_ID Edge_ID;

  /// Default constructors
  Simple_manifold_2() 
    : _pane(NULL), _is_str(0), _with_ghost(0), _nspe(0),
      _maxsize_real_nodes(-1), _maxsize_real_elmts(-1), 
      _size_real_borders(0), _size_ghost_borders(0), _size_rg_borders(0) {}

  /// Constructors
  explicit Simple_manifold_2( const COM::Pane *p, 
			      const Simple_manifold_2 *parent=NULL,
			      bool with_ghost=true)
  { init(p, parent, with_ghost); }

  /// Obtain a const pointer to the pane
  const COM::Pane *pane() const { return _pane; }

  /** \name Size of nodes
   * \{
   */
  /// Number of nodes of the pane
  int size_of_nodes()    const { return _pane->size_of_nodes(); }

  /// Maximum number of nodes allowed in the pane.
  int maxsize_of_nodes() const { return _pane->maxsize_of_nodes(); }

  /// Number of real nodes of the pane
  int size_of_real_nodes()    const { return _pane->size_of_real_nodes(); }

  /// Maximum number of real nodes allowed in the pane.
  int maxsize_of_real_nodes() const { return _maxsize_real_nodes; }

  /// Number of ghost nodes of the pane
  int size_of_ghost_nodes()    const { return _pane->size_of_ghost_nodes(); }

  /// Maximum number of ghost nodes allowed in the pane.
  int maxsize_of_ghost_nodes() const { return _pane->maxsize_of_ghost_nodes();}
  //\}

  /** \name Size of faces
   */ 
  /// Number of elements of the pane
  int size_of_elements() const { return _pane->size_of_elements(); }

  /// Maximum number of elements allowed in the pane
  int maxsize_of_elements()    const { return _pane->maxsize_of_elements(); }

  /// Number of real elements of the pane
  int size_of_real_elements()    const { return _pane->size_of_real_elements(); }

  /// Maximum number of real elements allowed in the pane
  int maxsize_of_real_elements()    const { return _maxsize_real_elmts; }

  /// Number of ghost elements of the pane
  int size_of_ghost_elements()    const { return _pane->size_of_ghost_elements(); }

  /// Maximum number of ghost elements allowed in the pane
  int maxsize_of_ghost_elements()    const { return _pane->maxsize_of_ghost_elements(); }

  /// Number of elements of the pane
  int size_of_faces() const { return _pane->size_of_elements(); }

  /// Maximum number of elements allowed in the pane
  int maxsize_of_faces() const { return _pane->maxsize_of_elements(); }

  /// Number of real elements of the pane
  int size_of_real_faces() const { return _pane->size_of_real_elements(); }

  /// Maximum number of real elements allowed in the pane
  int maxsize_of_real_faces() const { return _pane->maxsize_of_real_elements(); }

  /// Number of ghost elements of the pane
  int size_of_ghost_faces() const { return _pane->size_of_ghost_elements(); }

  /// Maximum number of ghost elements allowed in the pane
  int maxsize_of_ghost_faces() const { return _pane->maxsize_of_ghost_elements(); }

  /// Total number of triangles of the pane
  int size_of_triangles() const;

  /// Maximum number of triangles allowed in the pane.
  int maxsize_of_triangles() const;

  /// Number of real triangles of the pane
  int size_of_real_triangles() const;

  /// Maximum number of real triangles allowed in the pane
  int maxsize_of_real_triangles() const;

  /// Number of ghost triangles of the pane
  int size_of_ghost_triangles() const;

  /// Maximum number of ghost triangles allowed in the pane
  int maxsize_of_ghost_triangles() const;

  /// Total number of quadrilaterals of the pane
  int size_of_quadrilaterals() const;

  /// Maximum number of quadrilaterals allowed in the pane
  int maxsize_of_quadrilaterals() const;

  /// Number of real quadrilaterals of the pane
  int size_of_real_quadrilaterals() const;

  /// Maximum number of real quadrilaterals allowed in the pane
  int maxsize_of_real_quadrilaterals() const;

  /// Number of ghost quadrilaterals of the pane
  int size_of_ghost_quadrilaterals() const;

  /// Maximum number of ghost quadrilaterals allowed in the pane
  int maxsize_of_ghost_quadrilaterals() const;
  //\}
  
  /** \name Size of edges
   */ 
 /// Number of halfedges of the pane
  int size_of_halfedges() const;
  
  /// Number of real halfedges of the pane
  int size_of_real_halfedges() const;
  
  /// Number of ghost halfedges of the pane
  int size_of_ghost_halfedges() const;
  
  /// Number of edges of the pane
  int size_of_edges() const;

  /// Number of real edges of the pane
  int size_of_real_edges() const;

  /// Number of ghost edges of the pane
  int size_of_ghost_edges() const;
  //\}

  /** \name Size of border edges and nodes
   */
  /// Number of border edges of the whole pane
  int size_of_border_edges() const 
  { return _size_real_borders + _size_ghost_borders; }

  /// Number of border edges of the real part of the pane
  int size_of_real_border_edges() const
  { return _size_real_borders + _size_rg_borders; }

  /// Number of border edges of the ghost part of the pane
  int size_of_ghost_border_edges() const
  { return _size_ghost_borders + _size_rg_borders; }

  /// Number of isolated nodes
  int size_of_isolated_nodes() const
  { return _isovIDs.size(); }
  //\}

  /** \name Opposite edges 
   * \{
   */
  /// Get the ID of the opposite real edge of a given real or border edge.
  /// If its opposite is a border edge, it returns a border edge ID.
  Edge_ID get_opposite_real_edge( const Edge_ID &eID) const {
    COM_assertion( eID.is_border() || is_real_element( eID.eid()));

    if ( eID.is_border())
      return get_opposite_real_edge_border(eID);
    else 
      return get_opposite_real_edge_interior(eID);
  }

  /// Get the ID of the opposite ghost edge of a given real or border edge.
  /// If its opposite is a border edge, it returns a border edge ID.
  Edge_ID get_opposite_ghost_edge( const Edge_ID &eID) const { 
    COM_assertion( eID.is_border() || is_ghost_element( eID.eid()));

    if ( eID.is_border()) 
      return get_opposite_ghost_edge_border(eID);
    else 
      return get_opposite_ghost_edge_interior(eID);
  }

  /// Get the ID of the opposite edge of a given interior or border edge.
  /// If its opposite is a border edge, it returns a border edge ID.
  Edge_ID get_opposite_edge( const Edge_ID &eID) const {

    if ( eID.is_border()) 
      return get_opposite_edge_border(eID);
    else 
      return get_opposite_edge_interior(eID);
  }
  //\}

  /** \name Previous and next edges 
   * \{
   */
  /// Get the ID of the previous edge of the element or along the boundary
  Edge_ID get_prev_edge( const Edge_ID &eID) const {
    if ( !eID.is_border()) 
      return get_prev_edge_interior( eID);
    else {
      Edge_ID oeID = get_opposite_edge( eID);
      while ( !oeID.is_border()) 
	oeID = get_opposite_edge_interior( get_next_edge_interior( oeID));
      return oeID;
    }
  }

  /// Get the ID of the previous real edge of the element or along the boundary
  Edge_ID get_prev_real_edge( const Edge_ID &eID) const {
    if ( !eID.is_border()) 
      return get_prev_real_edge_interior( eID);
    else {
      Edge_ID oeID = get_opposite_real_edge( eID);
      while ( !oeID.is_border()) 
	oeID = get_opposite_real_edge_interior
	  ( get_next_real_edge_interior( oeID));
      return oeID;
    }
  }

  /// Get the ID of the previous ghost edge of the element or along the boundary
  Edge_ID get_prev_ghost_edge( const Edge_ID &eID) const {
    if ( !eID.is_border()) 
      return get_prev_ghost_edge_interior(eID);
    else {
      Edge_ID oeID = get_opposite_ghost_edge( eID);
      while ( !oeID.is_border()) 
	oeID = get_opposite_ghost_edge_interior
	  ( get_next_ghost_edge_interior( oeID));
      return oeID;
    }
  }

  /// Get the ID of the next edge of the element or along the boundary
  Edge_ID get_next_edge( const Edge_ID &eID) const {
    if ( !eID.is_border()) 
      return get_next_edge_interior( eID);
    else {
      Edge_ID oeID = get_opposite_edge_border( eID);
      while ( !oeID.is_border()) 
	oeID = get_opposite_edge_interior( get_prev_edge_interior( oeID));
      return oeID;
    }
  }
  
  /// Get the ID of the next real edge of the element or along the boundary
  Edge_ID get_next_real_edge( const Edge_ID &eID) const {
    if ( !eID.is_border()) 
      return get_next_real_edge_interior( eID);
    else {
      Edge_ID oeID = get_opposite_real_edge( eID);
      while ( !oeID.is_border()) 
	oeID = get_opposite_real_edge_interior
	  ( get_prev_real_edge_interior( oeID));
      return oeID;
    }
  }
  
  /// Get the ID of the next ghost edge of the element or along the boundary
  Edge_ID get_next_ghost_edge( const Edge_ID &eID) const {
    if ( !eID.is_border()) 
      return get_next_ghost_edge_interior( eID);
    else {
      Edge_ID oeID = get_opposite_ghost_edge( eID);
      while ( !oeID.is_border()) 
	oeID = get_opposite_ghost_edge_interior
	  ( get_prev_ghost_edge_interior( oeID));
      return oeID;
    }
  }
  //\}
  
  /** \name Nodes and edges connectivity
   * \{
   */
  /// Get the ID of the origin of a given edge. If the edge is between
  /// real and ghost, then consider the border edge as part of real.
  int get_origin( Edge_ID eID, Element_node_enumerator *ene_in=NULL) const  {
    if ( eID.is_border()) {
      Edge_ID opp;
      if (is_real_border_edge( eID)) opp = get_opposite_real_edge_border( eID);
      else opp = get_opposite_ghost_edge_border( eID);

      Element_node_enumerator ene( _pane, opp.eid());
      return ene[opp.lid()+1==ene.size_of_edges()?0:opp.lid()+1];
    }
    else {
      if ( ene_in) 
	return (*ene_in)[eID.lid()];
      else 
	return Element_node_enumerator( _pane, eID.eid())[eID.lid()];
    }
  }

  /// Get the ID of the destination of a given edge
  int get_destination( Edge_ID eID, Element_node_enumerator *ene_in=NULL) const {
    if ( eID.is_border()) {
      Edge_ID opp;
      if (is_real_border_edge( eID)) opp = get_opposite_real_edge_border( eID);
      else opp = get_opposite_ghost_edge_border( eID);
      
      Element_node_enumerator ene( _pane, opp.eid());
      return ene[opp.lid()];
    }
    else {
      if ( ene_in)
	return (*ene_in)[eID.lid()+1==ene_in->size_of_edges()?0:eID.lid()+1];
      else {
	Element_node_enumerator ene( _pane, eID.eid());
	return ene[eID.lid()+1==ene.size_of_edges()?0:eID.lid()+1];
      }
    }
  }
  
  /// Get the ID of an incident edge within the pane of a given node
  /// that originated from the node. If the vertex is on the pane 
  /// boundary, it will return a border edge. For isolated nodes,
  /// it returns an ID similar to that for a border edge, except that
  /// the element ID given will be the index of the isolated node 
  /// (starting from 1) plus the total number of border edges.
  Edge_ID get_incident_edge( int vID) const { 
    Edge_ID eID;
    if ( _is_str || !is_ghost_node( vID))
      eID = _ieIDs_real_or_str[ vID-1]; 
    else
      eID = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];

    // If the edge is between real and ghost, then return the real edge.
    if ( eID.is_border() && eID.eid()>_size_real_borders && 
	 eID.eid()<=_size_real_borders+_size_rg_borders) {
      eID = _oeIDs_real_or_str[ get_edge_index(_beIDs[eID.eid()-1])];
      COM_assertion( get_origin( eID)==vID);
    }
    return eID;
  }

  /// Get the ID of an incident real edge within the pane of a given node.
  /// The incident edge may encode an isolated node.
  Edge_ID get_incident_real_edge( int vID) const {
    COM_assertion( is_real_node( vID));
    Edge_ID eID = _ieIDs_real_or_str[ vID-1]; 

    if ( eID.is_border() && eID.eid()<=int(_beIDs.size()) &&
	 eID.eid()>_size_real_borders+_size_rg_borders) {
      // If vID is between real and ghost, and on the ghost boundary,
      // get the border edge between real and ghost.
      Edge_ID eID0 = eID = get_opposite_ghost_edge_border( eID);
      while ( (eID = get_opposite_ghost_edge_interior
	       ( get_next_ghost_edge_interior(eID))) != eID0 &&
	      !is_real_border_edge( eID));
      COM_assertion_msg( eID.is_border() && get_origin( eID)==vID,
			 "No real edge incident on given vertex"); 
    }

    return eID;
  }

  /// Get the ID of an incident real edge within the pane of a given node
  /// The incident edge may encode an isolated node.
  Edge_ID get_incident_ghost_edge( int vID) const {
    bool isghost = is_ghost_node( vID);
    Edge_ID eID;
    if ( _is_str || !isghost)
      eID = _ieIDs_real_or_str[ vID-1]; 
    else
      eID = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];
    if ( isghost) return eID;

    if ( eID.is_border() && eID.eid()<=_size_real_borders) {
      // If vID is between real and ghost, and on the real boundary,
      // get the border edge between real and ghost.
      eID = get_opposite_real_edge_border( eID);
      Edge_ID eID0 = eID;
      while ( (eID = get_opposite_real_edge_interior
	       ( get_next_real_edge_interior(eID))) != eID0 &&
	      !is_ghost_border_edge( eID));
      COM_assertion_msg( eID.is_border(),
			 "No ghost edge incident on given vertex"); 
    }

    return eID;
  }
  //\}

  /** \name Border nodes and edges
   * \{
   */
  /// Is a given edge on the boundary of the whole pane?
  bool is_border_edge( const Edge_ID &eID) const { 
    if ( !eID.is_border()) return false;
    if ( eID.eid()>int(_beIDs.size())) return false;

    return !_with_ghost || eID.eid() <= _size_real_borders ||
      eID.eid() > _size_real_borders+_size_rg_borders;
  }

  /// Determine whether a given edge is a border edge of real part of the
  /// whole pane (on physical border).
  bool is_pure_real_border_edge( const Edge_ID & eID) const {
    if ( !eID.is_border()) return false;

    return !_with_ghost || eID.eid() <= _size_real_borders;
  }

  /// Determine whether a given edge is a border edge of real part of the
  /// pane (either on physical border or between real and ghost elements)
  bool is_real_border_edge( const Edge_ID & eID) const {
    if ( !eID.is_border()) return false;

    return !_with_ghost || eID.eid() <= _size_real_borders+_size_rg_borders;
  }

  /// Determine whether a given edge is a border edge of ghost part of the
  /// pane (either on physical border or between real and ghost elements)
  bool is_ghost_border_edge( const Edge_ID & eID) const  {
    if ( !eID.is_border()) return false;
    if ( eID.eid()>int(_beIDs.size())) return false;

    return _with_ghost && eID.eid() > _size_real_borders;
  }

  /// Is the node isolated (i.e. not incident on any element)?
  bool is_isolated_node( int vID) const {
    // Ghost nodes are not allowed to be isolated.
    if ( _is_str || vID>_maxsize_real_nodes) return false;

    Edge_ID eID = _ieIDs_real_or_str[ vID-1];
    return eID.is_border() && eID.eid()>int(_beIDs.size());
  }
  
  /// Is the node on the pane boundary of real part (not isolated)?
  bool is_real_border_node( int vID) const { 
    if ( !is_real_node( vID)) return false;
    Edge_ID eID;
    if ( _is_str || !is_ghost_node( vID))
      eID = _ieIDs_real_or_str[ vID-1]; 
    else
      eID = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];

    return eID.is_border() && eID.eid() <= int(_beIDs.size()); 
  }

  /// Is the node on pane boundary (not isolated)?
  /// It assumes vID is incident on the ghost (no checking is performed).
  bool is_ghost_border_node( int vID) const { 
    Edge_ID eID;
    if ( _is_str || !is_ghost_node( vID))
      eID = _ieIDs_real_or_str[ vID-1]; 
    else
      eID = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];

    return eID.is_border() && eID.eid() <= int(_beIDs.size()); 
  }

  /// Is the node on pane boundary (not isolated)?
  bool is_border_node( int vID) const  { 
    Edge_ID eID;
    if ( _is_str || !is_ghost_node( vID))
      eID = _ieIDs_real_or_str[ vID-1]; 
    else
      eID = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];

    return is_border_edge( eID);
  }

  /// Get the border edge index. Return -1 if not a border edge.
  int get_border_edgeID( Edge_ID eID) const {
    if ( !eID.is_border()) { assert(false); return -1; }
    else return eID.eid();
  }

  /// Obtain a border edge on real part of the pane.
  Edge_ID get_a_real_border_edge() const {
    if ( _size_real_borders>0 || _size_rg_borders>0) 
      return Edge_ID( 1, Edge_ID::BndID);
    else 
      return Edge_ID();
  }

  /// Obtain a border edge on ghost part of the pane. Maybe between 
  /// real and ghost.
  Edge_ID get_a_ghost_border_edge() const  {
    if ( _size_ghost_borders>0 || _size_rg_borders>0) 
      return Edge_ID(_size_real_borders+_size_rg_borders+1, Edge_ID::BndID);
    else
      return Edge_ID();
  }

  /// Obtain the ID of a border edge of the whole pane
  Edge_ID get_a_border_edge() const { 
    if ( _size_real_borders>0) 
      return Edge_ID( 1, Edge_ID::BndID);
    else if ( _size_ghost_borders>0)
      return Edge_ID( _size_rg_borders, Edge_ID::BndID);
    else
      return Edge_ID();
  }

  /// Obtain the ID of a border node of the real part of the pane
  int get_a_real_border_node() const { 
    if ( _size_real_borders>0 || _size_rg_borders>0) 
      return get_origin(_beIDs[0]);
    else 
      return -1;
  }

  /// Obtain the ID of a border node of the ghost part of the pane. 
  /// The node, however, may be a real node.
  int get_a_ghost_border_node() const {
    if ( _size_ghost_borders>0 || _size_rg_borders>0) 
      return get_origin( _beIDs[_size_real_borders]);
    else
      return -1;
  }

  /// Obtain the ID of a border node
  int get_a_border_node() const {
    if ( _size_real_borders>0) 
      return get_origin(_beIDs[0]);
    else if ( _size_ghost_borders>0)
      return get_origin( _beIDs[_size_rg_borders]);
    else
      return -1;
  }
  //\}

  /// Is the given node a real node?
  bool is_real_node( int vID) const {
    if ( !_is_str) // Unstructured mesh
      return vID <= _maxsize_real_nodes;
    else // Structured mesh
      return _isghostnode.empty() || !_isghostnode[ vID-1]; 
  }

  /// Is the given node a ghost node?
  bool is_ghost_node( int vID) const {
    if ( !_is_str) // Unstructured mesh
      return vID > _maxsize_real_nodes;
    else // Structured mesh
      return !_isghostnode.empty() && _isghostnode[ vID-1]; 
  }

  /// Is the element incident on the give edge a real element?
  bool is_real_element( int eid ) const {
    if ( !_is_str) // Unstructured mesh
      return eid <= _maxsize_real_elmts;
    else // Structured mesh 
      return _isghostelmt.empty() || !_isghostelmt[ eid-1]; 
  }

  /// Is the element incident on the give edge a ghost element?
  bool is_ghost_element( int eid ) const {
    if ( !_is_str) // Unstructured mesh
      return eid > _maxsize_real_elmts;
    else // Structured
      return !_isghostelmt.empty() && _isghostelmt[ eid-1]; 
  }

  /// Get an index for internal edges.
  int get_edge_index( const Edge_ID &eid, const int offset=0) const 
  {  return (eid.eid()-offset-1)*_nspe+eid.lid(); }

  /// Get size of internal edges
  int upperbound_edge_index() const
  {  return size_of_elements()*_nspe; }

  /// Obtain all the border nodes, isolated nodes, and border edges.
  /// If ng is not NULL, then obtain the borders of the whole pane (including 
  /// the ghost part), and load the number of ghost nodes into ng.
  void get_borders( std::vector< bool> &is_border, 
		    std::vector< bool> &is_isolated, 
		    std::vector< Edge_ID > *b, 
		    int *ng=NULL) const;
  
protected:
  /// Initialize the database for the pane, including ghost information,
  /// opposite halfedges, incident halfedges, and border halfedges.
  /// Typically called by constructors.
  void init (const COM::Pane *p, const Simple_manifold_2 *parent=NULL,
	     bool with_ghost=true);

  /** Determine the ghost nodes and elements
   */
  void determine_ghosts();

  /** Determine the opposite halfedges of each halfedge, and 
   *  identify border halfedge
   */
  void determine_opposite_halfedges();

  /** Determine an incident halfedge for each node. If the node is 
   *  a border node, then determine its incident border halfedge. If the pane
   *  is unstructured and contains isolated nodes, determine an ordering
   *  of the isolated nodes.
   */
  void determine_incident_halfedges();

private:
  /** \name Opposite Edges 
   * \{
   */
  /// Get the opposite edge of an interior edge of the whole pane
  inline Edge_ID get_opposite_edge_interior( const Edge_ID &eID) const { 
    COM_assertion( !eID.is_border());

    if ( _is_str || !is_ghost_element( eID.eid())) {
      return _oeIDs_real_or_str[ get_edge_index( eID)];
    }
    else {
      Edge_ID opp;
      opp = _oeIDs_ghost[ get_edge_index(eID,_maxsize_real_elmts)];

      if ( !opp.is_border() || opp.eid()>_size_rg_borders+_size_real_borders)
	return opp;
      else {
	Edge_ID opp2 = _beIDs[ opp.eid()-1];
	COM_assertion( opp2 != eID);
	return opp2;
      }
    }
  }

  /// Get the opposite edge of an interior edge of the real part
  inline Edge_ID get_opposite_real_edge_interior( const Edge_ID &eID) const {
    COM_assertion( !eID.is_border() && is_real_element( eID.eid()));

    Edge_ID opp = _oeIDs_real_or_str[ get_edge_index(eID)]; 

    // If points to a ghost element, then we need to follow the link
    if ( !opp.is_border() && is_ghost_element( opp.eid()))
      opp = get_opposite_ghost_edge_interior( opp);

    return opp;
  }

  /// Get the opposite edge of an interior edge of the ghost part
  inline Edge_ID get_opposite_ghost_edge_interior( const Edge_ID &eID) const {
    COM_assertion( !eID.is_border() && is_ghost_element( eID.eid()));

    if ( _is_str)
      return _oeIDs_real_or_str[ get_edge_index(eID)]; 
    else
      return _oeIDs_ghost[ get_edge_index(eID,_maxsize_real_elmts)];
  }

  /// Get the opposite edge of a border edge for the whole pane.
  inline Edge_ID get_opposite_edge_border( const Edge_ID &eID) const {
    // Precondition: eID is a border edge of the whole pane
    COM_assertion( eID.is_border() && eID.eid()<=int(_beIDs.size()) &&
		   (eID.eid()<=_size_real_borders ||
		    eID.eid()>_size_real_borders+_size_rg_borders));

    return _beIDs[ eID.eid()-1]; 
  }

  /// Get the opposite edge of a border edge for the real pane. 
  inline Edge_ID get_opposite_real_edge_border( const Edge_ID &eID) const {
    // Precondition: eID is a border edge
    COM_assertion( eID.is_border() && 
		   eID.eid()<=_size_real_borders+_size_rg_borders);
    
    Edge_ID opp = _beIDs[ eID.eid()-1];
    
    // Postcondition: opp is incident on a real element.
    COM_assertion( is_real_element( opp.eid()));
    return opp;
  }

  /// Get the opposite edge of a border edge for the ghost pane. 
  inline Edge_ID get_opposite_ghost_edge_border( const Edge_ID &eID) const { 
    // Precondition: eID is a border edge
    COM_assertion( eID.is_border() && eID.eid()>_size_real_borders &&
		   eID.eid()<=int(_beIDs.size()));

    Edge_ID opp = _beIDs[ eID.eid()-1]; 

    if ( is_real_element( opp.eid())) {
      opp = _oeIDs_real_or_str[ get_edge_index(opp)];
      COM_assertion( opp != eID);
    }
    return opp;
  }
  //\}

  /** \name Previous and next Edges 
   * \{
   */
  Edge_ID get_prev_edge_interior( const Edge_ID &eID) const {
    // If not a border edge, then track it within an element.
    int i=(eID.lid()?eID.lid()-1:_nspe-1);

    if ( _is_str || !is_ghost_element( eID.eid())) {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i));
      while ( _oeIDs_real_or_str[eindex--]==Edge_ID()) { --i; assert(i); }
    }
    else {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i), _maxsize_real_elmts);
      while ( _oeIDs_ghost[eindex--]==Edge_ID()) { --i; assert(i); }
    }
    return Edge_ID( eID.eid(), i);
  }

  Edge_ID get_prev_real_edge_interior( const Edge_ID &eID) const {
    COM_assertion( !is_ghost_element( eID.eid()));

    // If not a border edge, then track it within an element.
    int i=(eID.lid()?eID.lid()-1:_nspe-1);

    int eindex = get_edge_index( Edge_ID(eID.eid(), i));
    while ( _oeIDs_real_or_str[eindex--]==Edge_ID()) { --i; assert(i); }

    return Edge_ID( eID.eid(), i);   
  }

  Edge_ID get_prev_ghost_edge_interior( const Edge_ID &eID) const {
    COM_assertion( is_ghost_element( eID.eid()));

    // If not a border edge, then track it within an element.
    int i=(eID.lid()?eID.lid()-1:_nspe-1);

    if ( _is_str) {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i));
      while ( _oeIDs_real_or_str[eindex--]==Edge_ID()) { --i; assert(i); }
    }
    else {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i), _maxsize_real_elmts);
      while ( _oeIDs_ghost[eindex--]==Edge_ID()) { --i; assert(i); }
    }
    return Edge_ID( eID.eid(), i);
  }

  Edge_ID get_next_edge_interior( const Edge_ID &eID) const {
    // If not a border edge, then track it within an element.
    int i=eID.lid()+1;
    if ( i>=_nspe) return Edge_ID( eID.eid(), 0);

    if ( _is_str || !is_ghost_element( eID.eid())) {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i));
      if ( _oeIDs_real_or_str[eindex]==Edge_ID()) i=0;
    }
    else {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i), _maxsize_real_elmts);
      if ( _oeIDs_ghost[eindex]==Edge_ID()) i=0;
    }
    return Edge_ID( eID.eid(), i);
  }

  Edge_ID get_next_real_edge_interior( const Edge_ID &eID) const {
    COM_assertion( !is_ghost_element( eID.eid()));

    // If not a border edge, then track it within an element.
    int i=eID.lid()+1;
    if ( i>=_nspe) return Edge_ID( eID.eid(), 0);

    int eindex = get_edge_index( Edge_ID(eID.eid(), i));
    if ( _oeIDs_real_or_str[eindex]==Edge_ID()) i=0;

    return Edge_ID( eID.eid(), i);
  }

  Edge_ID get_next_ghost_edge_interior( const Edge_ID &eID) const {
    COM_assertion( is_ghost_element( eID.eid()));

    // If not a border edge, then track it within an element.
    int i=eID.lid()+1;
    if ( i>=_nspe) return Edge_ID( eID.eid(), 0);

    if ( _is_str) {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i));
      if ( _oeIDs_real_or_str[eindex]==Edge_ID()) i=0;
    }
    else {
      int eindex = get_edge_index( Edge_ID(eID.eid(), i), _maxsize_real_elmts);
      if ( _oeIDs_ghost[eindex]==Edge_ID()) i=0;
    }
    return Edge_ID( eID.eid(), i);
  }

  //\}
protected:
  const COM::Pane        *_pane;      // The associated pane object
  
  bool                   _is_str;     // Whether the pane is structured.
  bool                   _with_ghost; // Whether has ghost nodes/elements.

  // Element connectivity tables for unstructured meshes.
  std::vector<const COM::Connectivity*>  _elem_conns;
  int                    _nspe;       // Max number of sides per element

  int                    _maxsize_real_nodes; // maxnumber of real nodes.
  int                    _maxsize_real_elmts; // maxnumber of real elements.

  int                    _size_real_borders;  // #real border edges
  int                    _size_ghost_borders; // #ghost border edges
  int                    _size_rg_borders;    // #edges between real and ghost

  std::vector<bool>      _isghostnode;       // Is ghost node
  std::vector<bool>      _isghostelmt;       // Is ghost element

  // Opposite halfedge ID of each edge in real elements or in structured meshes
  std::vector<Edge_ID>   _oeIDs_real_or_str;
  // Opposite halfedge ID of each edge in ghost elements of unstructured meshes
  std::vector<Edge_ID>   _oeIDs_ghost;

  // Incident edge ID of each real node of unstructured mesh, 
  // or each node of structured meshes.
  std::vector<Edge_ID>   _ieIDs_real_or_str;
  // Incident edge ID of each ghost node of unstructured mesh
  std::vector<Edge_ID>   _ieIDs_ghost;

  // Mapping from border edges to their opposites.
  // It has three parts: pure-real, between real and ghost, and pure ghost.
  // If an edge is between real and ghost, then it has three IDs
  // (one in real element, one in ghost element, and one with border ID),
  // and they point to each other as follows:
  // 1. beID of the border instance points to the real instance
  // 2. oeID of the real instance points to the ghost instance
  // 3. oeID of the ghost instance points to the border instance.
  std::vector<Edge_ID>   _beIDs;

  // Isolated node IDs
  std::vector<int>       _isovIDs;
};

MAP_END_NAMESPACE

#endif






