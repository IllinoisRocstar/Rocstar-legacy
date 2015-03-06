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
// $Id: Simple_manifold_2.C,v 1.12 2008/12/06 08:43:21 mtcampbe Exp $

#include "Simple_manifold_2.h"
#include "Dual_connectivity.h"
#include <iterator>

MAP_BEGIN_NAMESPACE

void Simple_manifold_2::init( const COM::Pane *p,
			      const Simple_manifold_2 *parent,
			      bool with_ghost) {
  COM_assertion_msg( p, "Caught NULL pointer");
  _pane = p;
  _is_str = p->is_structured();

  // Initialize _nspe to 4 for structured or mixed meshes and to the
  // actual number of edges per element for other type of meshes.
  if ( _is_str) {
    _with_ghost = with_ghost && p->size_of_ghost_layers()>0;
    _nspe = 4;
  }
  else {
    // It is possible to have ghost nodes but no ghost elements and vice versa
    _with_ghost = with_ghost && 
      (p->size_of_ghost_nodes()>0 || p->size_of_ghost_elements()>0);

    p->elements( _elem_conns);
    _nspe = 0;
    // Loop through the connectivity
    for ( int i=_elem_conns.size()-1; i>=0; --i) {
      _nspe = std::max( _nspe, int( _elem_conns[i]->size_of_edges_pe()));
    }
  }

  _maxsize_real_nodes = p->maxsize_of_real_nodes();
  _maxsize_real_elmts = p->maxsize_of_real_elements();

  if ( parent) {
    _isghostnode = parent->_isghostnode;
    _isghostelmt = parent->_isghostelmt;

    // Copy border infomation from parent
    _oeIDs_real_or_str = parent->_oeIDs_real_or_str;
    _oeIDs_ghost = parent->_oeIDs_ghost;
    _ieIDs_real_or_str = parent->_ieIDs_real_or_str;
    _ieIDs_ghost = parent->_ieIDs_ghost;

    _beIDs = parent->_beIDs;
    _isovIDs = parent->_isovIDs;
  }
  else {
    determine_ghosts();
    determine_opposite_halfedges();
    determine_incident_halfedges();
  }
}

void Simple_manifold_2::
determine_ghosts() {
  // _isghostnode and _isghostelmt not used for unstructured meshes 
  // and meshes without ghost
  _isghostnode.clear();
  _isghostelmt.clear();

  if ( !_is_str || !_with_ghost) return;

  // Fill in _isghostnode and _isghostelmt for structured mesh with ghosts
  _isghostnode.resize( _pane->size_of_nodes(), false);
  _isghostelmt.resize( _pane->size_of_elements(), false);

  COM_assertion_msg( false, 
		     "Not fully implemented yet for structured mesh");
}

void Simple_manifold_2::
determine_opposite_halfedges() {

  // Compute the dual connectivity.
  Pane_dual_connectivity pdc( _pane, _with_ghost);
  
  int nr = _nspe*_pane->size_of_real_elements();
  _oeIDs_real_or_str.clear(); _oeIDs_real_or_str.resize( nr, Edge_ID());

  int ng = _with_ghost ? _nspe*_pane->size_of_ghost_elements():0;
  _oeIDs_ghost.clear(); _oeIDs_ghost.resize( ng, Edge_ID());

  // Buffer array for holding border edges between real and ghost
  std::vector<Edge_ID>   beIDs_rg;

  // Determine the neighbor elements from the dual connectivity.
  Element_node_enumerator ene( _pane, 1); 
  std::vector<int> ies1, ies2;
  std::vector<int> ies_common; ies_common.reserve(2);

  // Process real elements of unstructured meshes or structured meshes
  int nn = _is_str && _with_ghost
    ? _pane->size_of_elements():_pane->size_of_real_elements(); 
  for ( int j=0; j<nn; ++j, ene.next()) {
    const int ne = ene.size_of_edges(); 
    COM_assertion( _nspe>=ne);

    int ij = j*_nspe;
    for ( int i=0; i<ne; ++i, ++ij) {
      // Determine the elements that incident on both nodes of the edge
      pdc.incident_elements( ene[i], ies1);
      pdc.incident_elements( ene[(i==ne-1)?0:i+1], ies2);
      
      ies_common.clear();
      std::back_insert_iterator< std::vector<int> > ii(ies_common);;
      std::set_intersection( ies1.begin(), ies1.end(),
			     ies2.begin(), ies2.end(), ii);
      COM_assertion( ies_common.size()<=2);
      
      int eid=0;
      for ( std::vector<int>::iterator it_ies = ies_common.begin();
	    it_ies != ies_common.end(); ++it_ies)
	if ( *it_ies != ene.id()) { eid = *it_ies; break; }
      
      // Determing the local side ID
      if ( eid) {
	int node_id = ene[i];
	Element_node_enumerator ene_opp( _pane, eid);
	const int nk=ene_opp.size_of_edges();
	for ( int k=0; k<nk; ++k) if ( ene_opp[k] == node_id) {
	  Edge_ID opp = Edge_ID( eid, k?k-1:nk-1);
	  
	  bool isreal_edge = !_with_ghost || !_is_str || is_real_element( ene.id());
	  bool isghost_opp = _with_ghost && is_ghost_element( eid);

	  if ( isreal_edge || isghost_opp)
	    // ghost elements of structured mesh 
	    _oeIDs_real_or_str[ij] = opp;

	  // Insert border edges incident on ghost elements into _beIDs_rg
	  if ( isghost_opp && isreal_edge) {
	    beIDs_rg.push_back( Edge_ID( ene.id(), i));

	    Edge_ID bid = Edge_ID( beIDs_rg.size(), Edge_ID::BndID); 
	    if (_is_str)
	      _oeIDs_real_or_str[ get_edge_index( opp)] = bid;
	    else 
	      _oeIDs_ghost[ get_edge_index( opp, _maxsize_real_elmts)] = bid;
	  }

	  break;
	}
	COM_assertion( _oeIDs_real_or_str[ij] != Edge_ID());
      } // eid
      else {
	// Insert border edges of the real part into _beIDs
	_beIDs.push_back( Edge_ID(ene.id(), i));
	_oeIDs_real_or_str[ij] = Edge_ID( _beIDs.size(), Edge_ID::BndID);
      }
    }
  }
  
  // Save the number of real borders edges.
  _size_real_borders = _beIDs.size();
  _size_rg_borders = beIDs_rg.size();

  // Update the rg_border IDs
  for ( int i=0; i<_size_rg_borders; ++i) {
    Edge_ID gid = _oeIDs_real_or_str[get_edge_index(beIDs_rg[i])];

    Edge_ID *bid;
    if ( _is_str)
      bid = &_oeIDs_real_or_str[ get_edge_index( gid)];
    else
      bid = &_oeIDs_ghost[ get_edge_index( gid, _maxsize_real_elmts)];

    COM_assertion( bid->eid() == i+1);
    *bid = Edge_ID( bid->eid()+_size_real_borders, Edge_ID::BndID);
  }

  // Merge _beIDs and beIDs_rg together
  _beIDs.insert( _beIDs.end(), beIDs_rg.begin(), beIDs_rg.end());
  beIDs_rg.clear();

  // Process ghost elements
  nn = _is_str || !_with_ghost ? 0 : _pane->size_of_ghost_elements();
  if ( nn) ene = Element_node_enumerator( _pane, _maxsize_real_elmts+1); 
  for ( int j=0; j<nn; ++j, ene.next()) {
    const int ne = ene.size_of_edges(); 
    COM_assertion( _nspe>=ne);

    int ij = j*_nspe;
    for ( int i=0; i<ne; ++i, ++ij) {
      // Determine the elements that incident on both nodes of the edge
      pdc.incident_elements( ene[i], ies1);
      pdc.incident_elements( ene[(i==ne-1)?0:i+1], ies2);
      
      ies_common.clear();
      std::back_insert_iterator< std::vector<int> > ii(ies_common);;
      std::set_intersection( ies1.begin(), ies1.end(),
			     ies2.begin(), ies2.end(), ii);
      COM_assertion( ies_common.size()<=2);
      
      int eid=0;
      for ( std::vector<int>::iterator it_ies = ies_common.begin();
	    it_ies != ies_common.end(); ++it_ies)
	if ( *it_ies != ene.id()) { eid = *it_ies; break; }
      
      // Determing the local side ID
      if ( eid) {
	if ( is_real_element( eid)) continue;

	int node_id = ene[i];
	Element_node_enumerator ene_opp( _pane, eid);
	const int nk=ene_opp.size_of_edges();
	for ( int k=0; k<nk; ++k) if ( ene_opp[k] == node_id) {
	  _oeIDs_ghost[ij] = Edge_ID( eid, k?k-1:nk-1);
	  break;
	}
	COM_assertion( _oeIDs_ghost[ij] != Edge_ID());
      }
      else {
	// Insert border edges of the ghost part of the pane into _beIDs
	_beIDs.push_back( Edge_ID(ene.id(), i));
	_oeIDs_ghost[ij] = Edge_ID( _beIDs.size(), Edge_ID::BndID);
      }
    }
  }

  _size_ghost_borders = _beIDs.size() - _size_real_borders - _size_rg_borders;
}

void Simple_manifold_2::
determine_incident_halfedges() {
  COM_assertion_msg( _is_str || 
		     int(_pane->size_of_real_nodes())==_maxsize_real_nodes &&
		     int(_pane->size_of_real_elements())==_maxsize_real_elmts, 
		     "Support for maxsize not yet fully implemented");

  int nnodes, nelems;
  if ( _is_str) { // For structured meshes, real and ghost are stored together
    nnodes = _pane->size_of_nodes();
    nelems = _pane->size_of_elements();
  }
  else {
    nnodes = _pane->size_of_real_nodes();
    nelems = _pane->size_of_real_elements();
  }

  _ieIDs_real_or_str.clear(); _ieIDs_real_or_str.resize( nnodes, Edge_ID());

  Element_node_enumerator ene( _pane, 1); 
  for ( int j=0; j<nelems; ++j, ene.next()) {
    int ne=ene.size_of_edges();

    for ( int i=0; i<ne; ++i) {
      int vID=ene[i];

      // If the incident elements has not been assigned, or the
      // incident edge is a border edge, then assign the incident elements.
      Edge_ID prev( ene.id(),i?i-1:ne-1);
      Edge_ID eoppID = is_real_element( ene.id()) ?
	get_opposite_real_edge_interior( prev) :
	get_opposite_ghost_edge_interior( prev);

      Edge_ID &iid = _ieIDs_real_or_str[ vID-1];
      if ( eoppID.is_border() && !is_border_edge( iid)) {
	iid = eoppID;
	COM_assertion( get_origin( iid) == vID);
      }
      else if ( iid==Edge_ID()) {
	iid = Edge_ID( ene.id(), i);
	COM_assertion( get_origin( iid, &ene) == vID);
      }
    }

    // Special treatment for edge centers of quadratic elements
    int ne2 = std::min(ne+ne,ene.size_of_nodes());
    for ( int i=ne, ni=ne2; i<ni; ++i) {
      int vID=ene[i];

      // Make sure every edge centers points to one of its halfedge, and 
      // always points to the border halfedge if on the boundary.
      Edge_ID eID( ene.id(), i-ne);
      Edge_ID eoppID = is_real_element( ene.id()) ?
	get_opposite_real_edge_interior( eID) :
	get_opposite_ghost_edge_interior( eID);

      Edge_ID &iid = _ieIDs_real_or_str[ vID-1];
      if ( eoppID.is_border() && !is_border_edge( iid))
	iid =eoppID;
      else if ( iid==Edge_ID())
	iid = eID;
    }

    // Finally, handle face center. 
    if ( ne2<ene.size_of_nodes())
      _ieIDs_real_or_str[ene[ne2]-1] = Edge_ID(ene.id(),0);
  }

  if (!_is_str) {
    // Identify isolated nodes that are not incident on any element
    // and create a numbering for the isolated nodes.
    _isovIDs.clear();
    // Loop through all the nodes
    for ( int i=1; i<=nnodes; ++i) {
      Edge_ID &iid = _ieIDs_real_or_str[ i-1];
      if ( iid == Edge_ID()) {
	_isovIDs.push_back( i);
	iid = Edge_ID( _beIDs.size()+_isovIDs.size(), -1);
      }
    }
  }

  // We are done for structured mesh and unstructured without ghost
  nnodes = _with_ghost ? _pane->size_of_ghost_nodes() : 0;
  _ieIDs_ghost.clear(); _ieIDs_ghost.resize( nnodes, Edge_ID());
  if ( _is_str || nnodes==0) return;

  // Handle ghost nodes and elements
  nelems = _pane->size_of_ghost_elements();
  if ( nelems>0) ene = Element_node_enumerator( _pane, _maxsize_real_elmts+1);

  for ( int j=0; j<nelems; ++j, ene.next()) {
    int ne=ene.size_of_edges();

    for ( int i=0; i<ne; ++i) {
      int vID=ene[i];
      if ( is_real_node( vID)) continue; // skip real nodes.

      // If the incident elements has not been assigned, or the
      // incident edge is a border edge, then assign the incident elements.
      Edge_ID prev( ene.id(),i?i-1:ne-1);
      Edge_ID eoppID = get_opposite_ghost_edge_interior( prev);

      Edge_ID &iid = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];
      if ( eoppID.is_border() && !is_border_edge( iid)) {
	iid = eoppID;
	COM_assertion( get_origin( iid) == vID);
      }
      else if ( iid==Edge_ID()) {
	iid = Edge_ID( ene.id(), i);
	COM_assertion( get_origin( iid, &ene) == vID);
      }
    }

    // Special treatment for edge centers of quadratic elements
    int ne2 = std::min(ne+ne,ene.size_of_nodes());
    for ( int i=ne, ni=ne2; i<ni; ++i) {
      int vID=ene[i];
      if ( is_real_node( vID)) continue; // skip real nodes.

      // Make sure every edge centers points to one of its halfedge, and 
      // always points to the border halfedge if on the boundary.
      Edge_ID eID( ene.id(), i-ne);
      Edge_ID eoppID = get_opposite_ghost_edge_interior(eID);

      Edge_ID &iid = _ieIDs_ghost[ vID-_maxsize_real_nodes-1];
      if ( eoppID.is_border() && !is_border_edge( iid)) {
	iid =eoppID;
	COM_assertion( get_origin( iid) == vID);
      }
      else if ( iid==Edge_ID()) {
	iid = eID;
	COM_assertion( get_origin( iid, &ene) == vID);
      }
    }

    // Finally, handle face center.
    if ( ne2<ene.size_of_nodes())
      _ieIDs_ghost[ene[ne2]-_maxsize_real_nodes-1] = Edge_ID(ene.id(),0);
  }
}

void Simple_manifold_2::
get_borders( std::vector< bool> &is_border,
	     std::vector< bool> &is_isolated,
	     std::vector< Edge_ID > *b,
	     int *ng) const {

  int nr = size_of_real_nodes();
  int nnodes;
  
  if ( ng != NULL) {
    *ng = _with_ghost ? size_of_ghost_nodes() : 0;
    nnodes = nr + *ng;
  }
  else 
    nnodes = nr;

  // First, get the isolated nodes
  is_isolated.resize( nnodes, false);
  if ( !_is_str) {
    for ( int i=0; i<nr; ++i)
      is_isolated[ i] = is_isolated_node( i+1);
  }

  // Second, obtain the border nodes
  is_border.resize( nnodes, false);
  if ( ng && *ng) {
    for ( int i=0; i<nr; ++i)
      is_border[ i] = is_border_node( i+1);

    // Loop through all the nodes
    const int offset=_maxsize_real_nodes-nr+1, nlast = nr+*ng;
    for ( int i=nr; i<nlast; ++i)
      is_border[ i] = is_border_node( offset+i);
  }
  else {
    for ( int i=0; i<nr; ++i)
      is_border[ i] = is_real_border_node( i+1);
  }

  // Finally, fill in the border edges into b if exist.
  if ( b) {
    b->clear();

    if ( ng) {
      b->reserve( _size_real_borders+_size_ghost_borders);
      b->insert( b->end(), &_beIDs[0], &_beIDs[_size_real_borders]);

      int nstart=_size_real_borders+_size_rg_borders;
      b->insert( b->end(), &_beIDs[nstart],
		 &_beIDs[nstart+_size_ghost_borders]);
    }
    else {
      b->insert( b->end(), &_beIDs[0], 
		 &_beIDs[_size_real_borders+_size_rg_borders]);
    }
  }
}

#define SIZE_OF_ELMTS( prefix, name, nedges) \
int Simple_manifold_2:: prefix##name() const { \
  int n=0; \
  std::vector<const COM::Connectivity*>::const_iterator it=_elem_conns.begin(); \
  for ( ; it != _elem_conns.end(); ++it) { \
    if ( (*it)->size_of_edges_pe()==nedges) \
      n += (*it)-> prefix##items(); \
  } \
  return n; \
} 

SIZE_OF_ELMTS( size_of_, triangles, 3);
SIZE_OF_ELMTS( size_of_real_, triangles, 3);
SIZE_OF_ELMTS( size_of_ghost_, triangles, 3);

SIZE_OF_ELMTS( maxsize_of_, triangles, 3);
SIZE_OF_ELMTS( maxsize_of_real_, triangles, 3);
SIZE_OF_ELMTS( maxsize_of_ghost_, triangles, 3);

SIZE_OF_ELMTS( size_of_, quadrilaterals, 4);
SIZE_OF_ELMTS( size_of_real_, quadrilaterals, 4);
SIZE_OF_ELMTS( size_of_ghost_, quadrilaterals, 4);

SIZE_OF_ELMTS( maxsize_of_, quadrilaterals, 4);
SIZE_OF_ELMTS( maxsize_of_real_, quadrilaterals, 4);
SIZE_OF_ELMTS( maxsize_of_ghost_, quadrilaterals, 4);

#define SIZE_OF_EDGES( prefix, name, multiple) \
int Simple_manifold_2::prefix##name() const { \
  int n=prefix##border_edges(); \
  std::vector<const COM::Connectivity*>::const_iterator it=_elem_conns.begin(); \
  for ( ; it != _elem_conns.end(); ++it) { \
    n += (*it)->prefix##items()*(*it)->size_of_edges_pe(); \
  } \
  return n/=multiple; \
}
  
SIZE_OF_EDGES( size_of_, halfedges, 1);
SIZE_OF_EDGES( size_of_real_, halfedges, 1);
SIZE_OF_EDGES( size_of_ghost_, halfedges, 1);
  
SIZE_OF_EDGES( size_of_, edges, 2);
SIZE_OF_EDGES( size_of_real_, edges, 2);
SIZE_OF_EDGES( size_of_ghost_, edges, 2);

MAP_END_NAMESPACE






