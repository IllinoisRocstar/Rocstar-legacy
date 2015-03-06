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
// $Id: RFC_Window_base.C,v 1.27 2008/12/06 08:43:28 mtcampbe Exp $

// Base implementation of the windows for Rocface.

#include <set>
#include "RFC_Window_base.h"
#include "../Rocmap/include/Pane_connectivity.h"
#include "../Rocmap/include/Pane_boundary.h"
#include "../Rocsurf/include/Generic_element_2.h"

RFC_BEGIN_NAME_SPACE

/*! 
 * \param b the base COM::Pane object.
 * \param color the color of the parent window (BLUE or GREEN).
 */
RFC_Pane_base::RFC_Pane_base( Base *b, int color)
  : _base(b), _is_master(true), _quadratic(false), 
    _n_border(0), _n_isolated(0), _color(color) {}

void 
RFC_Pane_base::init() {
  
  // Determine whether the pane has quadratic elements
  if ( !_base->is_structured()) {
    _base->elements( _elem_conns);
    _elem_offsets.reserve( _elem_conns.size());

    for ( std::vector<const COM::Connectivity*>::const_iterator
	    it = _elem_conns.begin(); it != _elem_conns.end(); ++it) {
      _elem_offsets.push_back( (*it)->index_offset());
      const int ne = (*it)->size_of_edges_pe();
      const int nn = (*it)->size_of_nodes_pe();
      if ( nn > ne) _quadratic = true;
    }
  }

  MAP::Pane_boundary pb( _base);
  pb.determine_border_nodes( _is_border, _is_isolated);

  for ( int i=0, n=_is_border.size(); i<n; ++i) 
  { _n_border += _is_border[i]; _n_isolated += _is_isolated[i]; }
}

// Note: This is not a general routine for detecting whether two nodes are 
// coincident. It assumes that the two nodes belong to the same pane and
// the primary copy of the nodes also belongs to the same pane.
bool
RFC_Pane_base::coincident( int v1, int v2) const {
  if ( v1==v2) return true;
  V2b_table::const_iterator it1 = _v2b_table.find( v1);
  V2b_table::const_iterator it2 = _v2b_table.find( v2);

  if ( it1 == _v2b_table.end() && it2 != _v2b_table.end())
    return it2->second.first == id() && 
      _b2v_table.find( id())->second[it2->second.second] == v1;
  else if ( it1 != _v2b_table.end() && it2 == _v2b_table.end())
    return it1->second.first == id() && 
      _b2v_table.find( id())->second[it1->second.second] == v2;
  else if ( it1 != _v2b_table.end() && it2 != _v2b_table.end()) {
    RFC_assertion( it1->second.first==id() && it2->second.first==id());
    B2v_table::const_iterator bit=_b2v_table.find( id());
    return bit->second[it1->second.second]==bit->second[it2->second.second];
  }
  else 
    return false;
}

void RFC_Pane_base::
get_nat_coor_in_element( const int sn_id, 
			 Element_node_enumerator &ene, Point_2 &nc) const {
  RFC_assertion( sn_id >= 1 && sn_id <= int(_subnode_parents.size()));

  const Point_2S &p = _subnode_nat_coors[ sn_id-1];
  const Edge_ID &eid = _subnode_parents[ sn_id-1];

  if ( eid.face_id == ene.id()) {
    nc[0]=p[0]; nc[1]=p[1];
    normalize_nat_coor( eid.edge_id, ene.size_of_edges(), nc);
  }
  else {
    nc[0] = 1.-p[0]; nc[1]=0;
    int vid = Element_node_enumerator(_base,eid.face_id)[eid.edge_id];

    int i = get_lvid(ene, vid)-1;

    int num_edges = ene.size_of_edges();
    if ( i<0) i=num_edges-1;
    normalize_nat_coor( i, num_edges, nc);
  }
}

/*! \param id the local id of the subnode (starting from 1).
 */
int
RFC_Pane_base::parent_type_of_subnode( int id) const {
  const Point_2S &nc = _subnode_nat_coors[ id-1];
  if ( nc[0]==0 && nc[1]==0)
    return PARENT_VERTEX;
  if ( nc[1]==0)
    return PARENT_EDGE;
  return PARENT_FACE;
}

/*! Precondition: The parent type is PARENT_VERTEX.
 */
int 
RFC_Pane_base::get_parent_node( int id) const {
  const Edge_ID &eid=_subnode_parents[ id-1];
  Element_node_enumerator ene(_base, eid.face_id);

  return ene[eid.edge_id];  
}

Point_3
RFC_Pane_base::get_point_of_subnode( int i) const {
  const int fid = _subnode_parents[i-1].face_id;
  Element_node_enumerator ene( _base, fid);
  Point_2 nc; get_nat_coor_in_element( i, ene, nc);

  Nodal_coor_const coors;
  Field< const Nodal_coor_const, Element_node_enumerator> 
    ps( coors, coordinates(), ene);

  return SURF::Generic_element_2( ene.size_of_edges(), 
				  ene.size_of_nodes()).interpolate(ps, nc);
}

Bbox_3 
RFC_Pane_base::get_bounding_box() const {
  Bbox_3 b;
  for ( int i=1, size=size_of_nodes(); i<=size; ++i) 
    b += Bbox_3(get_point( i));
  return b;
}

RFC_Pane_base::Edge_ID
RFC_Pane_base::get_edge_id( const int face_lid, const int vertex_lid) const {
  if ( _base->is_structured()) {
    Element_node_enumerator ene( _base, face_lid, (COM::Connectivity *)NULL);
    for ( int k=0; k<4; ++k)
      if ( ene[k] == vertex_lid) return Edge_ID( face_lid, k);
    RFC_assertion(false); abort(); 
  }
  else {
    for ( std::vector<const COM::Connectivity*>::const_iterator
	    it = _elem_conns.begin(); it != _elem_conns.end(); ++it) {
      const int ne = (*it)->size_of_edges_pe();
      if ( face_lid > int((*it)->index_offset()) && 
	   face_lid <= int((*it)->index_offset()+(*it)->size_of_elements())) {
	Element_node_enumerator ene(_base, face_lid-(*it)->index_offset(), *it);
	
	for ( int k=0; k<ne; ++k)
	  if ( ene[k] == vertex_lid) return Edge_ID(face_lid,k);
	RFC_assertion(false); abort(); 
      }
    }
  }
  return Edge_ID(face_lid,-1);
}

int 
RFC_Pane_base::get_lvid( const Element_node_enumerator &ene, 
			 const int vid) const {
  int i = ene.vertex( vid);
  if ( i<0) {
    // Resort to a more expensive method.
    for ( int j=ene.size_of_nodes()-1; j>=0; --j) {
      if ( coincident(vid, ene[j])) { i = j; }
    }
    RFC_assertion( i>=0);
  }
  return i;
}

// If a node is a border node, it can have instances in multiple
// panes. The primary copy is the one with the smallest (pane_id, node_id)
// pair lexicalgraphically.
bool 
RFC_Pane_base::is_primary_node( const int vid) const {
  if ( !is_border_node(vid) && !is_isolated_node(vid)) return true;
  V2b_table::const_iterator it = _v2b_table.find( vid);
  RFC_assertion( it != _v2b_table.end() || !is_isolated_node( vid));
  return ( it == _v2b_table.end());
}

int 
RFC_Pane_base::size_of_primary_nodes() const {
  int n = size_of_nodes();
  for ( V2b_table::const_iterator it=_v2b_table.begin(), iend=_v2b_table.end();
	it != iend; ++it) {
    if ( !is_primary_node( it->first)) --n;
  }
  return n;
}

void 
RFC_Pane_base::build_v2b_from_b2v( const RFC_Window_base *w) {
  //`Loop through the B2v_table
  for ( B2v_table::iterator 
	  it=_b2v_table.begin(), iend=_b2v_table.end(); it != iend; ++it) {
    for ( int i=0, size=it->second.size(); i<size; ++i) {
      if ( it->first > id() && !is_isolated_node(it->second[i])) continue;

      std::pair<int,int> p(it->first, i);
      if ( it->first==id()) {
	RFC_assertion( (i&1)==1 && it->second[i-1] < it->second[i] ||
		       (i&1)==0 && it->second[i] < it->second[i+1]);
	if ( (i&1)==0 || is_isolated_node( it->second[i-1]))
	  continue;
	--p.second;
      }
      else { 
	const RFC_Pane_base &pane = w->pane( it->first);
	if ( pane.is_isolated_node( pane._b2v_table.find(id())->second[i]))
	  continue;
      }

      if ( _v2b_table.find( it->second[i]) == _v2b_table.end())
	_v2b_table[it->second[i]] = p;
    }
  }
}

RFC_Window_base::RFC_Window_base( Base *b, int c, MPI_Comm comm) 
  : _base(b), _verbose(1), _color(c), _map_comm( b, comm) {}
RFC_Window_base::~RFC_Window_base() {
  while ( !_pane_set.empty()) {
    delete _pane_set.begin()->second;
    _pane_set.erase( _pane_set.begin());
  }
}

int 
RFC_Window_base::size_of_nodes() const {
  int n = 0;
  // Loop through panes
  Pane_set::const_iterator pit=_pane_set.begin(), piend=_pane_set.end();
  for ( ; pit != piend; ++pit) {
    n += pit->second->size_of_primary_nodes();
  }

  return n;
}

int 
RFC_Window_base::size_of_faces() const {
  int n = 0;
  // Loop through panes
  Pane_set::const_iterator pit=_pane_set.begin(), piend=_pane_set.end();
  for ( ; pit != piend; ++pit) {
    n += pit->second->size_of_faces();
  }

  return n;
}

Bbox_3 
RFC_Window_base::get_bounding_box() const {
  Bbox_3 b;
  Pane_set::const_iterator it=_pane_set.begin(), iend=_pane_set.end();
  for ( ; it != iend; ++it)
    b += it->second->get_bounding_box();
  return b;
}

void
RFC_Window_base::build_pc_tables() {
  MAP::Pane_connectivity pc( _base->attribute(COM::COM_MESH),
			     _map_comm.mpi_comm());
  pc.compute_pconn( _base->attribute(COM::COM_PCONN));

  // Now copy from b2v into the tables of each pane
  Pane_set::iterator pane_it, pane_iend=_pane_set.end();

  for ( pane_it=_pane_set.begin(); pane_it != pane_iend; ++pane_it) {
    const COM::Attribute *pconn = 
      pane_it->second->_base->attribute(COM::COM_PCONN);

    int count=pc.pconn_offset(), s = pconn->size_of_real_items();
    const int *buf= ((const int*)pconn->pointer())+count;

    while ( count < s) {
      const int r_pid = *buf;
      const int r_size = *(buf+1);
      count += 2; buf += 2;
      
      Pane::B2v &b2 = pane_it->second->_b2v_table[ r_pid];
      b2.clear();
      b2.insert( b2.end(), buf, buf+r_size);

      count += r_size;
      buf += r_size;
    }
  }

  // Construct v2b
  for ( pane_it=_pane_set.begin(); pane_it != pane_iend; ++pane_it) {
    Pane &pane = *pane_it->second;
    pane.build_v2b_from_b2v( this);
  }
}

void
RFC_Pane_base::comp_nat_coors() {
  int nsn=size_of_subnodes();
  _subnode_normalized_nc.resize( nsn);
  _subnode_subID.resize( size_of_nodes());

  // Initialize _subnode_normalized_nc.
  for ( int i=0; i<nsn; ++i) {
    int f=_subnode_parents[i].face_id;
    Element_node_enumerator ene( _base, f);

    Point_2 p; get_nat_coor_in_element( i+1, ene, p);
    _subnode_normalized_nc[i] = Point_2S(p[0], p[1]);

    // Assign sub-node ID for each vertex in input mesh
    if (parent_type_of_subnode( i+1)==PARENT_VERTEX) {
      int vid = ene[_subnode_parents[i].edge_id];
      _subnode_subID[ vid-1] = i+1;
    }
  }
  
  int nsf=size_of_subfaces();
  _subface_nat_coors.resize( nsf);
  // Initialize _subface_nat_coors.
  Element_node_enumerator ene( base(), 1);
  for ( int i=0; i<nsf; ++i) {
    if ( ene.id() != _subface_parents[i]) {
      ene.next();
      if ( ene.pane()==NULL || ene.id() != _subface_parents[i]) 
	get_host_element_of_subface( i+1, ene);
    }
    Point_2 p;
    for ( int j=0; j<3; ++j) {
      get_nat_coor_in_element( _subfaces[i][j], ene, p);
      _subface_nat_coors[i][j] = Point_2S(p[0], p[1]);
    }
  }
}

void 
RFC_Window_base::export_window( RFC_Window_base *w) const {

  Pane_set::const_iterator pane_it, pane_iend=_pane_set.end();

  for ( pane_it=_pane_set.begin(); pane_it != pane_iend; ++pane_it) {
    Pane &pane = *pane_it->second;
    Pane &to_pane = *w->_pane_set.find( pane.id())->second;

    // Copy over the nodes information
    to_pane._subnode_parents = pane._subnode_parents;
    to_pane._subnode_counterparts = pane._subnode_counterparts;
    to_pane._subnode_nat_coors = pane._subnode_nat_coors;

    // Copy over the face information
    to_pane._subface_offsets = pane._subface_offsets;
    to_pane._subface_counterparts = pane._subface_counterparts;
    to_pane._subface_parents = pane._subface_parents;
    to_pane._subfaces = pane._subfaces;
    
    // Copy over the pane connectivity of the vertices
    to_pane._b2v_table = pane._b2v_table;
    to_pane._v2b_table = pane._v2b_table;

    to_pane.comp_nat_coors();
  }
}

RFC_END_NAME_SPACE






