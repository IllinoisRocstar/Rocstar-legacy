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
// $Id: RFC_Window_overlay.C,v 1.32 2008/12/06 08:43:29 mtcampbe Exp $

#include "RFC_Window_overlay.h"
#include "Overlay_primitives.h"
#include "HDS_accessor.h"
#include "Triangulation.h"
#include <functional>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cmath>

RFC_BEGIN_NAME_SPACE

using  namespace std;

const Point_3 &Vertex_overlay::point() const 
{ return pane()->get_point(this); }

HDS_accessor<Tag_true> RFC_Window_overlay::acc;
HDS_accessor<Tag_true> RFC_Pane_overlay::acc;

const static int scheme = Overlay_primitives::ANGLE_WEIGHTED;

// Perform the standard normalization and also 
// make every border vertex point to its incident border halfedge.
void HDS_overlay::normalize_border_vertices() {
  Base::normalize_border();
  
  // Loop through all the border halfedges
  Halfedge_iterator it;
  for ( it=border_halfedges_begin(); it!=halfedges_end(); ++it) {
    RFC_assertion(!it->is_border());
    ++it; RFC_assertion(it->is_border());
    Vertex *v = it->vertex();
    if ( v->halfedge() != &*it)
      v->set_halfedge( &*it);
  }
}

RFC_Pane_overlay::RFC_Pane_overlay( COM::Pane *b, int color)
  : Base(b, color), _hds( this) {}

// Constructor and deconstructors
RFC_Window_overlay::RFC_Window_overlay( COM::Window *b, 
					int color, const char *pre)
  : Base(b, color, MPI_COMM_SELF), out_pre(pre?pre:""), 
    _long_falseness_check(true) {
  init_feature_parameters();
  vector< Pane*> pns; panes(pns);
  vector< Pane*>::iterator pit=pns.begin(), piend=pns.end();
  for ( ; pit != piend; ++pit) {
    (*pit)->_window = this;
    (*pit)->build_hds();   // Create the DCEL data structure
  }

  // Preprocess pane connectivity.
  std::cout << "Building pane connectivity..." << std::flush;

  build_pc_tables();

  // Determine the counterpart for border edges of all panes.
  determine_counterparts();

  std::cout << "Done" << std::endl;
}
RFC_Window_overlay::~RFC_Window_overlay() {}

const RFC_Window_overlay::Halfedge *
RFC_Window_overlay::get_an_unmarked_halfedge() const {
  const Halfedge *hbrd=NULL;

  for ( Pane_set::const_iterator pit=_pane_set.begin(); 
	pit != _pane_set.end(); ++pit) {
    const RFC_Pane_overlay &pane =
      reinterpret_cast<const RFC_Pane_overlay &>(*pit->second);
    RFC_Pane_overlay::HDS::Halfedge_const_iterator 
      hit=pane._hds.halfedges_begin(), hend=pane._hds.border_halfedges_begin();

    for ( ; hit != hend; ++hit) if (!pane.marked( &*hit)) {
      if ( hbrd==NULL && pane.is_on_feature(hit->origin()))
	hbrd = &*hit;
      else if ( !pane.is_on_feature(hit->origin())) 
	return &*hit;
    }
  }
  return hbrd;
}

RFC_Window_overlay::Halfedge * 
RFC_Window_overlay::get_an_unmarked_halfedge() {
  return const_cast<Halfedge*>( ((const Self*)this)->
				get_an_unmarked_halfedge());
}

/** Check whether given two vertices correspond to the same physical point. */
bool 
RFC_Window_overlay::is_same_node( Vertex *v1, Vertex *v2) {
  if ( v1==v2) return true;
  if ( !v1->halfedge()->is_border() || !v2->halfedge()->is_border()) 
    return false;
  return v1->pane()->get_primary( v1) == v2->pane()->get_primary( v2);
}

// Constructs HDS for a pane from element connectivity array.
void 
RFC_Pane_overlay::build_hds() {
  RFC_assertion( _base);
  if ( _base->size_of_nodes() == 0) return;

  HDS_builder  B(_hds,true);

  B.begin_surface( _base->size_of_nodes(), _base->size_of_elements());
  // insert the vertices
  for (Size i=0; i<_base->size_of_nodes(); ++i) 
  { B.add_vertex( Vertex::Point()); }

  Element_node_enumerator ene( _base, 1);

  for (int i=_base->size_of_elements(); i>0; --i, ene.next()) {
    B.begin_facet();
    for ( int k=0, ne=ene.size_of_edges(); k<ne; ++k) {
      B.add_vertex_to_facet( ene[k]-1);
    }
    B.end_facet();
  }

  B.end_surface();

  // Normalize border vertices such that every border vertex point to its
  // incident border halfedge.
  _hds.normalize_border_vertices();

  // Insert the vertices in edge/face centers for quadratic elements
  if ( _quadratic) {
    Facet *fs = &*_hds.facets_begin();

    Element_node_enumerator ene( _base, 1);
    for (int i=_base->size_of_elements(); i>0; --i, ene.next(), ++fs) {
      int nn = ene.size_of_nodes();
      int ne = ene.size_of_edges();

      Halfedge *h = fs->halfedge();
      for ( int k=ne; k<ne+ne; ++k) {
	Vertex *v = get_vertex_from_id( ene[k]);
	v->set_halfedge( h);
	h = h->next();
      }
      if ( nn == 9) {
	Vertex *v = get_vertex_from_id( ene[8]);
	v->set_halfedge( fs->halfedge());
      }
    }
  }
}

// Evaluate normals for every vertex within a pane as the average of
// face normals of the faces of its incident faces.
void
RFC_Pane_overlay::evaluate_normals() {
  _nrmls.resize( _hds.size_of_vertices(), Vector_3(0.,0.,0.));

  int vid=1;
  for ( HDS::Vertex_iterator it=_hds.vertices_begin(), 
	  iend=_hds.vertices_end(); it != iend; ++it, ++vid) {
    Vertex *v = &*it;
    Vector_3 d(0.,0.,0.);

    Halfedge *h = v->halfedge(), *i = h;
    if (!h || h->destination() != v) continue;

    do {
      RFC_assertion( i->destination() == v);
      if ( !i->is_border()) {
	d += Overlay_primitives().get_face_normal( i, Point_2(0,0), scheme);
      }
    } while ( ( i = i->next()->opposite()) != h);
    // Note that the normal vector is not normalized.
    set_normal( vid, d);
  }
}

// Evaluate vertex normals as average of face normals of all incident faces.
void 
RFC_Window_overlay::evaluate_normals() {
  // First, evaluate normals for each pane
  Pane_set::iterator it, iend=_pane_set.end();
  for ( it=_pane_set.begin() ; it != iend; ++it) {
    if ( it->second->is_master())
      ((RFC_Pane_overlay*)it->second)->evaluate_normals();
  }

  // Second, perform reduction over all vertices
  reduce_normals_to_all( MPI_SUM);

  // Evaluate the one-sided normals for the halfedge edges incident 
  // on the features.
  for ( it=_pane_set.begin() ; it != iend; ++it) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*it->second;
    free_vector( pane._f_nrmls); free_vector( pane._f_tngts);
    free_vector( pane._f_n_index); free_vector( pane._f_t_index);

    pane._f_n_index.resize( pane.hds().size_of_halfedges(), -1);
    pane._f_t_index.resize( pane.hds().size_of_halfedges(), -1);
  }

  Overlay_primitives      op;
  Point_2  one(1,0);
  // Loop throught the sharp halfedges in 1-features
  for ( Feature_list_1::iterator it = _f_list_1.begin(), iend=_f_list_1.end();
	it != iend; ++it) {
    for ( Feature_1::iterator j=it->begin(); j!=it->end(); ++j) {
      Halfedge *k = *j, *kopp=acc.get_opposite(k);
      do {
	if (!k->is_border() && acc.get_pane(k)->
	    _f_n_index[acc.get_pane(k)->get_index(k)] == -1) {
	  // First, compute the vector
	  Vector_3 v = op.get_face_normal( k, one, scheme);
	  Halfedge *h0 = k, *h = acc.get_next_around_destination(k);
	  do {
	    if ( is_feature_1( h)) break;
	    RFC_assertion( !h->is_border());
	    v += op.get_face_normal( h, one, scheme);
	  } while ( (h=acc.get_next_around_destination(h)) != h0);

	  // Second, insert the vector into the table
	  normalize(v);
	  swap( h, h0);
	  do {
	    RFC_Pane_overlay &pane = *acc.get_pane( h);
	    if ( pane._f_nrmls.size() && v == pane._f_nrmls.back()) {
	      pane._f_n_index[ pane.get_index( h)] = pane._f_nrmls.size()-1;
	    }
	    else {
	      pane._f_n_index[ pane.get_index( h)] = pane._f_nrmls.size();
	      pane._f_nrmls.push_back( v);
	    }
	  } while ( (h=acc.get_next_around_destination(h)) != h0);
	}
      } while ( (k=(k==kopp)?*j:kopp) != *j);
      
      // Compute the tangent
      Vector_3 tng = ( k->destination()->point() -
			kopp->destination()->point());
      acc.get_pane( k)->add_tangent( k, tng);
      if ( !is_feature_0( acc.get_destination(k))) {
	Feature_1::iterator t = j; ++t;
	if ( t==it->end()) t = it->begin();
	Halfedge *h = acc.get_opposite(*t);
	acc.get_pane( h)->add_tangent( h, tng);
      }
      acc.get_pane( kopp)->add_tangent( kopp, tng);
      if ( !is_feature_0( acc.get_destination(kopp))) {
	Feature_1::iterator t = (j==it->begin()) ? it->end() : j; 
	Halfedge *h = *(--t);
	acc.get_pane( h)->add_tangent( h, tng);
      }
    }
  }

  // Loop through the vertices in 0-features
  for ( Feature_list_0::iterator it = _f_list_0.begin(), iend=_f_list_0.end();
	it != iend; ++it) {
    Halfedge *h = acc.get_halfedge(it->vertex());
    RFC_Pane_overlay &pane = *acc.get_pane( h);

    if ( !h->is_border() && pane._f_n_index[ pane.get_index( h)] == -1) {
      Halfedge *h0 = h;
      do {
	Vector_3 v = op.get_face_normal( h, one, scheme)+
	  op.get_face_normal( acc.get_opposite(h), Point_2(0,0), scheme);
	normalize(v);

	if ( pane._f_nrmls.size() && v == pane._f_nrmls.back()) {
	  pane._f_n_index[ pane.get_index( h)] = pane._f_nrmls.size()-1;
	}
	else {
	  pane._f_n_index[ pane.get_index( h)] = pane._f_nrmls.size();
	  pane._f_nrmls.push_back( v);
	}
      } while ( (h=acc.get_next_around_destination(h)) != h0);
    }
  }

  // Finally, normalize normals and tangents
  for ( it=_pane_set.begin() ; it != iend; ++it) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*it->second;

    if ( pane.is_master()) {
      for ( int i=0, size = pane._nrmls.size(); i<size; ++i) 
	normalize( pane._nrmls[i]);
      for ( int i=0, size = pane._f_tngts.size(); i<size; ++i) 
	normalize( pane._f_tngts[i]);
    }
  }
}

// Get the normal of a vertex.
const Vector_3 &
RFC_Pane_overlay::get_normal( const Halfedge *h, const Vertex *v) const { 
  RFC_assertion( acc.get_pane(h) == this && acc.get_pane(v)==this);

  if ( v != h->destination()) {
    h = h->prev();
    RFC_assertion( v == h->destination());
  }
  if ( h->is_border()) 
    h = acc.get_prev_around_destination( h);

  int j=_f_n_index[get_index( h)];
  if ( j < 0)
    return _nrmls[ get_index(v)];
  else
    return _f_nrmls[j];
}

// Get the tangents of a vertex on feature. 
// Note that the tangent is always along the direction of
// the halfedges in the feature list.
const Vector_3& 
RFC_Pane_overlay::get_tangent( const Halfedge *h, const Vertex *v) const {
  if ( v != h->destination()) {
    h = acc.get_opposite(h);
    return acc.get_pane( h)->get_tangent( h, h->destination());
  }
  RFC_assertion( is_feature_1(h));
  
  int j=_f_t_index[get_index( h)];
  RFC_assertion( j >= 0 && int(_f_tngts.size())>j);
  return _f_tngts[j];
}

// Get the normal of a vertex.
Vector_3 &
RFC_Pane_overlay::get_normal( Halfedge *h, Vertex *v) { 
  return const_cast< Vector_3&>(get_normal( h, (const Vertex*)(v)));
}

// Get the tangents of a vertex on feature.
Vector_3& 
RFC_Pane_overlay::get_tangent( Halfedge *h, Vertex *v) {
  return const_cast< Vector_3&>(get_tangent( h, (const Vertex*)(v)));
}

// Determine counterparts of border edges.
void
RFC_Pane_overlay::determine_counterparts() {
  typedef set< pair<int,int> >      V2h;
  typedef map<int, V2h>                  V2h_table;
  V2h_table  _v2h_table;

  //`Loop through the B2v_table
  for ( B2v_table::iterator 
	  it=_b2v_table.begin(), iend=_b2v_table.end(); it != iend; ++it) {
    for ( int i=0, size=it->second.size(); i<size; ++i) {
      pair<int,int> p(it->first, i);
      if ( it->first==id()) {
	if ( i&1) --p.second; else ++p.second;
      }
      _v2h_table[it->second[i]].insert( p);
    }
  }

  _cntrs.resize( hds().size_of_border_edges(), NULL);
  // Loop through all border edges.
  HDS_overlay::Halfedge_iterator hit = hds().border_halfedges_begin();
  HDS_overlay::Halfedge_iterator hend = hds().halfedges_end();
  for ( ; hit != hend; ++hit) {
    ++hit; RFC_assertion( hit->is_border());
    Vertex *v = hit->vertex();
    V2h_table::iterator it = _v2h_table.find( get_lid(v));
    if ( it == _v2h_table.end()) continue;

    V2h_table::iterator i0=_v2h_table.find(get_lid( hit->origin()));

    // If the origin not incident with any, there is no counterpart.
    if ( i0==_v2h_table.end()) continue;

    // check whether the two vertices are overlapping.
    V2h::iterator j0=i0->second.begin(), j1=it->second.begin();

    while ( j0 != i0->second.end() && j1 != it->second.end()) {
      if ( j0->first == j1->first) {
	bool found=false;
	V2h::iterator k0, k1;
	for ( k0=j0; k0!=i0->second.end()&&k0->first==j0->first; ++k0) {
	  if (found) continue;
	  for ( k1=j1; k1!=it->second.end()&&k1->first==j1->first; ++k1) {
	    if (found) continue;
	    // Check whether it is an edge in that pane
	    const RFC_Pane_overlay &p = _window->pane(k0->first);
	    const B2v  &b2v = p._b2v_table.find(id())->second;
	    pair<int,int> e( b2v[k0->second], b2v[k1->second]);
	    RFC_assertion( e.first>0 && e.second>0);

	    map< pair<int,int>, Halfedge*>::const_iterator 
	      b2e_it = p._bv2edges.find( e);
	    
	    if ( b2e_it != p._bv2edges.end()) {
	      _cntrs[get_border_index(&*hit)] = b2e_it->second;
	      found=true;
	    }
	    else if ( k0->first != id()) {
	      swap( e.first, e.second);
	      if (p._bv2edges.find( e) != p._bv2edges.end()) {
		std::cerr << "ERROR: Inconsistency in window \"" 
			  << _window->name() << "\"!!!! \n\tPanes " << id()
			  << " and " << j0->first 
			  << " have opposite orientation!" << std::endl
			  << "\tComputation can not continue. Aborting..."
			  << std::endl;
		RFC_assertion( p._bv2edges.find( e) == p._bv2edges.end()); 
		abort();
	      }
	    }
	  }
	}
	if ( found) { j0 = k0; j1 = k1; }
	else { ++j0; ++j1; }
      }
      else 
	if ( j0->first < j1->first) ++j0; else ++j1;
    }
  }

  // Fix physical border ones to have them point to themselves
  hit = hds().border_halfedges_begin();
  for ( vector<Halfedge*>::iterator it=_cntrs.begin(), iend=_cntrs.end(); 
	it!=iend; ++it, ++hit) {
    ++hit; RFC_assertion( hit->is_border());
    if ( *it == NULL) *it = &*hit;
  }

  // Determine the primaries of the border vertices
  _primaries.resize( hds().size_of_border_edges(), NULL);
  hit = hds().border_halfedges_begin();
  for ( vector<Vertex*>::iterator it=_primaries.begin();
	it != _primaries.end(); ++it, ++hit) {
    ++hit; RFC_assertion( hit->is_border());
    pair<Self*,int> 
      t=_window->get_primary( this, get_lid( hit->destination()));
    *it = t.first->get_vertex_from_id( t.second);
  }
}

void
RFC_Pane_overlay::construct_bvpair2edge() {
  HDS_overlay::Halfedge_iterator hit = hds().border_halfedges_begin();
  HDS_overlay::Halfedge_iterator hend = hds().halfedges_end();
  for ( ; hit != hend; ++ ++hit) {
    RFC_assertion( !hit->is_border());
    _bv2edges[ make_pair( get_lid( hit->origin()), 
			       get_lid( hit->destination()))] = &*hit;
  }
}

// Determine the counterparts for all pane border edges.
void
RFC_Window_overlay::determine_counterparts() {
  // Construct a mapping from vertex pair to border edges
  Pane_set::iterator it=_pane_set.begin(), iend=_pane_set.end();
  for (; it != iend; ++it) 
    ((RFC_Pane_overlay*)it->second)->construct_bvpair2edge();

  // Construct mapping from panal border edges to their counterpart.
  for ( it = _pane_set.begin(); it != iend; ++it)
    ((RFC_Pane_overlay*)it->second)->determine_counterparts();

  // Clear the mapping
  for ( it = _pane_set.begin(); it != iend; ++it) 
    ((RFC_Pane_overlay*)it->second)->_bv2edges.clear();
}

void
RFC_Window_overlay::unmark_alledges( ) {
  Pane_set::iterator it=_pane_set.begin(), iend=_pane_set.end();
  for ( ; it != iend; ++it) {
    RFC_Pane_overlay &p = (RFC_Pane_overlay &)*it->second;
    fill( p._e_marks.begin(), p._e_marks.end(), 0);
  }
}

// Functions for supporting the overlay algorithm
void RFC_Pane_overlay::create_overlay_data() { 
  _v_nodes.resize(0); 
  _e_node_list.resize(0);
  _e_node_buf.resize(0);
  _e_marks.resize(0);

  _v_nodes.resize( _hds.size_of_vertices(),0);
  _e_node_list.resize( _hds.size_of_halfedges(), INode_list( color()));
  _e_node_buf.resize( _hds.size_of_halfedges(), 0);
  _e_marks.resize( _hds.size_of_halfedges(), 0);
}

// Functions for supporting the overlay algorithm
void RFC_Pane_overlay::delete_overlay_data() { 
  free_vector( _v_nodes);
  free_vector( _e_node_list);
  free_vector( _e_node_buf);
  free_vector( _e_marks);

  free_vector( _cntrs); free_vector( _primaries);
  free_vector( _f_nrmls); free_vector( _f_tngts);
  free_vector( _f_n_index); free_vector( _f_t_index);
  free_vector( _ad_0); free_vector( _ea_0); free_vector( _fd_1);
}

void RFC_Window_overlay::create_overlay_data() {
  // Loop through panes
  Pane_set::iterator pit=_pane_set.begin(), piend=_pane_set.end();
  for ( ; pit != piend; ++pit) {
    RFC_Pane_overlay &p = (RFC_Pane_overlay &)*pit->second;
    p.create_overlay_data();
  }

}

void RFC_Window_overlay::delete_overlay_data() {
  // Loop through the panes to remove the buffer spaces
  Pane_set::iterator pit, piend;
  for (pit=_pane_set.begin(), piend=_pane_set.end(); pit != piend; ++pit) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*pit->second;
    if ( !pane.is_master()) continue;

    pane.delete_overlay_data();
  }
}

/*! \param idx the index of the subface (i.e., starting from 0).
 *  \param plid the local id of the parent face of the subface.
 *  \param h an halfedge of the its parent face in the pane.
 *  \param lids the local ids of the nodes of the subface.
 *  \param nc the local coordinates of the nodes of the subface within its 
 *            parent face.
 *  \param rp_id the pane id of the correponding subface in the other window.
 *  \param cnt the local id of the subface's counterpart in the other window.
 *  \param rids the local ids of the nodes of the correponding subface in the
 *            other window.
 */
void RFC_Pane_overlay::insert_subface(int idx, int plid, const int *lids, 
				      const Edge_ID *eids, const Point_2 *nc,
				      int rp_id, int cnt, const int *rids) {
  RFC_assertion( idx>=0 && idx<int(_subface_parents.size()) && cnt>=1);
  for ( int i=0; i<3; ++i) {
    _subfaces[idx][i] = lids[i];
    _subnode_parents[ lids[i]-1] = eids[i]; 
    _subnode_nat_coors[ lids[i]-1] = Point_2S(nc[i][0],nc[i][1]);
    _subnode_counterparts[ lids[i]-1] = Node_ID( rp_id, rids[i]);
  }
  _subface_parents[idx] = plid;
  _subface_counterparts[idx] = Face_ID( rp_id, cnt);
}

/*!
  Reduces values for each node form all its instances to all instances. 
*/
void 
RFC_Window_overlay::reduce_normals_to_all( MPI_Op op) {

  std::vector<void *> ptrs;
  // Loop through panes
  Pane_set::iterator pit=_pane_set.begin(), piend=_pane_set.end();
  for ( pit=_pane_set.begin(); pit != piend; ++pit) {
    RFC_Pane_overlay &pane=(RFC_Pane_overlay &)*pit->second;

    ptrs.push_back( &pane._nrmls[0]);
  }

  _map_comm.init( &ptrs[0], COM_DOUBLE, 3);
  _map_comm.begin_update_shared_nodes();
  _map_comm.reduce_on_shared_nodes( op);
  _map_comm.end_update_shared_nodes();
}

/*!
  Reduces nodal coordinates from primary nodes to others
*/
void 
RFC_Window_overlay::reduce_coordinates_to_all() {

  Pane_set::iterator it=_pane_set.begin(), iend=_pane_set.end();
  for ( it=_pane_set.begin(); it != iend; ++it) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*it->second;

    int s=pane.size_of_nodes();
    RFC_Pane_overlay::HDS::Vertex_iterator vit=pane.hds().vertices_begin(); 
    for ( int i=0; i<s; ++i, ++vit) if ( vit->halfedge()) {
      Vertex *v = &*vit, *vpri = acc.get_primary(v);
      
      if ( v != vpri) const_cast<Point_3&>(v->point()) = vpri->point();
    }
  }
}

RFC_END_NAME_SPACE






