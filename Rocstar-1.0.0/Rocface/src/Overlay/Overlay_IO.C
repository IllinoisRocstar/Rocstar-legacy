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
// $Id: Overlay_IO.C,v 1.19 2008/12/06 08:43:28 mtcampbe Exp $

//===============================================================
// This file contains the implementation of routines for exporting the 
//   common subdivision into RFC_Window_base to be used for data transfer.
// Author: Xiangmin Jiao
// Date:   06/14/2001
//===============================================================

#include "Overlay.h"
#include "Triangulation.h"

RFC_BEGIN_NAME_SPACE

/*! Insert the local id within a pane for a given subnode into 
 *  the global-to-local-mapping. The mapping is composed of two
 *  data structures, _subnode_ids storing one id for every subnode,
 *  and _subnode_imap, which stores a mapping from pane_id to local_id
 *  for the subnodes present in more than one panes.
 * \sa get_subnode_id
 */
void Overlay::
set_subnode_id( INode *i, int color, int pane_id, int l_id) {
  if ( color==BLUE) {
    int global_id = i->id();
    Node_ID &vid = _subnode_ids_b[ global_id];
    if ( vid.node_id < 0) { // Uninitialized
      vid = Node_ID( pane_id, l_id);
      _subnode_copies_b[global_id] = 1;
    }
    else {
      _subnode_imap_b[global_id][ vid.pane_id] = vid.node_id;
      std::map<int,int> &imap=_subnode_imap_b[global_id];
      RFC_assertion( imap.find(pane_id) == imap.end());
      imap[ pane_id] = l_id;
      ++_subnode_copies_b[global_id];
    }
  }
  else { 
    RFC_assertion( color==GREEN);
    int global_id = i->id();
    Node_ID &vid = _subnode_ids_g[ global_id];
    if ( vid.node_id < 0) { // Uninitialized
      vid = Node_ID( pane_id, l_id);
      _subnode_copies_g[global_id] = 1;
    }
    else {
      _subnode_imap_g[global_id][ vid.pane_id] = vid.node_id;
      std::map<int,int> &imap=_subnode_imap_g[global_id];
      RFC_assertion( imap.find(pane_id) == imap.end());
      imap[ pane_id] = l_id;
      ++_subnode_copies_g[global_id];
    }
  }
}

/*! Retrieve the local id within a given pane for a given subnode.
 * \sa set_subnode_id
 */
int Overlay::
get_subnode_id( const INode *i, int color, int pane_id) const {
  if ( color==BLUE) {
    int global_id = i->id();
    const Node_ID &vid = _subnode_ids_b[ global_id];

    if ( vid.pane_id == pane_id) 
      return vid.node_id;
    else
      return _subnode_imap_b.find( global_id)->second.find( pane_id)->second;
  }
  else { RFC_assertion( color == GREEN);
    int global_id = i->id();
    const Node_ID &vid = _subnode_ids_g[ global_id];

    if ( vid.pane_id == pane_id)
      return vid.node_id;
    else
      return _subnode_imap_g.find( global_id)->second.find( pane_id)->second;
  }
}

/*! Retrieve the number of copies for a given subnode.
 */
int Overlay::
get_subnode_copies( const INode *i, int color) const {
  if ( color==BLUE) {
    int global_id = i->id();
    return _subnode_copies_b[ global_id];
  }
  else { 
    RFC_assertion( color == GREEN);
    int global_id = i->id();
    return _subnode_copies_g[ global_id];
  }
}

/*! Retrieve the local id within a given pane for a given subnode.
 * \sa set_subnode_id
 */
void 
Overlay::convert_nat_coordinates( const INode *i, Halfedge *h, int color, 
				  int pane_id, int &lid, 
				  RFC_Pane_overlay::Edge_ID &eid, 
				  Point_2 &nc) const {
  lid = get_subnode_id( i, color, pane_id);
  i->nat_coor( color, nc);

  // ==== Determine eid
  RFC_Pane_overlay &pane = *acc.get_pane(h);
  eid.face_id = pane.get_lid( h->facet());

  // Determine eid.edge_id
  Element_node_enumerator ene( pane.base(), eid.face_id);
  Halfedge *b = i->halfedge( color);

  if ( b->facet() != h->facet()) {
    RFC_assertion( i->parent_type(color) != PARENT_FACE);
    RFC_Window_overlay &win = color==BLUE? *B : *G;
    RFC_assertion_code( int count=0); 
    // Find the local node id of b->origin() in the host pane of h
    while ( !win.is_same_node( b->origin(), h->destination())) {
      h = h->next(); RFC_assertion( ++count <= 4);
    }
    if ( i->parent_type(color) == PARENT_VERTEX)
      eid.edge_id = pane.get_lvid( ene, pane.get_lid( h->destination()));
    else {
      eid.edge_id = pane.get_lvid( ene, pane.get_lid( h->origin()));
      nc = Point_2( 1-nc[0], 0);
    }
  }
  else {
    eid.edge_id = pane.get_lvid( ene, pane.get_lid( b->origin()));
    
    if ( i->parent_type(color) == PARENT_FACE) {
      pane.normalize_nat_coor( eid.edge_id, ene.size_of_edges(), nc);
      eid.edge_id = 0;
    }
  }
}

// Assign numbers to an S-vertex within all its host panes
void Overlay::
number_a_subnode( INode *i, int color, 
		  std::map<int,std::pair<int,int> > &cnts) {
  Parent_type t = i->parent_type( color);
  Halfedge   *h = i->halfedge( color);

  switch ( t) {
  case PARENT_FACE: {
    int  pane_id = acc.get_pane(h)->id();
    set_subnode_id( i, color, pane_id, ++(cnts[ pane_id].second));
    break;
  }
  case PARENT_EDGE: {
    Halfedge *h0 = h;
    int  last_id = -1;
    do {
      int  pane_id = acc.get_pane(h)->id();
      if ( pane_id == last_id) continue;
      set_subnode_id( i, color, pane_id, ++(cnts[ pane_id].second));
      last_id = pane_id;
    } while ( (h=acc.get_opposite(h)) != h0);
    break;
  }
  default: {
    RFC_assertion( t == PARENT_VERTEX);

    Halfedge *h0 = h;
    int  last_id = -1;
    std::set<int>   panes;
    bool first_iter = true;
    do {
      int  pane_id = acc.get_pane(h)->id();
      if ( !first_iter && (pane_id==last_id||panes.find(pane_id)!=panes.end()))
	continue;
      set_subnode_id( i, color, pane_id, ++(cnts[ pane_id].first));
      panes.insert(last_id=pane_id);

      // Optimization assuming the halfedge structure is normalized
      if ( first_iter && !h->origin()->halfedge()->is_border()) break;
      first_iter = false;
    } while ( (h=acc.get_next_around_origin(h)) != h0);
    break;
  }
  }
}

// Count the number of S-vertices whose parents are vertices
void Overlay::
count_subnodes( INode *i, int color, 
		std::map<int,std::pair<int,int> > &cnts) {
  if  ( i->parent_type( color) == PARENT_VERTEX) {
    Halfedge   *h = i->halfedge( color);
    Halfedge *h0 = h;
    int  last_id = -1;
    std::set<int>   panes;
    bool first_iter = true;
    do {
      int  pane_id = acc.get_pane(h)->id();
      if ( !first_iter && (pane_id==last_id||panes.find(pane_id)!=panes.end()))
	continue;
      ++(cnts[ pane_id].second);
      panes.insert(last_id=pane_id);

      // Optimization assuming the halfedge structure is normalized
      if ( first_iter && !h->origin()->halfedge()->is_border()) break;
      first_iter = false;
    } while ( (h=acc.get_next_around_origin(h)) != h0);
  }
}

// Determine the numbering system for the S-vertices within panes.
void Overlay::
number_subnodes() {
  std::vector<RFC_Pane_overlay*>   b_ps, g_ps;
  B->panes( b_ps);  G->panes( g_ps);
  
  // Maps from pane ids to the number of S-vertices processed
  std::map< int, std::pair<int,int> >   b_vertex_counts;
  std::map< int, std::pair<int,int> >   g_vertex_counts;

  std::vector<RFC_Pane_overlay*>::iterator pi;
  for ( pi=b_ps.begin(); pi!=b_ps.end(); ++pi)
    b_vertex_counts[(*pi)->id()] = std::make_pair(0,0);
  for ( pi=g_ps.begin(); pi!=g_ps.end(); ++pi)
    g_vertex_counts[(*pi)->id()] = std::make_pair(0,0);

  // Loop through the S-vertices
  // First, count the number of subvertices host at vertices
  for ( std::list< INode*>::const_iterator 
	  it=inodes.begin(); it!=inodes.end(); ++it) {
    count_subnodes( *it, BLUE, b_vertex_counts);
    count_subnodes( *it, GREEN, g_vertex_counts);
  }

  // Second, assign IDs for subnodes
  int n=inodes.size();
  _subnode_ids_b.resize( n, Node_ID( 0,-1));
  _subnode_ids_g.resize( n, Node_ID( 0,-1));
  _subnode_copies_b.resize( n, 0);
  _subnode_copies_g.resize( n, 0);
  int i=0;
  for ( std::list< INode*>::const_iterator 
	  it=inodes.begin(); it!=inodes.end(); ++it,++i) {
    (*it)->set_id( i);
    number_a_subnode( *it, BLUE, b_vertex_counts);
    number_a_subnode( *it, GREEN, g_vertex_counts);
  }
  
  // Allocate space for subnodes.
  for ( pi=b_ps.begin(); pi!=b_ps.end(); ++pi)
    (*pi)->allocate_subnodes( b_vertex_counts[(*pi)->id()].second);
  for ( pi=g_ps.begin(); pi!=g_ps.end(); ++pi)
    (*pi)->allocate_subnodes( g_vertex_counts[(*pi)->id()].second);
}

// Determine the number system for the S-faces within panes
void Overlay::
number_subfaces() {
  std::vector<RFC_Pane_overlay*>   b_ps, g_ps;
  B->panes( b_ps);  G->panes( g_ps);

  typedef std::map< int, std::vector< int> > Subface_counts;
  Subface_counts cnts_b, cnts_g;

  std::vector<RFC_Pane_overlay*>::iterator pi;
  for ( pi=b_ps.begin(); pi!=b_ps.end(); ++pi)
    cnts_b[(*pi)->id()].resize( (*pi)->size_of_faces(), 0);
  for ( pi=g_ps.begin(); pi!=g_ps.end(); ++pi)
    cnts_g[(*pi)->id()].resize( (*pi)->size_of_faces(), 0);

  // First, count the number of S-faces by looping through all the blue faces
  for ( pi=b_ps.begin(); pi!=b_ps.end(); ++pi) {
    RFC_Pane_overlay *pane_b = *pi;
    Subface_counts::iterator cnts_it_b = cnts_b.find(pane_b->id());
    Subface_counts::iterator cnts_it_g = cnts_g.begin();

    for ( HDS::Facet_iterator fi=pane_b->hds().facets_begin(); 
	  fi!=pane_b->hds().facets_end(); ++fi) {
      // Construct a list of nodes for the blue face
      INode_const_list nodes;
      get_inodes_of_face( &*fi, nodes);
      RFC_assertion( nodes.size() > 2);

      Subface_list   sub_faces;
      // subdivide the face
      bool ret = subdivide( nodes, ++nodes.begin(), sub_faces, BLUE);
      if ( ret) {
	std::cerr << "ERROR: Got error code " << ret 
		  << " when subdividing face "
		  << fi - pane_b->hds().facets_begin() + 1
		  << " with node " 
		  << pane_b->get_index( fi->halfedge()->origin())+1
		  << " in pane " << pane_b->id() << std::endl;

	sub_faces.clear();
	subdivide( nodes, ++nodes.begin(), sub_faces, BLUE);
	RFC_assertion(!ret); abort();
      }
      if (sub_faces.empty()) {
	std::cerr << "ERROR: Error in enumerating subface in face "
		  << fi - pane_b->hds().facets_begin() + 1
		  << " in pane " << pane_b->id() << std::endl;
	RFC_assertion(!sub_faces.empty()); abort();
      }
      
      // Count the subfaces in the blue face
      for (Subface_list::iterator 
	     si=sub_faces.begin(), send=sub_faces.end(); si != send; ++si) {
	int num_tris = si->size()-2;
	Halfedge *s = get_parent_face( *si, GREEN);
	RFC_Pane_overlay *pane_g = acc.get_pane(s);
	if ( cnts_it_g->first != pane_g->id()) 
	  cnts_it_g=cnts_g.find(pane_g->id());

	cnts_it_b->second[pane_b->get_index(&*fi)] += num_tris;
	cnts_it_g->second[pane_g->get_index(s->facet())] += num_tris;
      }
    }
  }

  // Allocate space for subfaces and convert counts into offsets.
  for ( pi=b_ps.begin(); pi!=b_ps.end(); ++pi) {
    std::vector< int> &cnts = cnts_b[ (*pi)->id()];
    (*pi)->allocate_subfaces( cnts);
    if ( cnts.empty()) continue;
    // Convert counts into offsets
    int t1=cnts[0];
    cnts[0] = 0;
    for ( int i=1, n=cnts.size(); i<n; ++i) {
      int t2 = cnts[i];
      cnts[i] = cnts[i-1] + t1;
      t1 = t2;
    }
  }

  for ( pi=g_ps.begin(); pi!=g_ps.end(); ++pi) {
    std::vector< int> &cnts = cnts_g[ (*pi)->id()];
    (*pi)->allocate_subfaces( cnts);
    if ( cnts.empty()) continue;
    // Convert counts into offsets
    int t1=cnts[0];
    cnts[0] = 0;
    for ( int i=1, n=cnts.size(); i<n; ++i) {
      int t2 = cnts[i];
      cnts[i] = cnts[i-1] + t1;
      t1 = t2;
    }
  }

  Subface_counts &offsets_b=cnts_b, &offsets_g=cnts_g;

  // Third, we fill up the arrays for face-list and etc.
  for ( pi=b_ps.begin(); pi!=b_ps.end(); ++pi) {
    RFC_Pane_overlay *pane_b = *pi;
    const int pane_id_b = pane_b->id();

    Subface_counts::iterator offsets_it_b = offsets_b.find(pane_b->id());
    Subface_counts::iterator offsets_it_g = offsets_g.begin();

    for ( HDS::Facet_iterator fi=(*pi)->hds().facets_begin(); 
	  fi!=(*pi)->hds().facets_end(); ++fi) {

      // Construct a list of nodes for the blue face
      INode_const_list nodes;
      get_inodes_of_face( &*fi, nodes);
      RFC_assertion( nodes.size() > 2);

      Subface_list   sub_faces;
      // subdivide the face
      RFC_assertion_code( bool ret = )
	subdivide( nodes, ++nodes.begin(), sub_faces, BLUE);
      RFC_assertion( !ret);
      
      Halfedge         *b = fi->halfedge();
      Generic_element  e_b( count_edges(b));
      
      // output the subfaces of the blue face
      for (Subface_list::iterator 
	     si=sub_faces.begin(), send=sub_faces.end(); si != send; ++si) {
	int n = si->size(), num_tris = n-2;

	Halfedge        *g = get_parent_face( *si, GREEN);
	Generic_element  e_g( count_edges(g));

	std::vector< Point_2>   pnts_b( n), pnts_g( n);
	for ( int k=0; k<n; ++k) {
	  pnts_b[k] = get_nat_coor( *(*si)[k], e_b, b, BLUE);
	  pnts_g[k] = get_nat_coor( *(*si)[k], e_g, g, GREEN);
	}

	std::vector< Three_tuple<int> > tris( num_tris);
	Triangulation triangulation;
	// Triangulate the sub-face in both B and G.
	triangulation.triangulate( &pnts_b[0], n, &tris);

	RFC_Pane_overlay *pane_g = acc.get_pane(g);
	const int pane_id_g = pane_g->id();
	// Insert the triangles into the blue and green panes
	for ( int k=0; k<num_tris; ++k) {
	  int      lids_b[3], lids_g[3];
	  RFC_Pane_overlay::Edge_ID  eids_b[3], eids_g[3];
	  Point_2 ncs_b[3], ncs_g[3];

	  for ( int i=0; i<3; ++i) {
	    COM_assertion_msg( (*si)[tris[k][i]], "Empty intersection.");

	    convert_nat_coordinates( (*si)[tris[k][i]], b, BLUE, pane_id_b,
				     lids_b[i], eids_b[i], ncs_b[i]);
	    convert_nat_coordinates( (*si)[tris[k][i]], g, GREEN, pane_id_g,
				     lids_g[i], eids_g[i], ncs_g[i]);
	  }

	  if ( offsets_it_g->first != pane_g->id()) 
	    offsets_it_g=offsets_g.find(pane_g->id());

	  int idx_b = offsets_it_b->second[pane_b->get_index(&*fi)]++;
	  int idx_g = offsets_it_g->second[pane_g->get_index(g->facet())]++;

	  pane_b->insert_subface(idx_b, pane_b->get_lid( b->facet()), 
				 lids_b, eids_b, ncs_b,
				 pane_id_g, idx_g+1, lids_g);
	  pane_g->insert_subface(idx_g, pane_g->get_lid( g->facet()), 
				 lids_g, eids_g, ncs_g, 
				 pane_id_b, idx_b+1, lids_b);
	}
      }
    }
  }
}

void Overlay::
export_windows( RFC_Window_base *bw, RFC_Window_base *gw) {
  // Copy data from RFC_Window_overlay into RFC_Window_base.
  B->export_window( bw);
  G->export_window( gw);
}

RFC_END_NAME_SPACE






