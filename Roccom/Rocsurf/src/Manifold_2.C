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
// $Id: Manifold_2.C,v 1.32 2008/12/06 08:43:23 mtcampbe Exp $

#include "Manifold_2.h"
#include "Generic_element_2.h"
#include "../Rocmap/include/Pane_connectivity.h"
#include "../Rocmap/include/Pane_communicator.h"
#include "../Rocmap/include/Rocmap.h"
#include "../Rocblas/include/Rocblas.h"

#include <iterator>
#include "Rocsurf.h"
#include <algorithm>

SURF_BEGIN_NAMESPACE

// TODO: Save face normals so that h.normal() would be more efficient.

MPI_Op Window_manifold_2::OP_MAX=MPI_MAX;
MPI_Op Window_manifold_2::OP_MIN=MPI_MIN;
MPI_Op Window_manifold_2::OP_SUM=MPI_SUM;
MPI_Op Window_manifold_2::OP_PROD=MPI_PROD;
MPI_Op Window_manifold_2::OP_BOR=MPI_BOR;
MPI_Op Window_manifold_2::OP_BAND=MPI_BAND;
MPI_Op Window_manifold_2::OP_LOR=MPI_LOR;
MPI_Op Window_manifold_2::OP_LAND=MPI_LAND;
MPI_Op Window_manifold_2::OP_MAXABS=MPI_MAXLOC;
MPI_Op Window_manifold_2::OP_DIFF=MPI_MINLOC;

/// Default constructors
Pane_manifold_2::Pane_manifold_2() {}

/// Constructor from a pane
Pane_manifold_2::Pane_manifold_2( const COM::Pane *p,
				  const Pane_manifold_2 *parent) 
  : Simple_manifold_2( p, parent)
{
  if ( parent) {
    _cnt_pn = parent->_cnt_pn;
    _bd_cnt = parent->_bd_cnt;
    _nd_prm = parent->_nd_prm;
  }
  else; // Otherwise, they are filled by Window_manifold_2::init()
}

Pane_manifold_2::~Pane_manifold_2() {}


Halfedge Pane_manifold_2::
get_prev_edge( Edge_ID eID, Access_Mode mode) const 
{
  Edge_ID prv;
  switch ( mode) {
  case REAL_PANE:  
    prv = get_prev_real_edge( eID); break;
  case WHOLE_PANE: 
    prv = Simple_manifold_2::get_prev_edge( eID); break;
  case ACROSS_PANE: {
    // In the across-pane mode, it is illegal to access halfedges that are 
    // not on physical boundaries but on pane boundaries. So a pane-border
    // halfedge must be a physical border edge.
    if ( !eID.is_border())
    { prv = get_prev_real_edge( eID); break; }
    else { // the edge is on physical boundary.
      Halfedge h = get_opposite_edge( eID, ACROSS_PANE);
      while ( !h.is_border()) h = h.next().opposite();
      return h;
    }
  }
  default:  COM_assertion_msg(false, "Should never reach here");
  }

  return Halfedge( this, prv, mode); 
}

Halfedge Pane_manifold_2::
get_next_edge( Edge_ID eID, Access_Mode mode) const 
{
  Edge_ID nxt;
  switch ( mode) {
  case REAL_PANE:  
    nxt = get_next_real_edge( eID); break;
  case WHOLE_PANE: 
    nxt = Simple_manifold_2::get_next_edge( eID); break;
  case ACROSS_PANE: {
    // In the across-pane mode, it is illegal to access halfedges that are 
    // not on physical boundaries but on pane boundaries. So a pane-border
    // halfedge must be a physical border edge.
    if ( !is_real_border_edge(eID))
    { nxt = get_next_real_edge( eID); break; }
    else {  // the edge is on physical boundary.
      Halfedge h = get_opposite_edge( eID, ACROSS_PANE);
      while ( !h.is_border()) h = h.prev().opposite();
      return h;
    }
  }
  default:  COM_assertion_msg(false, "Should never reach here");
  }
  return Halfedge( this, nxt, mode);
}

int Pane_manifold_2::size_of_nodes( Access_Mode mode) const 
{
  switch (mode) {
  case REAL_PANE:  return size_of_real_nodes();
  case WHOLE_PANE: return Simple_manifold_2::size_of_nodes();
  case ACROSS_PANE: {
    int n = size_of_real_nodes();
    // Loop through border nodes
    for ( std::vector<Node>::const_iterator 
	    it=_nd_prm.begin(), iend=_nd_prm.end(); it != iend; ++it) {
      if ( it->pane_manifold()) --n;
    }
    return n;
  }
  default: 
    COM_assertion_msg(false, "Should never reach here");
  }

  return 0;
}

int Pane_manifold_2::size_of_edges( Access_Mode mode) const 
{
  switch (mode) {
  case REAL_PANE:  return size_of_real_edges();
  case WHOLE_PANE: return Simple_manifold_2::size_of_edges();
  case ACROSS_PANE: {
    int n = size_of_real_edges();

    int branch=0;
    // Loop through border edges
    for ( std::vector<Halfedge>::const_iterator 
	    it=_bd_cnt.begin(), iend=_bd_cnt.end(); it != iend; ++it) {
      Halfedge hopp = *it;
      if ( hopp.pane_manifold()) {
	if (hopp.pane()->id()<_pane->id()) --n;
	else if (hopp.pane()->id()==_pane->id()) ++branch;
      }
    }
    if ( branch) n-=branch/2;
    return n;
  }
  default: 
    COM_assertion_msg(false, "Should never reach here");
  }
  return 0;
}

void Window_manifold_2::init( const COM::Attribute *pmesh) {
  COM_assertion_msg( pmesh && pmesh->id()==COM::COM_MESH ||
		     pmesh->id()==COM::COM_PMESH, 
		     "Input to Window_manifold_2::init must be mesh or pmesh");
  if ( _buf_window) delete _buf_window;

  const COM::Window *w = pmesh->window();

  // Create a buffer window by inheriting from the given mesh.
  _buf_window = new COM::Window(w->name()+"-buf", w->get_communicator());
  _buf_window->inherit( const_cast<COM::Attribute*>(pmesh), "", 
			false, true, NULL, 0);

  // If pconn is not given, allocate pconn by cloning the pconn from user space
  COM::Attribute *pconn_user = 
    const_cast<COM::Attribute*>(w->attribute(COM::COM_PCONN));
  bool nopconn = ( pmesh->id() == COM::COM_MESH ||
		   MAP::Pane_connectivity::pconn_nblocks( pconn_user)==0);

  // Determine whether to compute pconn by itself.
  COM::Attribute *pconn_n = nopconn ? 
    _buf_window->inherit( pconn_user, "", true, true, NULL, 0) :
    _buf_window->attribute( COM::COM_PCONN);

  // Create an attribute for edge connectivity across panes
  COM::Attribute *pconn_e = 
    _buf_window->new_attribute( "pconn_e", 'p', COM_INT, 1, "");

  _buf_window->init_done();

  std::vector<const COM::Pane*> panes;
  _buf_window->panes(panes);

  std::vector<const MAP::Simple_manifold_2*> mani2(panes.size());

  // Initialize pane-manifolds.  
  _pms.resize( panes.size());
  for ( int i=panes.size()-1; i>=0; --i) {
    _pi_map[ panes[i]->id()] = i;  // Internal local pane-ID

    _pms[i].init( panes[i]);
    mani2[i] = &_pms[i];

    // _bd_cnt and _phy_bnd will be updated by determine_counterparts
    int nb = _pms[i].size_of_real_border_edges();
    _pms[i]._cnt_pn.resize( nb, 0);  
    _pms[i]._bd_cnt.resize( nb, Halfedge(NULL, Edge_ID(),ACROSS_PANE));

    // _nd_prm to be updated by determine_primaries
    _pms[i]._nd_prm.resize( nb+_pms[i].size_of_isolated_nodes(), 
			    Node(NULL,0,ACROSS_PANE));
  }

  // Compute node and edge correspondence if not given
  if ( !mani2.empty()) {
    MAP::Pane_connectivity pc( &mani2[0], w->get_communicator());
    pc.compute_pconn( nopconn?pconn_n:(COM::Attribute*)NULL, pconn_e);

    determine_counterparts( pconn_e); // Update _cnt_pn and _bd_cnt
    determine_primaries( pconn_n);    // Update _nd_prm
  }
  else {
    MAP::Pane_connectivity pc( pmesh, w->get_communicator());
    pc.compute_pconn( nopconn?pconn_n:(COM::Attribute*)NULL, pconn_e);
  }

  // Obtain the number of pconn blocks
  _pconn_nb = MAP::Pane_connectivity::pconn_nblocks( pconn_n);
}

void Window_manifold_2::determine_counterparts(const COM::Attribute *pconn_e) {

  const int pconn_offset=MAP::Pane_connectivity::pconn_offset();

  typedef std::map< std::pair<int,int>,
    std::pair< const int*, const int*> >  B2e_map;
  B2e_map bm;

  // First, loop through the pane-connectivity arrays in all local panes
  // to construct a mapping from the internal local pane ids (between 0 and
  // #local_panes-1) of each pair of abutting local panes to the addresses
  // in the pane-connectivity arrays corresponding to the communication
  // patterns of the two panes. This mapping is stored in B2e_map, and 
  // will allow convenient construction of the counterparts in the second step.
  // Note: As a side-product, this step also fills in _cnt_pn for the edges.
  for ( int i=0, n=_pms.size(); i<n; ++i) {
    Pane_manifold_2     &pm = _pms[i];
    const COM::Attribute *pconn_l = pm.pane()->attribute(pconn_e->id());
    const int *b2e_l = (const int*)pconn_l->pointer();

    int pane_id_l = i;

    for ( int j=pconn_offset, nj=pconn_l->size_of_real_items(); 
	  j<nj; j+=b2e_l[j+1]+2) {

      std::map<int,int>::iterator it= _pi_map.find( b2e_l[j]);
      bool is_remote = it == _pi_map.end();

      // Loop through the edges in the pconn to assign _cnt_pn.
      int pnid_other = b2e_l[j]; 
      if ( is_remote) pnid_other = -pnid_other;

      const Edge_ID *es = (const Edge_ID*)(&b2e_l[j+2]);
      for ( int k=0, nk=b2e_l[j+1]; k<nk; ++k) { 
	pm._cnt_pn[pm.get_border_edgeID(pm.get_opposite_real_edge(es[k]))-1]
	  = pnid_other;
	// Note: this handles branch-cut as well.
      }

      if ( is_remote) continue; // skip remote panes

      // Insert addresses of the pconn arrayes into B2e_map
      int pane_id_r = it->second;

      if ( pane_id_l <= pane_id_r)
	bm[ std::make_pair( pane_id_l, pane_id_r)].first = &b2e_l[j+1];
      if ( pane_id_l >= pane_id_r)
	bm[ std::make_pair( pane_id_r, pane_id_l)].second = &b2e_l[j+1];
    }
  }
  
  // Second, loop through the B2e_map to insert *local* counterparts of 
  // border-edges into _bd_cnt (i.e., if the counterpart of an edge is
  // on a remote process, the counterpart is ignored).
  for ( B2e_map::const_iterator it=bm.begin(); it!=bm.end(); ++it) {
    Pane_manifold_2     *p1 = &_pms[it->first.first];
    Pane_manifold_2     *p2 = &_pms[it->first.second];
    const int *v1 = it->second.first;
    const int *v2 = it->second.second;

    int is_branchcut = (it->first.first == it->first.second);
    COM_assertion_msg( ! is_branchcut || *v1%2==0, 
		       "Branch-cut must list nodes in pairs.");
    
    for ( int k=1, nk=*v1; k<=nk; ++k) {
      const Edge_ID e1 = (Edge_ID&)v1[k];
      const Edge_ID e2 = (Edge_ID&)v2[k+is_branchcut];
      
      p1->_bd_cnt[p1->get_border_edgeID(p1->get_opposite_real_edge(e1))-1] =
	Halfedge(p2, e2, ACROSS_PANE);
      
      p2->_bd_cnt[p2->get_border_edgeID(p2->get_opposite_real_edge(e2))-1] =
	Halfedge(p1, e1, ACROSS_PANE);

      if (is_branchcut) ++k;
    }
  }
}

void Window_manifold_2::determine_primaries( const COM::Attribute *pconn_n) {
  // Using the pane connectivity to construct a helper map bm, which 
  // maps from the pane IDs of each pair of incident panes to its 
  // correponding arrays in b2v.
  // Note in each pane-ID pair, the first ID is no greater than the second.
  typedef std::map< std::pair<int,int>, 
    std::pair< const int*, const int*> >  B2v_map;
  B2v_map bm;

  const int pconn_offset=MAP::Pane_connectivity::pconn_offset();

  for ( int i=0, n=_pms.size(); i<n; ++i) {
    const COM::Attribute *pconn_l = _pms[i].pane()->attribute(pconn_n->id());
    const int *b2v_l = (const int*)pconn_l->pointer();
    int pane_id_l = i;
    
    for ( int j=pconn_offset, nj=pconn_l->size_of_real_items(); 
	  j<nj; j+=b2v_l[j+1]+2) {

      std::map<int,int>::iterator it= _pi_map.find( b2v_l[j]);
      if ( it == _pi_map.end()) continue; // skip remote panes

      int pane_id_r = it->second;

      if ( pane_id_l <= pane_id_r)
	bm[ std::make_pair( pane_id_l, pane_id_r)].first = &b2v_l[j+1];
      if ( pane_id_l >= pane_id_r)
	bm[ std::make_pair( pane_id_r, pane_id_l)].second = &b2v_l[j+1];
    }
  }
  
  // Now loop through bm to compute _nd_prm within the process.
  for ( B2v_map::const_iterator it=bm.begin(); it!=bm.end(); ++it) {
    Pane_manifold_2     *p1 = &_pms[it->first.first];
    Pane_manifold_2     *p2 = &_pms[it->first.second];
    const int *v1 = it->second.first, *v2=it->second.second;

    int is_branchcut = (it->first.first == it->first.second);
    COM_assertion_msg( ! is_branchcut || *v1%2==0, 
		       "Branch-cut must list nodes in pairs.");

    for ( int k=1, nk=*v1; k<=nk; ++k) {
      Node node1( p1, v1[k], ACROSS_PANE);
      Node node2( p2, v2[k+is_branchcut], ACROSS_PANE);

      int i1 = p1->get_bnode_index( v1[k]);
      if ( i1>=0 && !node2.is_isolated() &&
	   ( node2.halfedge().is_border() || 
	     node1.is_isolated() && p1->_nd_prm[i1].pane()==0 )) {
	p1->_nd_prm[i1] = node2;
      }
      else {
	int i2 = p2->get_bnode_index( v2[k+is_branchcut]);
	if ( i2 >= 0 && !node1.is_isolated() &&
	     (node1.halfedge().is_border() || p2->_nd_prm[i2].pane()==0)) {
	  p2->_nd_prm[i2] = node1;
	}
      }

      if (is_branchcut) ++k;
    }
  }
}

Window_manifold_2::~Window_manifold_2() { 
  if (_cc) delete _cc; _cc=NULL;
  if ( _buf_window) delete _buf_window;
}

void Window_manifold_2::init_communicator() {
  if ( _cc) { delete _cc; }
  
  _cc = new MAP::Pane_communicator( _buf_window, 
				    _buf_window->get_communicator());
}

/// Obtain shortest edge length of incident edges of each node or element.
void Window_manifold_2::
shortest_edge_length( COM::Attribute *lens) {
  COM_assertion_msg( lens && (lens->is_nodal() || lens->is_elemental()),
		     "Argument must be nodal or elemental attribute");
  COM_assertion_msg( lens->size_of_components()== 1 &&
		     COM_compatible_types(lens->data_type(), COM_DOUBLE),
		     "Argument must be double-precision scalars"); 

  if ( lens->is_nodal()) {
    compute_shortest_edgelen_nodes( lens);
  }
  else {
    compute_shortest_edgelen_elements( lens);   
  }
}

/// Obtain shortest edge length of incident edges of each element.
void Window_manifold_2::
compute_shortest_edgelen_nodes( COM::Attribute *lens) {
  std::vector< COM:: Pane*> panes;
  lens->window()-> panes( panes); 
  std::vector< COM::Pane*>::const_iterator it = panes.begin();

  // Initialize to HUGE_VAL
  double inf=HUGE_VAL;
  Rocblas::copy_scalar( &inf, lens);

  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    const Point_3<Real> *pnts = 
      reinterpret_cast<const Point_3<Real>*>(pane.coordinates()); 
    Real *lp = (Real *)(pane.attribute( lens->id())->pointer());
    
    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1);
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next()) {
      int nn=ene.size_of_nodes();
      int uindex = ene[0]-1;
      for (int k = 0, ne=ene.size_of_edges(); k<ne; k++){
	int vindex=ene[(k+1)%ne]-1;
	double sqlen = (pnts[ uindex]-pnts[ vindex]).squared_norm();

	lp[uindex] = std::min( lp[uindex], sqlen);
	lp[vindex] = std::min( lp[vindex], sqlen);
	if ( nn>ne) lp[uindex+ne] = std::min( lp[uindex+ne], sqlen);
	if ( nn>ne+ne) lp[uindex+ne+ne] = std::min( lp[uindex+ne+ne], sqlen);

	uindex = vindex;
      }
    }
  }

  // Reduce on shared nodes and then compute squared roots.
  reduce_on_shared_nodes( lens, OP_MIN);
  Rocblas::sqrt( lens, lens);
}


/// Obtain shortest edge length of incident edges of each element.
void Window_manifold_2::
compute_shortest_edgelen_elements( COM::Attribute *lens) {
  std::vector< COM:: Pane*> panes;
  lens->window()-> panes( panes); 
  std::vector< COM::Pane*>::const_iterator it = panes.begin();
  
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    const Point_3<Real> *pnts = 
      reinterpret_cast<const Point_3<Real>*>(pane.coordinates()); 
    Real *lp = (Real *)(pane.attribute( lens->id())->pointer());
    
    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next(),++lp) {
      Real sqlen = HUGE_VAL;

      int uindex = ene[0]-1;
      for (int k = 0, ne=ene.size_of_edges(); k<ne; k++){
	int vindex=ene[(k+1)%ne]-1;
	sqlen = std::min( sqlen, (pnts[ uindex]-pnts[ vindex]).squared_norm());
	uindex = vindex;
      }
      *lp = std::sqrt(sqlen);
    }
  }
}

void Window_manifold_2::
update_bd_normals( const COM::Attribute *normals,
		   bool to_normalize) {
  const COM::Attribute *normals_inherited = normals;
  COM_assertion( normals->size_of_components()== 3 &&
		 COM_compatible_types(normals->data_type(), COM_DOUBLE));

  // Allocate buffer arrays
  if ( normals->window()!=_buf_window)
    normals_inherited = _buf_window->inherit
      ( const_cast< COM::Attribute *>(normals), "facenormals__CNNTEMP", 
	false, true, NULL, 0);
  COM::Attribute *nrm=_buf_window->new_attribute( "normals__PMTEMP", 
						  'p', COM_DOUBLE, 3, "");
  COM::Attribute *pconn_g=_buf_window->new_attribute( "pconn__PMTEMP", 
						      'p', COM_INT, 1, "");
  COM::Attribute *pconn_e=_buf_window->attribute( "pconn_e");


  // Fill in normals and pconn
  for ( PM_iterator it=pm_begin(), iend=pm_end(); it != iend; ++it) {
    COM::Pane *pn = const_cast<COM::Pane*>(it->pane());
    const Vector_3<Real> *nrms_face = reinterpret_cast<Vector_3<Real> *>
      (pn->attribute( normals_inherited->id())->pointer());

    COM::Attribute *nrm_pn = pn->attribute( nrm->id());
    int n = it->size_of_real_border_edges();
    nrm_pn->set_size( n, 0);
    it->_bd_nrms.resize( n);

    Vector_3<Real> *nrms_bd = &it->_bd_nrms[0];
    _buf_window->set_array( nrm->name(), pn->id(), nrms_bd);

    for ( int i=0; i<n; ++i) {
      Edge_ID eid=it->get_opposite_real_edge(Edge_ID(i+1, Edge_ID::BndID));
      
      nrms_bd[i] = nrms_face[eid.eid()-1];
      if ( to_normalize) nrms_bd[i].normalize();
    }

    // Construct pane-connectivity
    it->convert_pconn_edge2ghost( pn->attribute( pconn_e->id()), 
				  pn->attribute( pconn_g->id()));
  }
  _buf_window->init_done(false);

  // Perform communication
  MAP::Rocmap::update_ghosts( nrm, pconn_g);

  _buf_window->delete_attribute( pconn_g->name());
  _buf_window->delete_attribute( nrm->name());
  if ( normals_inherited != normals)
    _buf_window->delete_attribute( normals_inherited->name());
  _buf_window->init_done(false);
}

void Window_manifold_2::
update_bd_flags( const COM::Attribute *flags) {
  const COM::Attribute *flags_inherited = flags;
  COM_assertion( flags->size_of_components()== 1 &&
		 COM_compatible_types(flags->data_type(), COM_INT));

  // Allocate buffer arrays
  if ( flags->window()!=_buf_window)
    flags_inherited = _buf_window->inherit
      ( const_cast< COM::Attribute *>(flags), "faceflags__CNNTEMP", 
	false, true, NULL, 0);
  COM::Attribute *flg=_buf_window->new_attribute( "flags__PMTEMP", 
						  'p', COM_INT, 1, "");
  COM::Attribute *pconn_g=_buf_window->new_attribute( "pconn__PMTEMP", 
						      'p', COM_INT, 1, "");
  COM::Attribute *pconn_e=_buf_window->attribute( "pconn_e");

  // Fill in flags and pconn
  for ( PM_iterator it=pm_begin(), iend=pm_end(); it != iend; ++it) {
    COM::Pane *pn = const_cast<COM::Pane*>(it->pane());
    const int *flgs_face = reinterpret_cast<int *>
      (pn->attribute( flags_inherited->id())->pointer());

    COM::Attribute *flg_pn = pn->attribute( flg->id());
    int n = it->size_of_real_border_edges();
    flg_pn->set_size( n, 0);
    it->_bd_flgs.resize( n);

    int *flgs_bd = &it->_bd_flgs[0];
    _buf_window->set_array( flg->name(), pn->id(), flgs_bd);

    for ( int i=0; i<n; ++i) {
      Edge_ID eid=it->get_opposite_real_edge(Edge_ID(i+1, Edge_ID::BndID));
      
      flgs_bd[i] = flgs_face[eid.eid()-1];
    }

    // Construct pane-connectivity
    it->convert_pconn_edge2ghost( pn->attribute( pconn_e->id()), 
				  pn->attribute( pconn_g->id()));
  }
  _buf_window->init_done(false);

  // Perform communication
  MAP::Rocmap::update_ghosts( flg, pconn_g);

  _buf_window->delete_attribute( pconn_g->name());
  _buf_window->delete_attribute( flg->name());
  if ( flags_inherited != flags)
    _buf_window->delete_attribute( flags_inherited->name());
  _buf_window->init_done(false);
}

void Window_manifold_2::
update_bdedge_bitmap( const COM::Attribute *bitmap) {
  const COM::Attribute *bm_inherited = bitmap;
  COM_assertion( bm_inherited->size_of_components()== 1 &&
		 COM_compatible_types(bm_inherited->data_type(), COM_CHAR));

  // Allocate buffer arrays
  if ( bm_inherited->window()!=_buf_window)
    bm_inherited = _buf_window->inherit
      ( const_cast< COM::Attribute *>(bitmap), "facebm__CNNTEMP", 
	false, true, NULL, 0);
  COM::Attribute *bm=_buf_window->new_attribute( "bm__PMTEMP", 
						 'p', COM_CHAR, 1, "");
  COM::Attribute *pconn_g=_buf_window->new_attribute( "pconn__PMTEMP", 
						      'p', COM_INT, 1, "");
  COM::Attribute *pconn_e=_buf_window->attribute( "pconn_e");

  // Fill in bm and pconn
  for ( PM_iterator it=pm_begin(), iend=pm_end(); it != iend; ++it) {
    COM::Pane *pn = const_cast<COM::Pane*>(it->pane());
    const char *bms_face = reinterpret_cast<char *>
      (pn->attribute( bm_inherited->id())->pointer());

    COM::Attribute *bm_pn = pn->attribute( bm->id());
    int n = it->size_of_real_border_edges();
    bm_pn->set_size( n, 0);
    it->_bd_bm.resize( n);

    char *bms_bd = &it->_bd_bm[0];
    _buf_window->set_array( bm->name(), pn->id(), bms_bd);

    for ( int i=0; i<n; ++i) {
      Edge_ID eid=it->get_opposite_real_edge(Edge_ID(i+1, Edge_ID::BndID));
      
      bms_bd[i] = bms_face[eid.eid()-1] & (1<<eid.lid());
    }

    // Construct pane-connectivity
    it->convert_pconn_edge2ghost( pn->attribute( pconn_e->id()), 
				  pn->attribute( pconn_g->id()));
  }
  _buf_window->init_done(false);

  // Perform communication
  MAP::Rocmap::update_ghosts( bm, pconn_g);

  _buf_window->delete_attribute( pconn_g->name());
  _buf_window->delete_attribute( bm->name());
  if ( bm_inherited != bitmap)
    _buf_window->delete_attribute( bm_inherited->name());
  _buf_window->init_done(false);
}

void Window_manifold_2::
accumulate_bd_values( const COM::Attribute *vals) {
  COM_assertion( vals->size_of_components()==1);
  const COM::Attribute *vals_inherited = vals;
  // Allocate buffer arrays
  if ( vals->window()!=_buf_window)
    vals_inherited = _buf_window->inherit
      ( const_cast< COM::Attribute *>(vals), "vals__CNNTEMP", false, true, NULL, 0);
  COM::Attribute *buf=_buf_window->new_attribute( "buf__PMTEMP", 'p', 
						  vals->data_type(), 1, "");
  COM::Attribute *pconn_g=_buf_window->new_attribute( "pconn__PMTEMP", 
						      'p', COM_INT, 1, "");
  COM::Attribute *pconn_e=_buf_window->attribute( "pconn_e");

  int size_base_type = COM_get_sizeof( vals->data_type(),1);

  // Fill in vals and pconn
  for ( PM_iterator it=pm_begin(), iend=pm_end(); it != iend; ++it) {
    COM::Pane *pn = const_cast<COM::Pane*>(it->pane());
    const char *vals_face = reinterpret_cast<char *>
      (pn->attribute( vals_inherited->id())->pointer());

    COM::Attribute *buf_pn = pn->attribute( buf->id());
    int n = it->size_of_real_border_edges();
    buf_pn->set_size( n, 0);

    char *val_bd;
    _buf_window->resize_array( buf->name(), pn->id(), (void**)&val_bd);

    for ( int i=0; i<n; ++i) {
      Edge_ID eid=it->get_opposite_real_edge(Edge_ID(i+1, Edge_ID::BndID));
      int fid = eid.eid();

      std::copy( &vals_face[(fid-1)*size_base_type],
		 &vals_face[fid*size_base_type], &val_bd[i*size_base_type]);
    }

    // Construct pane-connectivity
    it->convert_pconn_edge2ghost( pn->attribute( pconn_e->id()), 
				  pn->attribute( pconn_g->id()));
  }
  _buf_window->init_done(false);

  // Perform communication
  MAP::Rocmap::update_ghosts( buf, pconn_g);

  // Reduce on the values.
  for ( PM_iterator it=pm_begin(), iend=pm_end(); it != iend; ++it) {
    COM::Pane *pn = const_cast<COM::Pane*>(it->pane());
    char *vals_face = reinterpret_cast<char *>
      (pn->attribute( vals_inherited->id())->pointer());

    int n = it->size_of_real_border_edges();

    COM::Window::Pointer_descriptor ptr(NULL);
    _buf_window->get_array( buf->name(), pn->id(), ptr);
    char *val_bd = (char*)ptr.ptr;

    for ( int i=0; i<n; ++i) {
      Edge_ID eid=it->get_opposite_real_edge(Edge_ID(i+1, Edge_ID::BndID));
      
      switch (vals->data_type()) {
      case COM_CHAR:	vals_face[eid.eid()-1] += val_bd[i]; break;
      case COM_INT:	
      case COM_INTEGER: 
	(int&)vals_face[(eid.eid()-1)*size_base_type] += 
	  (int&)val_bd[i*size_base_type]; break;
      case COM_FLOAT:	
      case COM_REAL: 
	(float&)vals_face[(eid.eid()-1)*size_base_type] += 
	  (float&)val_bd[i*size_base_type]; break;
      case COM_DOUBLE:
      case COM_DOUBLE_PRECISION: 
	(double&)vals_face[(eid.eid()-1)*size_base_type] += 
	  (double&)val_bd[i*size_base_type]; break;
      default:
	COM_assertion_msg( false, "Unsupported type for reduction"); break;
    }
    }
  }

  _buf_window->delete_attribute( pconn_g->name());
  _buf_window->delete_attribute( buf->name());
  if ( vals_inherited != vals)
    _buf_window->delete_attribute( vals_inherited->name());
  _buf_window->init_done(false);
}

void Pane_manifold_2::
convert_pconn_edge2ghost( const COM::Attribute *pconn_e, 
			  COM::Attribute *pconn_g) {
  const int *b2e_l = (const int*)pconn_e->pointer();

  int n=pconn_e->size_of_items();
  pconn_g->set_size( n+n+1, n+n);
  int *buf = (int*)pconn_g->allocate( 1, n+n+1, true);

  buf[0] = 0;         // Shared-node part is empty
  buf[1] = b2e_l[ 0]; // Number of communicating panes

  // Extracting the edge-ID part.
  for ( int j=1; j<n; j+=b2e_l[j+1]+2) {
    buf[j+1] = b2e_l[j]; // Pane ID
    buf[j+2] = b2e_l[j+1]; // Number of items

    const Edge_ID *es = (const Edge_ID*)(&b2e_l[j+2]);
    for ( int k=0, nk=b2e_l[j+1]; k<nk; ++k) { 
      buf[j+k+3] = get_opposite_real_edge(es[k]).eid();
      COM_assertion( buf[j+k+3]>=1 && buf[j+k+3]<=size_of_real_border_edges());
    }
  }

  // Copy the sending part into the receiving part.
  std::copy( &buf[1], &buf[n+1], &buf[n+1]);
}

// Evaluate nodal normals
void Window_manifold_2::
compute_nodal_normals( COM::Attribute *nrms, 
		       int scheme,
		       bool to_normalize,
		       const COM::Attribute *weights) {
  COM_assertion_msg( weights==NULL || weights->is_elemental(),
		     "Weights for elemental normals must be elemental");
  if ( _cc==NULL) init_communicator();

  // Inherit normals onto the window
  COM::Attribute *nodal_normals;
  if ( nrms->window() != _buf_window)
    nodal_normals = _buf_window->inherit( nrms, "nodal_normals__CNNTEMP", 
					  false, true, NULL, 0);
  else 
    nodal_normals = nrms;

  COM::Attribute *elem_normals = _buf_window->
    new_attribute( "elem_normals__CNNTEMP", 'e', COM_DOUBLE, 3, "");
  _buf_window->resize_array( elem_normals, NULL);
  _buf_window->init_done();

  if ( scheme == E2N_AREA) {
    int normalize=false;
    Rocsurf::compute_element_normals( elem_normals, &normalize);
    elements_to_nodes( elem_normals, nodal_normals, E2N_ONE, NULL, NULL, true);
  }
  else {
    Rocsurf::compute_element_normals( elem_normals);
    elements_to_nodes( elem_normals, nodal_normals, scheme, weights);
  }

  // Normalize the vectors
  if ( to_normalize) {
    std::vector<COM::Pane*>::iterator it=_cc->panes().begin();
    for (int i=0, n=_cc->panes().size(); i<n; ++i, ++it) {
      int nnodes = (*it)->size_of_real_nodes();
      Vector_3<double>* ptrs = 
	(Vector_3<double>*)(*it)->attribute(nodal_normals->id())->pointer();
      for ( int j=0; j<nnodes; ++j)
	ptrs[j].normalize();
    }
  }

  // Deallocate buffer in reverse order
  _buf_window->delete_attribute( elem_normals->name());
  if ( nrms->window() != _buf_window)
    _buf_window->delete_attribute( nodal_normals->name());
  _buf_window->init_done( false);
}

// Convert elemental values to nodal values.
void Window_manifold_2::
elements_to_nodes( const COM::Attribute *e_vals, 
		   COM::Attribute *n_vals, 
		   const int scheme,
		   const COM::Attribute *e_weights,
		   COM::Attribute *n_weights,
		   const int tosum) {
  COM_assertion_msg( e_vals && e_vals->is_elemental(),
		     "First argument must be elemental attribute");
  COM_assertion_msg( n_vals && n_vals->is_nodal(),
		     "Second argument must be nodal attribute");
  COM_assertion_msg( scheme!=E2N_USER || 
		     e_weights && e_weights->is_elemental(),
		     "Third argument must be elemental attribute");
  COM_assertion_msg( !n_weights || n_weights->is_nodal() && 
		     COM_compatible_types(COM_DOUBLE, n_weights->data_type()),
		     "Output weights must be nodal with double precision");

  // Inherit nodal and elemental values onto the window
  COM::Attribute *nodal_vals;
  if ( n_vals->window() != _buf_window)
    nodal_vals = _buf_window->inherit( n_vals, "nodal_vals__E2NTEMP", 
				       false, true, NULL, 0);
  else 
    nodal_vals = n_vals;

  const COM::Attribute *elem_vals;
  if ( e_vals->window() != _buf_window)
    elem_vals = _buf_window->inherit
      ( const_cast<COM::Attribute*>(e_vals), "elem_vals__E2NTEMP", 
	false, true, NULL, 0);
  else
    elem_vals = e_vals;

  // Inherit nodal and elemental weights onto the window
  COM::Attribute *nodal_weights; 
  if ( n_weights && n_weights->window()!=_buf_window) 
    nodal_weights = _buf_window->inherit( n_weights, "nodal_weights__E2NTEMP", 
					  false, true, NULL, 0);
  else {
    nodal_weights = _buf_window->new_attribute( "nodal_weights__E2NTEMP", 'n',
						COM_DOUBLE, 1, "");
    _buf_window->resize_array( nodal_weights, NULL);
  }

  const COM::Attribute *elem_weights; 
  if ( e_weights && e_weights->window()!=_buf_window) 
    elem_weights = _buf_window->inherit
      ( const_cast<COM::Attribute*>(e_weights), "elem_weights__E2NTEMP", 
	false, true, NULL, 0);
  else
    elem_weights = e_weights;
  _buf_window->init_done( false);

  // Initialize communicator
  if ( _cc==NULL) init_communicator();
  int local_npanes = _cc->panes().size();

  // Initialize buffer spaces for nodal weights.
  std::vector< Real*> weights_ptrs(local_npanes);
  std::vector< int>   weights_strds(local_npanes);
  
  int ncomp = nodal_vals->size_of_components();
  COM_assertion_msg( elem_vals->size_of_components()==ncomp,
		     "Numbers of components must match");

  for (int i=0; i<local_npanes; ++i) {
    int nn=_cc->panes()[i]->size_of_real_nodes();
    Real *p;
    int  strd;
    COM::Attribute *a = _cc->panes()[i]->attribute(nodal_weights->id());
    p = weights_ptrs[i] = reinterpret_cast<Real*>(a->pointer());
    strd = weights_strds[i] = a->stride();

    // Initialize values to 0.
    for ( int k=0; k<nn; ++k, p+=strd) *p = 0.;
  }

  Vector_3<Real> J[2];
  Vector_2<Real> nc(0.5,0.5);

  Element_node_vectors_k_const<Point_3<Real> > ps;
  Element_vectors_k_const<Real>                elem_vals_evk;
  Element_node_vectors_k<Real>                 nodal_vals_evk;
  Element_vectors_k_const<Real>                elem_weights_evk;
  Element_node_vectors_k<Real>                 nodal_weights_evk;

  // Compute nodal sums and weights on each processor
  std::vector< COM::Pane*>::const_iterator it=_cc->panes().begin();
  for (int i=0; i<local_npanes; ++i, ++it) { // Loop through the panes
    COM::Pane &pane = **it;

    const Point_3<Real> *pnts = reinterpret_cast<const Point_3<Real>*>
      (pane.coordinates());
    const COM::Attribute *elem_vals_pane = 
      pane.attribute( elem_vals->id());
    const COM::Attribute *elem_weights_pane = 
      elem_weights ? pane.attribute( elem_weights->id()) : NULL;
    COM::Attribute *nodal_vals_pane = 
      pane.attribute( nodal_vals->id());

    // Initialize values of nodal_vals_pane to 0.
    for ( int d=1; d<=ncomp; ++d) {
      COM::Attribute *nvpi = ncomp==1?nodal_vals_pane:(nodal_vals_pane+d);
      Real *p=reinterpret_cast<Real*>(nvpi->pointer());
      for ( int j=0,s=nvpi->stride(),n=nvpi->size_of_real_items()*s; j<n; j+=s)
	p[j] = 0.;
    }

    // Loop through the elements of the pane
    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_real_elements(); j>0; --j, ene.next()) {
      ps.set( pnts, ene, 1);
      elem_vals_evk.set( elem_vals_pane, ene);
      nodal_vals_evk.set( nodal_vals_pane, ene);
      nodal_weights_evk.set( weights_ptrs[i], ene, weights_strds[i]);

      int ne=ene.size_of_edges();
      int nn=ene.size_of_nodes();
      Real w = 1.;
      switch ( scheme) {
      case E2N_USER:
      case E2N_AREA:
	if ( scheme == E2N_USER) {
	  // Use user specified weights.
	  elem_weights_evk.set( elem_weights_pane, ene);
	  w = elem_weights_evk[0];
	} // Continue to the case of E2N_ONE
	else { 
	  Generic_element_2 e(ne, nn);
	  e.Jacobian( ps, nc, J);
	  
	  const Vector_3<Real> v = Vector_3<Real>::cross_product( J[0], J[1]);
	  w = std::sqrt(v.squared_norm());
	  if ( ne==3) w*=0.5;
	}  // Continue to the case of E2N_ONE
      case E2N_ONE: {
	// Update nodal weights
	for ( int k=0; k<nn; ++k)
	  nodal_weights_evk[k] += w;
	
	// Update nodal sums
	for ( int d=0; d<ncomp; ++d) {
	  Real t = w*elem_vals_evk(0,d);
	  for ( int k=0; k<nn; ++k)
	    nodal_vals_evk(k,d) += t;
	}
	break;
      }
      case E2N_ANGLE:
      case E2N_SPHERE: {
	for ( int k=0; k<ne; ++k) { 
	  J[0] = ps[k==ne-1?0:k+1]-ps[k]; J[1] = ps[k?k-1:ne-1]-ps[k]; 
	  double s = std::sqrt((J[0]*J[0])*(J[1]*J[1]));
	  if ( s>0) {
	    double cosw = J[0]*J[1]/s; 
	    if (cosw>1) cosw=1; else if ( cosw<-1) cosw=-1;
	    w = std::acos( cosw);

	    if ( scheme==SURF::E2N_SPHERE)
	      w = std::sin(w)/s; 

	    // Update nodal weights
	    nodal_weights_evk[k] += w;
	    
	    // Update nodal sums
	    for ( int d=0; d<ncomp; ++d)
	      nodal_vals_evk(k,d) += w*elem_vals_evk(0,d);
	  }
	}
	for ( int k=ne; k<nn; ++k) {
	  // Update nodal weights
	  nodal_weights_evk[k] += 1;

	  // Update nodal sums
	  for ( int d=0; d<ncomp; ++d)
	    nodal_vals_evk(k,d) += elem_vals_evk(0,d);
	}
	break;
      }

      default: COM_assertion_msg(false, "Should never reach here");
      }
    }
  }

  // Performan reductions on shared nodes for nodal sums
  reduce_on_shared_nodes( nodal_vals, OP_SUM);

  // Performan reductions on shared nodes for nodal weights
  _cc->init( &(void*&)weights_ptrs[0], COM_DOUBLE, 1, NULL, NULL);
  _cc->begin_update_shared_nodes();
  _cc->reduce_on_shared_nodes( MPI_SUM);
  _cc->end_update_shared_nodes();

  if ( !tosum) {
    // Divide nodal sums by weights on each processor
    it=_cc->panes().begin();
    for (int i=0; i<local_npanes; ++i, ++it) { // Loop through the panes
      COM::Pane &pane = **it;
      COM::Attribute *nodal_vals_pane = pane.attribute( nodal_vals->id());

      for ( int d=1; d<=ncomp; ++d) {
	COM::Attribute *nvpi = ncomp==1?nodal_vals_pane:(nodal_vals_pane+d);
	Real *v=reinterpret_cast<Real*>(nvpi->pointer());
	Real *w = weights_ptrs[i];
	for ( int j=0,js=nvpi->stride(),n=nvpi->size_of_real_items()*js,
		k=0, ks=weights_strds[i]; j<n; j+=js, k+=ks) {
	  if ( w[k]==0) {
	    std::cout << "***Rocsurf Error: Got zero weight for node " 
		      << j+1 << " in pane " << pane.id() << std::endl;
	  }
	  if ( w[k] == 0) v[j] = 0;
	  else v[j] /= w[k];
	}
      }
    }
  }

  // Delete the temporary attributs in reverse order.
  if ( elem_weights != e_weights) 
    _buf_window->delete_attribute( elem_weights->name());
  _buf_window->delete_attribute( nodal_weights->name());
  if ( elem_vals != e_vals) 
    _buf_window->delete_attribute( elem_vals->name());
  if (nodal_vals != n_vals) 
    _buf_window->delete_attribute( nodal_vals->name());
  _buf_window->init_done( false);
}

void Window_manifold_2::compute_normals( COM::Attribute *normal,
					 int scheme,
					 bool to_normalize) {
  COM_assertion_msg( normal, "Encountered NULL pointer/");
  COM_assertion_msg( normal->size_of_components()==3,
		     "Normal must have three components");
  COM_assertion_msg( COM_compatible_types(normal->data_type(), COM_DOUBLE),
		     "Base type of Normal must be double precision"); 

  if ( normal->is_nodal()) {
    compute_nodal_normals( normal, scheme, to_normalize);
  }
  else {
    COM_assertion_msg( normal->is_elemental(), 
		       "Normal must be nodal or elemental");
    int to_nrm = to_normalize;
    Rocsurf::compute_element_normals( normal, &to_nrm);
  }
}

void Window_manifold_2::perturb_mesh( double range) {
  // Allocate buffer arrays
  COM::Attribute *nrm=_buf_window->new_attribute( "normals__PMTEMP", 
						  'n', COM_DOUBLE, 3, "");
  _buf_window->resize_array( nrm, NULL);
  
  COM::Attribute *rnd=_buf_window->new_attribute( "randoms__PMTEMP", 
						  'n', COM_DOUBLE, 1, "");
  _buf_window->resize_array( rnd, NULL);

  COM::Attribute *lens=_buf_window->new_attribute( "lengths__PMTEMP", 
						   'n', COM_DOUBLE, 1, "");
  _buf_window->resize_array( lens, NULL);

  _buf_window->init_done( false);

  // Obtain random numbers between -range and +range
  double dbl_range=range*2;
  Rocblas::rand_scalar( &dbl_range, rnd);
  Rocblas::sub_scalar( rnd, &range, rnd);
  reduce_on_shared_nodes( rnd, OP_MAXABS);

  // Compute normal direction and multiply it with random numbers
  compute_nodal_normals( nrm);
  compute_shortest_edgelen_nodes( lens);
  Rocblas::mul( rnd, lens, rnd);
  Rocblas::mul( nrm, rnd, nrm);

  // Add normal components to coordinates
  COM::Attribute *nc = _buf_window->attribute(COM::COM_NC);
  Rocblas::add( nc, nrm, nc);

  // Deallocate buffers
  _buf_window->delete_attribute( lens->name());
  _buf_window->delete_attribute( rnd->name());
  _buf_window->delete_attribute( nrm->name());
  _buf_window->init_done( false);
}

void Window_manifold_2::
reduce_on_shared_nodes( COM::Attribute *attr, MPI_Op op) {
  COM_assertion( attr->is_nodal());

  COM::Attribute *attr_inherited = attr;
  if ( attr->window() != _buf_window) {
    // If attribute is not on buffer-window, inherit it.
    attr_inherited = _buf_window->inherit(attr, "SuRf_ReDuCe_On_ShArEd_TeMp__",
					  COM::Pane::INHERIT_USE, true, NULL, 0);
    _buf_window->init_done( false);
  }
  _cc->init( attr_inherited);
  _cc->begin_update_shared_nodes();
  if ( op == OP_MAXABS) 
    _cc->reduce_maxabs_on_shared_nodes();
  else if ( op == OP_DIFF)
    _cc->reduce_diff_on_shared_nodes();
  else
    _cc->reduce_on_shared_nodes( op);
  _cc->end_update_shared_nodes();
  
  if ( attr_inherited != attr) {
    _buf_window->delete_attribute( attr_inherited->name());
    _buf_window->init_done( false);
  }
}

int
Window_manifold_2::size_of_nodes( Access_Mode mode) const {
  int n = 0;
  // Loop through panes
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( ; it != iend; ++it) {
    n += it->size_of_nodes( mode);
  }
  return n;
}

int 
Window_manifold_2::size_of_faces( Access_Mode mode) const {
  int n = 0;
  // Loop through panes
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( ; it != iend; ++it) {
    n += it->size_of_faces( mode);
  }

  return n;
}

int 
Window_manifold_2::size_of_triangles( Access_Mode mode) const {
  int n = 0;
  // Loop through panes
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( ; it != iend; ++it) {
    n += it->size_of_triangles( mode);
  }

  return n;
}

int 
Window_manifold_2::size_of_quadrilaterals( Access_Mode mode) const {
  int n = 0;
  // Loop through panes
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( ; it != iend; ++it) {
    n += it->size_of_quadrilaterals( mode);
  }

  return n;
}

int 
Window_manifold_2::size_of_edges( Access_Mode mode) const {
  int n = 0;
  // Loop through panes
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( ; it != iend; ++it) {
    n += it->size_of_edges( mode);
  }

  return n;
}

/// Obtain the address of an attribute associated with the node.
const void *Node::addr( const COM::Attribute *a) const {
  COM_assertion_msg( a, "Unexpected NULL pointer");
  COM_assertion_msg( a->is_nodal(), "Expect a nodal attribute");

  const COM::Attribute *attr;

  const COM::Pane *pn = _pm->pane();
  int pid = pn->id();
  if ( a->pane()->id() == pid) 
    attr = a;
  else {
    if ( a->window() != pn->window()) {
      pn = &a->window()->pane( pid);
      COM_assertion_msg( pn, "Pane does not exist");
    }
    attr=pn->attribute( a->id());
    COM_assertion_msg( attr, "Attribute does not exist");
  }

  return ((const char*)attr->pointer()) +
    COM_get_sizeof( attr->data_type(), attr->stride()*(_vID-1));
}

/// Obtain the address of an attribute associated with its bounded element.
const void *Halfedge::addr( const COM::Attribute *a) const {
  COM_assertion( !is_border());
  COM_assertion_msg( a, "Unexpected NULL pointer");
  COM_assertion_msg( a->is_elemental(), "Expect a element-centered attribute");

  const COM::Attribute *attr;

  const COM::Pane *pn = _pm->pane();
  int pid = pn->id();
  if ( a->pane()->id() == pid) 
    attr = a;
  else {
    if ( a->window() != pn->window()) {
      pn = &a->window()->pane( pid);
      COM_assertion_msg( pn, "Pane does not exist");
    }
    attr=pn->attribute( a->id());
    COM_assertion_msg( attr, "Attribute does not exist");
  }

  return ((const char*)attr->pointer()) +
    COM_get_sizeof( attr->data_type(), attr->stride()*(_eID.eid()-1));
}

Vector_3<Real>
get_normal( const Halfedge &h, const Vector_2<Real> &nc) {
  if ( h.is_border()) {
    // Precondition: boundary normal must have been updated.
    return h.pane_manifold()->get_bd_normal( h.id()); 
  }

  Element_node_enumerator ene( h.pane(), h.id().eid());
  const int ne = ene.size_of_edges();
  Generic_element_2 e(ne);

  // First evaluate face normal
  Element_node_vectors_k_const<Point_3<Real> > ps;
  ps.set((const Point_3<Real>*)(h.pane()->coordinates()), ene,1);

  Vector_3<Real> J[2];
  e.Jacobian( ps, nc, J);
  return Vector_3<Real>::cross_product( J[0], J[1]);
}

Vector_3<Real> Halfedge::normal() const {
  return get_normal( *this, Vector_2<Real>( 0.5, 0.5));
}

Vector_3<Real> get_deformed_normal( const Halfedge &h, 
				    const Vector_2<Real> &nc, 
				    const COM::Attribute *disp) 
{
  COM_assertion_msg( disp, "Unexpected NULL pointer");
  COM_assertion_msg( disp->is_nodal(), "disp must be nodal attribute");

  // Obtain the deformed coordinates
  Point_3<Real> points[4]; 

  int ne = 0;
  Halfedge hit = h;
  do {
    Node v=hit.origin();
    points[ne] = v.point() + *(Vector_3<Real>*)v.addr( disp);
    ++ne;
    COM_assertion( ne<=4);
  } while ( (hit=hit.next()) != h);

  // Evaluate the Jacobian and the cross product
  Vector_3<Real> J[2];
  Generic_element_2(ne).Jacobian( points, nc, J);

  return Vector_3<Real>::cross_product( J[0], J[1]);
}

Vector_3<Real> Halfedge::
deformed_normal( const COM::Attribute *disp) const {
  return get_deformed_normal( *this, Vector_2<Real>( 0.5, 0.5), disp);
}

/// Return the dihedral angle at the edge. If the edge is a border edge,
/// then return pi. Note that this must be called after update_bd_normals
/// has been called.
double Halfedge::dihedral_angle() const {
  const double pi=3.14159265358979;
  if ( is_physical_border()) return pi;

  Halfedge hopp = opposite();
  if ( hopp.is_physical_border()) return pi;

  Vector_3<Real> normals[2] = { normal(), hopp.normal() };

  double cos_a = normals[0]*normals[1] 
    / std::sqrt( (normals[0]*normals[0])*(normals[1]*normals[1]));

  // Resolve roundoff error
  if ( cos_a>1) cos_a=1;
  if ( cos_a<-1) cos_a=-1;

  return std::acos( cos_a);
}

// Assign global ids for all nodes 
void Window_manifold_2::
assign_global_nodeIDs( std::vector<std::vector<int> > &gids) const {
  // Initialize the vector gids
  gids.clear(); gids.resize( size_of_panes());
  std::map<int, std::vector<int>*> maps;
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( int i=0; it!=iend; ++it, ++i) {
    gids[i].resize( it->size_of_real_nodes(), 0);
    maps[it->pane()->id()] = &gids[i];
  }

  Access_Mode mode = ACROSS_PANE;
  // Loop through the panes to assign primary nodes
  int gid = 0;
  it=pm_begin();
  for ( int i=0; it!=iend; ++it, ++i) {
    for (int v=0, nv=it->size_of_real_nodes(); v<nv; ++v) {
      if ( it->is_primary( v+1, mode))
	gids[i][v] = ++gid;
    }
  }
  COM_assertion( gid == size_of_nodes( mode));

  // Loop through the panes to assign other nodes
  it=pm_begin();
  for ( int i=0; it!=iend; ++it, ++i) {
    for (int v=0, nv=it->size_of_real_nodes(); v<nv; ++v) {
      if (gids[i][v]==0) {
	// Obtain the primary.
	Node nd = it->get_primary(v+1, mode);
	gids[i][v] = (*maps[nd.pane()->id()])[nd.id()-1];
      }
    }
  }
}

void Window_manifold_2::serialize_window( COM::Window *outwin) const {
  // Clearn up the output window
  outwin->delete_pane(0);

  // Build a renumbering of nodes
  std::vector<std::vector<int> >  nodes_gids;
  assign_global_nodeIDs( nodes_gids);

  Access_Mode mode = ACROSS_PANE;
  // Create a pane
  int nnodes=size_of_nodes( mode);
  outwin->set_size( "nc", 1, nnodes);

  double *coors;
  outwin->resize_array( "nc",1,(void**)&coors);

  // Mesh vertices
  int gid=0;
  // Loop through the panes to write the nodes
  PM_const_iterator it=pm_begin(), iend=pm_end();
  for ( ; it!=iend; ++it) {
    const COM::Pane &pn = *it->pane();
    const double *p = pn.coordinates();

    for ( int i=0, nn=pn.size_of_real_nodes(); i<nn; ++i) {
      if ( it->is_primary( i+1, mode)) {
	std::copy( &p[3*i], &p[3*i+3], &coors[3*gid]);
	++gid;
      }
    }
  }
  COM_assertion( gid==nnodes);

  // Mesh triangles.
  int *tris;
  outwin->set_size( ":t3:", 1, size_of_faces(REAL_PANE));
  outwin->resize_array( ":t3:", 1, (void**)&tris);
  
  int i=0, k=0;
  for ( it=pm_begin(); it!=iend; ++it, ++i) {
    int fn=it->size_of_faces(REAL_PANE); if ( fn==0) continue;

    Element_node_enumerator ene( it->pane(), 1);

    for ( int f=0; f<fn; ++f, ++k, ene.next()) {
      for ( int j=0; j<3; ++j) {
	tris[3*k+j] = nodes_gids[i][ene[j]-1];
      }
    }
  }

  outwin->init_done();
}


SURF_END_NAMESPACE






