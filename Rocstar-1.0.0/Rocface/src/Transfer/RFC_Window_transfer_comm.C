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
/* $Id: RFC_Window_transfer_comm.C,v 1.22 2008/12/06 08:43:29 mtcampbe Exp $ */

#include "RFC_Window_transfer.h"
#include <cstdio>

#include <limits>
#define QUIET_NAN   std::numeric_limits<Real>::quiet_NaN()

RFC_BEGIN_NAME_SPACE

// Obtain the list of incident subfaces in each pane of the opposite mesh.
void 
RFC_Window_transfer::
incident_faces( std::map< int, std::vector<int> > &opp_subface_lists) const {
  RFC_assertion(opp_subface_lists.empty());

  // Loop through the panes to generate subface list
  for (Pane_set::const_iterator 
	 pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    const RFC_Pane_transfer &pane = (const RFC_Pane_transfer &)*pi->second;
    
    for ( int i=0, size=pane.size_of_subfaces(); i<size; ++i) {
      Face_ID oppf = pane._subface_counterparts[i];
      opp_subface_lists[ oppf.pane_id].push_back( oppf.face_id);
    }
  }
}

// Create copies of the panes from the opposite subface lists.
void
RFC_Window_transfer::
replicate_metadata( const RFC_Window_transfer &opp_win) {

  std::map< int, std::vector<int> > opp_subface_lists;
  opp_win.incident_faces( opp_subface_lists);

  // First, make local copies of topology information of remote panes.
  std::vector< int> pane_ids; pane_ids.reserve( opp_subface_lists.size());

  for ( std::map< int, std::vector<int> >::const_iterator 
	  it=opp_subface_lists.begin(); it!=opp_subface_lists.end(); ++it) {
    pane_ids.push_back( it->first);
  }
  replicate_metadata( &pane_ids[0], pane_ids.size());
  
  // Fill in _recv_faces and _recv_nodes for each remote pane.
  // Loop through the subface_lists
  for ( std::map< int, std::vector<int> >::const_iterator 
	  it=opp_subface_lists.begin(); it!=opp_subface_lists.end(); ++it) {

    RFC_Pane_transfer &pn = pane( it->first);
    if (pn.is_master()) continue;

    RFC_assertion(pn._recv_faces.empty());
    // Loop through the subfaces of pn
    const std::vector< int> &subface_list = it->second;
    std::set<int> face_list;
    std::set<int> node_list;
    for ( int i=0, size=subface_list.size(); i<size; ++i) {
      int parent = pn._subface_parents[ subface_list[i]-1];
      face_list.insert( parent);

      Element_node_enumerator  ene( pn.base(), parent);

      for ( int k=0, kn=ene.size_of_nodes(); k<kn; ++k)
	node_list.insert( ene[k]);

      // Loop through the subnodes of the subface, and insert the nodes 
      // in their host facets.
      Three_tuple<int>  &f = pn._subfaces[ subface_list[i]-1];
      for ( int j=0; j<3; ++j) {
	int parent = pn._subnode_parents[ f[j]-1].face_id;
	Element_node_enumerator  ene( pn.base(), parent);

	for ( int k=0, kn=ene.size_of_nodes(); k<kn; ++k)
	  node_list.insert( ene[k]);
      }
    }

    pn._recv_faces.reserve( face_list.size());
    pn._recv_faces.insert( pn._recv_faces.end(), face_list.begin(), face_list.end());
    pn._recv_nodes.reserve( node_list.size());
    pn._recv_nodes.insert( pn._recv_nodes.end(), node_list.begin(), node_list.end());
  }

  // Fill in _send_faces and _send_nodes for each local pane.
  const std::map<int, std::pair<int, int> > &opp_panemap = opp_win._pane_map;
  int rank=comm_rank();

  // Loop through the panes. 
  for (Pane_set::iterator pi=_pane_set.begin(); pi!=_pane_set.end(); ++pi) {

    // For the current pane, obtain a list of its faces that
    //  need to be sent to each remote process.
    RFC_Pane_transfer &pane = (RFC_Pane_transfer &)*pi->second;

    // Creaet a list and put into the following object
    std::map< int, std::set<int> > sfs;
    std::map< int, std::set<int> > sns;
    
    for ( int i=0, size=pane.size_of_subfaces(); i<size; ++i) {
      // If the counterpart of a subface is owned by a remote process P,
      // insert the parent of the subface into the face list for P.
      Face_ID oppf = pane._subface_counterparts[i];
      int remote_rank = opp_panemap.find( oppf.pane_id)->second.first;

      if ( remote_rank != rank) {
	int parent = pane._subface_parents[i];
	sfs[ remote_rank].insert( parent);

	std::set<int>  &ns=sns[remote_rank];
	Element_node_enumerator  ene( pane.base(), parent);

	for ( int k=0, kn=ene.size_of_nodes(); k<kn; ++k)
	  ns.insert( ene[k]);

	// Loop through the subnodes of the subface, and insert the nodes 
	// in their host facets.
	Three_tuple<int>  &f = pane._subfaces[ i];
	for ( int j=0; j<3; ++j) {
	  int parent = pane._subnode_parents[ f[j]-1].face_id;
	  Element_node_enumerator  ene( pane.base(), parent);

	  for ( int k=0, kn=ene.size_of_nodes(); k<kn; ++k)
	    ns.insert( ene[k]);
	}
      }
    }

    // Copy from sfs to pane._send_faces
    RFC_assertion(pane._send_faces.empty());
    for ( std::map<int, std::set<int> >::const_iterator
	    it=sfs.begin(); it!=sfs.end(); ++it) {
      std::vector<int> &vec=pane._send_faces[ it->first]; 
      vec.reserve(it->second.size()); 
      vec.insert( vec.end(), it->second.begin(), it->second.end());
    }

    // Copy from sns to pane._send_nodes
    RFC_assertion(pane._send_nodes.empty());
    for ( std::map<int, std::set<int> >::const_iterator
	    it=sns.begin(); it!=sns.end(); ++it) {
      std::vector<int> &vec=pane._send_nodes[ it->first]; 
      vec.reserve(it->second.size()); 
      vec.insert( vec.end(), it->second.begin(), it->second.end());
    }
  }
}

// Cache a copy of the meta-data for the given panes
void
RFC_Window_transfer::replicate_metadata( int *pane_ids, int n) {
  // Determine the owners of the panes
  std::vector< int>  npanes_recv( comm_size(), 0);
  std::vector< int>  npanes_send( comm_size(), 0);
  int rank=comm_rank();

  for ( int i=0; i<n; ++i) {
    RFC_assertion( _pane_map.find(pane_ids[i]) != _pane_map.end());
    std::pair< int, int>  p = _pane_map.find( pane_ids[i])->second;
    if ( p.first != rank) ++npanes_recv[ p.first];
  }

  MPI_Alltoall( &npanes_recv[0], 1, MPI_INT, 
		&npanes_send[0], 1, MPI_INT, _comm);

  // Create a buffer for the displacements of the buffer for pane ids.
  std::vector<int> displs_recv( npanes_recv.size()+1); 
  counts_to_displs( npanes_recv, displs_recv);
  std::vector<int> displs_send( npanes_send.size()+1);
  counts_to_displs( npanes_send, displs_send);

  std::vector< int>  ids_recv( displs_recv.back());
  for ( int i=0; i<n; ++i) {
    std::pair< int, int>  p = _pane_map.find( pane_ids[i])->second;
    if ( p.first != rank) ids_recv[ displs_recv[p.first]++] = pane_ids[i];
  }
  counts_to_displs( npanes_recv, displs_recv);

  std::vector< int>  ids_send( displs_send.back());

  MPI_Alltoallv(&ids_recv[0], &npanes_recv[0],&displs_recv[0], MPI_INT,
		&ids_send[0], &npanes_send[0],&displs_send[0], MPI_INT, _comm);

  
  // Prepare the data structures for sending. 
  for ( int i=0, size=npanes_send.size(), k=0; i<size; ++i) {
    for ( int j=0; j<npanes_send[i]; ++j, ++k) {
      init_send_buffer( ids_send[k], i);
    }
  }

  // Prepare the data structures for receiving data (replication)
  for ( int i=0, size=npanes_recv.size(), k=0; i<size; ++i) {
    for ( int j=0; j<npanes_recv[i]; ++j, ++k) {
      init_recv_buffer( ids_recv[k], i);
    }
  }

  _replicated = true;
}

// Cache a copy of the given facial data. Also cache coordinates if
// replicate_coor is true.
void
RFC_Window_transfer::replicate_data( const Facial_data_const &data, 
				     bool replicate_coor) {
  std::vector< MPI_Request> other_requests, recv_requests;

  other_requests.reserve( _replic_panes.size()+_panes_to_send.size());
  recv_requests.reserve( _replic_panes.size());

  std::vector< RFC_Pane_transfer*> recv_panes; 
  recv_panes.reserve(_replic_panes.size());

  int totalNumPanes = _pane_map.size();
  int d = data.dimension(), ierr;

  // Loop through the replicated panes
  // Initiate receive of data buffers from remote processes
  for (std::map< int, RFC_Pane_transfer*>::iterator 
	 it=_replic_panes.begin(),iend=_replic_panes.end(); it != iend; ++it) {
    RFC_Pane_transfer *p=it->second;
    p->_data_buf_id = data.id();

    p->_data_buf.reserve( p->size_of_faces()*d);
    p->_data_buf.resize( p->_recv_faces.size()*d, QUIET_NAN);

    std::pair< int, int>  s = _pane_map.find( p->id())->second;
    MPI_Request req;
    ierr = MPI_Irecv( &p->_data_buf[0], p->_data_buf.size()*sizeof(Real), 
		      MPI_BYTE, s.first, 100+s.second, _comm, &req);
    RFC_assertion( ierr==0);
    recv_requests.push_back( req);
    recv_panes.push_back( it->second);
    
    if ( replicate_coor) {
      p->_coor_buf.resize( p->size_of_faces()*3);
      ierr = MPI_Irecv( &p->_coor_buf[0], 
			p->_recv_faces.size()*3*sizeof(Real), MPI_BYTE, 
			s.first, 100+totalNumPanes+s.second, _comm, &req);
      RFC_assertion( ierr==0);
      other_requests.push_back( req);
    }
  }

  // Initiate send of data buffers to remote processes
  for (std::set< std::pair<int, RFC_Pane_transfer*> >::const_iterator
	 it=_panes_to_send.begin(),iend=_panes_to_send.end(); it!=iend; ++it){
    RFC_Pane_transfer *p=it->second;

    std::pair< int, int>  s = _pane_map.find( p->id())->second;

    // Initialize the send buffer
    std::vector<int>  &send_faces=p->_send_faces[it->first];
    std::vector<Real> &data_buf=p->_send_buffers[it->first];
    const Real *addr = p->pointer(data.id());

    // We only allocate buffer if all entries are not sent
    if ( int(p->_send_faces.size()) != p->size_of_faces()) {
      data_buf.resize( send_faces.size()*d, QUIET_NAN);

      for ( int i=send_faces.size()-1; i>=0; --i) {
	for ( int j=d-1; j>=0; --j) {
	  data_buf[i*d+j] = addr[(send_faces[i]-1)*d+j];
	}
      }
      addr = &data_buf[0];
    }

    MPI_Request req;
    ierr = MPI_Isend( const_cast<Real*>(addr), send_faces.size()*d*sizeof(Real), 
		      MPI_BYTE, it->first, 100+s.second, _comm, &req);

    RFC_assertion( ierr==0);
    other_requests.push_back( req);

    if ( replicate_coor) {
      ierr = MPI_Isend( const_cast<Real*>(p->coordinates()), 
			p->_send_faces[it->first].size()*3*sizeof(Real), MPI_BYTE,
			it->first, 100+totalNumPanes+s.second, _comm, &req);
      RFC_assertion( ierr==0);
      other_requests.push_back( req);
    }
  }

  // Processing received data arrays
  while ( !recv_requests.empty()) {
    int        index;
    MPI_Status stat;

    ierr = MPI_Waitany(recv_requests.size(), &recv_requests[0], &index, &stat);
    RFC_assertion( ierr==0);
    RFC_Pane_transfer *p=recv_panes[index];

    // Reorganize the data buffer as a dense array, and fill
    // unused entries by NaN.
    std::vector<Real> &buf = p->_data_buf;
    buf.resize( p->size_of_nodes()*d, QUIET_NAN);
      
    std::vector<int>  &faces = p->_recv_faces;
    for ( int i=faces.size()-1; i>=0; --i) {
      if ( i==faces[i]-1) break;
      for ( int j=d-1; j>=0; --j) {
	buf[(faces[i]-1)*d+j] = buf[i*d+j];
	buf[i*d+j] = QUIET_NAN;
      }
    }

    // Remove the received message from the list
    recv_requests.erase( recv_requests.begin()+index);
    recv_panes.erase( recv_panes.begin()+index);
  }

  // Wait for all send requests to finish
  wait_all( other_requests.size(), &other_requests[0]);
}

// Cache a copy of the given nodal data. Also cache coordinates if
// replicate_coor is true.
void
RFC_Window_transfer::replicate_data( const Nodal_data_const &data, 
				     bool replicate_coor) {
  std::vector< MPI_Request> other_requests, recv_requests;

  other_requests.reserve( _replic_panes.size()+_panes_to_send.size());
  recv_requests.reserve( _replic_panes.size());

  std::vector< RFC_Pane_transfer*> recv_panes; 
  recv_panes.reserve(_replic_panes.size());

  int totalNumPanes = _pane_map.size();
  int d = data.dimension(), ierr;

  // Loop through the replicated panes
  // Initiate receive of data buffers from remote processes
  for (std::map< int, RFC_Pane_transfer*>::iterator 
	 it=_replic_panes.begin(),iend=_replic_panes.end(); it != iend; ++it) {
    RFC_Pane_transfer *p=it->second;
    p->_data_buf_id = data.id();

    p->_data_buf.reserve( p->size_of_nodes()*d);
    p->_data_buf.resize( p->_recv_nodes.size()*d);

    std::pair< int, int>  s = _pane_map.find( p->id())->second;
    MPI_Request req;
    ierr = MPI_Irecv( &p->_data_buf[0], p->_data_buf.size()*sizeof(Real), 
		      MPI_BYTE, s.first, 100+s.second, _comm, &req);
    RFC_assertion( ierr==0);
    recv_requests.push_back( req);
    recv_panes.push_back( it->second);
    
    if ( replicate_coor) {
      p->_coor_buf.resize( p->size_of_nodes()*3);
      ierr = MPI_Irecv( &p->_coor_buf[0], 
			p->_recv_nodes.size()*3*sizeof(Real), MPI_BYTE, 
			s.first, 100+totalNumPanes+s.second, _comm, &req);
      RFC_assertion( ierr==0);
      other_requests.push_back( req);
    }
  }

  // Initiate send of data buffers to remote processes
  for (std::set< std::pair<int, RFC_Pane_transfer*> >::const_iterator
	 it=_panes_to_send.begin(),iend=_panes_to_send.end(); it!=iend; ++it){
    RFC_Pane_transfer *p=it->second;

    std::pair< int, int>  s = _pane_map.find( p->id())->second;

    // Initialize the send buffer
    std::vector<int>  &send_nodes=p->_send_nodes[it->first];
    std::vector<Real> &data_buf=p->_send_buffers[it->first];
    const Real *addr = p->pointer(data.id());

    // We only allocate buffer if all entries are not sent
    if ( int(p->_send_nodes.size()) != p->size_of_nodes()) {
      data_buf.resize( send_nodes.size()*d);

      for ( int i=send_nodes.size()-1; i>=0; --i) {
	for ( int j=d-1; j>=0; --j) {
	  data_buf[i*d+j] = addr[(send_nodes[i]-1)*d+j];
	}
      }
      addr = &data_buf[0];
    }

    MPI_Request req;
    ierr = MPI_Isend( const_cast<Real*>(addr), send_nodes.size()*d*sizeof(Real), 
		      MPI_BYTE, it->first, 100+s.second, _comm, &req);
    RFC_assertion( ierr==0);
    other_requests.push_back( req);

    if ( replicate_coor) {
      ierr = MPI_Isend( const_cast<Real*>(p->coordinates()), 
			p->_send_nodes[it->first].size()*3*sizeof(Real), MPI_BYTE,
			it->first, 100+totalNumPanes+s.second, _comm, &req);
      RFC_assertion( ierr==0);
      other_requests.push_back( req);
    }
  }

  // Processing received data arrays
  while ( !recv_requests.empty()) {
    int        index;
    MPI_Status stat;

    ierr = MPI_Waitany(recv_requests.size(), &recv_requests[0], &index, &stat);
    RFC_assertion( ierr==0);

    RFC_Pane_transfer *p=recv_panes[index];
    std::vector<Real> &buf = p->_data_buf;

    // Reorganize the data buffer as a dense array, and fill
    // unused entries by NaN.
    std::vector<int>  &nodes = p->_recv_nodes;
    buf.resize( p->size_of_nodes()*d, QUIET_NAN);
    
    for ( int i=nodes.size()-1; i>=0; --i) {
      int lid = nodes[i]-1;
      if ( i==lid) break;
      for ( int j=d-1; j>=0; --j) {
	buf[lid*d+j] = buf[i*d+j];
	buf[i*d+j] = QUIET_NAN;
      }
    }

    // Remove the received message from the list
    recv_requests.erase( recv_requests.begin()+index);
    recv_panes.erase( recv_panes.begin()+index);
  }

  // Wait for all send requests to finish
  wait_all( other_requests.size(), &other_requests[0]);
}

void
RFC_Window_transfer::reduce_to_all( Nodal_data &data, MPI_Op op) {
  std::vector<void *> ptrs; ptrs.reserve( _pane_set.size());

  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer&)*pi->second;

    ptrs.push_back( pane.pointer( data.id()));
  }

  _map_comm.init( &ptrs[0], COM_DOUBLE, data.dimension());

  _map_comm.begin_update_shared_nodes();
  _map_comm.reduce_on_shared_nodes( op);
  _map_comm.end_update_shared_nodes();
}

void
RFC_Window_transfer::reduce_maxabs_to_all( Nodal_data &data) {
  std::vector<void *> ptrs; ptrs.reserve( _pane_set.size());

  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer&)*pi->second;

    ptrs.push_back( pane.pointer( data.id()));
  }

  _map_comm.init( &ptrs[0], COM_DOUBLE, data.dimension());

  _map_comm.begin_update_shared_nodes();
  _map_comm.reduce_maxabs_on_shared_nodes();
  _map_comm.end_update_shared_nodes();
}

//=================== Lower level communication routines
void
RFC_Window_transfer::barrier() const {
  int ierr = MPI_Barrier( _comm); RFC_assertion( ierr==0);
}

bool
RFC_Window_transfer::is_root() const { 
  int rank; MPI_Comm_rank( _comm, &rank);
  return (rank==0);
}

void
RFC_Window_transfer::wait_all( int n, MPI_Request *requests) {
  if ( n>0) {
    std::vector<MPI_Status>   statuses(n);
    int ierr = MPI_Waitall( n, requests, &statuses[0]);
    RFC_assertion( ierr==0);
  }
}

void
RFC_Window_transfer::wait_any( int n, MPI_Request *requests, 
			       int *index, MPI_Status *stat) {
  if ( n>0) {
    MPI_Status s; if ( stat==NULL) stat = &s;
    int ierr = MPI_Waitany( n, requests, index, stat);
    RFC_assertion( ierr==0);
  }
}

void
RFC_Window_transfer::allreduce( Real *data, int n, MPI_Op op) const {
  std::vector<Real>  buf( data, data+n);
  RFC_assertion( sizeof( Real) == sizeof( double));
  MPI_Allreduce( &buf[0], data, n, MPI_DOUBLE, op, _comm);
}

void
RFC_Window_transfer::init_send_buffer( int pane_id, int to_rank) {
  _panes_to_send.insert( std::pair<int,RFC_Pane_transfer*>( to_rank, 
							    &pane(pane_id)));
}

COM_EXTERN_MODULE( Rocin);

void
RFC_Window_transfer::init_recv_buffer( int pane_id, int from_rank) {

  RFC_assertion( _pane_set.find( pane_id) == _pane_set.end());

  COM::Pane *base_pane = new COM::Pane( (COM::Window*)NULL, pane_id);
  RFC_Pane_transfer *pane = new RFC_Pane_transfer( base_pane, color());
  _replic_panes[ pane_id] = pane;

  std::string fname = get_sdv_fname( _prefix.c_str(), pane_id, _IO_format);
  if ( _IO_format == SDV_BINARY) {
    std::ifstream is( fname.c_str()); RFC_assertion( is);

    pane->read_binary( is, NULL, base_pane);
  }
  else {
    // Read in using Rocin.
    COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "RFC_IN");
    int hdl_read = COM_get_function_handle( "RFC_IN.read_windows");
    int hdl_obtain = COM_get_function_handle( "RFC_IN.obtain_attribute");

    // Define the base-window and sdv-window names
    std::string base_material = get_prefix_base( _prefix.c_str());
    std::string sdv_material = base_material+"_sdv";

    std::string buf_wname(_bufwin_prefix); buf_wname.append( base_material);
    std::string sdv_wname=buf_wname+"_sdv";

    // Read the parent pane and the subdivided pane from the given file.
    MPI_Comm comm_self = MPI_COMM_SELF;
    COM_call_function( hdl_read, fname.c_str(), _bufwin_prefix, 
		       (base_material+" "+sdv_material).c_str(), &comm_self);

    int hdl_all = COM_get_attribute_handle( (buf_wname+".all").c_str());
    COM_call_function( hdl_obtain, &hdl_all, &hdl_all, &pane_id);
    hdl_all = COM_get_attribute_handle( (sdv_wname+".all").c_str());
    COM_call_function( hdl_obtain, &hdl_all, &hdl_all, &pane_id);

    pane->read_rocin( sdv_wname, buf_wname, base_pane);

    // Delete the windows created by Rocin.
    COM_delete_window( buf_wname.c_str());
    COM_delete_window( sdv_wname.c_str());

    // Unload Rocin
    COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocin, "RFC_IN");
  }

  pane->init();
}

void
RFC_Window_transfer::clear_replicated_data() {
  // Loop through the replicated panes
  std::map< int, RFC_Pane_transfer*>::iterator it=_replic_panes.begin();
  std::map< int, RFC_Pane_transfer*>::iterator iend=_replic_panes.end();
  for ( ; it != iend; ++it) {
    it->second->_data_buf_id = -1;
    std::vector<Real> t;
    it->second->_data_buf.swap( t); // clear up the memory space
  }
}

RFC_END_NAME_SPACE






