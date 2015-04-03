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
// $Id: Pane_communicator.C,v 1.36 2009/08/27 14:04:49 mtcampbe Exp $

/** \file Pane_communicator.C
 *  Handles communication  of shared nodes across panes.
 */

#include <cassert>
#include <cstring>

#include "Pane_communicator.h"
#include "Pane_connectivity.h"
#include "roccom.h"

MAP_BEGIN_NAMESPACE

Pane_communicator::Pane_communicator( COM::Window *w, MPI_Comm c)
  : _appl_window( w), _comm(COMMPI_Initialized()?c:MPI_COMM_NULL), 
    _total_npanes(-1) 
{ 
  _my_pconn_id = COM::COM_PCONN;
  _appl_window->panes( _panes);
  const COM::Window::Proc_map &proc_map = _appl_window->proc_map();
  _total_npanes = proc_map.size();

  // Determine a mapping from user-defined pane ids to a set of 
  // internal pane IDs, which are unique and contiguous across all processes
  // to be used for defining unique tags for MPI messages.
  std::map<int,int>::const_iterator it=proc_map.begin();
  for ( int i=0; i<_total_npanes; ++i, ++it) 
    _lpaneid_map[ it->first] = i;
}

/// Initialize the communication buffers.
void Pane_communicator::init( COM::Attribute *att, 
			      const COM::Attribute* my_pconn){
  // Note that the attribute must be on the attribute window.
  COM_assertion( att->window() == _appl_window &&
		 ( my_pconn ==NULL || my_pconn->window() == _appl_window));

  if(my_pconn){
    _my_pconn_id = my_pconn->id();
  }
  else{
    _my_pconn_id = COM::COM_PCONN;
    COM_assertion_msg( att && (att->is_nodal() || att->is_elemental()),
		       "Pane_communicator must be initialized with a nodal or elemental attribute");
  }

  int  att_id = att->id();
  int  local_npanes = _panes.size();

  void** pointers = new void*[local_npanes];
  int *sizes = new int[local_npanes];
  int *strides = new int[local_npanes];

  for ( int i=0; i < local_npanes; ++i) {
    COM::Attribute *attribute = _panes[i]->attribute(att_id);
    pointers[i] = attribute->pointer();
    sizes[i] = attribute->size_of_real_items();
    strides[i] = attribute->stride();
  }
  init(pointers, att->data_type(), att->size_of_components(), sizes, strides);
  delete[] pointers;  pointers = NULL;
  delete[] sizes;     sizes=NULL;
  delete[] strides;   strides = NULL;
}

/// Initialize the communication buffers.
void Pane_communicator::init( void** ptrs, COM_Type type, 
			      int ncomp, const int *sizes, const int *strds) {
  
  //=== Initialize local objects from the arguments
  _type = type;
  _ncomp = ncomp;
  _ncomp_bytes = COM_get_sizeof( _type, _ncomp);

  int local_npanes = _panes.size();
  _shr_buffs.resize( local_npanes);
  _rns_buffs.resize( local_npanes);
  _gnr_buffs.resize( local_npanes);
  _rcs_buffs.resize( local_npanes);
  _gcr_buffs.resize( local_npanes);
  _ptrs.clear(); _ptrs.insert( _ptrs.end(), ptrs, ptrs+local_npanes);

  _sizes.clear();
  if ( sizes) 
    _sizes.insert( _sizes.end(), sizes, sizes+local_npanes);
  else {
    // Default values of sizes are the numbers of nodes
    _sizes.resize( local_npanes);
    for ( int i=0; i<local_npanes; ++i)
      _sizes[i] = _panes[i]->size_of_real_nodes();
  }

  _strds.clear(); 
  if ( strds) _strds.insert( _strds.end(), strds, strds+local_npanes);
  else _strds.resize( local_npanes, _ncomp);

  // Allocate buffer space for outbuf and ghosts
  for ( int i=0; i<local_npanes; ++i) {
    int pid = _panes[i]->id(), lpid=lpaneid(pid);

    // Obtain the pane connectivity of the local pane.
    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();
    int vs_size=pconn->size_of_real_items();

    // Note that the pane connectivity may be inherited from a parent
    // window, and the current window may contain only a subset of the
    // panes of the parent window. Here we should consider only the panes
    // in the current window

    // Initialize buffers for shared nodes.
    int index = 0;
    init_pane_comm_buffers( _shr_buffs[i], vs, index, vs_size, lpid);

    const int vs_gsize=pconn->size_of_items();
    // Initialize the ghost information buffers.
    if(vs_size<vs_gsize)
      init_pane_comm_buffers(_rns_buffs[i], vs, vs_size, vs_gsize, lpid);
    else
      _rns_buffs[i].clear();

    if(vs_size<vs_gsize)
      init_pane_comm_buffers(_gnr_buffs[i], vs, vs_size, vs_gsize, lpid);
    else
      _gnr_buffs[i].clear();

    if(vs_size<vs_gsize)
      init_pane_comm_buffers(_rcs_buffs[i], vs, vs_size, vs_gsize, lpid);
    else
      _rcs_buffs[i].clear();

    if(vs_size<vs_gsize)
      init_pane_comm_buffers(_gcr_buffs[i], vs, vs_size, vs_gsize, lpid);
    else
      _gcr_buffs[i].clear();
  }
}

// Initialize a Pane_comm_buffers for ghost information.
// ptr points to the first entry of a pconn ghost block
// on entry, ptr[index] is the first entry in the block
// at exit, ptr[index] is the first entry in the next block
void Pane_communicator::
init_pane_comm_buffers(std::vector< Pane_comm_buffers>& pcbv,
		       const int* ptr, int& index, const int n_items,
		       const int lpid) {
  if ( n_items == 0) return;

  // Determine the number of communicating panes
  int count=0, np= ptr[index];
  int block_size = 1; 

  for (int j=0; j<np; block_size+=ptr[index+block_size+1]+2, ++j) {
    if (owner_rank( ptr[index+block_size]) >=0) ++count;
  }
  COM_assertion_msg(block_size+index <= n_items, "Out of pconn bounds.");
  ++index;

  pcbv.resize( count);

  int qid=0, lqid=0;
  // Loop through communicating panes for shared nodes.
  for (int p=0, j=0; p<np; ++p, index+=ptr[index+1]+2) {
    if (owner_rank( qid=ptr[index]) <0) continue;
 
    // Define a unique tag to be used by MPI_Isend and MPI_Irecv.
    lqid=lpaneid(qid);
    Pane_comm_buffers &pcb = pcbv[j++];
    pcb.rank = owner_rank( qid);
    // Numbers have to be positive, try to avoid conflicting tags with other apps.
    // assuming other apps are low, 100 is a decent guess.
    pcb.tag = 100 + ((lpid>lqid) ? 
		     lpid*_total_npanes+lqid : lqid*_total_npanes+lpid);
    pcb.tag = pcb.tag%32768;
    pcb.index = index;
  }
  COM_assertion_msg( index <= n_items, "Out of bound of pconn");
}

// Initiates updating by calling MPI_Isend and MPI_Irecv.
void Pane_communicator::begin_update( const Buff_type btype,
				      std::vector<std::vector<bool> > *involved) {
  COM_assertion_msg(_reqs_recv.empty(),
		    "Cannot begin a new update until all prior updates are finished.");
  // Define and initialize variables
  int rank = COMMPI_Initialized() ? COMMPI_Comm_rank( _comm) : 0;

  MPI_Request req;
  _reqs_send.clear(); _reqs_recv.clear(); _reqs_indices.clear();
  int local_npanes = _panes.size();
  int tag_max = _total_npanes*_total_npanes;
  if ( involved) { involved->clear(); involved->resize( local_npanes); }

  // Now copy data to outbuf. First loop through local panes
  for ( int i=0; i<local_npanes; ++i) {
    if ( involved) (*involved)[i].resize( _sizes[i], false);
    
    // Obtain the pane connectivity for the current pane
    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();

    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes; 

    // Obtain the current buffer
    std::vector< Pane_comm_buffers>   *buffs =NULL;

    if (btype == RNS)                  buffs = &_rns_buffs[i];

    else if (btype == RCS)             buffs = &_rcs_buffs[i]; 

    else if (btype == SHARED_NODE)     buffs = &_shr_buffs[i];

    else if (btype == GNR) {
      buffs = &_gnr_buffs[i];
    }
    else if (btype == GCR) 
    {
      buffs = &_gcr_buffs[i]; 
    }
    else
      COM_assertion_msg(false,"Invalid buffer type");

    // Loop through the communicating panes of the current pane
    for ( int j=0, nj=buffs->size(); j<nj; ++j) {
      Pane_comm_buffers* pcb  = &(*buffs)[j];// The current buffer.
     
      // Fill in outgoing buffers if sending.
      int bufsize=_ncomp_bytes * vs[ pcb->index+1];

      if ( btype != SHARED_NODE || _panes[i]->id() != vs[pcb->index]) { 
	// If not sending shared nodes to itself
	if(btype <= SHARED_NODE){
	  pcb->outbuf.resize( bufsize);

	  if ( involved) {
	    for ( int k=0, from=pcb->index+2,n=vs[pcb->index+1]; 
		  k<n; ++k, ++from)
	      (*involved)[i][vs[ from]-1] = true;
	  }

	  for ( int k=0, from=pcb->index+2,n=vs[pcb->index+1]; 
		k<n; ++k, ++from) {
	    std::memcpy( &pcb->outbuf[ _ncomp_bytes*k], 
			 &ptr[ strd_bytes*vs[ from]], _ncomp_bytes);
	  }

	  // Initiates send operations either locally or remotely
	  // if on same communicating process
	  if ( rank == pcb->rank) {
	    int tag = pcb->tag;

	    // If send locally, shift the tag in one of the two directions
	    if ( _panes[i]->id() > vs[pcb->index]) tag += tag_max;
	    tag = tag%32768;
	    // COMMPI uses DUMMY_MPI if MPI not initialized
	    int ierr=COMMPI_Isend( &pcb->outbuf[0], pcb->outbuf.size(), 
				   MPI_BYTE, 0, tag, MPI_COMM_SELF, &req);
	    COM_assertion( ierr==0);
	    _reqs_send.push_back( req);
	  }
	  else {
	    int ierr=MPI_Isend( &pcb->outbuf[0], pcb->outbuf.size(), MPI_BYTE, 
				pcb->rank, pcb->tag, _comm, &req);
	    COM_assertion( ierr==0);
    
	    _reqs_send.push_back( req);
	  }
	}

	// Initiates receive operations either locally or remotely
	if(btype >=SHARED_NODE){
	  pcb->inbuf.resize( bufsize);

	  if ( rank == pcb->rank) {
	    int tag = pcb->tag;
	    
	    // If recv locally, shift the tag in one of the two directions
	    // have to have unique tags for send/receive when we are sending
	    // between panes on the same process.
	    if ( _panes[i]->id() < vs[pcb->index]) tag += tag_max;
	    tag = tag%32768;
	    int ierr=COMMPI_Irecv( &pcb->inbuf[0], pcb->inbuf.size(), 
				   MPI_BYTE, 0, tag, MPI_COMM_SELF, &req);
	    COM_assertion( ierr==0);
	    
	    // Push the receive request into _reqs_recv and _reqs_indices
	    _reqs_recv.push_back( req); 
	    _reqs_indices.push_back( std::make_pair(i,(j<<4)+btype));
	  }
	  else {
	    int ierr=MPI_Irecv( &pcb->inbuf[0], pcb->inbuf.size(), MPI_BYTE, 
				pcb->rank,pcb->tag, _comm, &req);
	    COM_assertion( ierr==0);
	    
	    // Push the receive request into _reqs_recv and _reqs_indices
	    _reqs_recv.push_back( req);
	    _reqs_indices.push_back( std::make_pair(i,(j<<4)+btype));
	  }
	}
      }
      
      else { // btype == SHARED_NODE &&_panes[i]->id()==vs[pcb->index]
	pcb->inbuf.resize( bufsize);

	// A pane is sending to itself.
	// In a list of nodes which a pane shares with itself, the nodes are
	// listed in pairs.  IE a node list { 1,16,2,17,...} indicates that nodes
	// 1 and 3 are the same and nodes 5 and 13 are the same.
	// So, we copy the data value of a node into the outbuf of its shared node.
	// Then we can transfer the data directly.
	for (int k=0,from=pcb->index+2,n=vs[pcb->index+1]; k<n; k+=2, from+=2){
	  std::memcpy( &pcb->inbuf[ _ncomp_bytes*(k+1)], 
		       &ptr[strd_bytes*vs[ from]], _ncomp_bytes);
	  std::memcpy( &pcb->inbuf[ _ncomp_bytes*k], 
		       &ptr[strd_bytes*vs[ from+1]], _ncomp_bytes);
	}
      }
    }
  }
  if(COMMPI_Initialized())
    MPI_Barrier(_comm);
}

// Finalizes updating shared nodes by call MPI_Waitall on all send requests. 
void Pane_communicator::end_update() {
  if ( _comm!=MPI_COMM_NULL) {

    std::vector< MPI_Status> status( _reqs_send.size());
    int ierr=0;
    if (_reqs_send.size())
      ierr=MPI_Waitall( _reqs_send.size(), &_reqs_send[0], &status[0]);
    COM_assertion( ierr==0);
  }
  _reqs_send.resize(0);
}

/// Local level implementation of reduce operations 
template <class T>
void reduce_int( MPI_Op op, T *a, T *b, int size) throw(int) {
  if (op == MPI_SUM)
    { for (int i=0; i<size; ++i) b[i] += a[i]; }
  else if ( op == MPI_PROD)
    { for (int i=0; i<size; ++i) b[i] *= a[i]; }
  else if ( op == MPI_MAX) 
    { for (int i=0; i<size; ++i) b[i] = std::max(a[i], b[i]); }
  else if ( op == MPI_MIN)
    { for (int i=0; i<size; ++i) b[i] = std::min(a[i], b[i]); }
  else if ( op == MPI_BOR)
    { for (int i=0; i<size; ++i) b[i] |= a[i]; }
  else if ( op == MPI_BAND)
    { for (int i=0; i<size; ++i) b[i] &= a[i]; }
  else if ( op == MPI_LOR)
    { for (int i=0; i<size; ++i) b[i] = a[i] || b[i]; }
  else if ( op == MPI_LAND)
    { for (int i=0; i<size; ++i) b[i] = a[i] && b[i]; }
  else throw(-1);
}

/// Local level implementation of reduce operations 
template <class T>
void reduce_real( MPI_Op op, T *a, T *b, int size) throw(int) {
  if (op == MPI_SUM)
    { for (int i=0; i<size; ++i) b[i] += a[i]; }
  else if ( op == MPI_PROD)
    { for (int i=0; i<size; ++i) b[i] *= a[i]; }
  else if ( op == MPI_MAX) 
    { for (int i=0; i<size; ++i) b[i] = std::max(a[i], b[i]); }
  else if ( op == MPI_MIN)
    { for (int i=0; i<size; ++i) b[i] = std::min(a[i], b[i]); }
  else throw(-1);
}

// Perform a reduction operation using locally cached values of the shared 
// nodes, assuming begin_update_shared_nodes() has been called.
void Pane_communicator::reduce_on_shared_nodes( MPI_Op op) {
  while ( !_reqs_recv.empty()) {
    int index;

    // Wait for any receive request to finish and then process the request
    if ( _comm!=MPI_COMM_NULL) {
      MPI_Status status;
      int ierr = MPI_Waitany( _reqs_recv.size(), &_reqs_recv[0], 
			      &index, &status);
      COM_assertion( ierr == 0);
    }
    else
      index = _reqs_recv.size()-1;

    // Obtain the indices in _shr_buffs for the receive request
    int i=_reqs_indices[index].first, j=(_reqs_indices[index].second>>4);

    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes;

    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();

    Pane_comm_buffers &pcb = _shr_buffs[i][j];
    COM_assertion( int(pcb.inbuf.size()) == _ncomp_bytes*vs[ pcb.index+1]);
    
    switch ( _type) {
    case COM_CHAR:
    case COM_CHARACTER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_int(op, (char*)&pcb.inbuf[ _ncomp_bytes*k],
		   ((char*)&ptr[strd_bytes*vs[ to]]), _ncomp);
      break;
    case COM_INT:
    case COM_INTEGER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_int(op, (int*)&pcb.inbuf[ _ncomp_bytes*k],
		   (int*)&ptr[strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_FLOAT:
    case COM_REAL:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_real( op, (float*)&pcb.inbuf[ _ncomp_bytes*k],
		     (float*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_real( op, (double*)&pcb.inbuf[ _ncomp_bytes*k], 
		     (double*)&ptr[strd_bytes*vs[ to]], _ncomp);
      break;
    default:
      COM_assertion_msg(false, "Unknown data type"); // Not supported
    }

    // Remove the received message from the list
    _reqs_recv.erase(_reqs_recv.begin()+index);
    _reqs_indices.erase(_reqs_indices.begin()+index);
  }
}

template <class T>
void reduce_maxabs( T *a, T *b, int size) throw(int) {
  for (int i=0; i<size; ++i) {
    if ( std::abs( a[i])>std::abs(b[i])) b[i] = a[i];
  }
}

template <class T>
void reduce_minabs( T *a, T *b, int size) throw(int) {
  for (int i=0; i<size; ++i) {
    if ( std::abs( a[i])<std::abs(b[i])) b[i] = a[i];
  }
}

template <class T>
void reduce_diff( T *a, T *b, int size) throw(int) {
  bool isa_nonzero=0;

  for (int i=0; ; ++i) {
    if ( a[i]!=0) { isa_nonzero = 1; break; }
    else if (i==size) return; 
  }

  bool isb_nonzero=0;
  for (int i=0; i<size; ++i) 
    if ( b[i]!=0) { isb_nonzero = 1; break; }

  if ( isb_nonzero)
    for (int j=0; j<size; ++j) b[j] -= a[j];
  else
    for (int j=0; j<size; ++j) b[j] = a[j];
}

// This operation is all local
void Pane_communicator::reduce_maxabs_on_shared_nodes() {
  while ( !_reqs_recv.empty()) {
    int index;

    // Wait for any receive request to finish and then process the request
    if ( _comm!=MPI_COMM_NULL) {
      MPI_Status status;
      int ierr = MPI_Waitany( _reqs_recv.size(), &_reqs_recv[0], 
			      &index, &status);
      COM_assertion( ierr == 0);
    }
    else
      index = _reqs_recv.size()-1;

    // Obtain the indices in _shr_buffs for the receive request
    int i=_reqs_indices[index].first, j=(_reqs_indices[index].second>>4);

    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes;

    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();

    Pane_comm_buffers &pcb = _shr_buffs[i][j];
    COM_assertion( int(pcb.inbuf.size()) == _ncomp_bytes*vs[ pcb.index+1]);

    switch ( _type) {
    case COM_CHAR:
    case COM_CHARACTER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_maxabs( (char*)&pcb.inbuf[ _ncomp_bytes*k],
		       (char*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_INT:
    case COM_INTEGER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_maxabs( (int*)&pcb.inbuf[ _ncomp_bytes*k],
		       (int*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_FLOAT:
    case COM_REAL:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_maxabs( (float*)&pcb.inbuf[ _ncomp_bytes*k], 
		       (float*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_maxabs( (double*)&pcb.inbuf[ _ncomp_bytes*k], 
		       (double*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    default:
      COM_assertion_msg(false, "Unknown data type"); // Not supported
    }

    // Remove the received message from the list
    _reqs_recv.erase(_reqs_recv.begin()+index);
    _reqs_indices.erase(_reqs_indices.begin()+index);
  }
}

// This operation is all local
void Pane_communicator::reduce_minabs_on_shared_nodes() {
  while ( !_reqs_recv.empty()) {
    int index;

    // Wait for any receive request to finish and then process the request
    if ( _comm!=MPI_COMM_NULL) {
      MPI_Status status;
      int ierr = MPI_Waitany( _reqs_recv.size(), &_reqs_recv[0], 
			      &index, &status);
      COM_assertion( ierr == 0);
    }
    else
      index = _reqs_recv.size()-1;

    // Obtain the indices in _shr_buffs for the receive request
    int i=_reqs_indices[index].first, j=(_reqs_indices[index].second>>4);

    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes;

    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();

    Pane_comm_buffers &pcb = _shr_buffs[i][j];
    COM_assertion( int(pcb.inbuf.size()) == _ncomp_bytes*vs[ pcb.index+1]);

    switch ( _type) {
    case COM_CHAR:
    case COM_CHARACTER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_minabs( (char*)&pcb.inbuf[ _ncomp_bytes*k],
		       (char*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_INT:
    case COM_INTEGER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_minabs( (int*)&pcb.inbuf[ _ncomp_bytes*k],
		       (int*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_FLOAT:
    case COM_REAL:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_minabs( (float*)&pcb.inbuf[ _ncomp_bytes*k], 
		       (float*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_minabs( (double*)&pcb.inbuf[ _ncomp_bytes*k], 
		       (double*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    default:
      COM_assertion_msg(false, "Unknown data type"); // Not supported
    }

    // Remove the received message from the list
    _reqs_recv.erase(_reqs_recv.begin()+index);
    _reqs_indices.erase(_reqs_indices.begin()+index);
  }
}

// This operation is all local
void Pane_communicator::reduce_diff_on_shared_nodes() {
  while ( !_reqs_recv.empty()) {
    int index;

    // Wait for any receive request to finish and then process the request
    if ( _comm!=MPI_COMM_NULL) {
      MPI_Status status;
      int ierr = MPI_Waitany( _reqs_recv.size(), &_reqs_recv[0], 
			      &index, &status);
      COM_assertion( ierr == 0);
    }
    else
      index = _reqs_recv.size()-1;

    // Obtain the indices in _shr_buffs for the receive request
    int i=_reqs_indices[index].first, j=(_reqs_indices[index].second>>4);
    
    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes;

    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();

    Pane_comm_buffers &pcb = _shr_buffs[i][j];
    COM_assertion( int(pcb.inbuf.size()) == _ncomp_bytes*vs[ pcb.index+1]);

    switch ( _type) {
    case COM_CHAR:
    case COM_CHARACTER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_diff( (char*)&pcb.inbuf[ _ncomp_bytes*k],
		     (char*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_INT:
    case COM_INTEGER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_diff( (int*)&pcb.inbuf[ _ncomp_bytes*k],
		     (int*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_FLOAT:
    case COM_REAL:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_diff( (float*)&pcb.inbuf[ _ncomp_bytes*k], 
		     (float*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	reduce_diff( (double*)&pcb.inbuf[ _ncomp_bytes*k], 
		     (double*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    default:
      COM_assertion_msg(false, "Unknown data type"); // Not supported
    }

    // Remove the received message from the list
    _reqs_recv.erase(_reqs_recv.begin()+index);
    _reqs_indices.erase(_reqs_indices.begin()+index);
  }
}

template <class T>
void update_value( T *a, T *b, int size) throw(int) {
  for (int i=0; i<size; ++i) {
    b[i] = a[i];
  }
}

// This operation is all local
void Pane_communicator::update_ghost_values() {
  while ( !_reqs_recv.empty()) {
    int index;

    // Wait for any receive request to finish and then process the request
    if ( _comm!=MPI_COMM_NULL) {
      MPI_Status status;
      int ierr = MPI_Waitany( _reqs_recv.size(), &_reqs_recv[0], 
			      &index, &status);
      COM_assertion( ierr == 0);
    }
    else
      index = _reqs_recv.size()-1;

    // Obtain the indices in _shr_buffs for the receive request
    int i=_reqs_indices[index].first, j=(_reqs_indices[index].second>>4);

    int btype = (_reqs_indices[index].second)&15;

    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes;

    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();

    Pane_comm_buffers &pcb = (btype==GNR) ? _gnr_buffs[i][j] : _gcr_buffs[i][j] ;
    COM_assertion( int(pcb.inbuf.size()) == _ncomp_bytes*vs[ pcb.index+1]);

    switch ( _type) {
    case COM_CHAR:
    case COM_CHARACTER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	update_value( (char*)&pcb.inbuf[ _ncomp_bytes*k],
		      (char*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_INT:
    case COM_INTEGER:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	update_value( (int*)&pcb.inbuf[ _ncomp_bytes*k],
		      (int*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_FLOAT:
    case COM_REAL:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	update_value( (float*)&pcb.inbuf[ _ncomp_bytes*k], 
		      (float*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:
      for ( int k=0, nk=vs[pcb.index+1], to=pcb.index+2; k<nk; ++k, ++to)
	update_value( (double*)&pcb.inbuf[ _ncomp_bytes*k], 
		      (double*)&ptr[ strd_bytes*vs[ to]], _ncomp);
      break;
    default:
      COM_assertion_msg(false, "Unknown data type"); // Not supported
    }

    // Remove the received message from the list
    _reqs_recv.erase(_reqs_recv.begin()+index);
    _reqs_indices.erase(_reqs_indices.begin()+index);
  }
}

// This operation is all local
void Pane_communicator::reduce_average_on_shared_nodes(){  
  // Sum the values from all shared nodes
  reduce_on_shared_nodes(MPI_SUM);

  // A map from shared nodes' local ids to multiplicity
  std::map<int,int> nodes_to_mult;
  std::map<int,int>::iterator nodes_to_mult_pos;
  int num_panes = (int)_panes.size();
  //loop through all local panes, constructing multiplicity of nodes
  //then iterate through nodes, dividing by multiplicity
  for(int i = 0; i < num_panes; ++i){
    const COM::Attribute *pconn = _panes[i]->attribute(_my_pconn_id);
    const int *vs = (const int*)pconn->pointer();
    
    // make sure we only consider shared nodes, not ghost nodes
    int vs_size=pconn->size_of_real_items();
    int strd_bytes = COM_get_sizeof( _type, _strds[i]);
    // Shift the pointer by -1 because node IDs in pconn start from 1
    char *ptr = ((char*)_ptrs[i])-strd_bytes;
    
    // Determine the multiplicity of shared nodes.  Multiplicity is the 
    // number of panes which own the node, so at least 2 for shared nodes.
    nodes_to_mult.clear();
    for(int j=1; j<vs_size; j+= vs[j+1]+2){
      // If this pane hasn't been inherited, skip it
      if (owner_rank( vs[j])>= 0){
	// Loop through nodes, ++mult if already seen.  Else mult=2
	for(int k=0; k< vs[j+1]; ++k){
	  nodes_to_mult_pos = nodes_to_mult.find(vs[j+k+2]);
	  if (nodes_to_mult_pos != nodes_to_mult.end())
	    nodes_to_mult_pos->second++;
	  else
	    nodes_to_mult.insert(std::make_pair(vs[j+k+2],2));
	}
      }
    }

    // Loop through the list of shared nodes, dividing data by their multiplicity
    // Remember the assumption that stride == ncomp
    int shared_id,mult;
    switch ( _type) {
    case COM_CHAR:
    case COM_CHARACTER:
      for(nodes_to_mult_pos=nodes_to_mult.begin();
	  nodes_to_mult_pos!=nodes_to_mult.end();
	  ++nodes_to_mult_pos){
	shared_id = nodes_to_mult_pos->first;

	mult = nodes_to_mult_pos->second;
	for(int k = 0; k < _ncomp; ++k){
	  ((char*)&ptr[ strd_bytes*shared_id])[k] /= mult;
	}
      }
      break;
    case COM_INT:
    case COM_INTEGER:
      for(nodes_to_mult_pos=nodes_to_mult.begin();
	  nodes_to_mult_pos!=nodes_to_mult.end();
	  ++nodes_to_mult_pos){
	shared_id = nodes_to_mult_pos->first;

	mult = nodes_to_mult_pos->second;
	for(int k = 0; k < _ncomp; ++k){
	  ((int*)&ptr[ strd_bytes*shared_id])[k] /= mult;
	}
      }
      break;
    case COM_FLOAT:
    case COM_REAL:
      for(nodes_to_mult_pos=nodes_to_mult.begin();
	  nodes_to_mult_pos!=nodes_to_mult.end();
	  ++nodes_to_mult_pos){
	shared_id = nodes_to_mult_pos->first;
	mult = nodes_to_mult_pos->second;
	for(int k = 0; k < _ncomp; ++k){
	  ((float*)&ptr[ strd_bytes*shared_id])[k] /= (float)mult;
	}
      }
      break;
    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:
      for(nodes_to_mult_pos=nodes_to_mult.begin();
	  nodes_to_mult_pos!=nodes_to_mult.end();
	  ++nodes_to_mult_pos){
	shared_id = nodes_to_mult_pos->first;
	mult = nodes_to_mult_pos->second;
	for(int k = 0; k < _ncomp; ++k){
	  ((double*)&ptr[ strd_bytes*shared_id])[k] /= (double)mult;
	}
      }
      break;
    default:
      COM_assertion_msg(false, "Unknown data type"); // Not supported
    }
  }
} 
  
MAP_END_NAMESPACE






