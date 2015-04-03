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
// $Id: Pane_communicator.h,v 1.20 2009/08/27 14:04:49 mtcampbe Exp $

/** \file Pane_communicator.h
 *  Handles communication of shared nodes, ghost nodes or ghost cells
 *  across panes.  No overlap is permitted among the three types of
 *  communication.
 */

#ifndef _PANE_COMMUNICATOR_H
#define _PANE_COMMUNICATOR_H

#include "mapbasic.h"
#include "roccom_devel.h"
#include "commpi.h"
#include "roccom_assertion.h"

MAP_BEGIN_NAMESPACE

/// Encapsulates information about distribution of panes on all processes
/// and handles across pane communication for shared nodes, ghost nodes,
/// and ghost cells.  However,there can be no overlap among the 3 types
/// of communication since they share the same set of tags and request 
/// buffers.
class Pane_communicator {
protected:
  /// Buffers for outgoing and incoming messages of a specific pane
  /// to be communicated with another pane (either local or global).
  /// If the Pane_comm_buffers is used for ghost information, then
  /// it will be used either for outgoing messages and have an empty
  /// inbuf OR it will be used for incoming messages and have an empty
  /// outbuf.
  struct Pane_comm_buffers {
    // Default constructor
    Pane_comm_buffers() : rank(-1), tag(-1), index(-1) {}

    int                   rank;   // rank for communicating process
    int                   tag;    // tag for MPI message
    int                   index;  // starting index of information 
                                  // between the two panes in the 
                                  // pconn of the local pane
    std::vector< char>    outbuf; // buffer for outgoing messages
    std::vector< char>    inbuf;  // buffer for incoming messages
  };
public:
  // Note: One can use the RNS and GNR for sending and receiving data for
  // boundary edges with a custmized pconn.
  enum Buff_type {
    RNS,         // Real nodes to send
    RCS,         // Real cells to send.
    SHARED_NODE, // Shared nodes to send and receive.
    GNR,         // Ghost nodes to receive.
    GCR          // Ghost cells to receive.
  };

  /// Constructor from a communicator.
  /// Also initialize the internal data structures of the communicator,
  /// in particular the internal pane IDs.
  explicit Pane_communicator( COM::Window *w, MPI_Comm c=MPI_COMM_WORLD);
  
  /// Initialize the communication buffers.
  ///  ptrs is an array of pointers to the data for all local panes
  ///  type is the base data type (such as MPI_INT)
  ///  ncomp is the number of component per node
  ///  sizes is the number of items per pane. If null, then use number of nodes.
  ///  strides are the strides for each array. If null, then use ncomp for all.
  void init( void** ptrs, COM_Type type, int ncomp, 
	     const int *sizes=NULL, const int *strds=NULL);

  ///  Initialize the communication buffers.
  ///  att is a pointer to the attribute
  ///  my_pconn stores pane-connectivity
  void init( COM::Attribute *att, const COM::Attribute *my_pconn = NULL);

  /// Obtain the MPI communicator for the object
  MPI_Comm mpi_comm() const { return _comm; }

  /// Obtains all the local panes.
  std::vector<COM::Pane*> &panes() { return _panes; }

  /// Obtain the process rank of a given pane. If the pane does not exist,
  /// it will return -1.
  int owner_rank( const int pane_id) const 
  { return _appl_window->owner_rank( pane_id); }

  /// Obtain the total number of panes on all processes
  int total_npanes() const { return _total_npanes; }

  /// For a given pane, obtain an internal pane ID which is 
  /// unique and contiguous across processes
  int lpaneid( const int pane_id) const {
    COM_assertion( _total_npanes>0); 
    return _lpaneid_map.find(pane_id)->second;
  }

  /// Initiates updating shared nodes by calling MPI_Isend and MPI_Irecv.
  /// If involved is not NULL, then at exit it contains a vector of bitmaps, 
  /// each of which corresponds to a local pane, and the size of each bitmap 
  /// is the number of real nodes for its corresponding local pane. Upon exit, 
  /// the bits corresponding to shared nodes are set to true.  The information
  /// in involved is required to overlap communication and computation during
  /// reduction operations.
  void begin_update_shared_nodes(std::vector<std::vector<bool> > *involved=NULL){
    begin_update(SHARED_NODE,involved);
  }

  /// Initiates updating ghost nodes by calling MPI_Isend and MPI_Irecv.
  void begin_update_ghost_nodes(){
    begin_update(RNS);
    begin_update(GNR);
  }

  /// Initiates updating ghost nodes by calling MPI_Isend and MPI_Irecv.
  void begin_update_ghost_cells(){
    begin_update(RCS);
    begin_update(GCR);
  }

  /// Finalizes shared node updating by calling MPI_Waitall on all send requests.
  void end_update_shared_nodes() {
    end_update();
  }

  /// Finalizes ghost node updating by calling MPI_Waitall on all send requests.
  void end_update_ghost_nodes(){
    update_ghost_values();
    end_update();
  }

  /// Finalizes ghost cell updating by calling MPI_Waitall on all send requests.
  void end_update_ghost_cells(){
    update_ghost_values();
    end_update();
  }

  /// Perform a reduction operation using locally cached values of the shared 
  /// nodes, assuming begin_update_shared_nodes() has been called.
  void reduce_on_shared_nodes( MPI_Op);

  /// Reduce to the value with the maximum absolute value using locally cached
  /// values of shared nodes.
  void reduce_maxabs_on_shared_nodes();

  /// Reduce to the value with the maximum absolute value using locally cached
  /// values of shared nodes.
  void reduce_minabs_on_shared_nodes();

  /// Compute difference of non-zero values of each shared node, assuming
  /// there are at most two non-zero values.
  void reduce_diff_on_shared_nodes();

  void update_ghost_values();

  /// Reduce to the average of values using locally cached values of the shared
  /// nodes, assuming begin_update_shared_nodes() has been called.
  void reduce_average_on_shared_nodes();

protected:
  /// Initialize a Pane_comm_buffers for ghost information
  void init_pane_comm_buffers(std::vector< Pane_comm_buffers>& pcb,
			      const int* ptr, int& index, const int n_items, const int lpid);

  /// Finalizes updating by calling MPI_Waitall on all send requests.
  void end_update();

  /// Initiates updating by calling MPI_Isend and MPI_Irecv.
  /// See begin_update_shared_nodes for description of involved.
  void begin_update(const Buff_type btype,
		    std::vector<std::vector<bool> > *involved=NULL);

  /// The id of the pconn being used.
  int                           _my_pconn_id;

  /// Reference to the application window
  COM::Window                  *_appl_window;
  /// MPI Communicator
  const MPI_Comm                _comm;
  /// Vector of all local panes
  std::vector<COM::Pane*>       _panes;
  /// The total number of panes on all processes
  int                           _total_npanes;
  /// Mapping from user-defined pane ids to internal IDs, which 
  /// are unique and contiguous across all processes, which are useful
  /// for defining unique tags for MPI messages.
  std::map<int,int>             _lpaneid_map;
  /// The base data type, number of components, and the number of bytes of
  /// all components for the data to be communicated.
  int                              _type, _ncomp, _ncomp_bytes;
  /// An array of pointers to the data for all local panes
  std::vector< void*>              _ptrs;
  /// The sizes of the arrays for all local panes.
  std::vector< int>                _sizes;
  /// The strides of the arrays for all local panes.
  std::vector< int>                _strds;
  /// Shared node pane communication buffers. The outer vector corresponds 
  /// to local panes; the inner vectors corresponds to the 
  //  communicating panes for each local pane.
  std::vector< std::vector< Pane_comm_buffers> > _shr_buffs;
  /// Buffer for real nodes to send.
  std::vector< std::vector< Pane_comm_buffers> > _rns_buffs;
  /// Buffer for ghost nodes to receive.
  std::vector< std::vector< Pane_comm_buffers> > _gnr_buffs;
  /// Buffer for real cells to send.
  std::vector< std::vector< Pane_comm_buffers> > _rcs_buffs;
  /// Buffer for ghost cells to receive.
  std::vector< std::vector< Pane_comm_buffers> > _gcr_buffs;
  /// Arrays of pending nonblocking MPI requests.  Same format as _shr_buffs.
  std::vector<MPI_Request>         _reqs_send, _reqs_recv;
  /// The indices in buffs for each pending nonblocking receive request.
  std::vector<std::pair<int,int> > _reqs_indices;

private:
  // Disable the following operators
  Pane_communicator( const Pane_communicator &);
  Pane_communicator &operator=( const Pane_communicator&); 
};

MAP_END_NAMESPACE

#endif  /* _PANE_COMMUNICATOR_H */






