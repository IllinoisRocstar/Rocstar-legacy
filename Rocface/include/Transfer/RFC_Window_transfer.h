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
// $Id: RFC_Window_transfer.h,v 1.14 2008/12/06 08:43:27 mtcampbe Exp $

//===============================================================
// This file defines RFC_Window_transfer and RFC_Pane_transfer, the
//    basic data structures for data transfer.
// Author: Xiangmin Jiao
// Date:   June. 15, 2001
//===============================================================

#ifndef RFC_WINDOW_TRANSFER_H
#define RFC_WINDOW_TRANSFER_H

#include "RFC_Window_base.h"
#include "Vector_n.h"
#include "commpi.h"

RFC_BEGIN_NAME_SPACE

class RFC_Window_transfer;
template < class _Tag> class RFC_Data;
template < class _Tag> class RFC_Data_const;
struct Tag_facial {};
struct Tag_nodal {};
typedef RFC_Data_const< Tag_nodal>            Nodal_data_const;
typedef RFC_Data_const< Tag_facial>           Facial_data_const;
typedef RFC_Data< Tag_nodal>                  Nodal_data;
typedef RFC_Data< Tag_facial>                 Facial_data;

// RFC_Pane_transfer is built based on RFC_Pane_base with extension of
//    extra attribute for storing buffers for data transfer.
class RFC_Pane_transfer : public RFC_Pane_base {
public:
  typedef RFC_Pane_transfer                              Self;
  typedef RFC_Pane_base                                  Base;
  friend class RFC_Window_transfer;

  RFC_Pane_transfer( COM::Pane *b, int c);
  virtual ~RFC_Pane_transfer();

  RFC_Window_transfer *window() { return _window; }
  const RFC_Window_transfer *window() const { return _window; }

  Real *pointer( int i) { 
    if ( i>=0) {
      if ( is_master()) 
	return Base::pointer(i);
      else {
	RFC_assertion( _data_buf_id == i);
	return &_data_buf.front();
      }
    }
    else
      return &_buffer[-i-1][0];
  }
  const Real *pointer( int i) const
  { return const_cast<Self*>(this)->pointer(i); }
  const Real *coordinates() const { 
    if ( is_master()) 
      return Base::coordinates();
    else {
      return &_coor_buf.front();
    }
  }
  bool need_recv( int i) const { 
    // Check whether an element is on the receiver list
    if ( _to_recv == NULL) return true;
    RFC_assertion( i>=1 && i<=size_of_faces());
    return _to_recv[ i-1];
  }

  Real *get_emm( int f) {
    RFC_assertion( f>=1 && f<=size_of_faces()); 
    return &_emm_buffer[ _emm_offset[f-1]]; 
  }
  const Real *get_emm( int f) const {
    RFC_assertion( f>=1 && f<=size_of_faces()); 
    return &_emm_buffer[ _emm_offset[f-1]];
  }

  bool is_master() const { return _base->window()!=NULL; }

private:
  // Data member 
  RFC_Window_transfer  *_window;    // Point to its parent window.

  std::vector< std::vector<Real> >  _buffer;       // Buffer for PCG
  std::vector< int>                 _emm_offset;   // Element mass matrix 
  std::vector< Real>                _emm_buffer;

  int                               _data_buf_id;
  std::vector< Real>                _coor_buf;
  std::vector< Real>                _data_buf;

  std::vector< MPI_Request>         _msg_requests;
  int                              *_to_recv;

  //======= Data members to reduce communication volume for data transfer
  // List of faces and nodes to be received from owner process.
  // These arrays are allocated only if this pane is cached copy.
  std::vector<int>                    _recv_faces;
  std::vector<int>                    _recv_nodes;

  // Maps from process ids to the lists of faces and nodes to be sent.
  // These fields are used only if this pane is the master copy.
  std::map<int, std::vector<int> >    _send_faces;
  std::map<int, std::vector<int> >    _send_nodes;
  std::map<int, std::vector<Real> >   _send_buffers;
};

// A window is a collection of panes.
class RFC_Window_transfer : public RFC_Window_derived< RFC_Pane_transfer> {
public:
  typedef RFC_Window_transfer                           Self;
  typedef RFC_Window_derived<RFC_Pane_transfer>         Base;

  RFC_Window_transfer( COM::Window *b, int color, 
		       MPI_Comm com, const char *pre=NULL, 
		       const char *format=NULL);
  virtual ~RFC_Window_transfer();

  RFC_Pane_transfer &pane( const int pid);
  const RFC_Pane_transfer &pane( const int pid) const;

  // ========  Functions for supporting data transfer algorithms
  void init_facial_buffers( const Facial_data &nd, int);
  Facial_data facial_buffer( int);
  void delete_facial_buffers();

  void init_nodal_buffers( const Nodal_data &nd, int n, bool init_emm);
  Nodal_data nodal_buffer( int);
  void delete_nodal_buffers();

  // Set _to_recv tags for the next data transfer algorithm.
  // If tag is NULL, reset the tags to NULL.
  void set_tags( const COM::Attribute *tag);

  // ============ Communication subroutines for source panes ==================
  /// Returns whether replication has been performed.
  bool replicated() const { return _replicated; }

  /// Replicate the metadata of a remote pane only the local process
  void replicate_metadata( int *pane_ids, int n);
  /// Clear all the replicate data but keep metadata.
  void clear_replicated_data();
  /// Determine the remote panes that are incident with local panes.
  void incident_panes( std::vector<int> &pane_ids);

  /// Obtain the list of incident faces in each pane of the opposite mesh.
  void incident_faces( std::map< int, std::vector<int> > &) const;

  /// Replicate the remote panes that are in contact with the local panes
  /// in opp_win.
  void replicate_metadata( const RFC_Window_transfer &opp_win);

  /// Replicate the given data from remote processes onto local process.
  /// Replicate coordinates only if replicate_coor is true. 
  void replicate_data( const Facial_data_const &data, bool replicate_coor);
  void replicate_data( const Nodal_data_const &data, bool replicate_coor);

  //============= communication subroutines for target panes ==================
  void reduce_to_all( Nodal_data &, MPI_Op);
  void reduce_maxabs_to_all( Nodal_data &);

  // ===== Lower level communication routines =============
  //! Block until all processes of the window have reached here.
  void barrier() const;
  //! Check whether the process has rank 0.
  bool is_root() const;
  int  comm_rank() const 
  { return COMMPI_Initialized()?COMMPI_Comm_rank( _comm):0; }
  int  comm_size() const 
  { return COMMPI_Initialized()?COMMPI_Comm_size( _comm):1; }

  void wait_all( int n, MPI_Request *requests);
  void wait_any( int n, MPI_Request *requests, int *index, MPI_Status *stat=NULL);

  void allreduce( Array_n &arr, MPI_Op op) const
  { allreduce( arr.begin(), arr.end()-arr.begin(), op); }
  void allreduce( Real *x, MPI_Op op) const
  { allreduce( x, 1, op); }

private:
  void allreduce( Real *, int n, MPI_Op op) const;

  // Initialize the data structures _pane_map and _num_panes.
  void counts_to_displs( const std::vector<int> &counts,
			 std::vector<int> &displs) const {
    RFC_assertion( !displs.empty() && counts.size()>=displs.size()-1);
    displs[0]=0;
    for ( int i=1, size=displs.size(); i<size; ++i) {
      displs[i] = displs[i-1]+counts[i-1];
    }
  }

  void init_send_buffer( int pane_id, int to_rank);
  void init_recv_buffer( int pane_id, int from_rank);

private:
  int                                            _buf_dim;
  MPI_Comm                                       _comm;
  std::map< int, std::pair<int, int> >           _pane_map;
  std::vector<int>                               _num_panes;
  std::map< int, RFC_Pane_transfer*>             _replic_panes;
  bool                                           _replicated;

  std::set< std::pair<int, RFC_Pane_transfer*> > _panes_to_send; //<to_rank, p>
  const std::string                              _prefix;
  const int                                      _IO_format;
};

//================================================================
//  Wrapper classes for accessing data fields of a window.
//================================================================
template <class _Tag>
class RFC_Data_const {
public:
  typedef Vector_n               Value;
  typedef Array_n                Value_nonconst;
  typedef Array_n_const          Value_const;
  typedef unsigned int           Size;
  typedef _Tag                   Tag;

  RFC_Data_const() : _id(0), _dim(0) {}
  RFC_Data_const( const RFC_Window_transfer &win, const char *aname) {
    const COM::Attribute *a = win.attribute( aname);
    _id = a->id(); _dim = a->size_of_components();
  }
  RFC_Data_const( const COM::Attribute *a) 
    : _id(a->id()), _dim(a->size_of_components()) {}
  RFC_Data_const( int i, int d) 
    : _id( i), _dim(d) {}

  int  id() const { return _id; }
  Size dimension() const { return _dim; }
  bool is_valid( const RFC_Pane_transfer *p, int i) const
  { return true; }

  // Get the data associated with node v
  Value_const get_value( const RFC_Pane_transfer *p, int v) const {
    RFC_assertion( v>=1);
    return Value_const( p->pointer( _id) + (v-1)*_dim, _dim);
  }

  Value_const get_value( const Real *buf, int v) const {
    RFC_assertion( v>=1);
    return Value_const( buf + (v-1)*_dim, _dim);
  }

  Tag tag() const { return Tag(); }
protected:
  int _id;
  unsigned int _dim;
};


// Wrapper for node centered data.
template < class _Tag>
class RFC_Data : public RFC_Data_const<_Tag> {
public:
  typedef RFC_Data_const<_Tag>       Base;
  typedef Vector_n                   Value;
  typedef Array_n                    Value_nonconst;
  typedef Array_n_const              Value_const;
  typedef unsigned int               Size;
  typedef _Tag                       Tag;
  
  RFC_Data() {}
  RFC_Data( RFC_Window_transfer &win, const char *aname) : Base( win, aname) {}
  RFC_Data( COM::Attribute *a)  :  Base( a) {}
  RFC_Data( int i, int d)       :  Base(i, d) {}

  // Get the data associated with node v
  Value_nonconst get_value( RFC_Pane_transfer *p, int v) {
    RFC_assertion( v>=1);
    return Value_nonconst( p->pointer( _id) + (v-1)*_dim, _dim);
  }
  // Get the data associated with node v
  Value_const get_value( const RFC_Pane_transfer *p, int v) const {
    RFC_assertion( v>=1);
    return Value_const( p->pointer( _id) + (v-1)*_dim, _dim);
  }
  void set_value( RFC_Pane_transfer *p, int v, const Value_const &vec) {
    RFC_assertion( _dim == vec.dimension() && v>=1);
    std::copy( vec.begin(), vec.end(), p->pointer( _id) + (v-1)*_dim);
  }

  Value_nonconst get_value( Real *buf, int v) const {
    RFC_assertion( v>=1);
    return Value_nonconst( buf + (v-1)*_dim, _dim);
  }
  Value_const get_value( const Real *buf, int v) const {
    RFC_assertion( v>=1);
    return Value_const( buf + (v-1)*_dim, _dim);
  }
  void set_value( Real *buf, int v, const Value_const &vec) {
    RFC_assertion( _dim == vec.dimension() && v>=1);
    std::copy( vec.begin(), vec.end(), buf + (v-1)*_dim);
  }

  Tag tag() const { return Tag(); }
protected:
  using RFC_Data_const<_Tag>::_id;
  using RFC_Data_const<_Tag>::_dim;
};

RFC_END_NAME_SPACE

#endif






