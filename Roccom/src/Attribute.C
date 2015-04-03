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
// $Id: Attribute.C,v 1.29 2008/12/06 08:43:25 mtcampbe Exp $

/** \file Attribute.C
 *  Contains the implementation of the Attribute object.
 *  @see Attribute.h
 */
/* Author: Xiangmin Jiao */

#include "Attribute.h"
#include "Window.h"
#include "Pane.h"
#include "roccom_assertion.h"
#include "commpi.h"
#include <cstring>

COM_BEGIN_NAME_SPACE

/** \name Keywords
 */
/// The names of the keywords.
const char    *Attribute::_keywords[COM_NUM_KEYWORDS] = 
  { "nc", "1-nc", "2-nc", "3-nc", "conn", "mesh", 
    "pconn", "ridges", "1-ridges", "2-ridges", "pmesh", "atts", "all" };

/// The locations of keywords.
const char     Attribute::_keylocs[] =  
  { 'n', 'n', 'n', 'n', 'e', 'p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'};

/// The datatypes of keywords.
const COM_Type Attribute::_keytypes[] = 
  { COM_DOUBLE, COM_DOUBLE, COM_DOUBLE, COM_DOUBLE, COM_METADATA, 
    COM_METADATA, COM_INT, COM_INT, COM_INT, COM_INT, COM_METADATA, 
    COM_METADATA, COM_METADATA };

/// The sizes of keywords.
const COM_Type Attribute::_keysizes[] = 
  { 3, 1, 1, 1, 0, 0, 1, 2, 1, 1, 0, 0, 0 };

//\}

Attribute::Attribute( Pane *pane, Attribute *parent, 
		      const std::string &name, int id)
  : _pane( pane), _id(id), _gap(0), _status(0)
{
  if ( !parent)
    throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST,
			 append_frame(fullname(), Attribute::Attribute));
  _name = name.size()?name:parent->name();

  _parent = NULL; _status = STATUS_NOT_INITIALIZED;
  Attribute *root = parent->root();
  _loc=root->_loc; _ncomp=root->_ncomp; _type=root->_type; 
  _cap = root->_cap; _strd = root->_strd; _nbytes_strd = root->_nbytes_strd;
  _unit=root->_unit; _nitems = root->_nitems; _ngitems = root->_ngitems;
}

Window       *Attribute::window()        { return _pane->window(); }
const Window *Attribute::window()  const { return _pane->window(); }

std::string Attribute::fullname() const 
{ return window()->name()+"."+name(); }

const void *Attribute::get_addr( int i, int j) const throw(COM_exception) {
  if ( _parent) return root()->get_addr( i,j);

  if ( j>=_ncomp) throw COM_exception( COM_ERR_INVALID_DIMENSION);
 
  if ( _ncomp>1) return this[j+1].get_addr( i);

  // Check that i is between 0 and size_of_items-1.
  if ( i<0 || i>=size_of_items()) 
    throw COM_exception( COM_ERR_INDEX_OUT_OF_BOUNDS, append_frame
			 ( fullname(), Attribute::get_addr));
  return  ((char*)_ptr)+i*_nbytes_strd;
}

bool Attribute::size_set() const 
{ 
  if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->size_set();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->size_set();
  else if ( _parent)
    return _parent->size_set();
  else
    return _nitems>=0;
}

int Attribute::size_of_items() const 
{ 
  if ( _pane->ignore_ghost())
    return size_of_real_items();
  else if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->size_of_items();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->size_of_items();
  else if ( _parent) 
    return _parent->size_of_items();
  else
    return _nitems<=0 ? 0 : _nitems;
}

int Attribute::maxsize_of_items() const 
{ 
  if (_pane->ignore_ghost()) 
    return maxsize_of_real_items();
  if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->maxsize_of_items();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->maxsize_of_items();
  else if ( _parent)
    return _parent->maxsize_of_items();
  else
    return  capacity();
}

int Attribute::size_of_ghost_items() const 
{ 
  if ( _pane->ignore_ghost()) 
    return 0;
  else if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->size_of_ghost_items();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->size_of_ghost_items();
  else if ( _parent)
    return _parent->size_of_ghost_items();
  else
    return _ngitems;
}

int Attribute::maxsize_of_ghost_items() const 
{ 
  if ( _pane->ignore_ghost()) 
    return 0;
  else if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->maxsize_of_ghost_items();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->maxsize_of_ghost_items();
  else if ( _parent)
    return _parent->maxsize_of_ghost_items();
  else
    return _nitems<=0 ? 0 :_ngitems + (_cap-_nitems);
}

int Attribute::size_of_real_items() const 
{ 
  if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->size_of_real_items();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->size_of_real_items();
  else if ( _parent)
    return _parent->size_of_real_items();
  else
    return _nitems<=0 ? 0:_nitems - _ngitems - _gap;
}

int Attribute::maxsize_of_real_items() const
{ 
  if ( _loc == 'n' && _id != COM_NC) 
    return _pane->attribute(COM_NC)->maxsize_of_real_items();
  else if ( _loc == 'e' && _id != COM_CONN)
    return _pane->attribute(COM_CONN)->maxsize_of_real_items();
  else if ( _parent)
    return _parent->maxsize_of_real_items();
  else
    return _nitems<=0 ? 0 : _nitems - _ngitems;
}

void Attribute::set_size( int nitems, int ngitems) throw(COM_exception)
{
  if ( _parent)
    throw COM_exception( COM_ERR_CHANGE_INHERITED,
			 append_frame( fullname(), Attribute::set_size));

  COM_assertion( (_loc!='n'||_id==COM_NC) && (_loc!='e'||_id==COM_CONN));
  _nitems = nitems; _ngitems = ngitems; 

  if ( _ncomp>1) {
    // Set sizes for each individial component
    for ( int i=1; i<=_ncomp; ++i) {
      this[i]._nitems = nitems; this[i]._ngitems = ngitems;
    }
  }
}

void Attribute::set_pointer( void *p, int strd, int cap, 
			     int offset, bool is_const) throw(COM_exception)
{
  int nitems=size_of_items(), ncomp=size_of_components();

  // Check whether received a local variable
  // const long int stackSize = 2097152; // 2MB
  // long int dist = std::abs(((long int)p) - (long int)(&nitems));
  // if ( p && dist <= stackSize)
  //   throw COM_exception( COM_ERR_INVALID_ADDR, 
  // 			 append_frame( fullname(), Attribute::set_pointer));

  if ( p && strd!=1 && strd<ncomp) 
    throw COM_exception( COM_ERR_INVALID_STRIDE,
			 append_frame( fullname(), Attribute::set_pointer));
  if ( p && cap<nitems) 
    throw COM_exception( COM_ERR_INVALID_CAPACITY,
			 append_frame( fullname(), Attribute::set_pointer));

  _cap = cap;
  _strd = strd;
  int basesize = get_sizeof( data_type(), 1);
  _nbytes_strd = basesize*strd;

  if ( _status == STATUS_ALLOCATED) deallocate();
  if ( p && offset) {
    if ( strd==1)
      _ptr = ((char*)p) + (offset*cap)*_nbytes_strd;
    else // Staggerred
      _ptr = ((char*)p) + offset*basesize;
  }
  else
    _ptr = p;
  _status = is_const?STATUS_SET_CONST:STATUS_SET;

  if ( ncomp >1 && _id>=0) for ( int i=1; i<=ncomp; ++i) {
    this[i].set_pointer( _ptr, _strd, _cap, i-1, is_const);
  }
}

void Attribute::
copy_array( void *buf, int strd, int n, 
	    int offset, int direction) throw (COM_exception) 
{
  if (direction==COPY_IN) {
    if ( is_const())
      throw COM_exception( COM_ERR_ATTRIBUTE_CONST,
			   append_frame( fullname(), Attribute::copy_array));

    if ( _status==STATUS_NOT_INITIALIZED && _parent) 
    { _cap = root()->_cap; _strd = root()->_strd; _nbytes_strd=root()->_nbytes_strd; }
  }

  int nitems=size_of_items(), ncomp=size_of_components();

  if ( n ==0) { if ( direction==COPY_IN) return; else n = nitems; }
  if ( strd==0) strd=ncomp;
  
  if ( !buf && n)
    throw COM_exception( COM_ERR_NULL_POINTER, 
			 append_frame( fullname(), Attribute::copy_array));

  // strd must be either 1 or at least as large as _strd
  if ( strd!=1 && strd<ncomp) 
    throw COM_exception( COM_ERR_INVALID_STRIDE,
			 append_frame( fullname(), Attribute::copy_array));

  if ( offset<0 || n+offset>_cap)
    throw COM_exception( COM_ERR_INVALID_SIZE, 
			 append_frame( fullname(), Attribute::copy_array));

  int basesize = get_sizeof( data_type());

  char *ptr0 = (char*)pointer();
  if (offset) ptr0 += offset*ncomp*basesize;

  if ( _strd == strd && (_strd == ncomp || n==_cap && strd==1) ) {
    // two arrays have the same layout
    if ( direction == COPY_IN)
      std::memcpy( ptr0, buf, n*ncomp*basesize);
    else
      std::memcpy( buf, ptr0, n*ncomp*basesize);
  }
  else if ( _strd>1 && strd>1) {
    // the components are stored contiguously in both arrays
    char *p_buf=(char*)buf;
    char *p_att = ptr0;
    int strd_att_in_bytes = _nbytes_strd;
    int strd_buf_in_bytes =  strd*basesize;
    int vecsize = ncomp*basesize;

    for ( int i=0, ni=std::min(n, nitems); i<ni; ++i) {
      if ( direction == COPY_IN)
	std::memcpy( p_att, p_buf, vecsize);
      else
	std::memcpy( p_buf, p_att, vecsize);
      p_buf += strd_buf_in_bytes;
      p_att += strd_att_in_bytes;
    }
  }
  else {
    // layouts of two arrays are very different from each
    char *p_buf=(char*)buf;
    char *p_att = ptr0;

    int strd_att_in_bytes = _nbytes_strd;
    int strd_buf_in_bytes =  strd*basesize;
    int step_att_in_bytes= (_strd==1?_cap:1)*basesize;
    int step_buf_in_bytes = (strd==1?n:1)*basesize;
    for ( int i=0, ni=std::min(n, nitems); i<ni; ++i) {
      for ( int j=0, offset_buf=0, offset_att=0; j<ncomp; ++j) {
	if ( direction == COPY_IN)
	  std::memcpy( p_att+offset_att, p_buf+offset_buf, basesize);
	else
	  std::memcpy( p_buf+offset_buf, p_att+offset_att, basesize);
	offset_buf += step_buf_in_bytes;
	offset_att += step_att_in_bytes;
      }
      p_buf += strd_buf_in_bytes;
      p_att += strd_att_in_bytes;
    }
  }
}

// Append n _ncomp-vectors from "from" to the array.
void Attribute::
append_array( const void *from, int strd, int nitem) throw(COM_exception)
{
  if ( !is_panel() && !is_windowed() || size_of_ghost_items())
    throw COM_exception( COM_ERR_APPEND_ARRAY,
			 append_frame(fullname(), Attribute::append_array));

  int offset = size_of_items();
  copy_array( const_cast<void*>(from), strd, nitem, offset);
  set_size( offset+nitem);
}

void *Attribute::allocate( int strd, int cap, bool force) throw( COM_exception)
{
  int nitems=size_of_items(), ncomp=size_of_components();
  
  if ( strd!=1 && strd<ncomp) 
    throw COM_exception( COM_ERR_INVALID_STRIDE,
			 append_frame( fullname(), Attribute::allocate));
  if ( cap<nitems) 
    throw COM_exception( COM_ERR_INVALID_CAPACITY,
			 append_frame( fullname(), Attribute::allocate));

  if ( !force && (_parent || _status==STATUS_SET || _status==STATUS_SET_CONST))
    throw COM_exception( COM_ERR_RESIZE,
			 append_frame( fullname(), Attribute::allocate));

  try {
    int type = data_type();
    // Go ahead to allocate for the attribute if it was not initialized 
    // and not inherited or it is forced to overwrite previously set address.
    int nold, old_cap;
    if ( _status == STATUS_NOT_INITIALIZED || force) 
    { nold = -1; old_cap = 0; }
    else {
      old_cap = _cap;
      nold = old_cap*get_sizeof(type, std::max(_strd,ncomp)); 
    }

    int nnew = cap*get_sizeof(type, std::max(strd,ncomp));

    // if the capacity is not big enough or the stride is changed.
    if ( nold < nnew || strd != _strd) {
      // Deallocate the old array and copy values to the new one
      char *old_ptr = (char*)_ptr;
      int  old_strd = _strd;

      if ( nnew) {
	_ptr = new char[nnew];
	std::fill_n( (char*)_ptr, nnew, 0);
	if (_ptr==NULL) 
	  throw COM_exception( COM_ERR_OUT_OF_MEMORY,
			       append_frame( fullname(), Attribute::allocate));
      }
      else _ptr = NULL;
      _cap = cap; _strd = strd; _nbytes_strd = get_sizeof( data_type(), strd);
	
      // Copy data from old array to the new.
      if ( old_cap && _ptr) {
	if ( _ncomp ==1 || _id<0) // Copy for connectivity and for scalars
	  copy_array( old_ptr, old_strd, std::min(old_cap,_cap));
	else { // loop through individual components
	  for ( int i=1; i<=ncomp; ++i) {
	    Attribute *ai = this+i;
	    char *old_ptr_i = (char*)ai->_ptr; old_strd=ai->_strd; 
	    old_cap = ai->_cap;
	    ai->set_pointer( _ptr, _strd, _cap, i-1, false);
	    ai->copy_array( old_ptr_i, old_strd, std::min(old_cap,_cap));

	    // Delete the individual components
	    if ( ai->_status == STATUS_ALLOCATED && old_ptr_i) 
	      delete [] old_ptr_i;
	  }
	}
      }
      else {
	if ( _ncomp >1 && _id>=0) for ( int i=1; i<=ncomp; ++i) {
	  this[i].set_pointer( _ptr, _strd, _cap, i-1, false);
	}
      }

      // Delete the old array for all components
      if ( _status == STATUS_ALLOCATED && old_ptr) delete [] old_ptr;
      
      _status = STATUS_ALLOCATED;
      if ( _parent) _parent=NULL; // Break inheritance.
    }
  }
  CATCHEXP_APPEND(Attribute::allocate) CATCHBADALLOC_APPEND(Attribute::allocate);
  return _ptr;
}

void Attribute::
inherit( Attribute *parent, bool clone, bool withghost, int depth) throw(COM_exception) 
{
  Attribute *root = parent->root();
  _loc=root->_loc; _ncomp=root->_ncomp; _type=root->_type; 
  _cap = root->_cap; _strd = root->_strd; 
  _nbytes_strd = get_sizeof( _type, _strd); _unit=root->_unit;

  if ( !clone) {
    _parent = parent;

    // Inherit individual components
    if ( _ncomp>1 && _id>=0) try {
      for ( int i=1; i<=_ncomp; ++i) {
	this[i].inherit( parent+i, clone, withghost, 1);
      }
    } CATCHEXP_APPEND(Attribute::inherit);
  }
  else {
    _parent = NULL;
    // Set size
    if ( withghost) {
      _nitems = parent->size_of_items(); 
      _ngitems = parent->size_of_ghost_items();
    }
    else {
      _nitems = parent->size_of_real_items(); 
      _ngitems = 0;
    }
    if ( depth>0) return;

    // Allocate and copy from parent if parent was initialized.
    try {
      if (parent->initialized()) allocate( _ncomp, _cap, true);
      else deallocate();
    }  CATCHEXP_APPEND(Attribute::inherit) CATCHBADALLOC_APPEND(Attribute::allocate);

    // Set pointer for individual components if allocated.
    if ( _ncomp>1 && _id>=0) try {
      for ( int i=1; i<=_ncomp; ++i) {
	this[i].inherit( parent+i, clone, withghost, 1);
	if (allocated())
	  this[i].set_pointer( _ptr, _strd, _cap, i-1, false);
      }
    } CATCHEXP_APPEND(Attribute::inherit);
  }
}

int Attribute::deallocate() throw( COM_exception) {
  try {
    // Deallocate attribute and set individual components to not initialized.
    if ( _status != STATUS_ALLOCATED) return -1; // failed
    _status = STATUS_NOT_INITIALIZED; 
    if ( _ptr) { delete [] (char*)_ptr; _ptr = NULL; }

    if ( _ncomp>1 && _id>=0) 
      for ( int i=1; i<=_ncomp; ++i)
	{ this[i]._ptr = NULL; this[i]._status = STATUS_NOT_INITIALIZED; }
  }
  CATCHEXP_APPEND(Attribute::deallocate);
  return 0;
}

int Attribute::get_sizeof( COM_Type type, int count) {
  switch (type) {
  case COM_CHAR:
  case COM_UNSIGNED_CHAR:
  case COM_BYTE:
  case COM_BOOL:
  case COM_CHARACTER:      return count;

  case COM_UNSIGNED_SHORT:
  case COM_SHORT:          return count*sizeof(short);
    
  case COM_INT: 
  case COM_INTEGER: 
  case COM_LOGICAL:
  case COM_UNSIGNED:       return count*sizeof(int);

  case COM_UNSIGNED_LONG:
  case COM_LONG:           return count*sizeof(long int);

  case COM_FLOAT:
  case COM_REAL:           return count*sizeof(float);
    
  case COM_DOUBLE_PRECISION:
  case COM_DOUBLE:         return count*sizeof(double);

  case COM_LONG_DOUBLE:    return count*sizeof(long double);

  case COM_COMPLEX:        return count*2*sizeof(float);
  case COM_DOUBLE_COMPLEX: return count*2*sizeof(double);

  case COM_MPI_COMMC:      return count*sizeof(MPI_Comm);
  case COM_MPI_COMMF:      return count*sizeof(int);

  case COM_F90POINTER:     return count*64;
  case COM_METADATA:  
  case COM_VOID:           return sizeof(void*)*count;
  case COM_OBJECT:         return sizeof(COM_Object*)*count;
  case COM_STRING:         return count;

  default:  COM_assertion(false); return -1;
  }
}

bool Attribute::compatible_types( COM_Type t1, COM_Type t2) {
  if (t1 == t2) return true;
  if ( Attribute::get_sizeof( t1, 1) != Attribute::get_sizeof( t2, 1)) 
    return false;

  switch( t1) {
  case COM_CHAR:
  case COM_CHARACTER:
  case COM_UNSIGNED_CHAR:
  case COM_BYTE:
  case COM_SHORT: 
  case COM_UNSIGNED_SHORT: 
    return true; // size matching
  case COM_INT:
  case COM_INTEGER:
  case COM_UNSIGNED:
  case COM_LONG:
  case COM_UNSIGNED_LONG:
    return t2 == COM_INT || t2 == COM_INTEGER || t2 == COM_UNSIGNED ||
      t2 == COM_UNSIGNED_LONG || t2 == COM_LONG;
  case COM_FLOAT:
  case COM_REAL:
    return t2 == COM_FLOAT || t2 == COM_REAL;
  case COM_DOUBLE:
  case COM_DOUBLE_PRECISION:
    return t2 == COM_DOUBLE_PRECISION || t2 == COM_DOUBLE;
  default:
    return false;
  }
}


COM_END_NAME_SPACE






