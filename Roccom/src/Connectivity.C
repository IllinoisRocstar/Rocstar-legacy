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
// $Id: Connectivity.C,v 1.11 2008/12/06 08:43:25 mtcampbe Exp $

/** \file Connectivity.C
 *  Contains the implementation of the Connectivity object.
 *  @see Connectivity.h
 */
/* Author: Xiangmin Jiao */

#include "Connectivity.h"
#include "Pane.h"
#include "roccom_assertion.h"
#include <cstring>

COM_BEGIN_NAME_SPACE

// Tables of pre-defined connectivity types
const int Connectivity::_sizes[TYPE_MAX_CONN][SIZE_MAX_CONN] = 
{ 
  /* type, dim, order, nnodes, ncorn, nedges, nfaces */
  {ST1,      1, 1,     2,      2,     1,     0},
  {ST2,      2, 1,     4,      4,     4,     1},
  {ST3,      3, 1,     8,      8,    12,     6},
  {BAR2,     1, 1,     2,      2,     1,     0},
  {BAR3,     1, 2,     3,      2,     1,     0},
  {TRI3,     2, 1,     3,      3,     3,     1},
  {TRI6,     2, 2,     6,      3,     3,     1},
  {QUAD4,    2, 1,     4,      4,     4,     1},
  {QUAD8,    2, 2,     8,      4,     4,     1},
  {QUAD9,    2, 2,     9,      4,     4,     1},
  {TET4,     3, 1,     4,      4,     6,     4},
  {TET10,    3, 2,     10,     4,     6,     4},
  {PYRIMID5, 3, 1,     5,      5,     8,     5},
  {PYRIMID14,3, 2,     14,     5,     8,     5},
  {PRISM6,   3, 1,     6,      6,     9,     5},
  {PRISM15,  3, 2,     15,     6,     9,     5},
  {PRISM18,  3, 2,     18,     6,     9,     5},
  {HEX8,     3, 1,     8,      8,    12,     6},
  {HEX20,    3, 2,     20,     8,    12,     6},
  {HEX27,    3, 2,     27,     8,    12,     6}
};

const int *Connectivity::get_addr( int i, int j) const throw(COM_exception) {
  if ( _parent)  return root()->get_addr( i,j);

  // Check that i is between 0 and size_of_items-1.
  if ( i<0 || i>=size_of_items()) 
    throw COM_exception( COM_ERR_INDEX_OUT_OF_BOUNDS, append_frame
			 ( fullname(), Attribute::get_addr));

  int offset = ( _strd == 1) ? (j*_cap) : j;
  return  ((const int*)_ptr)+offset+i*_strd;
}

void Connectivity::set_size( int nitems, int ngitems) throw(COM_exception) {
  if ( _parent)
    throw COM_exception( COM_ERR_CHANGE_INHERITED,
			 append_frame( fullname(), Connectivity::set_size));

  if ( !is_structured()) {
    _nitems = nitems; _ngitems = ngitems; 
    _pane->refresh_connectivity();
  }
  else {
    _nitems = dimension(); _ngitems = ngitems; 
    if ( initialized()) _pane->refresh_connectivity();
  }
}

/// Set the index of the first element.
void Connectivity::set_offset( Size offset) throw(COM_exception) { 
  _offset = offset; 
}

Connectivity::Size Connectivity::size_of_elements() const 
{ 
  if ( !is_structured()) 
    return size_of_items();
  else {
    COM_assertion_msg( _ngitems==0 || !_pane->ignore_ghost(),
		       "Only unstructured meshes supports inheritance without ghost");

    const int *sizes=pointer();
    if ( sizes==NULL) return 0;
    int n=0;
    switch ( _size_info[TYPE_ID]) {
    case ST1: n=sizes[0]-1; break;
    case ST2: n=(sizes[0]-1)*(sizes[1]-1); break;
    case ST3: n=(sizes[0]-1)*(sizes[1]-1)*(sizes[2]-1); break;
    default: COM_assertion(false); // Should never reach here.
    }
    return std::max(0,n);
  }
}

Connectivity::Size Connectivity::size_of_ghost_elements() const 
{ 
  if ( !is_structured()) 
    return size_of_ghost_items(); 
  else {
    const int *sizes=pointer();
    if ( sizes==NULL) return 0;
    int n=0;
    switch ( _size_info[TYPE_ID]) {
    case ST1: n=2*_ngitems; break;
    case ST2: n=(sizes[0]-1)*(sizes[1]-1)-
		(sizes[0]-1-2*_ngitems)*(sizes[1]-1-2*_ngitems); break;
    case ST3: n=(sizes[0]-1)*(sizes[1]-1)*(sizes[2]-1)-
		(sizes[0]-1-2*_ngitems)*(sizes[1]-1-2*_ngitems)*
		(sizes[2]-1-2*_ngitems); break;
    default: COM_assertion(false); // Should never reach here.
    }
    return std::max(0,n);
  }
}


Connectivity::Size Connectivity::size_of_real_elements() const 
{ 
  if ( !is_structured())
    return size_of_real_items(); 
  else {
    const int *sizes=pointer();
    if ( sizes==NULL) return 0;
    int n=0;
    switch ( _size_info[TYPE_ID]) {
    case ST1: n=sizes[0]-1-2*_ngitems; break;
    case ST2: n=(sizes[0]-1-2*_ngitems)*(sizes[1]-1-2*_ngitems); break;
    case ST3: n=(sizes[0]-1-2*_ngitems)*(sizes[1]-1-2*_ngitems)*
		(sizes[2]-1-2*_ngitems); break;
    default: COM_assertion(false); // Should never reach here.
    }
    return std::max(0,n);
  }
}

Connectivity::Size Connectivity::size_of_nodes() const 
{ 
  if ( !is_structured()) 
    return _pane->size_of_nodes();
  else {
    COM_assertion_msg( _ngitems==0 || !_pane->ignore_ghost(),
		       "Only unstructured meshes supports inheritance without ghost");

    const int *sizes=pointer();
    if ( sizes==NULL) return 0;
    switch ( _size_info[TYPE_ID]) {
    case ST1: return sizes[0];
    case ST2: return sizes[0]*sizes[1];
    case ST3: return sizes[0]*sizes[1]*sizes[2];
    default: COM_assertion(false); // Should never reach here.
    }
    return 0;
  }
}

Connectivity::Size Connectivity::size_of_ghost_nodes() const 
{ 
  if ( !is_structured()) 
    return _pane->size_of_ghost_nodes();
  else {
    const int *sizes=pointer();
    if ( sizes==NULL) return 0;
    switch ( _size_info[TYPE_ID]) {
    case ST1: return 2*_ngitems;
    case ST2: return sizes[0]*sizes[1]-
		(sizes[0]-2*_ngitems)*(sizes[1]-2*_ngitems);
    case ST3: return sizes[0]*sizes[1]*sizes[2]-
		(sizes[0]-2*_ngitems)*(sizes[1]-2*_ngitems)*
		(sizes[2]-2*_ngitems);
    default: COM_assertion(false); // Should never reach here.
    }
    return 0;
  }
}

Connectivity::Size Connectivity::size_of_real_nodes() const 
{ 
  if ( !is_structured())
    return _pane->size_of_real_nodes();
  else {
    const int *sizes=pointer();
    if ( sizes==NULL) return 0;
    switch ( _size_info[TYPE_ID]) {
    case ST1: return sizes[0]-2*_ngitems;
    case ST2: return (sizes[0]-2*_ngitems)*(sizes[1]-2*_ngitems);
    case ST3: return (sizes[0]-2*_ngitems)*(sizes[1]-2*_ngitems)*
		(sizes[2]-2*_ngitems);
    default: COM_assertion(false); // Should never reach here.
    }
    return 0;
  }
}

void Connectivity::set_pointer(void *p, int strd, int cap, bool is_const) 
  throw(COM_exception)
{
  if ( !is_structured()) {
    Attribute::set_pointer( p, strd, cap, 0, is_const);
  }
  else {
    int ndims = dimension();
    Attribute::set_size( ndims, _ngitems);
    _ptr = Attribute::allocate( 1, ndims, true);
    copy_array( p, strd, ndims);

    // Update number of nodes and elements
    COM_assertion( _parent==NULL);
    _pane->refresh_connectivity();
  }
}

const int* Connectivity::get_size_info( const std::string &aname) 
{
  int idx = 2;
  switch (aname[1]) {
  case 'b':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='2') 
    { COM_assertion( _sizes[BAR2][TYPE_ID]==BAR2); return _sizes[BAR2]; }
    if ( aname[idx]=='3') 
    { COM_assertion( _sizes[BAR3][TYPE_ID]==BAR3); return _sizes[BAR3]; }
    break;
  case 't':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='3') 
    { COM_assertion( _sizes[TRI3][TYPE_ID]==TRI3); return _sizes[TRI3]; }
    if ( aname[idx]=='6') 
    { COM_assertion( _sizes[TRI6][TYPE_ID]==TRI6); return _sizes[TRI6]; }
    break;
  case 'q':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='4') 
    { COM_assertion( _sizes[QUAD4][TYPE_ID]==QUAD4); return _sizes[QUAD4]; }
    if ( aname[idx]=='8') 
    { COM_assertion( _sizes[QUAD8][TYPE_ID]==QUAD8); return _sizes[QUAD8]; }
    if ( aname[idx]=='9') 
    { COM_assertion( _sizes[QUAD9][TYPE_ID]==QUAD9); return _sizes[QUAD9]; }
    break;
  case 'T':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='4') 
    { COM_assertion( _sizes[TET4][TYPE_ID]==TET4); return _sizes[TET4]; }
    if ( aname[idx]=='1' && aname[idx+1]=='0') 
    { COM_assertion( _sizes[TET10][TYPE_ID]==TET10); return _sizes[TET10]; }
    break;
  case 'H':
  case 'B':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='8') 
    { COM_assertion( _sizes[HEX8][TYPE_ID]==HEX8); return _sizes[HEX8]; }
    if ( aname[idx]=='2') {
      if ( aname[idx+1]=='0') 
      { COM_assertion( _sizes[HEX20][TYPE_ID]==HEX20); return _sizes[HEX20]; }
      if ( aname[idx+1]=='7') 
      { COM_assertion( _sizes[HEX27][TYPE_ID]==HEX27); return _sizes[HEX27]; }
    }
    break;
  case 'P':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='5') {
      COM_assertion( _sizes[PYRIMID5][TYPE_ID]==PYRIMID5);
      return _sizes[PYRIMID5];
    }
    if ( aname[idx]=='6') {
      COM_assertion( _sizes[PRISM6][TYPE_ID]==PRISM6);
      return _sizes[PRISM6];
    }

    if ( aname[idx]=='1') {
      if ( aname[idx+1] == '4') { 
	COM_assertion( _sizes[PYRIMID14][TYPE_ID]==PYRIMID14); 
	return _sizes[PYRIMID14]; 
      }

      if ( aname[idx+1] == '5') {
	COM_assertion( _sizes[PRISM15][TYPE_ID]==PRISM15); 
	return _sizes[PRISM15];
      }

      if ( aname[idx+1] == '8') {
	COM_assertion( _sizes[PRISM18][TYPE_ID]==PRISM18); 
	return _sizes[PRISM18];
      }
    }
    break;
  case 'W':
    while (aname[idx]=='0') idx++;
    if ( aname[idx]=='6') {
      COM_assertion( _sizes[PRISM6][TYPE_ID]==PRISM6);
      return _sizes[PRISM6];
    }

    if ( aname[idx]=='1') {
      if ( aname[idx+1] == '5') {
	COM_assertion( _sizes[PRISM15][TYPE_ID]==PRISM15); 
	return _sizes[PRISM15];
      }

      if ( aname[idx+1] == '8') {
	COM_assertion( _sizes[PRISM18][TYPE_ID]==PRISM18); 
	return _sizes[PRISM18];
      }
    }
    break;
  case 's':
    if ( aname[idx++]=='t') {
      while (aname[idx]=='0') idx++;
      if ( aname[idx]=='3') 
      { COM_assertion( _sizes[ST3][TYPE_ID]==ST3); return _sizes[ST3]; }
      
      if ( aname[idx]=='2') 
      { COM_assertion( _sizes[ST2][TYPE_ID]==ST2); return _sizes[ST2]; }
      
      if ( aname[idx]=='1') 
      { COM_assertion( _sizes[ST1][TYPE_ID]==ST1); return _sizes[ST1]; }
    }
    break;
  default: break;
  }

  return NULL;
}

COM_END_NAME_SPACE






