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
// $Id: Attribute.h,v 1.18 2008/12/06 08:43:24 mtcampbe Exp $

/** \file Attribute.h
 * Contains the prototype of Attriubte.
 * @see Attribute.C Pane.h Window.h
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_ATTRIBUTE_H__
#define __ROCCOM_ATTRIBUTE_H__

#include <string>
#include "roccom_exception.h"

COM_BEGIN_NAME_SPACE

class Pane;
class Window;

enum COM_Keywords { COM_NC, COM_NC1, COM_NC2, COM_NC3, COM_CONN, 
		    COM_MESH, COM_PCONN, COM_RIDGES, COM_RIDGES1, COM_RIDGES2,
		    COM_PMESH, COM_ATTS, COM_ALL, COM_NUM_KEYWORDS};

/** An Attribute object is a data member of a window. 
 *  It can be associated with a window, a pane, nodes, or elements.
 *  An attribute can be a vector of length size_of_items() with
 *  size_of_components() components. 
 */
class Attribute {
public:
  typedef unsigned char     Shorter_size; ///< One byte unsighed int
  typedef unsigned int      Size;         ///< Unsighed int

  /** \name Constructors and destructors
   *  \{
   */
  /// Default constructor.
  Attribute()
    : _pane(NULL), _parent(NULL), _id(-1), _loc(0), _ncomp(0), _type(0), 
      _nitems(-1), _ngitems(0), _gap(0), _status(STATUS_NOT_INITIALIZED), 
      _ptr(NULL), _strd(0), _nbytes_strd(0), _cap(0) {}
  
protected:
  /// Constructor for keywords. The default nitems for keywords is 0.
  Attribute( Pane *pane, int i) 
    : _pane(pane), _parent(NULL), _name( _keywords[i]), _id(i), 
      _loc(_keylocs[i]), _ncomp(_keysizes[i]), _type(_keytypes[i]), 
      _unit( i<=COM_NC3?"m":""), _nitems(0), _ngitems(0), _gap(0),
      _status(STATUS_NOT_INITIALIZED), _ptr(NULL), _strd(0), 
      _nbytes_strd(0), _cap(0) {}
public:  
  /** Create an attribute with name n in window w.
   *  \param pane pointer to its owner pane object.
   *  \param name attribute name.
   *  \param id attribute index.
   *  \param loc location ('w', 'p', 'n', or 'e').
   *  \param type base data type.
   *  \param ncomp number of components.
   *  \param unit unit of the attribute.
   */
  Attribute( Pane *pane, const std::string &name, int id,
	     Shorter_size loc, int type, const int ncomp, 
	     const std::string &unit)
    : _pane(pane), _parent(NULL), _name( name), _id(id), _loc(loc), 
      _ncomp(ncomp), _type(type), _unit(unit), _nitems(-1), _ngitems(0),
      _gap(0), _status(STATUS_NOT_INITIALIZED), _ptr(0), _strd(0), 
      _nbytes_strd(0), _cap(0) {}

  /** Inherit an attribute from another.
   *  \param pane pointer to its owner pane object.
   *  \param parent parent attribute (for supporting inheritance).
   *  \param name attribute name.
   *  \param id attribute index.
   */
  Attribute( Pane *pane, Attribute *parent, 
	     const std::string &name, int id);

  /// Destructors.
  ~Attribute() { 
    if (!_parent) deallocate(); 
    _pane=NULL; _parent=NULL; _id=-1; _loc=0; _ncomp=0; _type=0; 
    _nitems=-1, _ngitems=0; _status=STATUS_NOT_INITIALIZED; _ptr=NULL; 
    _strd=0; _nbytes_strd=0; _cap=0;
  }
  //\}

  /** \name Identity
   * \{
   */
  /// Obtain the name of the attribute.
  const std::string &name()  const { return _name; }

  /// Obtain the full name of the attribute including window name 
  /// suitable for printing out error messages.
  std::string fullname() const;

  /// Obtain the id (or index) of the attribute.
  int id()  const { return _id; }

  /// Parent attribute used by this object. It determines the
  /// the meta-data of the attribute. If the array of this attribute 
  /// is not set, then this attribute also uses the pointer of its parant.
  Attribute *parent()  { return _parent;  }
  const Attribute *parent() const { return _parent; }

  /// Root of use-inheritance. The basic properties of the attribute 
  /// (location, data type, number of components, and unit) are copied from
  /// the root during inheritance and hence later changes in the root 
  /// (by renewing the root) will not be reflected in sub-attribute. 
  /// Other types of information (sizes, pointer, stride, and capacity) 
  /// always use those of the root attribute.
  Attribute *root()  { 
    Attribute *a=this;
    while ( a->_parent) a=a->_parent;
    return a;
  }
  const Attribute *root() const { 
    const Attribute *a=this;
    while ( a->_parent) a=a->_parent;
    return a;
  }
  //\}

  /** \name Physical address
   * \{
   */
  /// Obtain a constant pointer to the physical address.
  const void *pointer()   const { return _status?_ptr:root()->_ptr; }
  /// Obtain a modifiable pointer to the physical address.
  void       *pointer() throw(COM_exception) { 
    if ( is_const()) throw COM_exception( COM_ERR_ATTRIBUTE_CONST);
    return _status?_ptr:root()->_ptr; 
  }

  /// Obtain the address of the jth component of the ith item, where
  /// 0<=i<size_of_items. This function is recursive and relatively expensive, 
  /// and hence should be used only for performance-insenstive tasks. 
  const void *get_addr( int i, int j=0) const throw(COM_exception);

  void *get_addr( int i, int j=0) throw(COM_exception) {
    if ( is_const()) throw COM_exception( COM_ERR_ATTRIBUTE_CONST);
    return (void*)(((const Attribute*)this)->get_addr(i,j));
  }
  //\}

  /** \name Access methods
   * \{
   */
  /// Obtain a constant pointer to the owner pane of the attribute.
  const Pane   *pane()  const { return _pane; }
  /// Obtain a modifiable pointer to the owner pane of the attribute.
  Pane         *pane()        { return _pane; }

  /// Obtain a constant pointer to the parent window of the attribute.
  const Window *window()  const;
  /// Obtain a modifiable pointer to the parent window of the attribute.
  Window       *window();

  /** Obtain the location of the attribute. 
   *  It is encoded as follows: 'w' for windowed attribute,
   *  'p' for panal attribute, 'n' for nodal attribute, and
   *  'e' for elemental attribute.
   */
  Shorter_size  location()  const { return _loc; }
  /// Checks whether the attribute is associated with the window.
  bool is_windowed()      const { return _loc == 'w'; }
  /// Checks whether the attribute is associated with a pane.
  bool is_panel()         const { return _loc == 'p'; }
  /// Checks whether the attribute is associated with an element.
  bool is_elemental()     const { return _loc == 'e'; }
  /// Checks whether the attribute is associated with a node.
  bool is_nodal()         const { return _loc == 'n'; }

  /// Obtain the data type of each component of the attribute.
  COM_Type    data_type() const { return _type; }

  /// Obtain the unit of the attribute.
  const std::string &unit() const  { return _unit; }
  
  /// Obtain the number of components in the attribute.
  int         size_of_components() const { return _ncomp; }

  /// Obtain the number of items in the attribute.
  int         size_of_items() const;

  /// Obtain the maximum allowed number of items in the attribute. 
  /// Reserved for Roccom3.1
  int         maxsize_of_items() const;

  /// Obtain the number of ghost items in the attribute.
  int         size_of_ghost_items() const;

  /// Obtain the maximum allowed number of items in the attribute. 
  /// Reserved for Roccom3.1
  int         maxsize_of_ghost_items() const;

  /// Obtain the number of real items in the attribute.
  int        size_of_real_items() const;

  /// Obtain the maximum allowed number of real items in the attribute. 
  /// Reserved for Roccom3.1
  int         maxsize_of_real_items() const;

  /// Check whether the number of items of the attribute is zero.
  bool        empty()     const  { return root()->_nitems<= 0; }

  /// Obtain the capacity of the array.
  int         capacity() const { return _status?_cap:root()->_cap; }

  /// Obtain the stride of the attribute in base datatype.
  int         stride()    const  { return _status?_strd:root()->_strd; }

  /// Obtain the stride of the attribute in bytes.
  int         stride_in_bytes() const 
  { return _status?_nbytes_strd:root()->_nbytes_strd; }

  /// Obtain the status of the attribute.
  int status() const 
  { if ( _parent) return STATUS_USE; else return _status; }

  /// Returns whether the array for the attribute has been set or allocated.
  bool initialized() const { return _status||root()->_status; }

  /// Returns whether the size for the attribute has been set.
  bool size_set() const;

  /// Returns whether the array for the attribute has been set or allocated.
  bool allocated() const { return _status==STATUS_ALLOCATED; }

  /// Returns whether the array is set to be read-only.
  bool is_const() const { return root()->_status==STATUS_SET_CONST; }

  /// Check how the attribute values are organized. 
  /// It returns true if the components of the attribute associated with
  //  a pane/node/element are not stored in consecutive memory space.
  bool   is_staggered()   const { return stride()!=size_of_components(); }

  static int get_sizeof( COM_Type type, int count=1);

  static bool compatible_types( COM_Type t1, COM_Type t2);

  static bool is_digit( char c) { return c>='0' && c<='9'; }
  //\}

  /// Set the size of items and ghost items. Can be changed only if the 
  /// attribute is a root.
  void set_size( int nitems, int ngitems=0) throw(COM_exception);

  /// Allocate memory for the attribute. 
  /// The attribute must be a root if the attribute is to be allocated.
  /// If from is not NULL, copy its value to the newly allocated array.
  void *allocate( int strd, int cap, bool force) throw(COM_exception);

  /// Deallocate memory if it was allocated by allocate(). 
  /// Return 0 if deallocation is successful.
  int deallocate() throw(COM_exception);

  enum Copy_dir { COPY_IN, COPY_OUT };
  // Copy n _ncomp-vectors from "buf" to the array if direction is COPY_IN
  // or copy to "buf" if direction is COPY_OUT.
  void copy_array( void *buf, int strd, int nitem,
		   int offset=0, int direction=COPY_IN) throw(COM_exception);

  // Append n _ncomp-vectors from "from" to the array.
  void append_array( const void *from, int strd, int nitem) throw(COM_exception);

protected:
  /// Set the physical address of the attribute values.
  void set_pointer( void *p, int strd, int cap, 
		    int offset, bool is_const) throw(COM_exception);
    
  /// Inherit from parent. If depth>0, then the procedure is for the subcomponents.
  void inherit( Attribute *a, bool clone, bool withghost, int depth=0) throw(COM_exception);

protected:
  Pane        *_pane;       ///< Pointer to its owner pane.
  Attribute   *_parent;     ///< Parent attribute being used.

  std::string  _name;       ///< Name of the attribute.
  int          _id;         ///< Id field data.
  Shorter_size _loc;        ///< Location. 'w' for windowed attribute,
                            ///< 'p' for panal attribute,
                            ///< 'n' for nodal attribute, 
                            ///< 'e' for elemental attribute

  int          _ncomp;      ///< Number of components
  COM_Type     _type;       ///< Base data type of the attribute.
  std::string  _unit;       ///< Unit of the attribute.

  int          _nitems;     ///< Size of total items. Default value is -1.
  int          _ngitems;    ///< Size of ghost items
  int          _gap;        ///< Gap between the IDs of real and ghost items.

  enum { STATUS_NOT_INITIALIZED=0, STATUS_SET=1, STATUS_SET_CONST=2,
	 STATUS_USE=3, STATUS_ALLOCATED=4 };
  Shorter_size _status;     ///< Indicating whether it has been initialized

  void        *_ptr;          ///< Physical address of the attribute.
  int          _strd;         ///< Stride
  int          _nbytes_strd;  ///< Number of bytes of the stride 
  int          _cap;          ///< Capacity

  static const char     *_keywords[COM_NUM_KEYWORDS]; ///< List of keywords
  static const char      _keylocs[COM_NUM_KEYWORDS];  ///< Default locations
  static const COM_Type  _keytypes[COM_NUM_KEYWORDS]; ///< Default data types
  static const int       _keysizes[COM_NUM_KEYWORDS]; ///< Default sizes

private:
  /// Disable copy constructor and copy operator.
  Attribute( const Attribute &a);
  Attribute &operator=( const Attribute&);
};

COM_END_NAME_SPACE

#endif






