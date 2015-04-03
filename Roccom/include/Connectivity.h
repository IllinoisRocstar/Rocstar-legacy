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
// $Id: Connectivity.h,v 1.17 2008/12/06 08:43:24 mtcampbe Exp $

/** \file Connectivity.h
 * Contains the prototype of Connectivity.
 * @see Attribute.h Connectivity.C
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_CONNECTIVITY_H__
#define __ROCCOM_CONNECTIVITY_H__

#include "Attribute.h"

COM_BEGIN_NAME_SPACE

/** Encapsulates an element-connectivity of a mesh. It supports both
 *  structured and unstructured meshes.
 */
class Connectivity : protected Attribute {
public:
  enum Connectivity_type 
    { ST1, ST2, ST3, /* structured mesh */
      BAR2, BAR3,    /* 1d unstructured mesh */
      TRI3, TRI6, QUAD4, QUAD8, QUAD9, /* 2d unstructured mesh */
      TET4, TET10, PYRIMID5, PYRIMID14, PRISM6, PRISM15, PRISM18,
      HEX8, HEX20, HEX27, /* 3d unstructured mesh */ 
      TYPE_MAX_CONN};
  enum Connectivity_info 
    { TYPE_ID, SIZE_DIM, ORDER, SIZE_NNODES, SIZE_NCORN, 
      SIZE_NEDGES, SIZE_NFACES, SIZE_MAX_CONN};

  using Attribute::Shorter_size;
  using Attribute::Size;
  using Attribute::name;
  using Attribute::fullname;
  using Attribute::id;
  using Attribute::location;
  using Attribute::deallocate;
  using Attribute::pane;
  using Attribute::window;
  using Attribute::data_type;
  using Attribute::size_of_components;
  using Attribute::size_of_items;
  using Attribute::size_of_real_items;
  using Attribute::size_of_ghost_items;
  using Attribute::maxsize_of_items;
  using Attribute::maxsize_of_real_items;
  using Attribute::maxsize_of_ghost_items;
  using Attribute::capacity;
  using Attribute::stride;
  using Attribute::empty;
  using Attribute::status;
  using Attribute::initialized;
  using Attribute::is_staggered;
  using Attribute::copy_array;

  /** \name Constructors and destructor
   * \{
   */
  /** Create an attribute with name n in window w.
   *  \param pane pointer to its owner pane object.
   *  \param parent parent connectivity (for supporting inheritance).
   *  \param name connectivity name.
   *  \param id connectivity ID (always negative).
   *  \param sizes size information about the element type.
   *  \param type base data type.
   */
  Connectivity( Pane *pane, const std::string &name, int id, 
		const int sizes[], int type=COM_INT)
    : Attribute( pane, name, id, 'p', type, 
		 /* for structured meshes, ncomp is 1 */
		 (sizes[TYPE_ID]<=ST3)?1:sizes[SIZE_NNODES], ""), 
      _offset(0), _size_info(sizes) {}

  /// Construct from another connectivity table.
  Connectivity( Pane *pane, Connectivity *con, 
		const std::string &name, int id)
    : Attribute( pane, (Attribute*)con, name, id),
      _offset(con->_offset), _size_info( con->_size_info) {}
  //\}

  /** \name Identity
   * \{
   */
  /// Parent attribute being used.
  Connectivity *parent()  { return (Connectivity*)_parent;  }
  const Connectivity *parent() const { return (const Connectivity*)_parent; }

  /// Root of use-inheritance.
  Connectivity *root()  
  { return (Connectivity*)(Attribute::root()); }
  const Connectivity *root() const 
  { return (const Connectivity*)(Attribute::root()); }

  /// Inherit a connectivity table.
  void inherit( Connectivity *parent, bool clone, bool withghost) throw(COM_exception) 
  { Attribute::inherit( parent, clone, withghost); }

  /// Obtain element type ID.
  int element_type()  const { return _size_info[TYPE_ID]; }

  /// Get the dimension of the mesh.
  int dimension() const { return _size_info[SIZE_DIM]; }

  /// Determine whether the mesh is quadratic
  bool is_structured() const 
  { return _size_info[TYPE_ID] <= ST3; }

  /// Determine whether the element type is quadratic
  bool is_quadratic() const { return _size_info[ORDER] == 2; }
  //\}

  /** \name Size information
   * \{
   */
  /// Get the number of corners per element of the current connectivity table.
  Size size_of_corners_pe() const 
  { return _size_info[SIZE_NCORN]; }

  /// Get the number of corners per element of the given type of element.
  static Size size_of_corners_pe( int type) 
  { return _sizes[type][SIZE_NCORN]; } 

  /// Get the number of nodes per element of the current connectivity table.
  Size size_of_nodes_pe() const 
  { return _size_info[SIZE_NNODES]; }

  /// Get the number of nodes per element of the given type of element.
  static Size size_of_nodes_pe( int type) 
  { return _sizes[type][SIZE_NNODES]; }

  /// Get the number of edges per element of the current connectivity table.
  Size size_of_edges_pe() const 
  { return _size_info[SIZE_NEDGES]; }

  /// Get the number of edges per element of the given type of element.
  static Size size_of_edges_pe(int type)  
  { return _sizes[type][SIZE_NEDGES]; }

  /// Get the number of faces per element of the current connectivity table.
  Size size_of_faces_pe() const 
  { return _size_info[SIZE_NFACES]; }
  
  /// Get the number of faces per element of the given type of element.
  static Size size_of_faces_pe( int type) 
  { return _sizes[type][SIZE_NFACES]; }
  
  /// Get the total number of elements (including ghost elements) in the table.
  Size  size_of_elements() const;
  
  /// Get the number of ghost elements.
  Size  size_of_ghost_elements() const;

  /// Get the number of real elements.
  Size  size_of_real_elements() const;

  /// Get the total number of nodes (including ghost nodes) of the owner pane.
  Size  size_of_nodes() const;

  /// Get the number of ghost nodes of the owner pane.
  Size  size_of_ghost_nodes() const;

  /// Get the number of real nodes of the owner pane.
  Size  size_of_real_nodes() const;

  /// Get the index of the first element.
  Size  index_offset()  const { return root()->_offset; }

  /// Get the number of nodes in i-dimension if the mesh is structured.
  Size size_i() const 
  { return pointer()[0]; }
  /// Get the number of nodes in j-dimension if the mesh is structured.
  Size size_j() const 
  { return dimension()>1?pointer()[1]:1; }
  /// Get the number of nodes in k-dimension if the mesh is structured.
  Size size_k() const 
  { return dimension()>2?pointer()[2]:1; }
  //\}

  /** \name Array information
   * \{
   */
  /// Get a constant pointer to the connectivity array.
  const int *pointer()  const { return (const int*)Attribute::pointer(); }

  /// Get a pointer to the connectivity array.
  int *pointer()  { return (int*)Attribute::pointer(); }

  /// Obtain the address of the jth component of the ith item, where
  /// 0<=i<size_of_items. This function is recursive and relatively expensive, 
  /// and hence should be used only for performance-insenstive tasks. 
  const int *get_addr( int i, int j=0) const throw(COM_exception);

  int *get_addr( int i, int j=0) throw(COM_exception) {
    if ( is_const()) throw COM_exception( COM_ERR_ATTRIBUTE_CONST);
    return (int*)(((const Connectivity*)this)->get_addr(i,j));
  }
  //\}

  /// Returns whether the array is set to be read-only.
  bool is_const() const { return is_structured() || Attribute::is_const(); }

  /** \name Helpers
   * \{
   */
  static bool is_element_name( const std::string &aname) {
    return aname[0] == ':';
  }

  /// Obtain the size info of pre-defined connectivity
  static const int*get_size_info( const std::string &aname);

  /// Allocate memory for unstructured mesh
  void *allocate( int strd, int cap, bool force) throw(COM_exception) {
    if ( !is_structured()) return Attribute::allocate( strd, cap, force);
    else throw COM_exception(COM_ERR_ALLOC_STRUCTURED);
  }

  /// Set the size of items and ghost items. Can be changed only if the 
  /// attribute is a root.
  void set_size( int nitems, int ngitems=0) throw(COM_exception);
  //\}

protected:
  /// Set pointer of connectivity table 
  void set_pointer( void *p, int strd, int cap, 
		    bool is_const) throw(COM_exception);

  /// Set the index of the first element.
  void set_offset( Size offset) throw(COM_exception);

protected:
  int   _offset;     ///< Offset of the first element.
  const int *_size_info;
  // Tables of pre-defined connectivity types
  static const int _sizes[TYPE_MAX_CONN][SIZE_MAX_CONN];
};

COM_END_NAME_SPACE

#endif






