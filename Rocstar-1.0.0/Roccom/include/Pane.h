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
// $Id: Pane.h,v 1.8 2008/12/06 08:43:24 mtcampbe Exp $

/** \file Pane.h
 * Contains the prototypes for the Pane object.
 * @see Pane.C Attribute.h Connectivity.h Window.h
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_PANE_H__
#define __ROCCOM_PANE_H__

#include <string>
#include <vector>
#include "Connectivity.h"

COM_BEGIN_NAME_SPACE

/** A Pane object contains a mesh, pane attribute, and field variables. 
 *  Mesh data include nodal coordinates and element connectivity.
 */
class Pane {
public:
  typedef std::vector<Attribute*>      Attr_set; ///< Vector of attributs.
  typedef std::vector<Connectivity*>   Cnct_set; ///< Vector of connectivities.
  typedef unsigned int                 Size;     ///< Unsighed int.
  enum OP_Init { OP_SET=1, OP_SET_CONST, OP_ALLOC, OP_RESIZE, OP_DEALLOC};
  enum Inherit_Modes { INHERIT_USE=0, INHERIT_CLONE, INHERIT_COPY };

  class Attribute_friend : public Attribute {
    explicit Attribute_friend( Attribute&);
  public:
    Attribute_friend( Pane *p, int i) : Attribute(p, i) {}
    using Attribute::set_pointer;
    using Attribute::inherit;
  };

  class Connectivity_friend : public Connectivity {
    explicit Connectivity_friend( Connectivity&);
  public:
    using Connectivity::set_pointer;
    using Connectivity::set_offset;
    using Connectivity::inherit;
  };

  /** \name Constructor and destructor
   *  \{
   */
  /// Create a pane in window w with ID i.
  Pane(Window *w, int i);

  /// Create a pane by copying from attribute descriptions from another pane.
  Pane(Pane *p, int i);

  /// Default destructor.
  virtual ~Pane();
  //\}

  /** \name Initialization
   *  \{
   */
  /// Finalize the initialization of a pane.
  void init_done() throw(COM_exception);
  //\}

  /** \name Identification
   *  \{
   */
  /// Obtain a constant pointer to its owner window object.
  const Window *window()  const { return _window; }
  /// Obtain a pointer to its owner window object.
  Window       *window()        { return _window; }

  /// Get the ID of the pane.
  int     id() const      { return _id; }

  /// Dimension of the pane.
  int  dimension() const 
  { return _cnct_set.empty()?0:_cnct_set[0]->dimension(); }

  /// Is mesh of the pane unstructured?
  bool is_unstructured() const 
  { return _cnct_set.size()!=1 || !_cnct_set[0]->is_structured(); }

  /// Does the pane contain more than one type of elements?
  bool is_mixed()   const { return _cnct_set.size()>1; }
  
  /// Is mesh of the pane structured?
  bool is_structured()   const 
  { return _cnct_set.size()==1 && _cnct_set[0]->is_structured(); }
  //\}

  /** \name Mesh management
   * \{
   */
  /// Get the total number of nodes in the pane (including ghost nodes).
  Size size_of_nodes() const 
  { return _attr_set[COM_NC]->size_of_items(); }

  /// Get the maximum number of real nodes in the pane (excluding ghost nodes).
  Size maxsize_of_nodes() const 
  { return _attr_set[COM_NC]->maxsize_of_items(); }

  /// Get the number of ghost nodes
  Size size_of_ghost_nodes() const 
  { return _attr_set[COM_NC]->size_of_ghost_items(); }

  /// Get the maximum number of real nodes in the pane (excluding ghost nodes).
  Size maxsize_of_ghost_nodes() const 
  { return _attr_set[COM_NC]->maxsize_of_ghost_items(); }

  /// Get the number of real nodes in the pane (excluding ghost nodes).
  Size size_of_real_nodes() const 
  { return _attr_set[COM_NC]->size_of_real_items(); }

  /// Get the maximum number of real nodes in the pane (excluding ghost nodes).
  Size maxsize_of_real_nodes() const 
  { return _attr_set[COM_NC]->maxsize_of_real_items(); }

  /// Get the total number of elements in the pane (including ghost elements).
  Size size_of_elements() const 
  { return _attr_set[COM_CONN]->size_of_items(); }

  /// Get the maximum number of elements allowed in the pane (including ghost elements)
  Size maxsize_of_elements() const 
  { return _attr_set[COM_CONN]->maxsize_of_items(); }

  /// Get the total number of ghost elements
  Size size_of_ghost_elements() const 
  { return _attr_set[COM_CONN]->size_of_ghost_items(); }

  /// Get the maximum number of elements allowed in the pane (including ghost elements)
  Size maxsize_of_ghost_elements() const 
  { return _attr_set[COM_CONN]->maxsize_of_ghost_items(); }

  /// Get the number of real elements in the pane (excluding ghost elements).
  Size size_of_real_elements() const 
  { return _attr_set[COM_CONN]->size_of_real_items(); }

  /// Get the maximum number of real elements allowed in the pane (excluding ghost elements).
  Size maxsize_of_real_elements() const 
  { return _attr_set[COM_CONN]->maxsize_of_real_items(); }

  /// Get the pointer to the values of the coordinates.
  /// It returns NULL if the coordinates are staggered.
  double *coordinates()   { return (double *)_attr_set[COM_NC]->pointer(); }
  /// Get the pointer to the values of the x-coordinates, 
  ///     if the coordinates are staggered.
  double *x_coordinates() { return (double *)_attr_set[COM_NC1]->pointer(); }
  /// Get the pointer to the values of the y-coordinates, 
  ///     if the coordinates are staggered.
  double *y_coordinates() { return (double *)_attr_set[COM_NC2]->pointer(); }
  /// Get the pointer to the values of the z-coordinates, 
  ///     if the coordinates are staggered.
  double *z_coordinates() { return (double *)_attr_set[COM_NC3]->pointer(); }

  /// Get a constant pointer to the values of the coordinates.
  const double *coordinates()   const 
  { return (const double *)_attr_set[COM_NC]->pointer(); }
  /// Get a constant pointer to the values of the x-coordinates, 
  ///     if the coordinates are staggered.
  const double *x_coordinates() const
  { return (const double *)_attr_set[COM_NC1]->pointer(); }
  /// Get a constant pointer to the values of the y-coordinates, 
  ///     if the coordinates are staggered.
  const double *y_coordinates() const
  { return (const double *)_attr_set[COM_NC2]->pointer(); }
  /// Get a constant pointer to the values of the z-coordinates, 
  ///     if the coordinates are staggered.
  const double *z_coordinates() const
  { return (const double *)_attr_set[COM_NC3]->pointer(); }

  /// Get a pointer to the values of the pane connectivity.
  int *pane_connectivity() 
  { return (int*)_attr_set[COM_PCONN]->pointer(); }

  /// Get a const pointer to the values of the pane connectivity.
  const int *pane_connectivity() const
  { return (const int *)_attr_set[COM_PCONN]->pointer(); }

  bool ignore_ghost() const { return _ignore_ghost; }

  void set_ignore_ghost(bool ignore){ _ignore_ghost = ignore; }
  //\}

  /** \name Structured meshes
   * \{
   */
  /// Dimension of the pane.
  /// Get the number of ghost layers for structured mesh
  Size size_of_ghost_layers() const 
  { return _cnct_set.empty()?0:_cnct_set[0]->size_of_ghost_items(); }

  /// Get the number of nodes in i-dimension if the mesh is structured.
  Size size_i() const 
  { return !is_structured()?0:_cnct_set[0]->size_i(); }
  /// Get the number of nodes in j-dimension if the mesh is structured.
  Size size_j() const 
  { return !is_structured()?0:_cnct_set[0]->size_j(); }
  /// Get the number of nodes in k-dimension if the mesh is structured.
  Size size_k() const 
  { return !is_structured()?0:_cnct_set[0]->size_k(); }
  //\}

  /** \name Attribute management
   * \{
   */
  /// Obtain all the attributes of the pane.
  void attributes( std::vector<Attribute*> &as);

  /// Obtain all the attributes of the pane.
  void attributes( std::vector< const Attribute*> &as) const
  { const_cast<Pane*>(this)->attributes((std::vector<Attribute*>&)as); }

  /// Obtain the attribute from given name.
  Attribute *attribute( const std::string &a); // Locate by name.
  /// Obtain the attribute from given name.
  const Attribute *attribute( const std::string &a) const
  { return const_cast<Pane*>(this)->attribute(a); }

  /// Obtain the attribute from its ID.
  Attribute *attribute( int i);

  /// Obtain the attribute from its ID.
  const Attribute *attribute( int i) const
  { return const_cast<Pane*>(this)->attribute(i); }
  //\}

  /** \name Connectivity management
   * \{
   */
  /// Obtain all the element connectivities of the pane.
  void connectivities( std::vector<Connectivity*> &es) 
  { es = _cnct_set; }

  void connectivities( std::vector<const Connectivity*> &es) const 
  { const_cast<Pane*>(this)->connectivities
      ( (std::vector<Connectivity*> &)es); }

  /// Obtain the connectivity table containing the element with the given ID.
  Connectivity *connectivity( Size i) throw(COM_exception);
  const Connectivity *connectivity( Size i) const throw(COM_exception)
  { return const_cast<Pane*>(this)->connectivity( i); }

  /// Obtain all the element connectivities of the pane.
  /// Kept for backward compatibility
  void elements( std::vector<Connectivity*> &es) { connectivities(es); }
  void elements( std::vector< const Connectivity*> &es) const
  { connectivities(es); }

  /// Update offsets and sizes of connectivity of an unstructured mesh.
  void refresh_connectivity() throw(COM_exception);
  //\}

protected: // The following functions to be called by the Window class.
  // Add a new attribute into the pane.
  Attribute *new_attribute( const std::string &aname, int aid, 
			    const char loc, const int type, int ncomp, 
			    const std::string &unit) throw(COM_exception);

  /** Insert an attribute onto the pane */
  void insert( Attribute *attr) throw(COM_exception);

  /// Delete an existing attribute with given id. 
  void delete_attribute( int id) throw (COM_exception);

  void reinit_attr( int aid, OP_Init op, void **addr, 
		    int strd, int cap) throw(COM_exception);

  /// Obtain the connectivity with the given name.
  const Connectivity *connectivity( const std::string &a) 
    const throw(COM_exception)
  { return const_cast<Pane*>(this)->connectivity( a); }

  Connectivity *connectivity( const std::string &a, 
			      bool insert=false) throw(COM_exception);

  void reinit_conn( Connectivity *con, OP_Init op, int **addr, 
		    int strd, int cap) throw (COM_exception);

  /// Inherit an attribute from another pane onto the current pane:
  Attribute *inherit( Attribute *from, const std::string &aname,
		      int mode, bool withghost) throw(COM_exception);

  /// Set the size of an attribute 
  void set_size( Attribute *a, int nitems, int ng) throw( COM_exception);

  /// Set the size of a connectivity table. 
  void set_size( Connectivity *con, int nitems, int ng) throw( COM_exception);

protected:
  Window*     _window;     ///< Point to the parent window.
  int         _id;         ///< Pane id
  Attr_set    _attr_set;   ///< Set of attributes
  Cnct_set    _cnct_set;   ///< Set of element connectivity
  bool        _ignore_ghost;  ///< Whether the ghosts were ignored

private:
#ifdef DOXYGEN
  // This is to fool DOXYGEN to generate the correct collabration diagram
  Attribute    *_attr_set;
  Connectivity *_cnct_set;
#endif
  // Disable the following two functions (they are dangerous)
  Pane( const Pane&);
  Pane &operator=( const Pane&);
};

COM_END_NAME_SPACE

#endif






