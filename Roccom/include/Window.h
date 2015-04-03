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
// $Id: Window.h,v 1.12 2008/12/06 08:43:24 mtcampbe Exp $

/** \file Window.h
 * Contains the prototypes for the Window object.
 * @see Window.C
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_WINDOW_H__
#define __ROCCOM_WINDOW_H__

#include "Function.h"
#include "Pane.h"
#include <map>

COM_BEGIN_NAME_SPACE

/** A Window object contains multiple panes and multiple data attributes.
 */
class Window {
  typedef std::map< std::string, Function>           Func_map;
  typedef std::map< std::string, Attribute*>         Attr_map;
  typedef std::map< int, Pane*>                      Pane_map;

  class Pane_friend : public Pane {
    explicit Pane_friend( Pane&);
  public:
    using Pane::new_attribute;
    using Pane::delete_attribute;
    using Pane::inherit;
    using Pane::set_size;
    using Pane::reinit_attr;
    using Pane::reinit_conn;
    using Pane::connectivity;
  };
  typedef Pane::OP_Init    OP_Init;

public:
  typedef std::map<int,int>                          Proc_map;

  // Used by get_array. Note that the default dimension is -1, which
  // is for void*. Nonnegative dimensions are reserved for Fortran pointers.
  struct Pointer_descriptor {
    explicit Pointer_descriptor( void *p, int d=-1) 
      : ptr(p), dim(d), n1(0), n2(0) {}
    void *at() { return ptr; }

    void *ptr;
    int  dim;
    int  n1, n2;
  };

  /** \name Constructor and destructor
   * \{
   */
  /** Create a window with a given name and MPI communicator.
   * \param name  name of the window
   * \param c     MPI communicator where the window resides
   */
  Window( const std::string &name, MPI_Comm c);

  /// Destructor.
  virtual ~Window();
  //\}

  /** \name Identity
   * \{
   */
  /// Obtain the window's name.
  const std::string &name() const { return _name; }

  /// Obtain the communicator of the window.
  MPI_Comm get_communicator() const { return _comm; }
  //\}

  /** \name Function and data management
   * \{
   */
  /// Initialize a Function record.
  void set_function( const std::string &fname, 
		     Func_ptr func,
		     const std::string &intents, 
		     const COM_Type *types, 
		     Attribute *a, 
		     bool if_f90=false) throw(COM_exception);

  /// Initialize a Function record.
  void set_function( const std::string &fname, 
		     Member_func_ptr func,
		     const std::string &intents, 
		     const COM_Type *types, 
		     Attribute *a, 
		     bool if_f90=false) throw(COM_exception);

  /** Create a new Attribute object with given properties.
   *  \param aname attribute name.
   *  \param loc   location ('w', 'p', 'n', or 'e').
   *  \param type  base data type.
   *  \param ncomp number of components.
   *  \param unit unit of the attribute.
   */
  Attribute *new_attribute( const std::string &aname, const char loc,
			    const int type, int ncomp,
			    const std::string &unit) throw(COM_exception);

  /** Delete an existing Attribute object. */
  void delete_attribute( const std::string &aname) throw(COM_exception);

  /** Set the sizes of an attribute for a specific pane.
   *  \param aname  attribute name
   *  \param pane_id pane ID
   *  \param nitems total number of items (including ghosts)
   *  \param ng     number of ghosts
   */
  void set_size( const std::string &aname, int pane_id,
		 int nitems, int ng=0) throw( COM_exception);

  /** Associate an array with an attribute for a specific pane.
   *  \param aname  attribute name
   *  \param pane_id  pane ID
   *  \param addr   address of the array
   *  \param strd   Stride between two items of each component
   *  \param cap    capacity of the array
   *  \seealso alloc_array, resize_array
   */
  void set_array( const std::string &aname, const int pane_id,
		  void *addr, int strd=0, int cap=0, bool is_const=false) 
    throw(COM_exception);

  /** Allocate memory for an attribute for a specific pane and 
   *  set addr to the address.
   *  \seealso alloc_array, resize_array, append_array
   */
  void alloc_array( const std::string &aname, const int pane_id,
		    void **addr, int strd=0, int cap=0) throw(COM_exception);

  /** Resize memory for an attribute for a specific pane and 
   *  set addr to the address.
   *  \seealso set_array, alloc_array, append_array
   */
  void resize_array( const std::string &aname, const int pane_id,
		     void **addr, int strd=-1, int cap=0) throw(COM_exception);

  void resize_array( Attribute *a, void **addr, 
		     int strd=-1, int cap=0) throw(COM_exception) 
  { reinit_attr( a, Pane::OP_RESIZE, addr, strd, cap); }

  void resize_array( Connectivity *c, void **addr, 
		     int strd=-1, int cap=0) throw(COM_exception) 
  { reinit_conn( c, Pane::OP_RESIZE, (int**)addr, strd, cap); }

  /** Append the given array to the end of the attribute on a specific
   *  pane, and reallocate memory for the attribute if necessary.
   *  \seealso set_array, alloc_array, resize_array
   */
  void append_array( const std::string &aname, const int pane_id,
		     const void *val, int v_strd, int v_size) throw(COM_exception);

  /** Deallocate memory for an attribute for a specific pane if 
   *  allocated by Roccom.
   *  \seealso alloc_array, resize_array
   */
  void dealloc_array( const std::string &aname,
		      const int pane_id=0) throw(COM_exception);

  void dealloc_array( Attribute *a) throw(COM_exception)
  { reinit_attr( a, Pane::OP_DEALLOC); }

  void dealloc_array( Connectivity *c) throw(COM_exception) 
  { reinit_conn( c, Pane::OP_DEALLOC); }

  /** Inherit the attributes of another window with a different name.
   *  Returns the corresponding value.
   *  \param from  attribute being copied from
   *  \param aname new name of the attribute
   *  \param cond  an integer pane-attribute
   *  \param val   value to be compared against cond
   *  \param inherit_mode mode of inheritance
   *  \param withghost wheather ghost nodes/elements should be ignored */
  Attribute *inherit( Attribute *from, const std::string &aname, 
		      int inherit_mode, bool withghost,
		      const Attribute *cond, int val) throw(COM_exception);

  /// Copy an attribute object onto another.
  void copy_attribute( const Attribute *from, 
		       Attribute *to) throw(COM_exception) {
    inherit( const_cast<Attribute*>(from), to->name(), 
	     Pane::INHERIT_COPY, true, NULL, 0);
  }
  
  /** Get the meta-information about an attribute.
   *  \param aname  attribute name
   *  \param l      location
   *  \param t      data type
   *  \param n      number of components
   *  \param u      unit
   */
  Attribute *get_attribute( const std::string &aname, char *l, int *t, 
			    int *n, std::string *u) const throw(COM_exception);
  
  /** Get the sizes of an attribute for a specific pane.
   *  \param aname  attribute name
   *  \param pane_id pane ID
   *  \param nitems total number of items (including ghosts)
   *  \param ng     number of ghosts
   */
  void get_size( const std::string &aname, int pane_id, 
		 int *nitems, int *ng) const throw( COM_exception);

  /** Get the status of an attribute or pane.
   *  \seealso Roccom_base::get_status()
   */
  int get_status( const std::string &aname, int pane_id) const 
    throw(COM_exception);

  /** Get the parent name of an attribute and load into name. 
   *  If the attribute has no parent, then name is empty.
   */
  void get_parent( const std::string &aname, int pane_id,
		   std::string &name) const throw(COM_exception);

  /** Get the address associated with an attribute for a specific pane.
   *  \param aname  attribute name
   *  \param pane_id    pane ID
   *  \param addr   address of the array
   *  \param strd   Stride between two items of each component
   *  \param cap    capacity of the array
   *  \seealso alloc_array, resize_array, copy_array
   */
  void get_array( const std::string &aname, const int pane_id, 
		  Pointer_descriptor &addr, 
		  int *strd=NULL, int *cap=NULL, bool is_const=false) 
    throw(COM_exception);

  /** Copy an attribute on a specific pane into a given array.
   *  \param aname   attribute name
   *  \param pane_id pane ID
   *  \param val     address of the user array
   *  \param v_strd  stride of user array. 0 (the default) indicates 
   *               number of components.
   *  \param v_size  number of items to be copied. 
   *               0 (the default) indicates number of items of the attribute.
   *  \param offset  starting item to be copied in the attribute.
   *  \seealso alloc_array, resize_array, get_array
   */
  void copy_array( const std::string &aname, const int pane_id, 
		   void *val, int v_strd=0, int v_size=0, 
		   int offset=0) const throw(COM_exception);

  /// Perform some final checking of the window.
  void init_done( bool pane_changed=true) throw(COM_exception);

  //\}

  /** \name Pane management
   * \{
   */
  /// Obtain the number of local panes in the window.
  int size_of_panes() const { return _pane_map.size(); }

  /// Obtain the total number of panes in the window on all processes.
  int size_of_panes_global() const { return _proc_map.size(); }

  /// Obtain the process rank that owns a given pane. Returns -1 if 
  /// the pane cannot be found in the process map.
  int owner_rank( const int pane_id) const;

  /// Return the last attribute id.
  int last_attribute_id() const { return _last_id; }

  /// Obtain the process map
  const Proc_map &proc_map() const { return _proc_map; }

  /// Remove the pane with given ID.
  void delete_pane( const int pane_id) throw(COM_exception) {
    if ( pane_id == 0) { // delete all panes
      _pane_map.clear();
    }
    else {
      Pane_map::iterator it = _pane_map.find( pane_id);
      if ( it == _pane_map.end()) throw COM_exception(COM_ERR_PANE_NOTEXIST);
      delete it->second;
      _pane_map.erase( it);
    }
  }

  //\}
 
  /** \name Miscellaneous
   *  \{
   */
  /// Find the pane with given ID. If not found, insert a pane with given ID.
  Pane &pane( const int pane_id, bool insert=false) throw(COM_exception);
  const Pane &pane( const int pane_id) const throw(COM_exception);

  /// Obtain all the local panes of the window.
  void panes( std::vector<int> &ps, int rank=-2);

  /// Obtain all the local panes of the window.
  void panes( std::vector<Pane*> &ps);
  /// Obtain all the local panes of the window.
  void panes( std::vector<const Pane*> &ps) const
  { const_cast<Window*>(this)->panes( (std::vector<Pane*> &)ps); }

  /// Obtain all the attributes of the pane.
  void attributes( std::vector<Attribute*> &as)
  { _dummy.attributes( as); }
  /// Obtain all the attributes of the pane.
  void attributes( std::vector< const Attribute*> &as) const
  { _dummy.attributes( as); }

  /// Obtain a pointer to the attribute metadata from its name.
  Attribute *attribute( const std::string &a) throw(COM_exception);
  const Attribute *attribute( const std::string &a) const throw(COM_exception)
  { return const_cast<Window*>(this)->attribute( a); }

  /// Obtain a pointer to the attribute metadata from its index.
  Attribute *attribute( int i) throw(COM_exception) 
  { return _dummy.attribute( i); }
  const Attribute *attribute( int i) const throw(COM_exception) 
  { return _dummy.attribute( i); }

  /// Obtain the function pointer from its name.
  Function *function( const std::string &f);
  const Function *function( const std::string &f) const
  { return const_cast<Window*>(this)->function( f); }
  //\}

protected:
  /** Implementation for setting (op==OP_SET or OP_SET_CONST), allocating 
   *   (op==OP_ALLOC), resizing (op==OP_RESIZE) and deallocating 
   *   (op==OP_DEALLOC) an array for a specific attribute.
   *  \param attr   attribute
   *  \param op     Operation (OP_SET, OP_SET_CONST, OP_ALLOC, OP_RESIZE)
   *  \param addr   address
   *  \param strd   stride
   *  \param cap    capacity
   */
  void reinit_attr( Attribute *attr, OP_Init op, void **addr=NULL, 
		    int strd=0, int cap=0) throw(COM_exception);

  /** Template implementation for setting (op==OP_SET or OP_SET_CONST), 
   *   allocating (op==OP_ALLOC), resizing (op==OP_RESIZE) and deallocating 
   *   (op==OP_DEALLOC) an array for a specific connectivity table.
   *  \param attr   connectivity table
   *  \param op     Operation (OP_SET, OP_SET_CONST, OP_ALLOC, OP_RESIZE)
   *  \param addr   address
   *  \param strd   stride
   *  \param cap    capacity
   */
  void reinit_conn( Connectivity *con, OP_Init op, int **addr=NULL, 
		    int strd=0, int cap=0) throw(COM_exception);

protected:
  Pane         _dummy;       ///< Dummy pane.
  std::string  _name;        ///< Name of the window.
  Attr_map     _attr_map;    ///< Map from attribute names to their metadata.
                             ///< It does not contain individual components.
  Func_map     _func_map;    ///< Map from function names to their metadata.
  Pane_map     _pane_map;    ///< Map from pane ID to their metadata.
  Proc_map     _proc_map;    ///< Map from pane ID to process ranks
 
  int          _last_id;     ///< The last used attribute index. The next
                             ///< available one is _last_id+1.
  MPI_Comm     _comm;        ///< the MPI communicator of the window.
  enum { STATUS_SHRUNK, STATUS_CHANGED, STATUS_NOCHANGE };
  int          _status;      ///< Status of the window.

private:
  // Disable the following two functions (they are dangerous)
  Window( const Window&);
  Window &operator=( const Window&);

private:
#ifdef DOXYGEN
  // This is to fool DOXYGEN to generate the correct collabration diagram
  Attribute   *_attr_map;
  Function    *_func_map;
  Pane        *_pane_map;
#endif /* DOXYGEN */

};

COM_END_NAME_SPACE

#endif






