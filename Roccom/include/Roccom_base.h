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
// $Id: Roccom_base.h,v 1.44 2008/12/06 08:43:24 mtcampbe Exp $

/** \file Roccom_base.h
 * Contains declaration of the base class for Roccom implementations.
 * @see Roccom_base.C
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_BASE_H__
#define __ROCCOM_BASE_H__

#include "roccom_devel.h"
#include "maps.h"
#include <set>

#ifdef __CHARMC__
#include "tcharm.h"
#endif

/// This file indirectly includes the following files:
/// iostream, map, string, vector, and roccom_basic.h

COM_BEGIN_NAME_SPACE

/** The base class for Roccom implementations. 
 */
class Roccom_base {
  typedef Roccom_map<Window*>                                    Window_map;
  typedef Roccom_map<std::pair<void *, std::set<std::string> > > Module_map;
  typedef Roccom_map<Attribute*>                                 Attribute_map;
public:
  typedef Window::Pointer_descriptor Pointer_descriptor;
    
  /** \name Initialization and Finalization
   * \{
   */
  /// Constructor. 
  Roccom_base( int *argc, char ***argv) throw( COM_exception, int);

  /// Destructor.
  ~Roccom_base();

  static void init( int *argc, char ***argv) throw(int);
  static void finalize() throw(int);
  static void abort( int ierr);

  /// Set the default communicator of Roccom. This communicator will be used
  /// as the default communicator for any new window.
  static void set_default_communicator( MPI_Comm comm) 
  { get_roccom()->_comm = comm; }

  /// Get the default communicator of Roccom. 
  static MPI_Comm get_default_communicator() 
  { return get_roccom()->_comm; }

  static void set_roccom( Roccom_base *);
  inline static Roccom_base *get_roccom();

  /// Checks whether Roccom has been initialized.
  static bool initialized() { return get_roccom()!=NULL; }
  //\}

  /** \name Module management
   *  \{
   */
  /// Load a module
  void load_module( const std::string &lname, 
		    const std::string &wname) throw(int);
  
  /// Unload a module
  void unload_module( const std::string &lname,
		      const std::string &wname, int dodl=1) throw(int);
  //\}

  /** \name Window and pane management
   * \{
   */
  /// Creates a window with given name.
  void new_window( const std::string &wname, 
		   MPI_Comm comm) throw(int);

  /// Deletes a window with given name.
  void delete_window( const std::string &wname) throw(int);
  
  /// Marks the end of the registration of a window.
  void window_init_done( const std::string &wname, 
			 bool panechanged = true) throw(int);
  
  /// Deletes a pane and its associated data.
  void delete_pane(const std::string &wname, const int pid) throw(int);

  //\}

  /** \name Attribute management
   * \{
   */
  /// Creates a new attribute for a window.
  void new_attribute( const std::string &wa, const char loc,
		      const int data_type, int size, 
		      const std::string &unit) throw(int);

  /// Delete an existing attribute from a window.
  void delete_attribute( const std::string &wa) throw(int);

  /// Set the sizes of an attribute. Note that for nodal or elemental data,
  /// setting sizes for one such attributes affects all other attributes.
  void set_size( const std::string &wa_str, 
		 int pane_id, int nitems, int ng=0) throw(int);

  /// Associates an object with a specific window.
  void set_object( const std::string &wa, const int pane_id,
		   void *obj_addr, void *casted_obj) throw(int);

  /// Associates an object with a specific window.
  void get_object( const std::string &wa, const int pane_id,
		   void **ptr) throw(int);

  /// Associates an array with an attribute for a specific pane.
  void set_array( const std::string &wa, const int pane_id,
		  void *addr, int strd=0, int cap=0, 
		  bool is_const=false) throw(int);

  template <class T>
  void set_bounds( const std::string &wa, const int pane_id,
		   T lbnd, T ubnd) throw(int)
  { set_bounds( wa, pane_id, (const void*)&lbnd, (const void*)&ubnd); }

  template <class T>
  void set_bounds( const std::string &wa, const int pane_id,
		   const T* lbnd, const T* ubnd) throw(int)
  { set_bounds( wa, pane_id, (const void*)&lbnd, (const void*)&ubnd); }

  void set_bounds( const std::string &wa, const int pane_id,
		   const void *lbnd, const void *ubnd) throw(int);

  void get_bounds( const std::string &wa, const int pane_id,
		   void *lbnd, void *ubnd) throw(int);

  int check_bounds( const std::string &wa, int pane_id) throw(int);

  /// Allocate space for an attribute on a specific pane and return the
  /// address by setting addr. Allocate for all panes if pane-id is 0,
  /// in which case, do not set addr.
  void allocate_array( const std::string &wa, const int pane_id=0,
		       void **addr=NULL, int strd=0, int cap=0) throw(int);

  /// Resize an attribute on a specific pane and return the
  /// address by setting addr. Resize for all panes if pane-id is 0,
  /// in which case, do not set addr. The difference between resize and
  /// allocate is that resize will reallocate memory only if the current
  /// array cannot accomodate the requested capacity.
  void resize_array( const std::string &wa, const int pane_id=0,
		     void **addr=NULL, int strd=-1, int cap=0) throw(int);

  /// Append an array to the end of the attribute on a specific pane and 
  /// return the new address by setting addr. 
  void append_array( const std::string &wa, const int pane_id,
		     const void *val, int v_strd, int v_size) throw(int);

  /// Use the subset of panes of another window 
  /// of which the given pane attribute has value val.
  void use_attribute( const std::string &wname, 
		      const std::string &pwname,
		      int withghost=1,
		      const char *cndname=NULL,
		      int val=0) throw(int);
  
  /// Clone the subset of panes of another window 
  /// of which the given pane attribute has value val.
  void clone_attribute( const std::string &wname, 
			const std::string &pwname,
			int withghost=1,
			const char *cndname=NULL,
			int val=0) throw(int);

  /// Copy an attribute onto another
  void copy_attribute( const std::string &wname, 
		       const std::string &pwname,
		       int withghost=1,
		       const char *cndname=NULL,
		       int val=0) throw(int);

  /// Copy an attribute onto another
  void copy_attribute( int trg_hdl, 
		       int src_hdl,
		       int withghost=1,
		       int ptn_hdl=0,
		       int val=0) throw(int);

  /// Deallocate space for an attribute in a pane, asuming the memory was
  /// allocated allocate_mesh or allocate_attribute.
  void deallocate_array( const std::string &wa,
			 const int pid=0) throw(int);

  /// Information retrieval
  /// Get the information about an attribute. The opposite of new_attribute.
  void get_attribute( const std::string &wa_str, char *loc, int *type, 
		      int *size, std::string *unit) throw(int);

  /// Get the sizes of an attribute. The opposite of set_size.
  void get_size( const std::string &wa_str, int pane_id, 
		 int *size, int *ng=0) throw(int);

  /** Get the status of an attribute. If the attribute name is empty, and pane
   *  ID is 0, then checks whether the window exist (return 0 if does and -1
   *  if not); if attribute name is empty and pane ID is >0, then check whether
   *  check whether the given pane exists (return 0 if so and -1 if not).
   *  Otherwise, it checks the status of an attribute and returns 
   *  one of the following values:
   *  -1: not exist.
   *   0:  not yet initialized
   *   1:  set by the user.
   *   2:  set by the user with const modifier.
   *   3:  inherited from (i.e., use) another attribute.
   *   4:  allocated by Roccom.
   */
  int get_status( const std::string &wa_str, int pane_id) throw(int);

  /// Get the address for an attribute on a specific pane.
  void get_array( const std::string &wa, const int pane_id,
		  void **addr, int *strd=NULL, 
		  int *cap=0, bool is_const=false) throw(int);

  /// Get the address for an attribute on a specific pane.
  void get_array( const std::string &wa, const int pane_id,
		  Pointer_descriptor &addr, int *strd=NULL, 
		  int *cap=0, bool is_const=false) throw(int);

  /// Copy an array from an attribute on a specific pane into a given buffer.
  void copy_array( const std::string &wa, const int pane_id,
		   void *val, int v_strd=0, int v_size=0, 
		   int offset=0) throw(int);

  void set_f90pointer( const std::string &waname, void *ptr,
		       Func_ptr f, long int l) throw(int);
  
  void get_f90pointer( const std::string &waname, void *ptr, 
		       Func_ptr f, long int l) throw(int);
  //\}

  /** \name Information retrieval
   * \{
   */
  MPI_Comm get_communicator( const std::string &wname) throw(int);

  /// Obtain a list of all window names
  void get_windows(std::vector<std::string> &) throw(int);

  /// Obtain a list of all module names
  void get_modules(std::vector<std::string> &) throw(int);

  /// Obtain the panes of a given window on a specific process. If rank is -2,
  /// then the current process is assumed. If rank is -1, then get the panes
  /// on all processes within the window's communicator.
  void get_panes( const std::string &wname, std::vector<int> &paneids_vec, 
		  int rank=-2, int **pane_ids=NULL) throw(int);


  /// Obtain the user-defined attributes of the given window.  
  void get_attributes( const std::string &wname, int *na,
		       std::string &str, char **names=NULL) throw(int);
  
  /// Obtain the connectivity tables of a pane of the given window.  
  void get_connectivities( const std::string &wname, int pane_id, int *nc, 
			   std::string &str, char **names=NULL) throw(int);

  /// Obtain the parent attribute's name of a given attribute on a given pane.
  /// If the attribute has no parent, then set name to empty.
  void get_parent( const std::string &waname, int pane_id,
		   std::string &str, char **name=NULL) throw(int);

  void free_buffer( int **buf);
  void free_buffer( char **buf);

  int get_window_handle( const std::string &wname) throw(int);
  Window *get_window_object( int hdl) throw(int);
  const Window *get_window_object( int hdl) const throw(int);

  Window *get_window_object( const std::string &wname) throw(int) 
  { return get_window_object( get_window_handle( wname)); }
  const Window *get_window_object( const std::string &wname) const throw(int)
  { return get_window_object( const_cast<Roccom_base*>(this)->
			      get_window_handle( wname)); }

  int get_attribute_handle( const std::string &waname) throw(int);
  int get_attribute_handle_const( const std::string &waname) throw(int);
  int get_function_handle( const std::string &wfname) throw(int);
  //\}

  /** \name Function management
   *  \{
   */
  /** Registers a function to the window.
   *  The names of the window and the function is give by wf in the format
   *  of "window.function" (null terminated). The address is given by ptr.
   *  The last two arguments specifies the number of the arguments,
   *  the intentions and types of the arguments.
   */
  void set_function( const std::string &wf, 
		     Func_ptr ptr, 
		     const std::string &intents,
		     const COM_Type *types, 
		     bool ff=false) throw(int);

  void set_member_function( const std::string &wf, 
			    Func_ptr ptr, 
			    const std::string &wa,
			    const std::string &intents,
			    const COM_Type *types, 
			    bool ff=false) throw(int);

  void set_member_function( const std::string &wf, 
			    Member_func_ptr ptr, 
			    const std::string &wa,
			    const std::string &intents,
			    const COM_Type *types, 
			    bool ff=false) throw(int);

  /// Get the number of arguments of a given function "window.function".
  int get_num_arguments( const std::string &wf) throw(COM_exception);

  /// Get the number of arguments of a given function from its handle
  int get_num_arguments( const int wf) throw(COM_exception);

  /** Invoke a function with given arguments.
   *  \param wf the handle to the function.
   *  \param count the number of input arguments.
   *  \param args the addresses to the arguments.
   *  \param lens the lengths of character strings.
   */
  void call_function( int wf, int count, void **args, 
		      const int *lens=NULL, 
		      bool from_c=true) throw(int);

  /** Nonblockingly invoke a function with given arguments.
   *  \param wf the handle to the function.
   *  \param count the number of input arguments.
   *  \param args the addresses to the arguments.
   *  \param reqid is set to the request of the current call.
   *  \param lens the lengths of character strings.
   */
  void icall_function( int wf, int count, void *args[], 
		       int *reqid, const int *lens=NULL) throw(int)
  { *reqid = 0; call_function( wf, count, args, lens); }

  /** Wait for the completion of a nonblocking call */
  void wait( int) {}
  /** Test whether a nonblocking call has finished. */
  int test( int) { return 1; }
  //\}

  /** \name Profiling and tracing tools
   *  \{
   */
  /// Determines whether verbose is on.
  int     get_verbose() const { return _verbose; }

  /// Determines whether verbose is on.
  int     get_debug() const { return _debug; }

  /// Changes the verbose setting.
  void    set_verbose( int v) { 
    _verbose = v; _verb1 = ( _verbose<=0) ? 0 :  (_verbose+1)%2+1;
  }

  /// Turns on debuging messages
  void    set_debug( bool debug) {
    _debug = debug;
  }

  void set_function_verbose( int i, int level) throw(int);

  /// This subroutine turns on (or off) profiling if i==1 (or ==0).
  /// It (re-)initializes all profiling info to 0.
  void set_profiling( int i); 
  void set_profiling_barrier( int hdl, MPI_Comm comm);
  void print_profile( const std::string &fname, const std::string &header);
  //\}

  /** \name Miscellaneous
   *  \{
   */
  /// Gets the size of the data type given by its index. \see DDT
  static int get_sizeof( COM_Type type, int count=1);

  /// Get the error code
  int get_error_code() const { return _errorcode; }

  void turn_on_exception()  { _exception_on = true; }
  void turn_off_exception() { _exception_on = false; }

  enum { FPTR_NONE=0, FPTR_INSERT=1, FPTR_APPEND=2};
  int f90ptr_treat() const { return _f90ptr_treat; }
  //\}

protected:
  template < class T>
  void set_member_function_helper( const std::string &wf, 
				   T ptr, 
				   const std::string &wa,
				   const std::string &intents,
				   const COM_Type *types, 
				   bool ff=false) throw(int);

  std::pair<int,int> get_f90pntoffsets( const Attribute *a);

  /** \name Window management
   * \{
   */
  /// Obtains a reference to the Window object from its name.
  Window &get_window( const std::string &wname) throw(COM_exception);

  /// Obtains a constant reference to the Window object from its name.
  const Window &get_window( const std::string &wname) const throw(COM_exception)
  { return const_cast<Roccom_base*>(this)->get_window(wname); }

  /// Obtains a reference to an attribute from its handle.
  Attribute &get_attribute( const int) throw(COM_exception);
  /// Obtains a const reference to an attribute from its handle.
  const Attribute &get_attribute( const int) const throw(COM_exception);

  /// Obtains a reference to an attribute from its handle.
  Function &get_function( const int) throw(COM_exception);
  /// Obtains a const reference to an attribute from its handle.
  const Function &get_function( const int) const throw(COM_exception);

  // \}

  /** \name Miscellaneous
   * \{
   */
  //====================================================
  // Some useful utilities for implementing a specific Roccom implementation
  //====================================================

  /// Extracts the window and attribute names from "window.attribute".
  /// Returns nonzero if fails.
  int split_name( const std::string &wa, std::string &wname, 
		  std::string &aname, bool tothrow=true) throw(COM_exception);

  void proc_exception( const COM_exception &, const std::string &) throw( int);
  //\}

protected:
  Roccom_base();  // Disable default constructor

protected: // Protected data members
  Module_map      _module_map;
  Window_map      _window_map;
  Attribute_map   _attr_map;
  Function_map    _func_map;

  std::string     _libdir;             ///< Library directory.
  std::vector<double> _timer;          ///< Timers for function calls
  int             _depth;              ///< Depth of procedure calls
  int             _verbose;            ///< Indicates whether verbose is on
  int             _verb1;              ///< Indicates whether to print detailed information
  bool            _debug;              ///< Indicated whether debug mode is on
  MPI_Comm        _comm;               ///< Default communicator of Roccom
  bool            _mpi_initialized;    ///< Indicates whether MPI was initialized by Roccom
  int             _errorcode;          ///< Error code
  bool            _exception_on;       ///< Indicates whether Roccom should throw exception
  bool            _profile_on;         ///< Indicates whether should profile

  int             _f90_mangling;       ///< Encoding name mangling.
                                       ///< -1: Unknown. 
                                       ///<  0: lower-case without appending
                                       ///<  1: upper-case without appending
                                       ///<  2: lower-case with appending 
                                       ///<  3: upper-case with appending
  int             _f90ptr_treat;       ///< Treatement of F90 pointers.
  int             _cppobj_casting;     ///< Treatement of C++ objects.
                                       ///< -1: Unknown
                                       ///< 0:  No casting to COM_Object
                                       ///< 1:  Casting to COM_Object

  static Roccom_base *roccom_base;
};

#ifndef __CHARMC__

/// Get a pointer to the Roccom object.
Roccom_base *Roccom_base::get_roccom() 
{ return roccom_base; }

#else

/** \name Global variable management/Thread-local variable management
 *  \{
 */
typedef enum {
  COMGLOB_FIRST=1,      /*First valid global variable ID*/
  COMGLOB_ROCCOM,       /*Index of Roccom's global data*/
  /*add new global variables before this line*/
  COMGLOB_LAST          /*Last global variable ID+1 (length of global table).*/
} COMGLOB_ID;

//extern "C" void *TCHARM_Get_global(int);
//extern "C" void TCHARM_Set_global( int, void *, void *);

/// Get a pointer to the Roccom object.
Roccom_base *Roccom_base::get_roccom() {
  extern bool use_tcharm_global;

  return ! use_tcharm_global ?
    roccom_base : (COM::Roccom_base *)TCHARM_Get_global(COMGLOB_ROCCOM);
}

//}

#endif // __CHARMC__

COM_END_NAME_SPACE

inline COM::Roccom_base *COM_get_roccom() 
{ COM::Roccom_base *roccom=COM::Roccom_base::get_roccom(); 
  COM_assertion_msg(roccom, 
		    "Roccom must be initialized before any Roccom calls."); 
  return roccom;
}

inline void COM_set_roccom( COM::Roccom_base *p) 
{ COM::Roccom_base::set_roccom(p); }

#endif // __ROCCOM_BASE_H__






