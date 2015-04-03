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
// $Id: Roccom_base.C,v 1.93 2008/12/06 08:43:25 mtcampbe Exp $

/** \file Roccom_base.C
 * Contains the base implementation of Roccom.
 * The more advanced implementations should reimplement the following: 
 *     window_init_done, delete_window, delete_pane, 
 *     and call_function, icall_function.
 * @see Roccom_base.h, Window.h
 */
/* Author: Xiangmin Jiao */

#include <iostream>
#include <sys/time.h>
#include <algorithm>
#ifndef STATIC_LINK 
#include <dlfcn.h>
#endif
#include <cctype>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <sstream>
#include "Roccom_base.h"
#include "commpi.h"

COM_BEGIN_NAME_SPACE

Roccom_base *Roccom_base::roccom_base=NULL;

#ifndef __CHARMC__

/// Set the Roccom pointer to the given object.
/// It was introduced to support processes.
void Roccom_base::set_roccom(Roccom_base *r)  { roccom_base = r; }

#else
bool use_tcharm_global = false;

void Roccom_base::set_roccom(Roccom_base *r) { 
  if ( COMMPI_Initialized() && COMMPI_Comm_size(MPI_COMM_WORLD)>1) {
    use_tcharm_global = true;
    TCHARM_Set_global(COMGLOB_ROCCOM, r, NULL);
  }
  else 
    roccom_base = r;
}

//}

#endif // __CHARMC__



#ifndef DOXYGEN_SHOULD_SKIP_THIS
/// Remove an argument from the argument list.
inline static void remove_arg( int *argc, char ***argv, int i) {
  for ( int j=i; j<*argc-1; ++j) (*argv)[j]=(*argv)[j+1];
  --*argc;
}
#endif

Roccom_base::Roccom_base( int *argc, char ***argv) throw( COM_exception, int)
  : _depth(0), _verbose(0), _verb1(0), _comm(MPI_COMM_WORLD),
    _mpi_initialized(false), _errorcode(0), _exception_on(true), _profile_on(0)
{
  _attr_map.add_object("",NULL);
  _func_map.add_object("",NULL);
  _errorcode = 0;

#ifdef PREFIX
  _libdir = std::string(PREFIX);
#endif

  const char *comhome = std::getenv( "ROCCOM_HOME");
  if ( comhome) _libdir = comhome;
  else {
    comhome = std::getenv( "ROCSTAR_HOME");
    if ( comhome) _libdir = comhome;
  }

  if ( !_libdir.empty()) _libdir.append( "/lib/");

  // Parse command-line options
  std::map<int,int> verb_maps; // Process-specific verbose level
  int i=1;

  while ( argc && i<*argc) {
    if ( std::strncmp((*argv)[i], "-com-v", 6) == 0) {
      // Check whether a number follows -com-v
      const char *s=(*argv)[i]+6;
      int rank = -1; // Default are all processes
      if ( s[0] != '\0') rank = std::atoi( s);

      int verb=1;
      if ( *argc>i+1 && (*argv)[i+1][0]>='0' && (*argv)[i+1][0]<='9')
      { verb = std::atoi( (*argv)[i+1]); remove_arg( argc, argv, i+1); }

      if ( rank == -1) set_verbose(verb); 
      else verb_maps[rank] = verb;

      remove_arg( argc, argv, i);
    }
    else if ( std::strcmp((*argv)[i], "-com-mpi") == 0) {
      if ( !COMMPI_Initialized()) _mpi_initialized=true;

      remove_arg( argc, argv, i);
    }
    else if ( std::strcmp((*argv)[i], "-com-home") == 0) {
      if ( *argc>i+1 && (*argv)[i+1][0]>='0' && (*argv)[i+1][0]<='9') { 
	_libdir = (*argv)[i+1];   _libdir.append( "/lib/");
	remove_arg( argc, argv, i+1); 
      }
      remove_arg( argc, argv, i);
    }
    else
      ++i;
  }

  // Initialize MPI if requested and MPI is not yet initialized.
  //  if ( _mpi_initialized){
    //    std::cout << "INITIALIZING MPI!!!" << std::endl;
  if(!COMMPI_Initialized())
    MPI_Init( argc, argv);
    //  }
  // Set process-specific verbose level
  int rank = 0;
  if ( COMMPI_Initialized()) rank = COMMPI_Comm_rank( MPI_COMM_WORLD);
  std::map<int,int>::const_iterator it=verb_maps.find( rank);
  if ( it!=verb_maps.end()) set_verbose( it->second);

  // Determine the F90 pointer treatment mode
  _f90_mangling = -1;
  _f90ptr_treat = -1; 

#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
  // For GNU C++, we know to use the csated address.
  _cppobj_casting = 1;
#elif defined(_POWER) || defined(__INTEL_COMPILER)
  // For IBM and INTEL compilers, we know to use the object's address
  _cppobj_casting = 0;
#else
  _cppobj_casting = -1;
#endif

  if ( _verbose) {
    std::cerr << "Roccom Version 3.0. CVS $Id: Roccom_base.C,v 1.93 2008/12/06 08:43:25 mtcampbe Exp $" << std::endl;
    std::cerr << "Roccom: Started with verbose level " << _verbose << std::endl;
#ifndef DUMMY_MPI
    if ( !COMMPI_Initialized())
      std::cerr << "Roccom: MPI not initialized. Running in serial mode\n";
#else
    std::cerr << "Roccom: Running in serial mode with DUMMY_MPI\n";
#endif
  }
}

Roccom_base::~Roccom_base() {
  // If MPI was initialized by Roccom, then call MPI_Finalize.
  if (_mpi_initialized) MPI_Finalize();
}

void Roccom_base::init( int *argc, char ***argv ) throw( int) {
  extern void printStackBacktrace();

  COM::Roccom_base *ptr = get_roccom();
  if ( ptr) throw( COM_ERR_WAS_INITIALIZED);

  ptr = new COM::Roccom_base( argc, argv);
  
  set_roccom( ptr);
}

void Roccom_base::finalize() throw( int) {
  COM::Roccom_base *ptr = get_roccom();
  if ( !ptr) throw( COM_ERR_WASNOT_INITIALIZED);

  delete ptr;
  set_roccom( NULL);
}

void Roccom_base::abort( int ierr) {
  if ( COMMPI_Initialized() ) MPI_Abort( MPI_COMM_WORLD, ierr);
  else exit( ierr); 
}

void
Roccom_base::load_module( const std::string &lname, 
			  const std::string &wname) throw(int)
{
#ifndef STATIC_LINK
  if ( _verb1>1) 
    std::cerr << "Loading module " << lname 
	      << " with arguments " << wname << "..." << std::endl;

  void *handle;
  std::string lname_found;

  // Obtain a reference to the library.
  int index = _module_map.find(lname).first;
  if ( index<0) { // Load the library
    std::string lname_short = std::string("lib") + lname + ".so";
    std::string lname_full = _libdir + lname_short;

    // Open the library
    handle = dlopen( lname_full.c_str(), RTLD_LAZY);
    if (handle == NULL)
      printf("dlopen error: %s\n", dlerror());

    if ( handle == NULL && !_libdir.empty()) {
      handle = dlopen( lname_short.c_str(), RTLD_LAZY);
      if (handle == NULL)
        printf("dlopen error: %s\n", dlerror());
      lname_found = lname_short;
    }
    else 
      lname_found = lname_full;
    

    if ( handle == NULL) {
      std::string libs;
      if ( _libdir.empty()) libs = lname_full;
      else libs = lname_full+" or "+lname_short;

      proc_exception( COM_exception(COM_ERR_COULD_OPENLIB, libs),
		      "Roccom_base::load_module");
    }

    // Insert a new entry into _module_map.
    index = _module_map.add_object( lname, Module_map::value_type());
    _module_map[index].first = handle;
  }
  else
    handle = _module_map[index].first;
  
  // Insert the handle and the window name into _module_map.
  _module_map[index].second.insert(wname);
  
  // Look for the symbol
  std::string fname = lname + "_load_module";

  // Set the default communicator to MPI_COMM_SELF
  MPI_Comm comm = MPI_COMM_SELF; 
  std::swap(_comm, comm);

  void *fptr = dlsym( handle, fname.c_str());
  if ( fptr != NULL) {
    // This is a C/C++ module
    typedef void(*Func1)(const char*);
    (*(Func1)fptr)( wname.c_str());
  }
  else {
    // Try out different name mangling schemes to figure out automatically.
    int ibegin= ( _f90_mangling == -1 ) ? 0 : _f90_mangling;
    int iend = (_f90_mangling == -1) ? 4: _f90_mangling;

      // 0: lowercase
      // 1: uppercase
      // 2: _
      // 3: uppercase _
      // 4: lowercase __
    for ( int i=ibegin; i<=iend; ++i) {
      if ( i & 1 == 1)
	std::transform( fname.begin(), fname.end(), fname.begin(), toupper);
      else
	std::transform( fname.begin(), fname.end(), fname.begin(), tolower);

      if ( i==2 || (i==3&&i==ibegin)) fname.append( "_");
      if (i==4) { fname.append( "_"); if (ibegin == 4) fname.append( "_"); }

      fptr = dlsym( handle, fname.c_str());
      if ( fptr) { _f90_mangling = i; break; }
    }

    if ( fptr != NULL) {
      // This is a Fortran module
      typedef void(*Func2)(const char*,long int);
      (*(Func2)fptr)( wname.c_str(), wname.size());
    }
    else {
      std::string msg;
      if ( ibegin == iend) msg = fname+" in "+lname_found;
      else msg = fname + " or any its lowercase w/o underscore in " + lname;

      msg.append( "\nError message from libdl is: ");
      msg.append( dlerror());
      proc_exception( COM_exception(COM_ERR_COULD_FINDSYM, msg),
		      "Roccom_base::load_module");
    }
  }

  // Restore the default communicator.
  std::swap(_comm, comm);

  if ( _verb1>1) 
    std::cerr << "Module " << lname << " loaded." << std::endl;
#endif
}

void
Roccom_base::unload_module( const std::string &lname,
			    const std::string &wname, int dodl) throw(int) {

#ifndef STATIC_LINK
  if ( _verb1>1) 
    std::cerr << "Unloading module " << lname << "..." << std::endl;

  // Obtain the window name corresponding to the library
  int index = _module_map.find(lname).first;
  if ( index<0)
    proc_exception( COM_exception( COM_ERR_MODULE_NOTLOADED, lname),
		    "Roccom_base::unload_module");

  std::set<std::string> &obj = _module_map[index].second;

  // If the module was loaded multiple times but window name is not 
  // specified, then throw exception.
  if ( wname.empty() && obj.size()>1)
    proc_exception( COM_exception( COM_ERR_MODULE_NOTLOADED, lname),
		    "Roccom_base::unload_module");

  std::set<std::string>::iterator it = wname.empty()?obj.begin():obj.find( wname);

  // If wname is not found, throw exception
  if ( it==obj.end())
    proc_exception( COM_exception( COM_ERR_MODULE_NOTLOADED, lname),
		    "Roccom_base::unload_module");

  // Look for the symbol
  std::string fname = lname + "_unload_module";
  std::string wname_str=wname.size()?wname:*obj.begin();

  void *handle = _module_map[index].first;
  void *fptr = dlsym( handle, fname.c_str());
  if ( fptr != NULL) {
    // This is a C/C++ module
    typedef void(*Func1)(const char*);
    (*(Func1)fptr)( wname_str.c_str());
  }
  else {
    if ( _f90_mangling != -1) {
      if ( _f90_mangling & 1 == 1)
	std::transform( fname.begin(), fname.end(), fname.begin(), toupper);
      else
	std::transform( fname.begin(), fname.end(), fname.begin(), tolower);

      if ( _f90_mangling ==2 || _f90_mangling == 3) fname.append( "_");

      fptr = dlsym( handle, fname.c_str());
    }

    if ( fptr != NULL) {
      // This is a Fortran module
      typedef void(*Func2)(const char*,long int);
      (*(Func2)fptr)( wname_str.c_str(), wname_str.size());
    }
    else {
      proc_exception( COM_exception(COM_ERR_COULD_FINDSYM, fname+" in "+lname),
		      "Roccom_base::unload_module");
    }
  }

  obj.erase( wname);

  if ( dodl && obj.size()==0) { // Unload module if all instances removed.
    dlclose( handle);
    _module_map.remove_object( lname);
  }

  if ( _verb1>1) 
    std::cerr << "Module " << lname << " unloaded" << std::endl;
#endif
}

// Create a window with given name
void Roccom_base::new_window( const std::string &name, MPI_Comm comm) throw(int) 
{
  // If MPI_COMM_NULL is used, then use the default communicator.
  if ( comm == MPI_COMM_NULL) comm = _comm;

  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: Creating window \"" << name << '"' 
		<< " with communicator " << comm << std::endl;
    
    if ( _window_map.find( name).second) 
      throw COM_exception( COM_WARN_DUP_WINDOW,
			   append_frame(name, Roccom_base::new_window));
    
    _window_map.add_object( name, new Window( name, comm));
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::new_window);
    std::string s("When processing "); s.append(name);
    proc_exception( ex, s);
  }
}

void Roccom_base::
window_init_done( const std::string &wname, bool panechanged) throw(int) 
{
  try {
    get_window( wname).init_done(panechanged);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::window_init_done);

    std::string msg = std::string("When completing the init of window ")+wname;
    proc_exception( ex, msg);
  }
  if ( _verb1>1) 
    std::cerr << "ROCCOM: Done creating window \"" << wname 
	      << '"' << std::endl;
}

void Roccom_base::delete_window( const std::string &name) throw(int) 
{
  try {
    if ( _verb1>1)
      std::cerr << "ROCCOM: Deleting window \"" << name << '"' << std::endl;

    _window_map.remove_object( name);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.ierr = COM_ERR_WINDOW_NOTEXIST;
    ex.msg = append_frame( ex.msg, Roccom_base::delete_window);
    std::string s;
    s = s + "When processing window " + name;
    proc_exception( ex, s);
  }
}

void Roccom_base::delete_pane( const std::string &wname,
			       const int pane_id) throw(int) 
{
  try {
    if ( _verb1>1)
      std::cerr << "ROCCOM: Delete pane " << pane_id << " of window "
		<< std::endl;

    get_window( wname).delete_pane( pane_id);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::delete_pane);
    char buf[10];
    std::sprintf( buf, "%d", pane_id);
    std::string msg = std::string( "When processing window ") 
      + wname + " for pane " + buf;
    proc_exception( ex, msg);
  }
}

void print_type( std::ostream &os, COM_Type type) {
  switch (type) {
  case COM_STRING:
    os << "STRING"; break;
  case COM_METADATA:
    os << "METADATA"; break;
  case COM_VOID:
    os << "VOID"; break;
  case COM_OBJECT:
    os << "COM_Object"; break;
  case COM_F90POINTER:
    os << "F90POINTER"; break;
  case COM_CHAR: case COM_CHARACTER:
    os << "CHAR"; break;
  case COM_DOUBLE:
  case COM_DOUBLE_PRECISION: 
    os << "double"; break;
  case COM_INT:
  case COM_LONG:
  case COM_INTEGER:
    os << "int"; break;
  case COM_FLOAT:
  case COM_REAL:
    os << "float"; break;
  case COM_LOGICAL:
    os << "logical"; break;
  case COM_MPI_COMMC:
  case COM_MPI_COMMF:
    os << "MPI_Comm"; break;
  default:
    os << "type(" << type << ")";
  }
}

// Register a new attribute with given name
void Roccom_base::
new_attribute( const std::string &wa, 
	       const char loc,
	       const int type, 
	       int size,
	       const std::string &unit) throw(int) 
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: new attribute \"" << wa
		<< "\"\n\tLocation: " << loc << "\n\tType: ";
      print_type( std::cerr, type);
      std::cerr << "\n\tSize: " << size << "\n\tUnit: \"" << unit << "\""
		<< std::endl;
    }

    // Invoke Window::new_attribute.
    std::string wname, aname;
    split_name( wa, wname, aname);
    
    get_window( wname).new_attribute( aname, loc, type, size, unit);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::new_attribute);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

// Register a new attribute with given name
void Roccom_base::
delete_attribute( const std::string &wa) throw(int) 
{
  try {
    if ( _verb1>1)
      std::cerr << "ROCCOM: delete attribute \"" << wa << std::endl;

    // Invoke Window::new_attribute.
    std::string wname, aname;
    split_name( wa, wname, aname);
    
    get_window( wname).delete_attribute( aname);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::delete_attribute);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::set_size( const std::string &wa, int pid,
			    int nitems, int ng) throw(int)
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Set size for attribute \"" 
		<< wa << '"' << " on pane " << pid 
		<< " to " << nitems << " items with " << ng 
		<< " ghosts" << std::endl;
    }

    // Invoke Window::set_size
    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).set_size( aname, pid, nitems, ng);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::set_size);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

// A helper class for detecting the scheme to use for set_object
class COM_Object_derived : public COM_Object {
public:
  static bool cast_obj() {
    typedef void(COM_Object::*Member_Func)( int *);
    Member_Func f = (Member_Func)&COM_Object_derived::validate_object;

    COM_Object_derived obj;
    int ierr; (reinterpret_cast<COM_Object*>((void *)&obj)->*f)( &ierr);
      
    return ierr != 0;
  }

protected:
  COM_Object_derived() : _cookie( DERIVED_COOKIE) {}

  virtual ~COM_Object_derived() {}

  void validate_object( int *ierr) {
    if ( _cookie != DERIVED_COOKIE) *ierr = -1;
    else *ierr = COM_Object::validate_object();
  }

protected:
  enum { DERIVED_COOKIE=3374833 };
  int _cookie;
};


// Associates an object with a specific window.
void Roccom_base::set_object( const std::string &wa, const int pid,
			      void *obj_addr, void *casted_addr) throw(int) 
{
  COM_assertion_msg( obj_addr, "Caught NULL pointer");
  int size;
  get_attribute( wa, NULL, NULL, &size, NULL);
  COM_assertion_msg( size==1, "Must register a scalar with set_object");

  if ( obj_addr == casted_addr) {
    set_array( wa, pid, obj_addr);
    return;
  }
  
  if ( _cppobj_casting == -1)
    _cppobj_casting = COM_Object_derived::cast_obj();

  if ( _cppobj_casting) // Use stride to store the offset
    set_array( wa, pid, casted_addr, (char*)casted_addr-(char*)obj_addr);
  else
    set_array( wa, pid, obj_addr);
}

// Associates an object with a specific window.
void Roccom_base::get_object( const std::string &wa, const int pid, 
			      void **ptr) throw(int) 
{
  int strd;
  get_array( wa, pid, ptr, &strd);

  if ( strd <= 1) return; else *(char**)ptr -= strd;
}

// Register the address of an arrays for a data field.
void Roccom_base::set_array( const std::string &wa, const int pid,
			     void *addr, int strd, int cap, 
			     bool is_const) throw(int) 
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Set array for \"" << wa << "\" on pane " 
		<< pid << " to " << addr << " with stride ";
      if ( strd) std::cerr << strd;
      else std::cerr << "==#components";

      std::cerr << " and capacity ";

      if ( cap) std::cerr << cap;
      else std::cerr << "==#items";

      std::cerr << std::endl;
    }

    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).set_array( aname, pid, addr, strd, cap, is_const);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::set_array);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::set_bounds( const std::string &wa, const int pane_id,
			      const void *lbnd, const void *ubnd) throw(int) 
{}

void Roccom_base::allocate_array( const std::string &wa, const int pid, 
				  void **addr, int strd, int cap) throw(int) 
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Allocate array for \"" << wa << "\" on pane " 
		<< pid << " with stride ";
      if ( strd) std::cerr << strd;
      else std::cerr << "==#components";

      std::cerr << " and capacity ";

      if ( cap) std::cerr << cap;
      else std::cerr << "==#items";

      std::cerr << std::endl;
    }

    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).alloc_array( aname, pid, addr, strd, cap);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::allocate_array);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::resize_array( const std::string &wa, const int pid, 
				void **addr, int strd, int cap) throw(int) {
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Resize array for \"" << wa << "\" on pane " 
		<< pid << " with stride ";
      if ( strd<0) std::cerr << "==current";
      else if (strd) std::cerr << strd;
      else std::cerr << "==#components";

      std::cerr << " and capacity ";

      if ( cap) std::cerr << cap;
      else std::cerr << "==#items";

      std::cerr << std::endl;
    }

    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).resize_array( aname, pid, addr, strd, cap);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::resize_array);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::append_array( const std::string &wa, const int pid, 
				const void *val, int v_strd, int v_size) throw(int)
{
  try {
    if ( _verb1>1)
      std::cerr << "ROCCOM: Appending array " << val << " for \"" << wa 
		<< "\" on pane " << pid << " with stride " << v_strd 
		<< " and size " << v_size << std::endl;

    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).append_array( aname, pid, val, v_strd, v_size);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::append_array);
    std::string s("When processing attribute "); s.append(wa);
    proc_exception( ex, s);
  }
}

void Roccom_base::
use_attribute( const std::string &waname, const std::string &pwaname, 
	       int withghost,const char *cndname, int val) throw(int)
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Using attribute \"" << pwaname 
		<< "\" onto \"" << waname << '"' << std::endl;
    }
    std::string pawname, paaname;
    split_name( pwaname, pawname, paaname);

    std::string awname, aaname;
    std::string::size_type ni = waname.find(".");
    if ( ni == std::string::npos) {
      awname = waname; aaname = "";
    }
    else 
      split_name( waname, awname, aaname);

    std::string cawname, caaname;
    if ( cndname && *cndname!=0)
      split_name( cndname, cawname, caaname);
    
    Attribute *cnd=caaname.empty()?NULL:get_window(cawname).attribute(caaname);
    if ( !caaname.empty() && cnd==NULL)
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, cndname);

    get_window(awname).inherit( get_window( pawname).attribute(paaname), 
				aaname, Pane::INHERIT_USE, withghost, cnd, val);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::use_attribute);
    std::string s;
    s = s + "When window " + waname + " uses attribute " + pwaname;
    proc_exception( ex, s);
  }
}

void Roccom_base::
clone_attribute( const std::string &waname, const std::string &pwaname,
		 int withghost, const char *cndname, int val) throw(int)
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Cloning attribute \"" << pwaname 
		<< "\" onto \"" << waname << '"' << std::endl;
    }
    std::string pawname, paaname;
    split_name( pwaname, pawname, paaname);

    std::string awname, aaname;
    std::string::size_type ni = waname.find(".");
    if ( ni == std::string::npos) {
      awname = waname; aaname = "";
    }
    else 
      split_name( waname, awname, aaname);

    std::string cawname, caaname;
    if ( cndname && *cndname!=0)
      split_name( cndname, cawname, caaname);

    Attribute *cnd=caaname.empty()?NULL:get_window(cawname).attribute(caaname);
    if ( !caaname.empty() && cnd==NULL)
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, cndname);

    get_window(awname).inherit( get_window( pawname).attribute(paaname), 
				aaname, Pane::INHERIT_CLONE, withghost, cnd, val);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::clone_attribute);
    std::string s;
    s = s + "When window " + waname + " clones attribute " + pwaname;
    proc_exception( ex, s);
  }
}

void Roccom_base::
copy_attribute( const std::string &waname, const std::string &pwaname,
		int withghost, const char *cndname, int val) throw(int)
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Copying attribute \"" << pwaname 
		<< "\" onto \"" << waname << '"' << std::endl;
    }
    std::string pawname, paaname;
    split_name( pwaname, pawname, paaname);

    std::string awname, aaname;
    std::string::size_type ni = waname.find(".");
    if ( ni == std::string::npos) {
      awname = waname; aaname = "";
    }
    else 
      split_name( waname, awname, aaname);

    std::string cawname, caaname;
    if ( cndname && *cndname!=0)
      split_name( cndname, cawname, caaname);

    Attribute *cnd=caaname.empty()?NULL:get_window(cawname).attribute(caaname);
    if ( !caaname.empty() && cnd==NULL)
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, cndname);

    get_window(awname).inherit( get_window( pawname).attribute(paaname), 
				aaname, Pane::INHERIT_COPY, withghost, cnd, val);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::copy_attribute);
    std::string s;
    s = s + "When window " + waname + " copies attribute " + pwaname;
    proc_exception( ex, s);
  }
}

void Roccom_base::
copy_attribute( int trg_hdl, int src_hdl,
		int withghost, int ptn_hdl, int val) throw(int)
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Copying attribute with handle \"" << src_hdl 
		<< "\" onto \"" << trg_hdl << '"' << std::endl;
    }

    if ( trg_hdl <= 0 || src_hdl <= 0)
      throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_HANDLE);

    if ( _attr_map.is_immutable( trg_hdl))
      throw COM_exception(COM_ERR_IMMUTABLE);

    Attribute *trg = &get_attribute(trg_hdl);
    trg->window()->inherit( &get_attribute(src_hdl), trg->name(),
			    Pane::INHERIT_COPY, withghost, 
			    ptn_hdl?&get_attribute(ptn_hdl):NULL, val);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::copy_attribute);
    std::ostringstream sout;

    sout << "When copying attribute " << src_hdl << " onto " 
	 << trg_hdl << " with pattern attribute " << ptn_hdl;
    proc_exception( ex, sout.str());
  }
}


void Roccom_base::deallocate_array( const std::string &wa, 
				    const int pid) throw(int) 
{
  try {
    if ( _verb1>1)
      std::cerr << "ROCCOM: Deallocate array for \"" << wa << "\" on pane " 
		<< pid << " to" << std::endl;

    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).dealloc_array( aname, pid);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::deallocate_array);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

// Register a new attribute with given name
void Roccom_base::
get_attribute( const std::string &wa, char *loc,
	       int *type, int *size, std::string *unit) throw(int) 
{
  try {
    if ( _verb1>1) // Print debugging info
      std::cerr << "ROCCOM: get attribute \"" << wa << "\"" << std::endl;

    // Invoke Window::get_attribute.
    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).get_attribute( aname, loc, type, size, unit);
    _errorcode = 0;

    if ( _verb1>1) { // Print debugging info
      if (loc)  std::cerr << "\tLocation: " << *loc << "\n\tType: \n";
      if (type) print_type( std::cerr, *type);    
      if ( size) std::cerr << "\tSize: " << *size << std::endl;
      if ( unit) std::cerr << "\tUnit: \"" << *unit << "\"" << std::endl;
    }
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_attribute);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::get_size( const std::string &wa, int pid,
			    int *nitems, int *ng) throw(int) 
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Get size for attribute \"" 
		<< wa << '"' << " for pane " << pid << std::endl;
    }

    // Invoke Window::get_size
    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).get_size( aname, pid, nitems, ng);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_size);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::get_array( const std::string &wa, const int pane_id,
			     void **addr, int *strd, int *cap, 
			     bool is_const) throw(int) 
{
  Pointer_descriptor ptr(NULL);
  get_array( wa, pane_id, ptr, strd, cap, is_const);
  if (addr) *addr = ptr.ptr;
}

// Get the address for an attribute on a specific pane.
void Roccom_base::get_array( const std::string &wa, const int pid,
			     Pointer_descriptor &addr, 
			     int *strd, int *cap, bool is_const) throw(int) {
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Get array for attribute \"" 
		<< wa << '"' << " on pane " << pid << std::endl;
    }

    // Invoke Window::get_size
    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).get_array( aname, pid, addr, strd, cap, is_const);

    if ( _verb1>1) {
      std::cerr << "ROCCOM: ";
      if ( strd) std::cerr << " stride is " << *strd;
      if ( cap) std::cerr << " capacity is " << *cap;
      std::cerr << std::endl;
    }

    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_array);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

// Get the status of an attribute.
int Roccom_base::get_status( const std::string &wa, int pid) throw(int) 
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Get status for attribute \"" 
		<< wa << '"' << " on pane " << pid << std::endl;
    }

    std::string wname, aname;
    split_name( wa, wname, aname, false);

    Window **w = _window_map.find( wname).second;

    if ( aname.empty() && pid == 0) {
      if ( w == NULL) return -1;
      else return 0;
    }

    int status = (**w).get_status( aname, pid);

    if ( _verb1>1) {
      std::cerr << "ROCCOM: ";
      std::cerr << " status is " << status << std::endl;
    }

    _errorcode = 0;
    return status;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_status);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }

  return 0;
}

void Roccom_base::copy_array( const std::string &wa, const int pid,
			      void *val, int v_strd, int v_size, 
			      int offset) throw(int) 
{
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: Copy array for attribute \"" 
		<< wa << '"' << " on pane " << pid << std::endl;
    }

    // Invoke Window::get_size
    std::string wname, aname;
    split_name( wa, wname, aname);

    get_window( wname).copy_array( aname, pid, val, v_strd, v_size, offset);

    if ( _verb1>1) {
      std::cerr << "ROCCOM: ";
      std::cerr << " stride is " << v_strd;
      std::cerr << " size is " << v_size;
      std::cerr << std::endl;
    }

    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::copy_array);
    std::string s;
    s = s + "When processing attribute " + wa;
    proc_exception( ex, s);
  }
}

void Roccom_base::get_bounds( const std::string &wa, const int pane_id,
			      void *lbnd, void *ubnd) throw(int) 
{}

int Roccom_base::check_bounds( const std::string &wa, 
			       int pane_id) throw(int) 
{ return 0; }

std::pair<int,int> // The pair of offset1 and offset2
Roccom_base::get_f90pntoffsets( const Attribute *a) {
  
  int n = Attribute::get_sizeof( COM_F90POINTER, a->size_of_components());

  COM_assertion_msg( _f90ptr_treat>=0, "No F90 pointer initalized");
  if ( _f90ptr_treat == FPTR_INSERT) 
    return std::pair<int,int>(n/2-sizeof(void*), n-sizeof(void*));
  else
    return std::pair<int,int>(0,n-sizeof(void*));
}

void Roccom_base::
set_f90pointer( const std::string &waname, void *ptr, 
		Func_ptr f, long int len) throw(int) {
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: set pointer \"" << waname 
		<< " to " << ptr << std::endl;

    std::string wname, aname;
    split_name( waname, wname, aname);

    Attribute *a = get_window( wname).attribute( aname);
    if ( a->location() != 'w')
      throw COM_exception( COM_ERR_NOT_A_WINDOW_ATTRIBUTE,
			   append_frame(waname, Roccom_base::set_f90pointer));
    if ( a->data_type() == COM_F90POINTER) {
      int n=get_sizeof(COM_F90POINTER, a->size_of_components());

      std::pair<int,int> offs = get_f90pntoffsets( a);
      std::memcpy( a->pointer(), (char*)ptr-offs.first, offs.second);

      if ( offs.second<n) {
	*(void**)((char*)a->pointer()+offs.second) = 
	  (char*)a->pointer()+offs.first+((char*)len-(char*)ptr);
      }
      
      COM_assertion_msg( _f90ptr_treat>=0, "No F90 pointer initalized");
      if ( _f90ptr_treat == FPTR_INSERT || _f90ptr_treat == FPTR_APPEND) {
	typedef void(*Func2)(void*,void*, long int, long int);
	*(void**)((char*)a->pointer()+offs.first)=NULL;
	(*(Func2)f)( ptr, (char*)a->pointer()+offs.first, 
		     len, *(long int*)((char*)a->pointer()+offs.second));
      }
      else {
	typedef void(*Func2)(void*,void*);
	(*(Func2)f)( (char*)ptr+offs.first, a->pointer());
      }

      if ( _verb1>1) {
	std::cerr << "ROCCOM: copied values ";
	for ( int i=0, ni=n/sizeof(void*); i<ni; ++i) 
	  std::cerr << ((void**)a->pointer())[i] << ' ';
	std::cerr << std::endl;
      }
    }
    else
      throw COM_exception( COM_ERR_NOT_A_POINTER,
			   append_frame(waname, Roccom_base::set_f90pointer));
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::set_f90pointer);
    proc_exception( ex, "");
  }
}

void Roccom_base::
get_f90pointer( const std::string &waname, void *ptr, 
		Func_ptr f, long int len) throw(int) {
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get pointer \"" << waname 
		<< " into " << ptr << std::endl;

    std::string wname, aname;
    split_name( waname, wname, aname);

    Attribute *a = get_window( wname).attribute( aname);
    if ( a->location() != 'w')
      throw COM_exception( COM_ERR_NOT_A_WINDOW_ATTRIBUTE,
			   append_frame(waname, Roccom_base::get_f90pointer));
    if ( a->data_type() == COM_F90POINTER) {
      std::pair<int,int> offs = get_f90pntoffsets( a);

      COM_assertion_msg( _f90ptr_treat>=0, "No F90 pointer initalized");
      if ( _f90ptr_treat == FPTR_INSERT || _f90ptr_treat == FPTR_APPEND) {
	typedef void(*Func2)(void*,void*, long int, long int);
	(*(Func2)f)( (char*)a->pointer()+offs.first, ptr, 
		     *(long int*)((char*)a->pointer()+offs.second), len);
      }
      else {
	typedef void(*Func2)(void*,void*);
	(*(Func2)f)( a->pointer(), (char*)ptr+offs.first);
      }
    }
    else
      throw COM_exception( COM_ERR_NOT_A_POINTER,
			   append_frame(waname, Roccom_base::get_f90pointer));
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_f90pointer);
    proc_exception( ex, "");
  }
}

MPI_Comm Roccom_base::
get_communicator( const std::string &wname) throw(int) {
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get the communicator of window \"" 
		<< wname << std::endl;
    if ( COMMPI_Initialized()) {
      Window &w = get_window(wname);
      _errorcode = 0;
      return w.get_communicator();
    }
    else
      return MPI_COMM_NULL;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_window_npanes);
    std::string s;
    s = s + "When processing window " + wname;
    proc_exception( ex, s);
  }
  return 0;
}

void Roccom_base::
get_panes( const std::string &wname, std::vector<int> &paneids_vec, 
	   int rank, int **pane_ids) throw(int){
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: get pane ids of window \"" 
		<< wname << "\" on ";
      if ( rank==-2) std::cerr << " this process";
      else if ( rank==-1) std::cerr << " all processes";
      else std::cerr << " process " << rank;

      std::cerr << std::endl;
    }

    get_window(wname).panes( paneids_vec, rank);

    if ( pane_ids) {
      *pane_ids = new int[paneids_vec.size()];
      std::copy( paneids_vec.begin(), paneids_vec.end(), *pane_ids);
    }
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_panes);
    std::string s;
    s = s + "When processing window " + wname;
    proc_exception( ex, s);
  }
}

void Roccom_base::
get_windows(std::vector<std::string> &names) throw(int){
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: get windows";
      //      if ( rank==-2) std::cerr << " this process";
      //      else if ( rank==-1) std::cerr << " all processes";
      //      else std::cerr << " process " << rank;
      
      std::cerr << std::endl;
    }
    names = _window_map.get_names();
    //    get_window(wname).panes( paneids_vec, rank);

    //    if ( pane_ids) {
    //      *pane_ids = new int[paneids_vec.size()];
    //      std::copy( paneids_vec.begin(), paneids_vec.end(), *pane_ids);
    //    }
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_windows);
    std::string s;
    s = s + "When processing get_windows.";
    proc_exception( ex, s);
  }
}

void Roccom_base::
get_modules(std::vector<std::string> &names) throw(int){
  try {
    if ( _verb1>1) {
      std::cerr << "ROCCOM: get modules";
      //      if ( rank==-2) std::cerr << " this process";
      //      else if ( rank==-1) std::cerr << " all processes";
      //      else std::cerr << " process " << rank;
      
      std::cerr << std::endl;
    }
    names = _module_map.get_names();
    //    get_window(wname).panes( paneids_vec, rank);

    //    if ( pane_ids) {
    //      *pane_ids = new int[paneids_vec.size()];
    //      std::copy( paneids_vec.begin(), paneids_vec.end(), *pane_ids);
    //    }
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_modules);
    std::string s;
    s = s + "When processing get_modules.";
    proc_exception( ex, s);
  }
}

void Roccom_base::get_attributes( const std::string &wname, int *natts, 
				  std::string &str, char **names) throw(int)
{
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get attributes of window \"" 
		<< wname << '"' << std::endl;

    std::vector<Attribute*> as;
    get_window(wname).attributes( as);

    str.clear();
    if ( natts) *natts = as.size();
    int i=0, n=as.size();
    if (n) for (;;) {
      str = str + as[i]->name();
      if ( ++i<n) str.append(" ");
      else break;
    }
    
    if ( names) {
      *names = new char[str.size()+1];
      std::copy( str.begin(), str.end(), *names);
      (*names)[str.size()]=0;
    }

    if ( _verb1>1) 
      std::cerr << "ROCCOM: Got attribute names: " << str << std::endl;

    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_attributes);
    std::string s;
    s = s + "When processing window " + wname;
    proc_exception( ex, s);
  }
}


void Roccom_base::
get_connectivities( const std::string &wname, int pane_id, int *natts, 
		    std::string &str, char **names) throw(int)
{
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get connectivities of window \"" << wname 
		<< "\" on pane " << pane_id << std::endl;

    std::vector<Connectivity*> as;
    get_window(wname).pane( pane_id).connectivities( as);

    if ( natts) *natts = as.size();

    str.clear();
    int i=0, n=as.size();
    if (n) for (;;) {
      str = str + as[i]->name();
      if ( ++i<n) str.append(" ");
      else break;
    }
    
    if ( names) {
      *names = new char[str.size()+1];
      std::copy( str.begin(), str.end(), *names);
      (*names)[str.size()]=0;
    }

    if ( _verb1>1) 
      std::cerr << "ROCCOM: Got connectivity names: " << str << std::endl;

    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_connectivities);
    std::string s;
    s = s + "When processing window " + wname;
    proc_exception( ex, s);
  }
}

void Roccom_base::
get_parent( const std::string &waname, int pane_id, 
	    std::string &str, char **name) throw(int)
{
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get parent of attribute \"" << waname 
		<< "\" on pane " << pane_id << std::endl;

    std::string wname, aname;
    split_name( waname, wname, aname);

    get_window( wname).get_parent( aname, pane_id, str);

    if ( name) {
      *name = new char[str.size()+1];
      std::copy( str.begin(), str.end(), *name);
      (*name)[str.size()]=0;
    }

    if ( _verb1>1) 
      std::cerr << "ROCCOM: Got parent name: " << str << std::endl;

    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_parent);
    std::string s;
    s = s + "When processing attribute " + waname;
    proc_exception( ex, s);
  }
}


void Roccom_base::free_buffer( char **buf) 
{ if ( buf) delete [] *buf; *buf = NULL; }

void Roccom_base::free_buffer( int **buf) 
{ if ( buf) delete [] *buf; *buf = NULL; }

int Roccom_base::
get_window_handle( const std::string &wname) throw(int) { 
  int n(-1);
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get handle of window \"" << wname << "\": ";
    std::pair<int,Window**> obj = _window_map.find( wname);
    COM_assertion(obj.first<0 || (*obj.second)->name()==wname);

    if ( obj.first>=0)
      n = obj.first+1; 
    
    if ( _verb1>1) {
      if ( n>0) std::cerr << n << std::endl;
      else std::cerr << " not found" << std::endl;      
    }
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_window_handle);
    proc_exception( ex, "");
  }
  return n;
}

Window * Roccom_base::
get_window_object( int hdl) throw(int)
{
  if ( hdl <= 0) 
    throw COM_exception( COM_ERR_INVALID_WINDOW_HANDLE,
			 append_frame("", Roccom_base::get_window));
  return _window_map[hdl-1];
}

const Window * Roccom_base::
get_window_object( int hdl) const throw(int)
{
  if ( hdl <= 0) 
    throw COM_exception( COM_ERR_INVALID_WINDOW_HANDLE,
			 append_frame("", Roccom_base::get_window_object));
  return _window_map[hdl-1];
}

int Roccom_base::
get_attribute_handle_const( const std::string &waname) throw(int) {
  int n(-1);
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get const handle of attribute \"" 
		<< waname << "\": ";

    std::string wname, aname;
    split_name( waname, wname, aname);
    
    Attribute *a = get_window( wname).attribute( aname);
    
    if ( a!=NULL) n=_attr_map.add_object( waname, a, true);

    if ( _verb1>1) {
      if ( n>0) std::cerr << n << std::endl;
      else std::cerr << " not found" << std::endl;      
    }
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_attribute_handle_const);
    proc_exception( ex, "");
  }
  return n;
}

int Roccom_base::
get_attribute_handle( const std::string &waname) throw(int) {
  int n(-1);
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get handle of attribute \"" << waname << "\": ";
    std::string wname, aname;
    split_name( waname, wname, aname);
    
    Attribute *a = get_window( wname).attribute( aname);
    
    if ( a!=NULL) n=_attr_map.add_object( waname, a, false);

    if ( _verb1>1) {
      if ( n>0) std::cerr << n << std::endl;
      else std::cerr << " not found" << std::endl;      
    }
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_attribute_handle);
    proc_exception( ex, "");
  }
  return n;
}

int Roccom_base::
get_function_handle( const std::string &wfname) throw(int) {
  int n(-1);
  try {
    if ( _verb1>1) 
      std::cerr << "ROCCOM: get handle of function \"" << wfname << "\": ";
    std::string wname, fname;
    split_name( wfname, wname, fname);
    
    Function *f = get_window( wname).function( fname);

    if ( f!=NULL) n=_func_map.add_object( wfname, f);

    if ( _verb1>1) {
      if ( n>0) std::cerr << n << std::endl;
      else std::cerr << " not found" << std::endl;      
    }
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::get_function_handle);
    proc_exception( ex, "");
  }
  return n;
}

void Roccom_base::set_function( const std::string &wfname, 
				Func_ptr ptr,
				const std::string &intents,
				const COM_Type *types, bool ff) throw(int) {
  try { 
    if ( _verb1>1) {
      int n=intents.size();
      std::cerr << "ROCCOM: init function \"" << wfname << '"'
		<< " to " << ptr << " with " << n << " parameters";

      for ( int i=0; i<n; ++i) {
	std::cerr << "\n\t" << intents[i] << ":";
	print_type( std::cerr, types[i]);
      }
      std::cerr << std::endl;
    }
    std::string wname, fname;
    split_name( wfname, wname, fname);

    if ( intents.size() > Function::MAX_NUMARG) 
      throw COM_exception( COM_ERR_TOO_MANY_ARGS,
			   append_frame(wfname, Roccom_base::set_function));

    for ( int i=0, size=intents.size(); i<size; ++i) {
      if (intents[i] != 'i' && intents[i] != 'o' && intents[i] != 'b' && 
	  intents[i] != 'I' && intents[i] != 'O' && intents[i] != 'B')
	throw COM_exception( COM_ERR_UNKNOWN_INTENT,
			     append_frame(wfname, Roccom_base::set_function));
      if ( types[i] < COM_MIN_TYPEID || types[i] > COM_MAX_TYPEID)
	throw COM_exception( COM_ERR_UNKNOWN_DATATYPE,
			     append_frame(wfname, Roccom_base::set_function));
    }
    
    get_window( wname).set_function( fname, ptr, intents, types, NULL, ff);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::set_function);
    std::string s;
    s = s + "When processing attribute " + wfname;
    proc_exception( ex, s);
  }
}

template <class T>
void Roccom_base::
set_member_function_helper( const std::string &wfname, 
			    T ptr,
			    const std::string &waname,
			    const std::string &intents,
			    const COM_Type *types, 
			    bool ff) throw(int) {
  try {
    if ( _verb1>1) {
      int n=intents.size();
      std::cerr << "ROCCOM: init function \"" << wfname  << '"'
		<< " to " << ptr << " with " << n << " parameters";
      for ( int i=0; i<n; ++i) {
	std::cerr << "\n\t" << intents[i] << ":";
	print_type( std::cerr, types[i]);
	if ( i==0) std::cerr << "\t\"" << waname << "\"";
      }
      std::cerr << std::endl;
    }
    std::string wname, fname;
    split_name( wfname, wname, fname);

    // Allows an additional implicit argument
    if ( intents.size() > Function::MAX_NUMARG+1) 
      throw COM_exception( COM_ERR_TOO_MANY_ARGS,
			   append_frame(wfname, Roccom_base::set_member_function));

    for ( int i=0, size=intents.size(); i<size; ++i) {
      if (intents[i] != 'i' && intents[i] != 'o' && intents[i] != 'b' && 
	  intents[i] != 'I' && intents[i] != 'O' && intents[i] != 'B')
	throw COM_exception( COM_ERR_UNKNOWN_INTENT,
			     append_frame(wfname, Roccom_base::set_member_function));
      if ( types[i] < COM_MIN_TYPEID || types[i] > COM_MAX_TYPEID)
	throw COM_exception( COM_ERR_UNKNOWN_DATATYPE,
			     append_frame(wfname, Roccom_base::set_member_function));
    }
    
    std::string wname1, aname1;
    split_name( waname, wname1, aname1);
    
    Attribute *a = get_window( wname1).attribute( aname1);

    if ( a==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST,
			   append_frame(wfname, Roccom_base::set_member_function));
    if ( !ff && a->data_type()==COM_F90POINTER) 
      throw COM_exception( COM_ERR_F90FUNC,
			   append_frame(wfname, Roccom_base::set_member_function));

    get_window( wname).set_function( fname, ptr, intents, types, a, ff);
    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::set_function);
    std::string s;
    s = s + "When processing attribute " + wfname;
    proc_exception( ex, s);
  }
}

void Roccom_base::set_member_function( const std::string &wfname, 
				       Func_ptr ptr,
				       const std::string &waname,
				       const std::string &intents,
				       const COM_Type *types, 
				       bool ff) throw(int) {
  set_member_function_helper( wfname, ptr, waname, 
			      intents, types, ff);
}

void Roccom_base::set_member_function( const std::string &wfname, 
				       Member_func_ptr ptr,
				       const std::string &waname,
				       const std::string &intents,
				       const COM_Type *types, 
				       bool ff) throw(int) {
  set_member_function_helper( wfname, ptr, waname, 
			      intents, types, ff);
}

int Roccom_base::
get_num_arguments( const std::string &wf) throw(COM_exception) {
  std::string wname, fname;
  split_name(wf, wname, fname);
  
  Function *func = get_window( wname).function( fname);
  if ( func==NULL) 
    throw COM_exception( COM_ERR_FUNCTION_NOTEXIST,
			 append_frame(wf, Roccom_base::set_member_function));
  return func->num_of_args();
}

int Roccom_base::
get_num_arguments( const int wf) throw(COM_exception) {
  Function *func = &get_function( wf);
  return func->num_of_args();
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
inline static double get_wtime() {

  ::timeval tv;
  gettimeofday( &tv, NULL);
  
  return tv.tv_sec + tv.tv_usec*1.e-6;
}
#endif

void Roccom_base::
call_function( int wf, int count, 
	       void **args, const int *lens, bool from_c) throw(int) {
  // Roccom prints out the trace upto (verb-1)/2 depth.
  // If verb is even, then it will also print the arguments.
  try {
    if ( wf==0) { 
      if ( _verbose) {
	std::cerr << "ROCCOM: ************* CALL(" << _depth << ") " 
		  << "on NOOP with " << count << " arguments" << std::endl;
      }
      return;  // Null function
    }

    Function *func = &get_function( wf);

    int  verb = std::max(_verbose, int(_func_map.verbs[ wf]))-_depth*2;
    if ( verb<=0) verb = 0; 
    else verb = (verb+1)%2+1;
    if ( verb) {
      std::cerr << "ROCCOM: CALL(" << _depth << ") " 
		<< _func_map.name(wf);
      if ( verb>1) std::cerr << '(';
    }

    std::vector<char> strs[Function::MAX_NUMARG+1];
    bool  needpostproc=false;
  
    int  li=0;
    void *ps[2*Function::MAX_NUMARG+1];
    int  lcount = 0;
    void **plen = NULL;
    if ( func->is_fortran()) plen = ps+func->num_of_args();

    // attr must be const to void throwing exception when pointer is called
    const Attribute *attr = func->attribute();
    int offset = (attr!=NULL);
    count += offset;
    args  -= offset;
    if ( count > func->num_of_args())
      throw COM_exception(COM_ERR_TOO_MANY_ARGS);

    if ( offset) {
      if ( verb>1)
	std::cerr << std::endl << '\t' << func->intent(0) << ": ";
      
      if ( func->is_rawdata( 0)) {
	if ( attr->is_const() && std::tolower(func->intent(0)) != 'i')
	  throw COM_exception( COM_ERR_ATTRIBUTE_CONST);
	  
	if ( attr->data_type()==COM_F90POINTER) {
	  std::pair<int,int> offs = get_f90pntoffsets( attr);
	  
	  ps[0] = (char*)const_cast<void*>(attr->pointer())+offs.first;
	  // Add pointer information for PortlandGroup Compiler

	  COM_assertion_msg( _f90ptr_treat>=0, "No F90 pointer initalized");
	  if ( _f90ptr_treat == FPTR_INSERT) {
	    *plen = *(void**)((char*)const_cast<void*>(attr->pointer())
			      +offs.second);
	    ++lcount; ++plen;
	  }
	}
	else 
	  ps[0] = (char*)const_cast<void*>(attr->pointer());
	
	if ( verb>1)
	  std::cerr << "VALUE OF\t@" << ps[0] << "\t\"" 
		    << attr->window()->name() << '.' << attr->name() << '"';
      }
      else {
	ps[0] = const_cast<Attribute*>(attr);
	if ( verb>1)
	  std::cerr << "METADATA\t@" << ps[0] << "\t\"" 
		    << attr->window()->name() << '.' << attr->name() << '"';
      }
    }

    for ( int i=offset; i<count; ++i) {
      if ( verb>1) {
	std::cerr << std::endl << '\t' << func->intent(i) << ": ";
      }

      if ( func->is_literal(i)) {
	ps[i] = args[i];
	COM_Type type = func->data_type(i);
	char intent = func->intent(i);
	if ( type == COM_CHARACTER || type == COM_CHAR || type == COM_STRING) {
	  if ( type == COM_STRING) {
	    // Make sure it is NULL terminated
	    if (lens && (intent=='i' || intent=='I' || 
			 intent=='b' || intent=='B') && 
		( lens[li]==0 || ((char*)args[i])[lens[li]-1] != '\0')) {
	      strs[i].resize( lens[li]+1, '\0');
	      std::strncpy( &strs[i][0], (char*)args[i], lens[li]);
	      ps[i] = &strs[i][0];
	      
	      if (intent=='b' || intent=='B') needpostproc=true;
	    }
	    if ( plen) { // Append the length info
	      *plen = (char*)NULL+std::strlen((char*)ps[i]); ++plen; ++lcount;
	    }
	  }
	  else if ( plen) {
	    *plen = (char*)NULL+1; ++plen; ++lcount;
	  }
  	  ++li;
        }
	else if ( type == COM_MPI_COMMC && !from_c) {
	  strs[i].resize( 2*sizeof( MPI_Comm));
	  ps[i] = &strs[i][0]+(sizeof( MPI_Comm)-
			       ((long int)&strs[i][0])%sizeof(MPI_Comm));
	  if (intent=='i' || intent=='I' || intent=='b' || intent=='B')
	    *(MPI_Comm*)ps[i] = COMMPI_Comm_f2c( *(int*)args[i], MPI_Comm());
	  if (intent=='o' || intent=='O' || intent=='b' || intent=='B') 
	    needpostproc=true;
	}
	else if ( type == COM_MPI_COMMF && from_c) {
	  strs[i].resize( 2*sizeof( int));
	  ps[i] = &strs[i][0]+(sizeof(int)-((long int)&strs[i][0])%sizeof(int));
	  if (intent=='i' || intent=='I' || intent=='b' || intent=='B')
	    *(int*)ps[i] = COMMPI_Comm_c2f( *(MPI_Comm*)args[i]);
	  if (intent=='o' || intent=='O' || intent=='b' || intent=='B') 
	    needpostproc=true;
	}
	if (verb>1) {
	  switch (type) {
	  case COM_STRING:
	    std::cerr << "STRING\t@" << args[i] << "\t";
	    if ( args[i]) std::cerr << '\"' << (char*)ps[i] << '\"'; 
	    break;
	  case COM_CHAR: case COM_CHARACTER:
	    std::cerr << "CHAR  \t@" << args[i] << "\t";
	    if ( args[i]) std::cerr << '\'' << *(char*)args[i] << '\''; 
	    break;
	  case COM_DOUBLE:
	  case COM_DOUBLE_PRECISION: 
	    std::cerr << "double\t@" << args[i] << '\t';
	    if ( args[i]) std::cerr << *(double*)args[i]; 
	    break;
	  case COM_INT:
	  case COM_LONG:
	  case COM_INTEGER:
	    std::cerr << "int   \t@" << args[i] << '\t';
	    if ( args[i]) std::cerr << *(int*)args[i]; 
	    break;
	    case COM_FLOAT:
	  case COM_REAL:
	    std::cerr << "float \t@" << args[i] << '\t';
	    if ( args[i]) std::cerr << *(float*)args[i]; 
	    break;
	  case COM_LOGICAL:
	    std::cerr << "logical\t@" << args[i] << '\t';
	    if ( args[i]) std::cerr << *(int*)args[i]; 
	    break;
	  case COM_MPI_COMMC:
	    std::cerr << "MPI_Comm (C)\t@" << args[i] << '\t';
	    if ( args[i]) std::cerr << *(MPI_Comm*)args[i]; 
	    break;
	  case COM_MPI_COMMF:
	    std::cerr << "MPI_Comm (F)\t@" << args[i] << '\t';
	    if ( args[i]) std::cerr << *(int*)args[i]; 
	    break;
	  default:
	    std::cerr << "type(" << type << ")\t@" << args[i];
	  }
	}
      }
      else {
	int h=*(int*)args[i];
	if ( h == 0 && func->intent(i)<='Z') { 
	  // Optional attribute received a 0 attribute handle
	  ps[i] = NULL;
	  if ( verb>1)
	    std::cerr << "ZERO ATTRIBUTE HANDLE";
	  continue;
	}

	// attr must be const to void throwing exception when pointer is called
	const Attribute *attr = &get_attribute( h);
	if ( attr->is_const() && std::tolower(func->intent(i)) != 'i')
	  throw COM_exception( COM_ERR_ATTRIBUTE_CONST);

	if ( func->is_rawdata( i)) {
	  ps[i] = const_cast<void*>(attr->pointer());
	  if ( verb>1)
	    std::cerr << "VALUE OF\t@" << ps[i] 
		      << "\t\"" << _attr_map.name(h) << '"';
	}
	else {
	  ps[i] = const_cast<Attribute*>(attr);
	  if ( verb>1)
	    std::cerr << "METADATA\t@" << ps[i] 
		      << "\t\"" << _attr_map.name(h) << '"';
	}
	
	if ( _attr_map.is_immutable( h) &&
	     toupper(func->intent(i)) != 'I') {
	  throw COM_exception(COM_ERR_IMMUTABLE);
	}
      }
    }

    if (offset && func->is_rawdata(0) && 
	(attr = func->attribute())->data_type()==COM_F90POINTER) {
      COM_assertion_msg( _f90ptr_treat>=0, "No F90 pointer initalized");

      // Append pointer information
      if ( _f90ptr_treat == FPTR_APPEND) {
	std::pair<int,int> offs = get_f90pntoffsets( attr);
	
	// Add pointer information for Intel Compiler
	*plen = *(void**)((char*)const_cast<void*>(attr->pointer())+offs.second);
	++lcount; ++plen;
      }
    }

    for ( int i=count, iend=func->num_of_args(); i<iend; ++i) {
      if ( !func->is_optional( i)) throw COM_exception(COM_ERR_TOO_FEW_ARGS);
      ps[i] = NULL;
      if ( verb>1) {
	std::cerr << std::endl << "OPT\t" << func->intent(i) << ": ";
	std::cerr << ps[i];
      }
    }
    if ( verb) {
      if ( verb>1) std::cerr << std::endl << ')';
      std::cerr << std::endl;
    }
    
    // Profiling it
    double t = 0;
    if ( _profile_on) { 
//RAF    MPI_Comm comm = func->communicator();
//RAF    if (comm!=MPI_COMM_NULL) MPI_Barrier( comm);
      t = get_wtime();
#ifdef _CHARM_THREADED_
//RAF      if (comm!=MPI_COMM_NULL) MPI_Barrier( comm);
#endif
    }
    ++_depth;
    if ( verb>1 && lcount>0) {
      std::cerr << "Invoking function with " << lcount 
		<< " additional implicit arguments: " << std::endl;
      for ( int i=0; i<lcount; ++i) {
	long int p=(long int)ps[func->num_of_args()+i];
	if ( p<0 || p>MAX_NAMELEN)
	  std::cerr << "\t@" << (void *)p << std::endl;
	else
	  std::cerr << '\t' << (long int)p << std::endl;
      }
    }

    // Invoke the function
    (*func)( func->num_of_args()+lcount, ps);
    --_depth;

    if ( _profile_on) {
//RAF      MPI_Comm comm = func->communicator();
//RAF      if (comm!=MPI_COMM_NULL) MPI_Barrier( comm);

      double tnew = get_wtime(); 
#ifdef _CHARM_THREADED_
//RAF      if (comm!=MPI_COMM_NULL) MPI_Barrier( comm);
#endif

      _func_map.counts[wf]++;

      double sec = tnew-t;
      _func_map.wtimes_tree[wf] += sec;
      _func_map.wtimes_self[wf] += sec;
      if ( int(_timer.size()) > _depth) 
	_func_map.wtimes_self[wf] -= _timer[_depth];

      _timer.resize( _depth,0);
      if (_depth>0)  _timer[_depth-1] += sec;
      if (_depth==0) _func_map.wtimes_tree[0] += sec;
    }

    if ( verb) {
      std::cerr << "ROCCOM: DONE(" << _depth << ") " << std::endl;
    }

    // Copy back strings
    if ( needpostproc) {
      for ( int i=offset; i<count; ++i) {
	COM_Type type = func->data_type(i);
	char intent = func->intent(i);
	if ( func->is_literal(i) && type == COM_STRING) {
	  if (intent=='b' || intent=='B') {
	    int n = strs[i].size();
	    if ( n>0) std::memcpy( args[i], &strs[i][0], n-1);
	  }
	}
	else if ( type == COM_MPI_COMMC && !from_c) {
	  if (intent=='o' || intent=='O' || intent=='b' || intent=='B')
	    *(int*)args[i] = COMMPI_Comm_c2f( *(MPI_Comm*)ps[i]);
	}
	else if ( type == COM_MPI_COMMF && from_c) {
	  if (intent=='o' || intent=='O' || intent=='b' || intent=='B')
	    *(MPI_Comm*)args[i] = COMMPI_Comm_f2c( *(int*)ps[i], MPI_Comm());
	}
      }
    }

    _errorcode = 0;
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::call_function);
    char buf[10];

    std::sprintf( buf, "%d", wf);
    std::string msg = std::string( "When processing function ");
    if ( wf>0) msg.append( _func_map.name(wf));

    msg.append( " with handle "); msg.append( buf);
    proc_exception( ex, msg);
  }
}

void Roccom_base::
set_function_verbose( int i, int level) throw(int) {
  _func_map.verbs[i] = level;
}

void Roccom_base::
set_profiling( int i) {
  if ( _verb1>1) 
    std::cerr << "ROCCOM: init profiling level to " << i << std::endl; 
  _profile_on = i; 
  std::fill(_func_map.wtimes_tree.begin(),_func_map.wtimes_tree.end(), 0);
  std::fill(_func_map.wtimes_self.begin(),_func_map.wtimes_self.end(), 0);
  std::fill(_func_map.counts.begin(),_func_map.counts.end(), 0);
}

void Roccom_base::
set_profiling_barrier( int hdl, MPI_Comm comm) {
  if ( hdl == 0) return;
  try {
    if ( hdl < 0 || hdl>=_func_map.size()) 
      throw COM_exception( COM_ERR_INVALID_FUNCTION_HANDLE);

    if ( _verb1>1) 
      std::cerr << "ROCCOM: init profiling barrier for function handle " << hdl
		<< std::endl << "\tFunction: " << _func_map.name(hdl)
		<< " Communicator: " << comm << std::endl; 

    // Set the communicator only if MPI has been initialized. 
    if ( COMMPI_Initialized()) {
      if ( comm==MPI_COMM_NULL) comm = _comm;
      get_function( hdl).set_communicator( comm);
    }
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Roccom_base::set_profiling_barrier);
    char buf[100];
    std::sprintf( buf, "Could not get function with handle %d", hdl);
    proc_exception( ex, buf);
  }
}

void Roccom_base::
print_profile( const std::string &fname, const std::string &header) {
  if ( !_profile_on) return;

  if ( _verb1>1)
     std::cerr << "ROCCOM: Appending profile into file \"" << fname
               << '"' << std::endl;
  std::multimap< double, int>  profs;
  typedef std::multimap< double, int>::value_type MMVT;

  for ( int i=1, n=_func_map.wtimes_self.size(); i<n; ++i) {
    if ( _func_map.wtimes_self[i] > 0.0)
      profs.insert( MMVT( -_func_map.wtimes_self[i], i));
  }

  std::FILE *of = NULL;
  if ( fname.size() == 0)
    of = stdout;
  else {
    of = std::fopen( fname.c_str(), "a");
    if ( of == NULL) {
      std::cerr << "ROCCOM: Could not open file \"" << fname
		<< "\"\nROCCOM: Giving up profiling" << std::endl;
      _profile_on = false; return;
    }
  }
  std::fputc('\n', of);

  if ( header.size() == 0)
    std::fputs( "************************Roccom simple profiling tool\
************************", of);
  else
    std::fputs( header.c_str(), of);

  std::fprintf( of, "\n%32s%12s%14s%14s\n", "Function", "#calls", 
                "Time(tree)", "Time(self)");
  std::fputs( "-------------------------------------------------------\
---------------------\n", of);

  std::multimap< double, int>::const_iterator i, iend;
  for ( i=profs.begin(), iend=profs.end(); i!=iend; ++i) {
    std::fprintf( of, "%32.32s%12d%14g%14g\n", 
		  _func_map.name(i->second).c_str(),
		  _func_map.counts[i->second], 
		  _func_map.wtimes_tree[i->second],
		  _func_map.wtimes_self[i->second]);
  }
  std::fputs( "-------------------------------------------------------\
---------------------\n", of);
  std::fprintf( of, "%32s%40g\n", "Total(top level calls)", 
		_func_map.wtimes_tree[0]);

  if ( of != stdout) std::fclose( of);
}


int Roccom_base::get_sizeof( COM_Type type, int count) 
{ return Attribute::get_sizeof( type, count); }

Window &Roccom_base::
get_window( const std::string &wname) throw(COM_exception)  
{
  Window **w = _window_map.find( wname).second;
  if ( w ==NULL) 
    throw COM_exception( COM_ERR_WINDOW_NOTEXIST,
			 append_frame(wname, Roccom_base::get_window));
  return **w;
}

Attribute &Roccom_base::
get_attribute( const int handle) throw(COM_exception) {
  Attribute *attr=NULL;

  if ( handle > 0 && handle<_attr_map.size()) 
    attr = _attr_map[handle];
  if (attr==NULL) {
    static char buf[10];
    std::sprintf( buf, "%d", handle);
    throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_HANDLE,
			 append_frame(buf, Roccom_base::get_attribute));
  }
  return *attr; 
}

const Attribute &Roccom_base::
get_attribute( const int handle) const throw(COM_exception) { 
  const Attribute *attr=NULL;

  if ( handle > 0 && handle<_attr_map.size()) 
    attr = _attr_map[handle];
  if (attr==NULL) {
    static char buf[10];
    std::sprintf( buf, "%d", handle);
    throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_HANDLE,
			 append_frame(buf, Roccom_base::get_attribute));
  }
  return *attr; 
}

Function &Roccom_base::
get_function( const int handle) throw(COM_exception) { 
  Function *func=NULL;

  if ( handle > 0 && handle<_func_map.size()) 
    func = _func_map[handle];

  if (func==NULL) {
    static char buf[10];
    std::sprintf( buf, "%d", handle);
    throw COM_exception( COM_ERR_INVALID_FUNCTION_HANDLE,
			 append_frame(buf, Roccom_base::get_function));
  }
  return *func; 
}

const Function &Roccom_base::
get_function( const int handle) const throw(COM_exception) { 
  const Function *func=NULL;

  if ( handle > 0 && handle<_func_map.size()) 
    func = _func_map[handle];
  if (func==NULL) {
    static char buf[10];
    std::sprintf( buf, "%d", handle);
    throw COM_exception( COM_ERR_INVALID_FUNCTION_HANDLE,
			 append_frame(buf, Roccom_base::get_function));
  }
  return *func; 
}

int Roccom_base::split_name( const std::string &wa, std::string &wname, 
			     std::string &aname, bool tothrow) throw( COM_exception) 
{
  std::string::size_type ni = wa.find(".");

  if ( ni == std::string::npos) 
    wname = wa;
  else {
    wname = wa.substr( 0, ni);
    aname = wa.substr( ni+1, wname.size()-ni-1);
  }

  if ( wname.empty() || aname.empty()) 
    if ( tothrow)
      throw COM_exception(COM_ERR_INVALID_ATTRIBUTE_NAME,
			  append_frame(wa,Roccom_base::split_name));
    else
      return 1;

  return 0;
}

void Roccom_base::
proc_exception( const COM_exception &ex, const std::string &s) throw( int) 
{
  extern void printStackBacktrace();
  _errorcode = ex.ierr;

  std::cerr << '\n' << ex << s << std::endl;
  if ( _exception_on && _errorcode>=1000) {
    printStackBacktrace();
    throw ex.ierr;
  }
}

COM_END_NAME_SPACE

#if !defined(STATIC_LINK)   /// Circumvent problem with missing variables
void *p_xargc;
void *p_xargv;
#endif






