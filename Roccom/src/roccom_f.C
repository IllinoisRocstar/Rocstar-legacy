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
// $Id: roccom_f.C,v 1.50 2008/12/06 08:43:25 mtcampbe Exp $

/** \file roccom_f.C
 *  Contains the wrapper routines for the F90 binding
 *   of Roccom implementation.
 *  @see utilities.f90, roccomf90.h, mapptr.h
 */

/** \file roccomf90.h
 * Defines the prototypes of the F90 interface and should be
 * included by F90 codes.
 *  @see utilities.f90, roccomf90.h, mapptr.h
 */

/** \file utilities.f90
 *  Some F90 routines for handling LOGICAL variables and F90 pointers.
 *  @see roccomf90.h, utilities.f90, mapptr.h
 */

/* Author: Xiangmin Jiao */

#include <iostream>
#include <cstdlib>
#include <cstring>
#include "roccom.h"
#include "mapptr.h"
#include "roccom_assertion.h"

USE_COM_NAME_SPACE

#ifndef DOXYGEN_SHOULD_SKIP_THIS

using std::string;

//================================================================
//============== Functions for initialization and finalization ==
//================================================================

extern "C" void COM_F_FUNC2(com_init,COM_INIT)() 
{ Roccom_base::init( NULL, NULL); }
  
extern "C" void COM_F_FUNC2(com_finalize,COM_FINALIZE)() 
{ Roccom_base::finalize(); }

extern "C" int COM_F_FUNC2(com_initialized,COM_INITIALIZED)()
{ return Roccom_base::initialized(); }

// Allows Fortran to call the abort function in the C stardard 
// or MPI_Abort if MPI was initialized.
extern "C" void COM_F_FUNC2( com_abort, COM_ABORT) ( const int &ierr) 
{ Roccom_base::abort( ierr); }

extern "C" void COM_F_FUNC2(com_set_default_communicator, 
			    COM_SET_DEFAULT_COMMUNICATOR)( const int &comm)
{ Roccom_base::set_default_communicator( COMMPI_Comm_f2c(comm, MPI_Comm())); }

extern "C" int COM_F_FUNC2(com_get_default_communicator, 
			   COM_GET_DEFAULT_COMMUNICATOR)()
{ return COMMPI_Comm_c2f( Roccom_base::get_default_communicator()); }

//================================================================
//================== Load and unload modules =====================
//================================================================

extern "C" void COM_F_FUNC2(com_load_module, COM_LOAD_MODULE)
  ( const char *libname, const char *winname, 
    int l1, int l2) 
{
  COM_get_roccom()->load_module( std::string( libname, l1),
				 std::string( winname, l2));
}

extern "C" void COM_F_FUNC2( com_unload_module_1, COM_UNLOAD_MODULE_1)
  ( const char *libname, int l1) 
{
  COM_get_roccom()->unload_module( std::string( libname, l1), "");
}

extern "C" void COM_F_FUNC2( com_unload_module_2, COM_UNLOAD_MODULE_2)
  ( const char *libname, const char *winname, int l1, int l2) 
{
  COM_get_roccom()->unload_module( std::string( libname, l1),
				   std::string( winname, l2));
}

//================================================================
//============== Functions for window management =================
//================================================================
// Create a window with the given name.
#define CHKLEN(x) COM_assertion( x>=0 && x<MAX_NAMELEN)

class Roccom_base_friend : public Roccom_base {
public:
  using Roccom_base::_f90ptr_treat;
};

extern "C" int COM_F_FUNC2( com_chkptr_c, COM_CHKPTR_C) 
  ( const int &stage, const char *str1, void *ptr1, 
    const char *str2, void *ptr2, 
    int len1, int len2, int len3, int len4);

inline int get_f90ptr_treat() {
  Roccom_base_friend *rcom = (Roccom_base_friend *)COM_get_roccom();

  if ( rcom->_f90ptr_treat<0)
    rcom->_f90ptr_treat = COM_F_FUNC2( com_chkptr_c, COM_CHKPTR_C)
      ( 0, 0, 0, 0, 0, 0, 0, 0, 0);

  return rcom->_f90ptr_treat;
}

extern "C" void COM_F_FUNC2(com_new_window_null,COM_NEW_WINDOW_NULL)
  ( const char *w_str, int w_len) 
{ CHKLEN(w_len); COM_get_roccom()->new_window( string( w_str, w_len), MPI_COMM_NULL); }

extern "C" void COM_F_FUNC2(com_new_window_mpi,COM_NEW_WINDOW_MPI)
  ( const char *w_str, const int &c, int w_len) {
  CHKLEN(w_len); 
  COM_get_roccom()->new_window
    ( string( w_str, w_len), COMMPI_Comm_f2c(c, MPI_Comm())); 
}

// Delete a window with the given name
extern "C" void COM_F_FUNC2(com_delete_window,COM_DELETE_WINDOW)
  ( const char *str, int len)
{ CHKLEN(len); COM_get_roccom()->delete_window( string( str, len)); }

extern "C" void COM_F_FUNC2(com_window_init_done_1arg, COM_WINDOW_INIT_DONE_1ARG)
  ( const char *w_str, int w_len) 
{ CHKLEN(w_len); COM_get_roccom()->window_init_done( string(w_str,w_len)); }

extern "C" void COM_F_FUNC2(com_window_init_done_2arg, COM_WINDOW_INIT_DONE_2ARG)
  ( const char *w_str, int *c, int w_len) 
{ CHKLEN(w_len); COM_get_roccom()->window_init_done( string(w_str,w_len), *c); }

extern "C" void COM_F_FUNC2(com_delete_pane,COM_DELETE_PANE)
  ( const char *w_str, const int &pid, int w_len) 
{ CHKLEN(w_len); COM_get_roccom()->delete_pane( string(w_str,w_len), pid); }

extern "C" void COM_F_FUNC2(com_new_attribute,COM_NEW_ATTRIBUTE)
  ( const char *wa_str, const char &loc, const int &type,
    const int &size, const char *u_str,
    int wa_len, int l_len, int u_len) 
{
  CHKLEN((int)wa_len); CHKLEN((int)l_len); CHKLEN((int)u_len);
  COM_get_roccom()->new_attribute( string( wa_str,(int)wa_len), loc, type, 
				   size, string( u_str,(int)u_len));
}

extern "C" void COM_F_FUNC2(com_delete_attribute,COM_DELETE_ATTRIBUTE)
  ( const char *wa_str, int wa_len) 
{
  CHKLEN(wa_len); 
  COM_get_roccom()->delete_attribute( string( wa_str,wa_len));
}

extern "C" void COM_F_FUNC2(com_set_size1, COM_SET_SIZE1)
  ( const char *wa_str, const int &pane_id, const int &size, int len)
{ 
  CHKLEN( len);
  COM_get_roccom()->set_size( std::string(wa_str,len), pane_id, size); 
}

extern "C" void COM_F_FUNC2(com_set_size2, COM_SET_SIZE2)
  ( const char *wa_str, const int &pane_id, 
    const int &size, const int &ng, int len)
{ 
  CHKLEN( len);
  COM_get_roccom()->set_size( std::string(wa_str,len), pane_id, size, ng); 
}

// Register an attribute name.
extern "C" void COM_F_FUNC2( com_map_cptr, COM_MAP_CPTR)
  ( void *x, void **p) 
{ if ( x==(void*)p) *p=NULL; else *p = x; }

// Register an attribute name.
extern "C" void COM_F_FUNC2( com_map_cptr_chr, COM_MAP_CPTR_CHR)
  ( void *x, void **p, void *, void*) 
{ if ( x==(void*)p) *p=NULL; else *p = x; }

// Register an attribute name.
extern "C" void COM_F_FUNC2( com_map_cptr_int, COM_MAP_CPTR_INT)
  ( void *x, void **p) 
{ if ( x==(void*)p) *p=NULL; else *p = x; }

// Register an attribute name.
extern "C" void COM_F_FUNC2( com_map_cptr_flt, COM_MAP_CPTR_FLT)
  ( void *x, void **p) 
{ if ( x==(void*)p) *p=NULL; else *p = x; }

// Register an attribute name.
extern "C" void COM_F_FUNC2( com_map_cptr_dbl, COM_MAP_CPTR_DBL)
  ( void *x, void **p) 
{ if ( x==(void*)p) *p=NULL; else *p = x; }

extern "C" void COM_F_FUNC2( com_set_array_null, COM_SET_ARRAY_NULL)
  ( const char *wa_str, const int &pane_id, int len)
{ COM_get_roccom()->set_array( std::string(wa_str,len), 
			       pane_id, NULL); }

extern "C" void COM_F_FUNC2( com_set_array_const_null, COM_SET_ARRAY_CONST_NULL)
  ( const char *wa_str, const int &pane_id, int len)
{ COM_get_roccom()->set_array( std::string(wa_str,len), 
			       pane_id, NULL); }

extern "C" void COM_F_FUNC2( com_set_external, COM_SET_EXTERNAL)
  ( const char *wa_str, const int &pane_id, void *p, int len)
{ COM_get_roccom()->set_array( std::string(wa_str,len),
			       pane_id, p); }

extern "C" void COM_F_FUNC2( com_set_external_const, COM_SET_EXTERNAL_CONST)
  ( const char *wa_str, const int &pane_id, void *p, int len)
{ COM_get_roccom()->set_array( std::string(wa_str,len),
			       pane_id, p, true); }

// Allows Fortran to execute a shell command as C does.
extern "C" int COM_F_FUNC2( com_call_system, COM_CALL_SYSTEM) 
  ( const char *com, int len)
{ std::string command(com, len); return system( command.c_str()); }

// Allows Fortran to call the exit function in the C standard.
extern "C" void COM_F_FUNC2( com_call_exit, COM_CALL_EXIT) 
  ( const int &ierr)
{ exit(ierr); }

// Allows Fortran to call the atexit function in the C standard.
extern "C" void COM_F_FUNC2( com_call_atexit, COM_CALL_ATEXIT) 
  ( Func_ptr func)
{ atexit(func); }

template <int dim, COM_Type type>
void *
com_get_address_f( void *addr, int l=0) {
  Func_ptr f;
  
  if ( dim==0) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_getptr_int0d,COM_GETPTR_INT0D);
    else if ( type==COM_REAL)
      f = COM_F_FUNC2(com_getptr_flt0d,COM_GETPTR_FLT0D);
    else if ( type==COM_DOUBLE)
      f = COM_F_FUNC2(com_getptr_dbl0d,COM_GETPTR_DBL0D);
    else
      COM_assertion(false);
  }
  else if ( dim==1) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_getptr_int1d,COM_GETPTR_INT1D);
    else if ( type==COM_REAL)		       
      f = COM_F_FUNC2(com_getptr_flt1d,COM_GETPTR_FLT1D);
    else if ( type==COM_DOUBLE)		       
      f = COM_F_FUNC2(com_getptr_dbl1d,COM_GETPTR_DBL1D); 
    else
      COM_assertion(false);
  }
  else if (dim==2) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_getptr_int2d,COM_GETPTR_INT2D);
    else if ( type==COM_REAL)		       
      f = COM_F_FUNC2(com_getptr_flt2d,COM_GETPTR_FLT2D);
    else if ( type==COM_DOUBLE)		       
      f = COM_F_FUNC2(com_getptr_dbl2d,COM_GETPTR_DBL2D);
    else
      COM_assertion(false);
  }
  else if (dim==3) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_getptr_int3d,COM_GETPTR_INT3D);
    else if ( type==COM_REAL)		       
      f = COM_F_FUNC2(com_getptr_flt3d,COM_GETPTR_FLT3D);
    else if ( type==COM_DOUBLE)		       
      f = COM_F_FUNC2(com_getptr_dbl3d,COM_GETPTR_DBL3D);
    else
      COM_assertion(false);
  }
  else
    COM_assertion(false);

  void *ptr=NULL;
  if ( l==0) {
    typedef void(*Func)(void*,void**);
    (*(Func)f)( addr, &ptr);
  }
  else {
    typedef void(*Func)(void*,void**, int);
    (*(Func)f)( addr, &ptr, l);
  }

  return ptr;
}

template <int dim, COM_Type type>
void
com_set_address_f( Roccom_base::Pointer_descriptor &ptr, 
		   void *addr, int l=0) {
  Func_ptr f;

  if ( dim==0) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_mapptr_int0d,COM_MAPPTR_INT0D);
    else if ( type==COM_REAL)
      f = COM_F_FUNC2(com_mapptr_flt0d,COM_MAPPTR_FLT0D); 
    else if ( type==COM_DOUBLE)
      f = COM_F_FUNC2(com_mapptr_dbl0d,COM_MAPPTR_DBL0D);
    else
      COM_assertion(false);
    int tonull=(ptr.at()==NULL);

    if ( l) {
      typedef void(*Func)(void*,int*,void*, int);
      (*(Func)f)( addr, &tonull, ptr.at(), l);
    }
    else {
      typedef void(*Func)(void*,int*,void*);
      (*(Func)f)( addr, &tonull, ptr.at());
    }
  }
  else if ( dim==1) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_mapptr_int1d,COM_MAPPTR_INT1D);
    else if ( type==COM_REAL)
      f = COM_F_FUNC2(com_mapptr_flt1d,COM_MAPPTR_FLT1D); 
    else if ( type==COM_DOUBLE)
      f = COM_F_FUNC2(com_mapptr_dbl1d,COM_MAPPTR_DBL1D);
    else
      COM_assertion(false);

    int tonull=(ptr.at()==NULL);
    if ( l) {
      typedef void(*Func)(void*,int*,void*,void*,int);
      (*(Func)f)( addr, &tonull, ptr.at(), &ptr.n1, l);
    }
    else {
      typedef void(*Func)(void*,int*,void*,void*);
      (*(Func)f)( addr, &tonull,ptr.at(), &ptr.n1);
    }
  }
  else if (dim==2) {
    if (type==COM_INTEGER)
      f = COM_F_FUNC2(com_mapptr_int2d,COM_MAPPTR_INT2D);
    else if ( type==COM_REAL)
      f = COM_F_FUNC2(com_mapptr_flt2d,COM_MAPPTR_FLT2D); 
    else if ( type==COM_DOUBLE)
      f = COM_F_FUNC2(com_mapptr_dbl2d,COM_MAPPTR_DBL2D);
    else
      COM_assertion(false);

    int tonull=(ptr.at()==NULL);
    if ( l) {
      typedef void(*Func)(void*,int*,void*,void*,void*,int);
      (*(Func)f)( addr, &tonull, ptr.at(), &ptr.n1, &ptr.n2, l);
    }
    else {
      typedef void(*Func)(void*,int*,void*,void*,void*);
      (*(Func)f)( addr, &tonull,ptr.at(), &ptr.n1, &ptr.n2);
    }
  }
  else
    COM_assertion(false);
}

template <int dim, COM_Type type>
void 
com_set_array_helper( const std::string &waname, int pane_id, void *addr, 
		      int strd, int cap, int l=0) {
  void *ptr=com_get_address_f<dim, type>( addr, l);

  COM_get_roccom()->set_array( waname, pane_id, ptr, strd, cap);
}

// The following set_array*_f90 routines are called by functions
// defined in utilities.f90.
#define COM_SET_ARRAY_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    int wa_len, int) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->set_array( string(wa_str,wa_len), pid, addr); \
}

COM_SET_ARRAY_SCALAR( com_set_array_chr, COM_SET_ARRAY_CHR);
COM_SET_ARRAY_SCALAR( com_set_array_int, COM_SET_ARRAY_INT);
COM_SET_ARRAY_SCALAR( com_set_array_flt, COM_SET_ARRAY_FLT);
COM_SET_ARRAY_SCALAR( com_set_array_dbl, COM_SET_ARRAY_DBL);

template <int dim, COM_Type type>
void 
com_set_array_f( const char *waname, int pane_id, void *addr, 
		 int strd, int cap, int l1, int l2) {
  int f90ptr_treat = get_f90ptr_treat();
  
  if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
    int wa_len=l2; CHKLEN(wa_len);
    com_set_array_helper<dim,type>( string(waname, wa_len), pane_id, addr, 
				    strd, cap, l1);
  }
  else { 
    int wa_len=l1; CHKLEN(wa_len);
    if ( f90ptr_treat == Roccom_base::FPTR_APPEND) {
      com_set_array_helper<dim,type>( string(waname, wa_len), pane_id, addr, 
				      strd, cap, l2);
    }
    else {
      com_set_array_helper<dim,type>( string(waname, wa_len), pane_id, addr, 
				      strd, cap);
    }
  }
}

// The following set_array*_f90 routines are called by functions
// defined in utilities.f90.
#define COM_SET_ARRAY_PTR( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    int l1, int l2) \
{ \
  com_set_array_f<dim,type>( wa_str, pid, addr, 0, 0, l1, l2);\
}

COM_SET_ARRAY_PTR( com_set_array_int1d, COM_SET_ARRAY_INT1D, 1, COM_INTEGER);
COM_SET_ARRAY_PTR( com_set_array_flt1d, COM_SET_ARRAY_FLT1D, 1, COM_REAL);
COM_SET_ARRAY_PTR( com_set_array_dbl1d, COM_SET_ARRAY_DBL1D, 1, COM_DOUBLE);
COM_SET_ARRAY_PTR( com_set_array_int2d, COM_SET_ARRAY_INT2D, 2, COM_INTEGER);
COM_SET_ARRAY_PTR( com_set_array_flt2d, COM_SET_ARRAY_FLT2D, 2, COM_REAL);
COM_SET_ARRAY_PTR( com_set_array_dbl2d, COM_SET_ARRAY_DBL2D, 2, COM_DOUBLE);
COM_SET_ARRAY_PTR( com_set_array_int3d, COM_SET_ARRAY_INT3D, 3, COM_INTEGER);
COM_SET_ARRAY_PTR( com_set_array_flt3d, COM_SET_ARRAY_FLT3D, 3, COM_REAL);
COM_SET_ARRAY_PTR( com_set_array_dbl3d, COM_SET_ARRAY_DBL3D, 3, COM_DOUBLE);

#define COM_SET_ARRAY_STRD_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid,  void *addr, const int &strd, \
    int wa_len) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->set_array( string(wa_str,wa_len), pid, addr, strd); \
}

COM_SET_ARRAY_STRD_SCALAR( com_set_array_int_strd, COM_SET_ARRAY_INT_STRD);
COM_SET_ARRAY_STRD_SCALAR( com_set_array_flt_strd, COM_SET_ARRAY_FLT_STRD);
COM_SET_ARRAY_STRD_SCALAR( com_set_array_dbl_strd, COM_SET_ARRAY_DBL_STRD);

// The following set_array*_f90 routines are called by functions
// defined in utilities.f90.
#define COM_SET_ARRAY_STRD_PTR( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    const int &strd, int l1, int l2) \
{ \
  com_set_array_f<dim,type>( wa_str, pid, addr, strd, 0, l1, l2);\
}

COM_SET_ARRAY_STRD_PTR( com_set_array_int1d_strd, COM_SET_ARRAY_INT1D_STRD, 
			1, COM_INTEGER);
COM_SET_ARRAY_STRD_PTR( com_set_array_flt1d_strd, COM_SET_ARRAY_FLT1D_STRD, 
			1, COM_REAL);
COM_SET_ARRAY_STRD_PTR( com_set_array_dbl1d_strd, COM_SET_ARRAY_DBL1D_STRD, 
			1, COM_DOUBLE);
COM_SET_ARRAY_STRD_PTR( com_set_array_int2d_strd, COM_SET_ARRAY_INT2D_STRD, 
			2, COM_INTEGER);
COM_SET_ARRAY_STRD_PTR( com_set_array_flt2d_strd, COM_SET_ARRAY_FLT2D_STRD, 
			2, COM_REAL);
COM_SET_ARRAY_STRD_PTR( com_set_array_dbl2d_strd, COM_SET_ARRAY_DBL2D_STRD, 
			2, COM_DOUBLE);
COM_SET_ARRAY_STRD_PTR( com_set_array_int3d_strd, COM_SET_ARRAY_INT3D_STRD, 
			3, COM_INTEGER);
COM_SET_ARRAY_STRD_PTR( com_set_array_flt3d_strd, COM_SET_ARRAY_FLT3D_STRD, 
			3, COM_REAL);
COM_SET_ARRAY_STRD_PTR( com_set_array_dbl3d_strd, COM_SET_ARRAY_DBL3D_STRD, 
			3, COM_DOUBLE);

#define COM_SET_ARRAY_DYN_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, const int &strd, \
    const int &cap, int wa_len, int) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->set_array( string(wa_str,wa_len), pid, addr, strd, cap); \
}

COM_SET_ARRAY_DYN_SCALAR( com_set_array_int_dyn, COM_SET_ARRAY_INT_DYN);
COM_SET_ARRAY_DYN_SCALAR( com_set_array_flt_dyn, COM_SET_ARRAY_FLT_DYN);
COM_SET_ARRAY_DYN_SCALAR( com_set_array_dbl_dyn, COM_SET_ARRAY_DBL_DYN);

#define COM_SET_ARRAY_DYN_PTR( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    const int &strd, const int &cap, int l1, int l2) \
{ \
  com_set_array_f<dim,type>( wa_str, pid, addr, strd, cap, l1, l2); \
}

COM_SET_ARRAY_DYN_PTR( com_set_array_int1d_dyn, COM_SET_ARRAY_INT1D_DYN, 
		       1, COM_INTEGER);
COM_SET_ARRAY_DYN_PTR( com_set_array_flt1d_dyn, COM_SET_ARRAY_FLT1D_DYN, 
		       1, COM_REAL);
COM_SET_ARRAY_DYN_PTR( com_set_array_dbl1d_dyn, COM_SET_ARRAY_DBL1D_DYN, 
		       1, COM_DOUBLE);
COM_SET_ARRAY_DYN_PTR( com_set_array_int2d_dyn, COM_SET_ARRAY_INT2D_DYN, 
		       2, COM_INTEGER);
COM_SET_ARRAY_DYN_PTR( com_set_array_flt2d_dyn, COM_SET_ARRAY_FLT2D_DYN, 
		       2, COM_REAL);
COM_SET_ARRAY_DYN_PTR( com_set_array_dbl2d_dyn, COM_SET_ARRAY_DBL2D_DYN, 
		       2, COM_DOUBLE);
COM_SET_ARRAY_DYN_PTR( com_set_array_int3d_dyn, COM_SET_ARRAY_INT3D_DYN, 
		       3, COM_INTEGER);
COM_SET_ARRAY_DYN_PTR( com_set_array_flt3d_dyn, COM_SET_ARRAY_FLT3D_DYN, 
		       3, COM_REAL);
COM_SET_ARRAY_DYN_PTR( com_set_array_dbl3d_dyn, COM_SET_ARRAY_DBL3D_DYN, 
		       3, COM_DOUBLE);

template <int dim, COM_Type type>
void 
com_set_array_const_helper(const std::string &waname, int pane_id, void *addr, 
			   int strd, int cap, int l=0) {

  void *ptr=com_get_address_f<dim,type>( addr, l);

  COM_get_roccom()->set_array( waname, pane_id, ptr, strd, cap, true);
}

// The following set_array_const*_f90 routines are called by functions
// defined in utilities.f90.
#define COM_SET_ARRAY_CONST_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    int wa_len, int) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->set_array( string(wa_str,wa_len), pid, addr, true); \
}

COM_SET_ARRAY_CONST_SCALAR( com_set_array_const_chr, COM_SET_ARRAY_CONST_CHR);
COM_SET_ARRAY_CONST_SCALAR( com_set_array_const_int, COM_SET_ARRAY_CONST_INT);
COM_SET_ARRAY_CONST_SCALAR( com_set_array_const_flt, COM_SET_ARRAY_CONST_FLT);
COM_SET_ARRAY_CONST_SCALAR( com_set_array_const_dbl, COM_SET_ARRAY_CONST_DBL);

template <int dim, COM_Type type>
void 
com_set_array_const_f( const char *waname, int pane_id, void *addr, 
		       int strd, int cap, int l1, int l2) {
  int f90ptr_treat = get_f90ptr_treat();

  if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
    int wa_len=l2; CHKLEN(wa_len);
    com_set_array_const_helper<dim,type>( string(waname, wa_len), pane_id, 
					  addr, strd, cap, l1);
  }
  else {
    int wa_len=l1; CHKLEN(wa_len);
    if ( f90ptr_treat == Roccom_base::FPTR_APPEND)
      com_set_array_const_helper<dim,type>( string(waname, wa_len), pane_id, 
					    addr, strd, cap, l2);
    else
      com_set_array_const_helper<dim,type>( string(waname, wa_len), pane_id, 
					    addr, strd, cap);
  }
}

// The following set_array_const*_f90 routines are called by functions
// defined in utilities.f90.
#define COM_SET_ARRAY_CONST_PTR( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    int l1, int l2) \
{ \
  com_set_array_const_f<dim,type>( wa_str, pid, addr, 0, 0, l1, l2);\
}

COM_SET_ARRAY_CONST_PTR( com_set_array_const_int1d, 
			 COM_SET_ARRAY_CONST_INT1D, 1, COM_INTEGER);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_flt1d, 
			 COM_SET_ARRAY_CONST_FLT1D, 1, COM_REAL);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_dbl1d, 
			 COM_SET_ARRAY_CONST_DBL1D, 1, COM_DOUBLE);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_int2d, 
			 COM_SET_ARRAY_CONST_INT2D, 2, COM_INTEGER);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_flt2d, 
			 COM_SET_ARRAY_CONST_FLT2D, 2, COM_REAL);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_dbl2d, 
			 COM_SET_ARRAY_CONST_DBL2D, 2, COM_DOUBLE);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_int3d, 
			 COM_SET_ARRAY_CONST_INT3D, 3, COM_INTEGER);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_flt3d, 
			 COM_SET_ARRAY_CONST_FLT3D, 3, COM_REAL);
COM_SET_ARRAY_CONST_PTR( com_set_array_const_dbl3d, 
			 COM_SET_ARRAY_CONST_DBL3D, 3, COM_DOUBLE);

#define COM_SET_ARRAY_CONST_STRD_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid,  void *addr, int &strd, \
    int wa_len) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->set_array( string(wa_str,wa_len), pid, addr, strd, true); \
}

COM_SET_ARRAY_CONST_STRD_SCALAR( com_set_array_const_int_strd, 
				 COM_SET_ARRAY_CONST_INT_STRD);
COM_SET_ARRAY_CONST_STRD_SCALAR( com_set_array_const_flt_strd, 
				 COM_SET_ARRAY_CONST_FLT_STRD);
COM_SET_ARRAY_CONST_STRD_SCALAR( com_set_array_const_dbl_strd, 
				 COM_SET_ARRAY_CONST_DBL_STRD);

// The following set_array_const*_f90 routines are called by functions
// defined in utilities.f90.
#define COM_SET_ARRAY_CONST_STRD_PTR( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    const int &strd, int l1, int l2) \
{ \
  com_set_array_const_f<dim,type>( wa_str, pid, addr, strd, 0, l1, l2);\
}

COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_int1d_strd, 
			      COM_SET_ARRAY_CONST_INT1D_STRD, 1, COM_INTEGER);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_flt1d_strd, 
			      COM_SET_ARRAY_CONST_FLT1D_STRD, 1, COM_REAL);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_dbl1d_strd, 
			      COM_SET_ARRAY_CONST_DBL1D_STRD, 1, COM_DOUBLE);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_int2d_strd, 
			      COM_SET_ARRAY_CONST_INT2D_STRD, 2, COM_INTEGER);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_flt2d_strd, 
			      COM_SET_ARRAY_CONST_FLT2D_STRD, 2, COM_REAL);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_dbl2d_strd, 
			      COM_SET_ARRAY_CONST_DBL2D_STRD, 2, COM_DOUBLE);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_int3d_strd, 
			      COM_SET_ARRAY_CONST_INT3D_STRD, 3, COM_INTEGER);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_flt3d_strd, 
			      COM_SET_ARRAY_CONST_FLT3D_STRD, 3, COM_REAL);
COM_SET_ARRAY_CONST_STRD_PTR( com_set_array_const_dbl3d_strd, 
			      COM_SET_ARRAY_CONST_DBL3D_STRD, 3, COM_DOUBLE);

#define COM_SET_ARRAY_CONST_DYN_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, const int &strd, \
    const int &cap, int wa_len, int) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->set_array( string(wa_str,wa_len), pid, addr, \
			       strd, cap, true); \
}

COM_SET_ARRAY_CONST_DYN_SCALAR( com_set_array_const_int_dyn, 
				COM_SET_ARRAY_CONST_INT_DYN);
COM_SET_ARRAY_CONST_DYN_SCALAR( com_set_array_const_flt_dyn, 
				COM_SET_ARRAY_CONST_FLT_DYN);
COM_SET_ARRAY_CONST_DYN_SCALAR( com_set_array_const_dbl_dyn, 
				COM_SET_ARRAY_CONST_DBL_DYN);

#define COM_SET_ARRAY_CONST_DYN_PTR( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pid, void *addr, \
    const int &strd, const int &cap, int l1, int l2) \
{ \
  com_set_array_const_f<dim,type>( wa_str, pid, addr, strd, cap, l1, l2); \
}

COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_int1d_dyn, 
			     COM_SET_ARRAY_CONST_INT1D_DYN, 1, COM_INTEGER);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_flt1d_dyn, 
			     COM_SET_ARRAY_CONST_FLT1D_DYN, 1, COM_REAL);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_dbl1d_dyn, 
			     COM_SET_ARRAY_CONST_DBL1D_DYN, 1, COM_DOUBLE);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_int2d_dyn, 
			     COM_SET_ARRAY_CONST_INT2D_DYN, 2, COM_INTEGER);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_flt2d_dyn, 
			     COM_SET_ARRAY_CONST_FLT2D_DYN, 2, COM_REAL);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_dbl2d_dyn, 
			     COM_SET_ARRAY_CONST_DBL2D_DYN, 2, COM_DOUBLE);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_int3d_dyn, 
			     COM_SET_ARRAY_CONST_INT3D_DYN, 3, COM_INTEGER);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_flt3d_dyn, 
			     COM_SET_ARRAY_CONST_FLT3D_DYN, 3, COM_REAL);
COM_SET_ARRAY_CONST_DYN_PTR( com_set_array_const_dbl3d_dyn, 
			     COM_SET_ARRAY_CONST_DBL3D_DYN, 3, COM_DOUBLE);

#define COM_SET_BOUNDS( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pane_id, \
    const void *lbound, const void *ubound, int len) \
{ \
  CHKLEN(len);\
  COM_get_roccom()->set_bounds( string(wa_str,len), pane_id, lbound, ubound); \
}

COM_SET_BOUNDS(com_set_bounds_int, COM_SET_BOUNDS_INT);
COM_SET_BOUNDS(com_set_bounds_flt, COM_SET_BOUNDS_FLT);
COM_SET_BOUNDS(com_set_bounds_dbl, COM_SET_BOUNDS_DBL);

enum Access_mode { AM_ALLOC, AM_RESIZE, AM_GET, AM_GETC, AM_COPY};
extern "C" void COM_F_FUNC2( com_alloc_array_win, COM_ALLOC_ARRAY_WIN)
  ( const char *wa_str, int len)
{ COM_get_roccom()->allocate_array( std::string( wa_str, len)); }

extern "C" void COM_F_FUNC2( com_alloc_array_pane, COM_ALLOC_ARRAY_PANE)
  ( const char *wa_str, const int &pane_id, int len)
{ COM_get_roccom()->allocate_array( std::string( wa_str, len), pane_id); }

template <int dim, COM_Type type, Access_mode mode>
void 
com_obtain_array_helper( const std::string &waname, int pane_id, void *addr, 
			 int &strd, int &cap, int offset, int l=0) {

  if ( mode != AM_RESIZE && strd<0) strd=0;

  Roccom_base::Pointer_descriptor ptr(NULL, dim);
  if ( mode==AM_ALLOC) {
    COM_get_roccom()->allocate_array( waname, pane_id, NULL, strd, cap);
    COM_get_roccom()->get_array( waname, pane_id, ptr);
  }
  else if ( mode==AM_RESIZE) {
    COM_get_roccom()->resize_array( waname, pane_id, NULL, strd, cap);
    COM_get_roccom()->get_array( waname, pane_id, ptr);
  }
  else if ( mode == AM_COPY) {
    if ( dim==0) ptr.ptr = addr; // If dim==0, it is not a pointer
    else ptr.ptr = com_get_address_f<dim,type>( addr, l);

    COM_get_roccom()->copy_array( waname, pane_id, ptr.ptr,
				  strd, cap, offset);
  }
  else {
    COM_get_roccom()->get_array( waname, pane_id, ptr, &strd, 
				 &cap, mode==AM_GETC);
  }
  
  if ( mode != AM_COPY) 
    com_set_address_f<dim, type>( ptr, addr, l);
}

template <int dim, COM_Type type, Access_mode mode>
void 
com_obtain_array_f( const char *waname, int pane_id, void *addr, 
		    int &strd, int &cap, int offset,
		    int l1, int l2) {

  if ( dim==0 && mode==AM_COPY) { // Not a Fortran pointer
    int wa_len=l1; CHKLEN(wa_len);
    com_obtain_array_helper<dim,type,mode>( string(waname, wa_len), pane_id, 
					    addr, strd, cap, offset);
  }
  else {
    int f90ptr_treat = get_f90ptr_treat();
    
    if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
      int wa_len=l2; CHKLEN(wa_len);
      com_obtain_array_helper<dim,type,mode>( string(waname, wa_len), pane_id,
					      addr, strd, cap, offset, l1);
    }
    else {
      int wa_len=l1; CHKLEN(wa_len);
      if ( f90ptr_treat == Roccom_base::FPTR_APPEND) 
	com_obtain_array_helper<dim,type,mode>( string(waname, wa_len), pane_id, 
						addr, strd, cap, offset, l2);
      else 
	com_obtain_array_helper<dim,type,mode>( string(waname, wa_len), pane_id, 
						addr, strd, cap, offset);
    }
  }
}

#define COM_OBTAIN_ARRAY( func, FUNC, dim, type, mode) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *waname, const int &pane_id, \
    void *addr, int l1, int l2) \
{ \
  int minusone=-1, zero = 0; \
  com_obtain_array_f<dim,type,mode>( waname, pane_id, addr, \
				     minusone, zero, zero, l1, l2); \
}

COM_OBTAIN_ARRAY(com_alloc_array_int, COM_ALLOC_ARRAY_INT, 
		 0, COM_INTEGER,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_int1d, COM_ALLOC_ARRAY_INT1D, 
		 1, COM_INTEGER,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_int2d, COM_ALLOC_ARRAY_INT2D, 
		 2, COM_INTEGER,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_flt, COM_ALLOC_ARRAY_FLT, 
		 0, COM_REAL,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_flt1d, COM_ALLOC_ARRAY_FLT1D, 
		 1, COM_REAL,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_flt2d, COM_ALLOC_ARRAY_FLT2D, 
		 2, COM_REAL,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_dbl, COM_ALLOC_ARRAY_DBL, 
		 0, COM_DOUBLE,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_dbl1d, COM_ALLOC_ARRAY_DBL1D, 
		 1, COM_DOUBLE,AM_ALLOC);
COM_OBTAIN_ARRAY(com_alloc_array_dbl2d, COM_ALLOC_ARRAY_DBL2D, 
		 2, COM_DOUBLE,AM_ALLOC);

extern "C" void COM_F_FUNC2( com_resize_array_win, COM_RESIZE_ARRAY_WIN)
  ( const char *wa_str, int len)
{ COM_get_roccom()->resize_array( std::string( wa_str, len)); }

extern "C" void COM_F_FUNC2( com_resize_array_pane, COM_RESIZE_ARRAY_PANE)
  ( const char *wa_str, const int &pane_id, int len)
{ COM_get_roccom()->resize_array( std::string( wa_str, len), pane_id); }

COM_OBTAIN_ARRAY(com_resize_array_int, COM_RESIZE_ARRAY_INT, 
		0, COM_INTEGER,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_int1d, COM_RESIZE_ARRAY_INT1D, 
		1, COM_INTEGER,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_int2d, COM_RESIZE_ARRAY_INT2D, 
		2, COM_INTEGER,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_flt, COM_RESIZE_ARRAY_FLT, 
		0, COM_REAL,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_flt1d, COM_RESIZE_ARRAY_FLT1D, 
		1, COM_REAL,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_flt2d, COM_RESIZE_ARRAY_FLT2D, 
		2, COM_REAL,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_dbl, COM_RESIZE_ARRAY_DBL, 
		0, COM_DOUBLE,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_dbl1d, COM_RESIZE_ARRAY_DBL1D, 
		1, COM_DOUBLE,AM_RESIZE);
COM_OBTAIN_ARRAY(com_resize_array_dbl2d, COM_RESIZE_ARRAY_DBL2D, 
		2, COM_DOUBLE,AM_RESIZE);

COM_OBTAIN_ARRAY(com_get_array_int, COM_GET_ARRAY_INT, 
		0, COM_INTEGER,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_int1d, COM_GET_ARRAY_INT1D, 
		1, COM_INTEGER,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_int2d, COM_GET_ARRAY_INT2D, 
		2, COM_INTEGER,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_flt, COM_GET_ARRAY_FLT, 
		0, COM_REAL,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_flt1d, COM_GET_ARRAY_FLT1D, 
		1, COM_REAL,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_flt2d, COM_GET_ARRAY_FLT2D, 
		2, COM_REAL,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_dbl, COM_GET_ARRAY_DBL, 
		0, COM_DOUBLE,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_dbl1d, COM_GET_ARRAY_DBL1D, 
		1, COM_DOUBLE,AM_GET);
COM_OBTAIN_ARRAY(com_get_array_dbl2d, COM_GET_ARRAY_DBL2D, 
		2, COM_DOUBLE,AM_GET);

COM_OBTAIN_ARRAY(com_get_array_const_int, COM_GET_ARRAY_CONST_INT, 
		0, COM_INTEGER,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_int1d, COM_GET_ARRAY_CONST_INT1D, 
		1, COM_INTEGER,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_int2d, COM_GET_ARRAY_CONST_INT2D, 
		2, COM_INTEGER,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_flt, COM_GET_ARRAY_CONST_FLT, 
		0, COM_REAL,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_flt1d, COM_GET_ARRAY_CONST_FLT1D, 
		1, COM_REAL,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_flt2d, COM_GET_ARRAY_CONST_FLT2D, 
		2, COM_REAL,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_dbl, COM_GET_ARRAY_CONST_DBL, 
		0, COM_DOUBLE,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_dbl1d, COM_GET_ARRAY_CONST_DBL1D, 
		1, COM_DOUBLE,AM_GETC);
COM_OBTAIN_ARRAY(com_get_array_const_dbl2d, COM_GET_ARRAY_CONST_DBL2D, 
		2, COM_DOUBLE,AM_GETC);

COM_OBTAIN_ARRAY(com_copy_array_int, COM_COPY_ARRAY_INT, 
		0, COM_INTEGER,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_int1d, COM_COPY_ARRAY_INT1D, 
		1, COM_INTEGER,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_int2d, COM_COPY_ARRAY_INT2D, 
		2, COM_INTEGER,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_int3d, COM_COPY_ARRAY_INT3D, 
		3, COM_INTEGER,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_flt, COM_COPY_ARRAY_FLT, 
		0, COM_REAL,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_flt1d, COM_COPY_ARRAY_FLT1D, 
		1, COM_REAL,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_flt2d, COM_COPY_ARRAY_FLT2D, 
		2, COM_REAL,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_flt3d, COM_COPY_ARRAY_FLT3D, 
		3, COM_REAL,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_dbl, COM_COPY_ARRAY_DBL, 
		0, COM_DOUBLE,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_dbl1d, COM_COPY_ARRAY_DBL1D, 
		1, COM_DOUBLE,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_dbl2d, COM_COPY_ARRAY_DBL2D, 
		2, COM_DOUBLE,AM_COPY);
COM_OBTAIN_ARRAY(com_copy_array_dbl3d, COM_COPY_ARRAY_DBL3D, 
		3, COM_DOUBLE,AM_COPY);

#define COM_OBTAIN_ARRAY_STRD( func, FUNC, dim, type, mode) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *waname, const int &pane_id, void *addr, \
    int &strd, int l1, int l2) \
{ \
  int zero = 0; \
  com_obtain_array_f<dim, type,mode>( waname, pane_id, addr, strd, zero, \
				      zero, l1, l2); \
}

COM_OBTAIN_ARRAY_STRD(com_alloc_array_int1d_strd,COM_ALLOC_ARRAY_INT1D_STRD,
		      1, COM_INTEGER, AM_ALLOC);
COM_OBTAIN_ARRAY_STRD(com_alloc_array_int2d_strd,COM_ALLOC_ARRAY_INT2D_STRD,
		      2, COM_INTEGER, AM_ALLOC);
COM_OBTAIN_ARRAY_STRD(com_alloc_array_flt1d_strd,COM_ALLOC_ARRAY_FLT1D_STRD,
		      1, COM_REAL, AM_ALLOC);
COM_OBTAIN_ARRAY_STRD(com_alloc_array_flt2d_strd,COM_ALLOC_ARRAY_FLT2D_STRD,
		      2, COM_REAL, AM_ALLOC);
COM_OBTAIN_ARRAY_STRD(com_alloc_array_dbl1d_strd,COM_ALLOC_ARRAY_DBL1D_STRD,
		      1, COM_DOUBLE, AM_ALLOC);
COM_OBTAIN_ARRAY_STRD(com_alloc_array_dbl2d_strd,COM_ALLOC_ARRAY_DBL2D_STRD,
		      2, COM_DOUBLE, AM_ALLOC);

COM_OBTAIN_ARRAY_STRD(com_resize_array_int1d_strd,COM_RESIZE_ARRAY_INT1D_STRD,
		      1, COM_INTEGER, AM_RESIZE);
COM_OBTAIN_ARRAY_STRD(com_resize_array_int2d_strd,COM_RESIZE_ARRAY_INT2D_STRD,
		      2, COM_INTEGER, AM_RESIZE);
COM_OBTAIN_ARRAY_STRD(com_resize_array_flt1d_strd,COM_RESIZE_ARRAY_FLT1D_STRD,
		      1, COM_REAL, AM_RESIZE);
COM_OBTAIN_ARRAY_STRD(com_resize_array_flt2d_strd,COM_RESIZE_ARRAY_FLT2D_STRD,
		      2, COM_REAL, AM_RESIZE);
COM_OBTAIN_ARRAY_STRD(com_resize_array_dbl1d_strd,COM_RESIZE_ARRAY_DBL1D_STRD,
		      1, COM_DOUBLE, AM_RESIZE);
COM_OBTAIN_ARRAY_STRD(com_resize_array_dbl2d_strd,COM_RESIZE_ARRAY_DBL2D_STRD,
		      2, COM_DOUBLE, AM_RESIZE);

COM_OBTAIN_ARRAY_STRD(com_get_array_int1d_strd,COM_GET_ARRAY_INT1D_STRD,
		      1, COM_INTEGER, AM_GET);
COM_OBTAIN_ARRAY_STRD(com_get_array_int2d_strd,COM_GET_ARRAY_INT2D_STRD,
		      2, COM_INTEGER, AM_GET);
COM_OBTAIN_ARRAY_STRD(com_get_array_flt1d_strd,COM_GET_ARRAY_FLT1D_STRD,
		      1, COM_REAL, AM_GET);
COM_OBTAIN_ARRAY_STRD(com_get_array_flt2d_strd,COM_GET_ARRAY_FLT2D_STRD,
		      2, COM_REAL, AM_GET);
COM_OBTAIN_ARRAY_STRD(com_get_array_dbl1d_strd,COM_GET_ARRAY_DBL1D_STRD,
		      1, COM_DOUBLE, AM_GET);
COM_OBTAIN_ARRAY_STRD(com_get_array_dbl2d_strd,COM_GET_ARRAY_DBL2D_STRD,
		      2, COM_DOUBLE, AM_GET);

COM_OBTAIN_ARRAY_STRD(com_get_array_const_int1d_strd,
		      COM_GET_ARRAY_CONST_INT1D_STRD, 1, COM_INTEGER, AM_GETC);
COM_OBTAIN_ARRAY_STRD(com_get_array_const_int2d_strd,
		      COM_GET_ARRAY_CONST_INT2D_STRD, 2, COM_INTEGER, AM_GETC);
COM_OBTAIN_ARRAY_STRD(com_get_array_const_flt1d_strd,
		      COM_GET_ARRAY_CONST_FLT1D_STRD, 1, COM_REAL, AM_GETC);
COM_OBTAIN_ARRAY_STRD(com_get_array_const_flt2d_strd,
		      COM_GET_ARRAY_CONST_FLT2D_STRD, 2, COM_REAL, AM_GETC);
COM_OBTAIN_ARRAY_STRD(com_get_array_const_dbl1d_strd,
		      COM_GET_ARRAY_CONST_DBL1D_STRD, 1, COM_DOUBLE, AM_GETC);
COM_OBTAIN_ARRAY_STRD(com_get_array_const_dbl2d_strd,
		      COM_GET_ARRAY_CONST_DBL2D_STRD, 2, COM_DOUBLE, AM_GETC);

COM_OBTAIN_ARRAY_STRD(com_copy_array_int_strd,COM_COPY_ARRAY_INT_STRD,
		      0, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_int1d_strd,COM_COPY_ARRAY_INT1D_STRD,
		      1, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_int2d_strd,COM_COPY_ARRAY_INT2D_STRD,
		      2, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_int3d_strd,COM_COPY_ARRAY_INT3D_STRD,
		      3, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_flt_strd,COM_COPY_ARRAY_FLT_STRD,
		      0, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_flt1d_strd,COM_COPY_ARRAY_FLT1D_STRD,
		      1, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_flt2d_strd,COM_COPY_ARRAY_FLT2D_STRD,
		      2, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_flt3d_strd,COM_COPY_ARRAY_FLT3D_STRD,
		      3, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_dbl_strd,COM_COPY_ARRAY_DBL_STRD,
		      0, COM_DOUBLE, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_dbl1d_strd,COM_COPY_ARRAY_DBL1D_STRD,
		      1, COM_DOUBLE, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_dbl2d_strd,COM_COPY_ARRAY_DBL2D_STRD,
		      2, COM_DOUBLE, AM_COPY);
COM_OBTAIN_ARRAY_STRD(com_copy_array_dbl3d_strd,COM_COPY_ARRAY_DBL3D_STRD,
		      3, COM_DOUBLE, AM_COPY);

#define COM_OBTAIN_ARRAY_DYN( func, FUNC, dim, type, mode) \
extern "C" void COM_F_FUNC2( func, FUNC) \
( const char *waname, const int &pane_id, void *addr, \
  int &strd, int &cap, int l1, int l2) \
{ \
  com_obtain_array_f<dim, type, mode>( waname, pane_id, addr, \
				      strd, cap, 0, l1, l2);\
}

COM_OBTAIN_ARRAY_DYN(com_alloc_array_int1d_dyn,COM_ALLOC_ARRAY_INT1D_DYN,
		    1, COM_INTEGER, AM_ALLOC);
COM_OBTAIN_ARRAY_DYN(com_alloc_array_int2d_dyn,COM_ALLOC_ARRAY_INT2D_DYN,
		    2, COM_INTEGER, AM_ALLOC);
COM_OBTAIN_ARRAY_DYN(com_alloc_array_flt1d_dyn,COM_ALLOC_ARRAY_FLT1D_DYN,
		    1, COM_REAL, AM_ALLOC);
COM_OBTAIN_ARRAY_DYN(com_alloc_array_flt2d_dyn,COM_ALLOC_ARRAY_FLT2D_DYN,
		    2, COM_REAL, AM_ALLOC);
COM_OBTAIN_ARRAY_DYN(com_alloc_array_dbl1d_dyn,COM_ALLOC_ARRAY_DBL1D_DYN,
		    1, COM_DOUBLE, AM_ALLOC);
COM_OBTAIN_ARRAY_DYN(com_alloc_array_dbl2d_dyn,COM_ALLOC_ARRAY_DBL2D_DYN,
		    2, COM_DOUBLE, AM_ALLOC);

COM_OBTAIN_ARRAY_DYN(com_resize_array_int1d_dyn,COM_RESIZE_ARRAY_INT1D_DYN,
		    1, COM_INTEGER, AM_RESIZE);
COM_OBTAIN_ARRAY_DYN(com_resize_array_int2d_dyn,COM_RESIZE_ARRAY_INT2D_DYN,
		    2, COM_INTEGER, AM_RESIZE);
COM_OBTAIN_ARRAY_DYN(com_resize_array_flt1d_dyn,COM_RESIZE_ARRAY_FLT1D_DYN,
		    1, COM_REAL, AM_RESIZE);
COM_OBTAIN_ARRAY_DYN(com_resize_array_flt2d_dyn,COM_RESIZE_ARRAY_FLT2D_DYN,
		    2, COM_REAL, AM_RESIZE);
COM_OBTAIN_ARRAY_DYN(com_resize_array_dbl1d_dyn,COM_RESIZE_ARRAY_DBL1D_DYN,
		    1, COM_DOUBLE, AM_RESIZE);
COM_OBTAIN_ARRAY_DYN(com_resize_array_dbl2d_dyn,COM_RESIZE_ARRAY_DBL2D_DYN,
		    2, COM_DOUBLE, AM_RESIZE);

COM_OBTAIN_ARRAY_DYN(com_get_array_int1d_dyn,COM_GET_ARRAY_INT1D_DYN,
		    1, COM_INTEGER, AM_GET);
COM_OBTAIN_ARRAY_DYN(com_get_array_int2d_dyn,COM_GET_ARRAY_INT2D_DYN,
		    2, COM_INTEGER, AM_GET);
COM_OBTAIN_ARRAY_DYN(com_get_array_flt1d_dyn,COM_GET_ARRAY_FLT1D_DYN,
		    1, COM_REAL, AM_GET);
COM_OBTAIN_ARRAY_DYN(com_get_array_flt2d_dyn,COM_GET_ARRAY_FLT2D_DYN,
		    2, COM_REAL, AM_GET);
COM_OBTAIN_ARRAY_DYN(com_get_array_dbl1d_dyn,COM_GET_ARRAY_DBL1D_DYN,
		    1, COM_DOUBLE, AM_GET);
COM_OBTAIN_ARRAY_DYN(com_get_array_dbl2d_dyn,COM_GET_ARRAY_DBL2D_DYN,
		    2, COM_DOUBLE, AM_GET);

COM_OBTAIN_ARRAY_DYN(com_get_array_const_int1d_dyn,
		    COM_GET_ARRAY_CONST_INT1D_DYN, 1, COM_INTEGER, AM_GETC);
COM_OBTAIN_ARRAY_DYN(com_get_array_const_int2d_dyn,
		    COM_GET_ARRAY_CONST_INT2D_DYN, 2, COM_INTEGER, AM_GETC);
COM_OBTAIN_ARRAY_DYN(com_get_array_const_flt1d_dyn,
		    COM_GET_ARRAY_CONST_FLT1D_DYN, 1, COM_REAL, AM_GETC);
COM_OBTAIN_ARRAY_DYN(com_get_array_const_flt2d_dyn,
		    COM_GET_ARRAY_CONST_FLT2D_DYN, 2, COM_REAL, AM_GETC);
COM_OBTAIN_ARRAY_DYN(com_get_array_const_dbl1d_dyn,
		    COM_GET_ARRAY_CONST_DBL1D_DYN, 1, COM_DOUBLE, AM_GETC);
COM_OBTAIN_ARRAY_DYN(com_get_array_const_dbl2d_dyn,
		    COM_GET_ARRAY_CONST_DBL2D_DYN, 2, COM_DOUBLE, AM_GETC);

COM_OBTAIN_ARRAY_DYN(com_copy_array_int_dyn,COM_COPY_ARRAY_INT_DYN,
		    0, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_int1d_dyn,COM_COPY_ARRAY_INT1D_DYN,
		    1, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_int2d_dyn,COM_COPY_ARRAY_INT2D_DYN,
		    2, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_int3d_dyn,COM_COPY_ARRAY_INT3D_DYN,
		    3, COM_INTEGER, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_flt_dyn,COM_COPY_ARRAY_FLT_DYN,
		    0, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_flt1d_dyn,COM_COPY_ARRAY_FLT1D_DYN,
		    1, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_flt2d_dyn,COM_COPY_ARRAY_FLT2D_DYN,
		    2, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_flt3d_dyn,COM_COPY_ARRAY_FLT3D_DYN,
		    3, COM_REAL, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_dbl_dyn,COM_COPY_ARRAY_DBL_DYN,
		    0, COM_DOUBLE, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_dbl1d_dyn,COM_COPY_ARRAY_DBL1D_DYN,
		    1, COM_DOUBLE, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_dbl2d_dyn,COM_COPY_ARRAY_DBL2D_DYN,
		    2, COM_DOUBLE, AM_COPY);
COM_OBTAIN_ARRAY_DYN(com_copy_array_dbl3d_dyn,COM_COPY_ARRAY_DBL3D_DYN,
		    3, COM_DOUBLE, AM_COPY);

#define COM_COPY_ARRAY_OFF( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
( const char *waname, const int &pane_id, void *addr, const int &strd, \
  const int &cap, const int &offset, int l1, int l2) \
{ \
  com_obtain_array_f<dim, type, AM_COPY>( waname, pane_id, addr, \
	const_cast<int&>(strd), const_cast<int&>(cap), offset, l1, l2);\
}

COM_COPY_ARRAY_OFF(com_copy_array_int_off,COM_COPY_ARRAY_INT_OFF,
		   0, COM_INTEGER);
COM_COPY_ARRAY_OFF(com_copy_array_int1d_off,COM_COPY_ARRAY_INT1D_OFF,
		   1, COM_INTEGER);
COM_COPY_ARRAY_OFF(com_copy_array_int2d_off,COM_COPY_ARRAY_INT2D_OFF,
		   2, COM_INTEGER);
COM_COPY_ARRAY_OFF(com_copy_array_int3d_off,COM_COPY_ARRAY_INT3D_OFF,
		   3, COM_INTEGER);
COM_COPY_ARRAY_OFF(com_copy_array_flt_off,COM_COPY_ARRAY_FLT_OFF,
		   0, COM_REAL);
COM_COPY_ARRAY_OFF(com_copy_array_flt1d_off,COM_COPY_ARRAY_FLT1D_OFF,
		   1, COM_REAL);
COM_COPY_ARRAY_OFF(com_copy_array_flt2d_off,COM_COPY_ARRAY_FLT2D_OFF,
		   2, COM_REAL);
COM_COPY_ARRAY_OFF(com_copy_array_flt3d_off,COM_COPY_ARRAY_FLT3D_OFF,
		   3, COM_REAL);
COM_COPY_ARRAY_OFF(com_copy_array_dbl_off,COM_COPY_ARRAY_DBL_OFF,
		   0, COM_DOUBLE);
COM_COPY_ARRAY_OFF(com_copy_array_dbl1d_off,COM_COPY_ARRAY_DBL1D_OFF,
		   1, COM_DOUBLE);
COM_COPY_ARRAY_OFF(com_copy_array_dbl2d_off,COM_COPY_ARRAY_DBL2D_OFF,
		   2, COM_DOUBLE);
COM_COPY_ARRAY_OFF(com_copy_array_dbl3d_off,COM_COPY_ARRAY_DBL3D_OFF,
		   3, COM_DOUBLE);

#define COM_APPEND_ARRAY_SCALAR( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *waname, const int &pane_id, \
    const void *val, const int &v_strd, const int &v_size, int wa_len) \
{ \
  CHKLEN(wa_len); \
  COM_get_roccom()->append_array( string(waname,wa_len), pane_id, \
				  val, v_strd, v_size); \
}

COM_APPEND_ARRAY_SCALAR( com_append_array_chr, COM_APPEND_ARRAY_CHR);
COM_APPEND_ARRAY_SCALAR( com_append_array_int, COM_APPEND_ARRAY_INT);
COM_APPEND_ARRAY_SCALAR( com_append_array_flt, COM_APPEND_ARRAY_FLT);
COM_APPEND_ARRAY_SCALAR( com_append_array_dbl, COM_APPEND_ARRAY_DBL);

template <int dim, COM_Type type>
void 
com_append_array_helper( const std::string &waname, int pane_id, 
			 const void *val, int v_strd, int v_size,
			 int l=0) {
  
  void *v_ptr=com_get_address_f<dim, type>( const_cast<void*>(val), l);

  COM_get_roccom()->append_array( waname, pane_id, v_ptr, v_strd, v_size);
}

template <int dim, COM_Type type>
void 
com_append_array_f( const char *waname, int pane_id, 
		    const void *val, int v_strd, int v_size,
		    int l1, int l2) {
  int f90ptr_treat = get_f90ptr_treat();
    
  if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
    int wa_len=l2; CHKLEN(wa_len);
    com_append_array_helper<dim,type>( string(waname, wa_len), pane_id, 
				       val, v_strd, v_size, l1);
  }
  else {
    int wa_len=l1; CHKLEN(wa_len);
    if ( f90ptr_treat == Roccom_base::FPTR_APPEND)
      com_append_array_helper<dim,type>( string(waname, wa_len), pane_id, 
					 val, v_strd, v_size, l2);
    else
      com_append_array_helper<dim,type>( string(waname, wa_len), pane_id,
					 val, v_strd, v_size);
  }
}

#define COM_APPEND_ARRAY( func, FUNC, dim, type) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *waname, const int &pane_id, \
    const void *val, int &v_strd, int &v_size, int l1, int l2) \
{ \
  com_append_array_f<dim,type>( waname, pane_id, \
                                val, v_strd, v_size, l1, l2); \
}

COM_APPEND_ARRAY( com_append_array_int1d, COM_APPEND_ARRAY_INT1D, 1, COM_INTEGER);
COM_APPEND_ARRAY( com_append_array_flt1d, COM_APPEND_ARRAY_FLT1D, 1, COM_REAL);
COM_APPEND_ARRAY( com_append_array_dbl1d, COM_APPEND_ARRAY_DBL1D, 1, COM_DOUBLE);
COM_APPEND_ARRAY( com_append_array_int2d, COM_APPEND_ARRAY_INT2D, 2, COM_INTEGER);
COM_APPEND_ARRAY( com_append_array_flt2d, COM_APPEND_ARRAY_FLT2D, 2, COM_REAL);
COM_APPEND_ARRAY( com_append_array_dbl2d, COM_APPEND_ARRAY_DBL2D, 2, COM_DOUBLE);
COM_APPEND_ARRAY( com_append_array_int3d, COM_APPEND_ARRAY_INT3D, 3, COM_INTEGER);
COM_APPEND_ARRAY( com_append_array_flt3d, COM_APPEND_ARRAY_FLT3D, 3, COM_REAL);
COM_APPEND_ARRAY( com_append_array_dbl3d, COM_APPEND_ARRAY_DBL3D, 3, COM_DOUBLE);

#define COM_GET_BOUNDS( func, FUNC) \
extern "C" void COM_F_FUNC2( func, FUNC) \
  ( const char *wa_str, const int &pane_id, \
    void *lbound, void *ubound, int len) \
{ \
  CHKLEN(len);\
  COM_get_roccom()->get_bounds( string(wa_str,len), pane_id, lbound, ubound); \
}

COM_GET_BOUNDS(com_get_bounds_int, COM_GET_BOUNDS_INT);
COM_GET_BOUNDS(com_get_bounds_flt, COM_GET_BOUNDS_FLT);
COM_GET_BOUNDS(com_get_bounds_dbl, COM_GET_BOUNDS_DBL);

extern "C" int COM_F_FUNC2(com_check_bounds,COM_CHECK_BOUNDS)
  ( const char *wa_str, const int &pane_id, int len)
{ 
  CHKLEN(len);
  return COM_get_roccom()->check_bounds( string(wa_str,len), pane_id); 
}

extern "C" void COM_F_FUNC2(com_use_attr, COM_USE_ATTR)
  ( const char *wname, const char *attr, int w_len, int a_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    use_attribute( string(wname, w_len), string(attr, a_len));
}

extern "C" void COM_F_FUNC2(com_clone_attr,COM_CLONE_ATTR)
  ( const char *wname, const char *attr, int w_len, int a_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    clone_attribute( string(wname, w_len), string(attr, a_len));
}

extern "C" void COM_F_FUNC2(com_copy_attr,COM_COPY_ATTR)
  ( const char *wname, const char *attr, int w_len, int a_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    copy_attribute( string(wname, w_len), string(attr, a_len));
}

extern "C" void COM_F_FUNC2(com_copy_attr_hdls,COM_COPY_ATTR_HDLS)
  ( const int &trg_hdl, const int &src_hdl) 
{
  COM_get_roccom()->copy_attribute( trg_hdl, src_hdl);
}

extern "C" void COM_F_FUNC2(com_use_attr_ghost, COM_USE_ATTR_GHOST)
  ( const char *wname, const char *attr, const int &with_ghost,
    int w_len, int a_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    use_attribute( string(wname, w_len), string(attr, a_len), with_ghost);
}

extern "C" void COM_F_FUNC2(com_clone_attr_ghost, COM_CLONE_ATTR_GHOST)
  ( const char *wname, const char *attr, const int &with_ghost,
    int w_len, int a_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    clone_attribute( string(wname, w_len), string(attr, a_len), with_ghost);
}

extern "C" void COM_F_FUNC2(com_copy_attr_ghost, COM_COPY_ATTR_GHOST)
  ( const char *wname, const char *attr, const int &with_ghost,
    int w_len, int a_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    copy_attribute( string(wname, w_len), string(attr, a_len), with_ghost);
}

extern "C" void COM_F_FUNC2(com_copy_attr_ghost_hdls,COM_COPY_ATTR_GHOST_HDLS)
  ( const int &trg_hdl, const int &src_hdl, const int &with_ghost ) 
{
  COM_get_roccom()->copy_attribute( trg_hdl, src_hdl, with_ghost);
}

extern "C" void COM_F_FUNC2(com_use_attr_sub, COM_USE_ATTR_SUB)
  ( const char *wname, const char *attr, const int &with_ghost, 
    const char *ptnname, const int &val,
    int w_len, int a_len, int p_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    use_attribute( string(wname, w_len), string(attr, a_len), 
		   with_ghost, string(ptnname, p_len).c_str(), val);
}

extern "C" void COM_F_FUNC2(com_clone_attr_sub, COM_CLONE_ATTR_sub)
  ( const char *wname, const char *attr, const int &with_ghost,  
    const char *ptnname, const int &val,
    int w_len, int a_len, int p_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    clone_attribute( string(wname, w_len), string(attr, a_len), 
		     with_ghost, string(ptnname, p_len).c_str(), val);
}

extern "C" void COM_F_FUNC2(com_copy_attr_sub, COM_COPY_ATTR_sub)
  ( const char *wname, const char *attr, const int &with_ghost,  
    const char *ptnname, const int &val,
    int w_len, int a_len, int p_len) 
{
  CHKLEN(w_len); CHKLEN(a_len);
  COM_get_roccom()->
    copy_attribute( string(wname, w_len), string(attr, a_len), 
		    with_ghost, string(ptnname, p_len).c_str(), val);
}

extern "C" void COM_F_FUNC2(com_copy_attr_ghost_sub_hdls,
			    COM_COPY_ATTR_GHOST_SUB_HDLS)
  ( const int &trg_hdl, const int &src_hdl, const int &with_ghost,
    const int &pnt_hdl, const int &val) 
{
  COM_get_roccom()->copy_attribute( trg_hdl, src_hdl, with_ghost, 
				    pnt_hdl, val);
}

extern "C" void COM_F_FUNC2(com_deallocate_win, COM_DEALLOCATE_WIN)
  ( const char *wa_str, int wa_len) 
{
  CHKLEN(wa_len); 
  COM_get_roccom()->deallocate_array( string( wa_str, wa_len), 0);
}

extern "C" void COM_F_FUNC2(com_deallocate_pane, COM_DEALLOCATE_PANE)
  ( const char *wa_str, const int &pid, int wa_len) 
{
  CHKLEN(wa_len); 
  COM_get_roccom()->deallocate_array(string(wa_str, wa_len), pid);
}

extern "C" void COM_F_FUNC2(com_get_size1, COM_GET_SIZE1)
  ( const char *wa_str, const int &pane_id, int *size, int len)
{ COM_get_roccom()->get_size( std::string(wa_str,len), pane_id, size); }

extern "C" void COM_F_FUNC2(com_get_size2, COM_GET_SIZE2)
  ( const char *wa_str, const int &pane_id, int *size, int *ng, int len)
{ COM_get_roccom()->get_size( std::string(wa_str,len), pane_id, size, ng); }

extern "C" void COM_F_FUNC2(com_get_attribute,COM_GET_ATTRIBUTE)
  ( const char *wa_str, char *loc, int *type, int  *size, 
    char *u_str,  int wa_len, int l_len, int u_len) 
{
  CHKLEN(wa_len); CHKLEN(l_len); CHKLEN(u_len);
  std::string unit;
  COM_get_roccom()->get_attribute( string( wa_str,wa_len), loc, type, 
				   size, &unit);

  int len=unit.size(), n=std::min(len, int(u_len));

  int u_len_int = u_len;
  COM_F_FUNC2( com_copy_string, COM_COPY_STRING) 
    ( unit.c_str(), &n, u_str, &u_len_int, n, u_len);
}

extern "C" void COM_F_FUNC2(com_set_function, COM_SET_FUNCTION)
  ( const char *wf_str, Func_ptr func, const char *intents,
    const COM_Type *types, int wf_len, int i_len) 
{
  CHKLEN(wf_len); CHKLEN(i_len);
  COM_get_roccom()->set_function( string(wf_str,wf_len), func,
				  string(intents,i_len), types, true);
}

extern "C" void COM_F_FUNC2(com_set_member_function, COM_SET_MEMBER_FUNCTION)
  ( const char *wf_str, Func_ptr func, const char *wa_str, const char *intents,
    const COM_Type *types, int wf_len, int a_len, int i_len) 
{
  CHKLEN(wf_len); CHKLEN(a_len); CHKLEN(i_len);
  COM_get_roccom()->set_member_function( string(wf_str,wf_len), func,
					 string(wa_str,a_len),
					 string(intents,i_len), 
					 types, true);
}

extern "C" void COM_F_FUNC2(com_get_communicator,COM_GET_COMMUNICATOR)
  ( const char *wname, int *comm, int w_len) 
{
  CHKLEN(w_len); 
  *comm = COMMPI_Comm_c2f(COM_get_roccom()->
			  get_communicator( string(wname, w_len))); 
}

extern "C" void COM_F_FUNC2(com_get_pane_ids_on_rank,COM_GET_PANE_IDS_ON_RANK)
  ( const char *wname, int *npanes, void *panes_ids, const int &rank,
    int l1, int l2) 
{
  int f90ptr_treat = get_f90ptr_treat();
  int w_len=(f90ptr_treat==Roccom_base::FPTR_INSERT)?l2:l1; CHKLEN(w_len);

  std::vector<int> vec;
  int *pids;
  COM_get_roccom()->get_panes( string(wname, w_len), vec, rank, &pids);
  *npanes = vec.size();
  
  if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
    int tonull = *npanes == 0;
    typedef void(*Func)(void*,int*,void*,void*,int);
    (*(Func)COM_F_FUNC2(com_mapptr_int1d,COM_MAPPTR_INT1D))
      ( panes_ids, &tonull, pids, npanes, l1);
  }
  else {
    int tonull = *npanes == 0;

    if  ( f90ptr_treat == Roccom_base::FPTR_APPEND) {
      typedef void(*Func)(void*,int*,void*,void*,int);
      (*(Func)COM_F_FUNC2(com_mapptr_int1d,COM_MAPPTR_INT1D))
	( panes_ids, &tonull, pids, npanes, l2);
    }
    else {
      typedef void(*Func)(void*,int*,void*,void*);
      (*(Func)COM_F_FUNC2(com_mapptr_int1d,COM_MAPPTR_INT1D))
	( panes_ids, &tonull, pids, npanes);
    }
  }
}

extern "C" void COM_F_FUNC2(com_get_pane_ids,COM_GET_PANE_IDS)
  ( const char *wname, int *npanes, void *panes_ids, 
    int l1, int l2) 
{
  COM_F_FUNC2(com_get_pane_ids_on_rank,COM_GET_PANE_IDS_ON_RANK)
    ( wname, npanes, panes_ids, -2, l1, l2);
}

extern "C" void COM_F_FUNC2(com_get_npanes,COM_GET_NPANES)
  ( const char *wname, int *npanes, int w_len) 
{
  CHKLEN(w_len);
  std::vector<int> vec;
  COM_get_roccom()->get_panes( string(wname, w_len), vec); 
  *npanes = vec.size();
}

static void mapcharptr( char *str, void *names, int f90ptr_treat, 
			int l1, int l2) {
  if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
    int len = std::strlen( str), tonull=(len==0);
    typedef void(*Func)(void*,int*,void*,void*, int);
    (*(Func)COM_F_FUNC2(com_mapptr_chr1d,COM_MAPPTR_CHR1D))
      ( names, &tonull, str, &len, l1);
  }
  else {
    int len = std::strlen( str), tonull=(len==0);
    
    if ( f90ptr_treat == Roccom_base::FPTR_APPEND) {
      typedef void(*Func)(void*,int*,void*,void*, int);
      (*(Func)COM_F_FUNC2(com_mapptr_chr1d,COM_MAPPTR_CHR1D))
	( names, &tonull, str, &len, l2);
    }
    else {
      typedef void(*Func)(void*,int*,void*,void*);
      (*(Func)COM_F_FUNC2(com_mapptr_chr1d,COM_MAPPTR_CHR1D))
	( names, &tonull, str, &len);
    }
  }
}

extern "C" void COM_F_FUNC2(com_get_attributes,COM_GET_ATTRIBUTES)
  ( const char *wname, int *na, void *names, 
    int l1, int l2) 
{
  int f90ptr_treat = get_f90ptr_treat();
  int w_len=(f90ptr_treat==Roccom_base::FPTR_INSERT)?l2:l1; CHKLEN(w_len);

  std::string str;
  char *atts;
  COM_get_roccom()->get_attributes( string(wname, w_len), na, str, &atts); 
    
  mapcharptr( atts, names, f90ptr_treat, l1, l2);
}

extern "C" void COM_F_FUNC2(com_get_connectivities,COM_GET_CONNECTIVITIES)
  ( const char *wname, const int &pane_id, int *nc, void *names, 
    int l1, int l2) 
{
  int f90ptr_treat = get_f90ptr_treat();

  int w_len=(f90ptr_treat==Roccom_base::FPTR_INSERT)?l2:l1; CHKLEN(w_len);

  std::string str;
  char *conns;
  COM_get_roccom()->get_connectivities( string(wname, w_len), pane_id,
					nc, str, &conns); 
    
  mapcharptr( conns, names, f90ptr_treat, l1, l2);
}

extern "C" void COM_F_FUNC2( com_get_parent, COM_GET_PARENT)
  ( const char *waname, const int &pane_id, void *parent, 
    int l1, int l2)
{ 
  int f90ptr_treat = get_f90ptr_treat();
  int w_len=(f90ptr_treat==Roccom_base::FPTR_INSERT)?l2:l1; CHKLEN(w_len);

  std::string str;
  char *name;
  COM_get_roccom()->get_parent( string(waname, w_len), pane_id, str, &name); 

  mapcharptr( name, parent, f90ptr_treat, l1, l2);
}

extern "C" int COM_F_FUNC2( com_get_status, COM_GET_STATUS)
  ( const char *waname, const int &pane_id, int len)
{ 
  CHKLEN(len); 
  return COM_get_roccom()->get_status( std::string(waname,len), pane_id); 
}

extern "C" void COM_F_FUNC2(com_free_buffer_char,COM_FREE_BUFFER_CHAR)
  ( void *buf, int l2) 
{
  char *ptr=NULL;
  typedef void(*Func)(void*,void**);
  (*(Func)COM_F_FUNC2(com_getptr_chr1d,COM_GETPTR_CHR1D))
    ( buf, &(void*&)ptr);

  COM_get_roccom()->free_buffer( &ptr);

  { // Nullify pointer
    int one=1, zero = 0;
    typedef void(*Func)(void*,int*,void*,void*);
    
    (*(Func)COM_F_FUNC2(com_mapptr_chr1d,COM_GETPTR_CHR1D))
      ( buf, &one, ptr, &zero);
  }
}
  
extern "C" void COM_F_FUNC2(com_free_buffer_int,COM_FREE_BUFFER_INT)
  ( void *buf, int l2) 
{
  int *ptr=NULL;
  typedef void(*Func)(void*,void**);
  (*(Func)COM_F_FUNC2(com_getptr_int1d,COM_GETPTR_INT1D))
    ( buf, &(void*&)ptr);

  COM_get_roccom()->free_buffer( &ptr);

  { // Nullify pointer
    int one=1, zero = 0;
    typedef void(*Func)(void*,int*,void*,void*);
    
    (*(Func)COM_F_FUNC2(com_mapptr_int1d,COM_GETPTR_INT1D))
      ( buf, &one, ptr, &zero);
  }
}

extern "C" int COM_F_FUNC2( com_get_window_handle, COM_GET_WINDOW_HANDLE)
  ( const char *wname, int len) 
{
  CHKLEN(len); 
  return COM_get_roccom()->get_window_handle( std::string(wname,len)); 
}

extern "C" int COM_F_FUNC2(com_get_attribute_handle, COM_GET_ATTRIBUTE_HANDLE)
  ( const char *waname, int len) 
{ 
  CHKLEN(len); 
  return COM_get_roccom()->get_attribute_handle( std::string(waname,len)); 
}

extern "C" int COM_F_FUNC2( com_get_attribute_handle_const, COM_GET_ATTRIBUTE_HANDLE_CONST)
  ( const char *waname, int len) {
 
  CHKLEN(len); 
  return COM_get_roccom()->get_attribute_handle_const( std::string(waname,len)); 
}

extern "C" int COM_F_FUNC2( com_get_function_handle, COM_GET_FUNCTION_HANDLE)
  ( const char *wfname, int len) 
{
  CHKLEN(len); 
  return COM_get_roccom()->get_function_handle( std::string(wfname,len)); 
}

extern "C" void COM_F_FUNC2(com_call_function, COM_CALL_FUNCTION)
  ( const int &wf, const int &argc, void *a1, void *a2, void *a3, void *a4, 
    void *a5, void *a6, void *a7, void *a8, void *a9, 
    void *aa, void *ab, void *ac, void *ad, void *ae) {
  void *args[] = {a1, a2, a3, a4, a5, a6, a7, a8, a9, aa, ab, ac, ad, ae};
  int lens[ Function::MAX_NUMARG];
  for ( int i=0; i<argc; ++i) 
    lens[i] = (unsigned int)((char *)(args[argc+i])-(char *)(0));
  COM_get_roccom()->call_function(wf, argc, args, lens, false);
}

extern "C" void COM_F_FUNC2( com_icall_function, COM_ICALL_FUNCTION)
  ( const int &wf, const int &argc, void *a1, void *a2, void *a3, void *a4, 
    void *a5, void *a6, void *a7, void *a8, void *a9, 
    void *aa, void *ab, void *ac, void *ad, void *ae, void *af) {
  void *args[] = {a1, a2, a3, a4, a5, a6, a7, a8, a9, aa, ab, ac, ad, ae};

  int lens[Function::MAX_NUMARG];
  for ( int i=0; i<argc-1; ++i) 
    lens[i] = (unsigned int)((char *)(args[argc+i])-(char *)(0));

  COM_get_roccom()->icall_function(wf, argc, args, lens, false);
}

extern "C" void COM_F_FUNC2(com_test, COM_TEST)
  ( const int &reqid, int *status) 
{

  *status = COM_get_roccom()->test( reqid);
  if ( *status == true)
    COM_F_FUNC2(com_set_true, COM_SET_TRUE)( status);
  else {
    COM_F_FUNC2(com_set_false, COM_SET_FALSE)( status);
    COM_assertion( *status == 0);
  }
}

extern "C" void COM_F_FUNC2(com_wait, COM_WAIT)
  ( const int &reqid) 
{ COM_get_roccom()->wait( reqid); }

extern "C" void COM_F_FUNC2( com_set_pointer, COM_SET_POINTER)
  ( const char *waname, void *ptr, Func_ptr func, 
    int l1, int l2) {

  if ( get_f90ptr_treat() == Roccom_base::FPTR_INSERT) {
    // Note: For Portland Group F90 compiler, l1 is the end-address of
    //       the pointer and l2 is the length of the character string.
    int len=l2; CHKLEN( len);
    COM_get_roccom()->set_f90pointer( std::string(waname,len), ptr, func, l1); 
  }
  else {
    int len=l1; CHKLEN( len);
    COM_get_roccom()->set_f90pointer( std::string(waname,len), ptr, func, l2);
  }
}

extern "C" void COM_F_FUNC2(com_get_pointer, COM_GET_POINTER)
  ( const char *waname, void *ptr, Func_ptr func, 
    int l1, int l2) {

  int f90ptr_treat = get_f90ptr_treat();
    
  if ( f90ptr_treat == Roccom_base::FPTR_INSERT) {
    // Note: For Portland Group F90 compiler, l1 is the end-address of
    //       the pointer and l2 is the length of the character string.
    int len=l2; CHKLEN( len);
    COM_get_roccom()->get_f90pointer( std::string(waname,len), ptr, func, l1); 
  }
  else {
    int len=l1; CHKLEN( len);
    COM_get_roccom()->get_f90pointer( std::string(waname,len), ptr, func, l2);
  }
}

extern "C" void COM_F_FUNC2( com_set_object, COM_SET_OBJECT)
  ( const char *waname, void *ptr, Func_ptr func, 
    int l1, int l2) {

  COM_F_FUNC2( com_set_pointer, COM_SET_POINTER)( waname, ptr, func, l1, l2); 
}

extern "C" void COM_F_FUNC2(com_get_object, COM_GET_OBJECT)
  ( const char *waname, void *ptr, Func_ptr func, 
    int l1, int l2) {

  COM_F_FUNC2( com_get_pointer, COM_GET_POINTER)( waname, ptr, func, l1, l2); 
}


//================================================================
//============== Functions for tracing and profiling =============
//================================================================

extern "C" void COM_F_FUNC2(com_set_verbose,COM_SET_VERBOSE)( const int &i)
{ COM_get_roccom()->set_verbose( i); }

extern "C" void COM_F_FUNC2(com_set_debug,COM_SET_DEBUG)( const bool &debug)
{ COM_get_roccom()->set_debug( debug); }

extern "C" void COM_F_FUNC2( com_set_profiling_barrier, COM_SET_PROFILING_BARRIER)
  ( const int &hdl, const int &comm) 
{
  COM_get_roccom()->set_profiling_barrier
    ( hdl, COMMPI_Comm_f2c(comm, MPI_Comm()));
}

extern "C" void COM_F_FUNC2(com_set_profiling, COM_SET_PROFILING) ( const int &i) 
{
  COM_get_roccom()->set_profiling( i);
}

extern "C" void COM_F_FUNC2( com_print_profile, COM_PRINT_PROFILE)
  ( const char *fname, const char *header, 
    int len, int hlen) 
{
  CHKLEN(len); CHKLEN(hlen);
  COM_get_roccom()->print_profile( std::string(fname,len),
				   std::string(header,hlen));
}

extern "C" int COM_F_FUNC2(com_get_sizeof, COM_GET_SIZEOF)
  ( const COM_Type *type, int *c) 
{ return COM_get_roccom()->get_sizeof( *type, *c); }

extern "C" int COM_F_FUNC2(com_get_error_code, COM_GET_ERROR_CODE)() 
{ return COM_get_roccom()->get_error_code(); }

COM_BEGIN_NAME_SPACE

/// Some big array to help determining the size of a Fortran 90 pointer.
typedef struct {
  int a[512];    ///< Some arbitrary big array
} Big_array;

extern "C" void COM_F_FUNC2( com_settypeinfo, COM_SETTYPEINFO)(Big_array *);

int get_sizeof_f90pointer() {
  Big_array w;

  COM_F_FUNC2( com_settypeinfo, COM_SETTYPEINFO)(&w);  

  if (w.a[0] != 333331) {
    return -1;
  }
  else {
    for (int i = 0; i < 512; i++) {
      if (w.a[i] == 333332) {
	return (i-1)*sizeof(int);
      }
    }
  }
  return -1;
}

COM_END_NAME_SPACE

extern "C" int COM_F_FUNC2( com_get_sizeof_f90pointer, COM_GET_SIZEOF_F90POINTER) ()
{ return get_sizeof_f90pointer(); }

extern "C" void COM_F_FUNC2( com_chkptr_begin, COM_CHKPTR_BEGIN)();

extern "C" void COM_F_FUNC2( com_chkptr_end, COM_CHKPTR_END)
  ( const char *str1, void *ptr1, const char *str2, void *ptr2,
    int len1, int len2, int len3, int len4);

extern "C" int COM_F_FUNC2( com_chkptr_c, COM_CHKPTR_C) 
  ( const int &stage, const char *str1, void *ptr1, 
    const char *str2, void *ptr2, 
    int len1, int len2, int len3, int len4) {

  static int insert_or_append;
  static int ptrinfo1, ptrinfo2;

  switch ( stage) {
  case 0: // Being called from COM_init
    insert_or_append = Roccom_base::FPTR_NONE;
    ptrinfo1 = ptrinfo2 = 0;

    COM_F_FUNC2( com_chkptr_begin, COM_CHKPTR_BEGIN)();
    break;
  case 1:
    if ( (int)len3 == 17 && (int)len4 == 33) 
      insert_or_append = Roccom_base::FPTR_INSERT;
    else 
      COM_assertion_msg( len1 == 17 && len2 == 33, 
			 "Incorrect handling of Fortran 90 pointers");

    if ( insert_or_append == Roccom_base::FPTR_INSERT) {
      ptrinfo1 = len1 - ((char*)ptr1-(char*)NULL);
      ptrinfo2 = len2 - ((char*)ptr2-(char*)NULL);
    }
    else {
      ptrinfo1 = len3 - ((char*)ptr1-(char*)NULL);
      ptrinfo2 = len4 - ((char*)ptr2-(char*)NULL);
    }
    
    COM_F_FUNC2( com_chkptr_end, COM_CHKPTR_END)
      ( str1, ptr1, str2, ptr2, len1, len2, len3, len4);
    break;
  case 2: { // This stage, the arguments are switched
    int verb = COM_get_roccom()->get_verbose();
    bool debug = COM_get_roccom()->get_debug();
    if ( insert_or_append == Roccom_base::FPTR_INSERT) {
      COM_assertion_msg( ptrinfo1 == len2-((char*)ptr2-(char*)NULL) &&
			 ptrinfo2 == len1-((char*)ptr1-(char*)NULL),
			 "Incorrect handling of Fortran 90 pointers");
      if (debug) 
	std::cerr << "Roccom: Setting f90 pointer treatment to INSERT" << std::endl;
    }
    else {
      if ( ptrinfo1 != 0 && ptrinfo1 == len4-((char*)ptr2-(char*)NULL) && 
	   ptrinfo2 != 0 && ptrinfo2 == len3-((char*)ptr1-(char*)NULL)) {
	if (debug) 
	  std::cerr << "Roccom: Setting f90 pointer treatment to APPEND. \n";
	insert_or_append = Roccom_base::FPTR_APPEND;
      }
      else if (debug) 
	std::cerr << "Roccom: Setting f90 pointer treatment to NONE.\n";
      
      if ( verb && (verb|1)==0) 
	std::cerr << "\tAt pass 1, " << "ptrinfo1 is " << ptrinfo1 
		  << " and ptrinfo2 is " << ptrinfo2
		  << " and\n\tat pass 2, ptrinfo1 is " 
		  << len3-((char*)ptr1-(char*)NULL) 
		  << " and ptrinfo2 is " 
		  << len4-((char*)ptr2-(char*)NULL) << std::endl;
      
      break;
    }
  }
  default: ;
  }
  return insert_or_append;
}

#endif







