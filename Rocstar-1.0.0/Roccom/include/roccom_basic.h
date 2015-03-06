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
/* $Id: roccom_basic.h,v 1.30 2008/12/06 08:43:24 mtcampbe Exp $ */

/** \file roccom_basic.h
 * This file contains some definitions of macros and constants for Roccoms.
 * @see roccom.h
 */
/* Author: Xiangmin Jiao
 * Creation date:   Jan. 10, 2001
 */

#ifndef __ROCCOM_BASIC_H__
#define __ROCCOM_BASIC_H__

#ifdef __cplusplus
/** \namespace COM
 *  The name space for Roccom. 
 */
/** Macro for marking the begining of the namespace */
#define COM_BEGIN_NAME_SPACE  namespace COM {
/** Macro for marking the end of the namespace */
#define COM_END_NAME_SPACE    }
/** Macro for using the namespace */
#define USE_COM_NAME_SPACE    using namespace COM;

class COM_Object {
public:
  COM_Object() : _cookie( COM_COOKIE) {}
  COM_Object( const COM_Object &m) : _cookie( &m? m._cookie : COM_COOKIE) {}
  virtual ~COM_Object() {}

  /** Return 0 if there is no error.
   *  Return 1 if pointers do not match.
   *  Return 2 if cookie does not match
   *  return 3 if neither matches. */
  int validate_object( void *obj=0) const { 
    int ierr=0;
    if ( obj && obj!=this) ierr+=1;
    if ( _cookie != COM_COOKIE) ierr += 2;
    return ierr;
  }

protected:
  enum { COM_COOKIE = 762266 };

  int _cookie;
};

typedef void (COM_Object::*Member_func_ptr)();
typedef void (COM_Object::*COM_Member_func_ptr)();
/**< Pointer of member functions. */
#endif
/*}*/

/** Macro for renaming Fortran functions. */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#ifdef COM_APPEND_TWOUNDERSCORE
#  define COM_F_FUNC(x)  x ## __
#elif defined(COM_APPEND_UNDERSCORE)
#  define COM_F_FUNC(x)  x ## _
#else
#  define COM_F_FUNC(x)  x
#endif

#include "FC.h"
#define COM_F_FUNC2( lowcase, uppercase ) FC_GLOBAL(lowcase, uppercase)
/* #ifdef COM_UPPERCASE
// #define COM_F_FUNC2( lowcase, uppercase) COM_F_FUNC( uppercase)
// #else
// #define COM_F_FUNC2( lowcase, uppercase) COM_F_FUNC( lowcase)
// #endif
*/

#endif

#ifdef STATIC_LINK
#  define COM_LOAD_MODULE_STATIC_DYNAMIC(moduleName,windowString) \
     { MPI_Comm comm = COM_get_default_communicator();	\
       COM_set_default_communicator(MPI_COMM_SELF);	\
       moduleName##_load_module(windowString); 	\
       COM_set_default_communicator(comm);	\
     }
#  define COM_UNLOAD_MODULE_STATIC_DYNAMIC(moduleName,windowString) \
     moduleName##_unload_module(windowString);

#  define COM_EXTERN_MODULE( moduleName) \
     extern "C" void moduleName##_load_module( const char *); \
     extern "C" void moduleName##_unload_module( const char *)
#else
#  define COM_LOAD_MODULE_STATIC_DYNAMIC(moduleName,windowString) \
     COM_load_module(#moduleName,windowString)
#  define COM_UNLOAD_MODULE_STATIC_DYNAMIC(moduleName,windowString) \
     COM_unload_module(#moduleName,windowString)

#  define  COM_EXTERN_MODULE( moduleName) 
#endif

/** The maxinum length of name string passed by users. */
#define MAX_NAMELEN    128

typedef int  COM_Type;              /**< Indices for derived data types.*/
typedef void (*Func_ptr)();         /**< Pointer of functions. */
typedef Func_ptr COM_Func_ptr;

/** Data types */
enum { 
  /** C/C++ Data types */
  COM_CHAR, COM_UNSIGNED_CHAR, COM_BYTE, COM_SHORT,
  COM_UNSIGNED_SHORT, COM_INT,  COM_UNSIGNED, COM_LONG,
  COM_UNSIGNED_LONG, COM_FLOAT,  COM_DOUBLE, COM_LONG_DOUBLE, COM_BOOL,
  /** Fortran Data types */
  COM_CHARACTER=13, COM_LOGICAL,  COM_INTEGER, COM_REAL,
  COM_DOUBLE_PRECISION, COM_COMPLEX, COM_DOUBLE_COMPLEX,
  /** Other */
  COM_MPI_COMMC, COM_MPI_COMM=COM_MPI_COMMC, COM_MPI_COMMF, 
  COM_MAX_TYPEID=COM_MPI_COMMF,
  COM_STRING = -1, COM_RAWDATA = -2, COM_METADATA = -3,  COM_VOID = -4, 
  COM_F90POINTER = -5, COM_OBJECT = -6, COM_MIN_TYPEID = -6 };

#endif






