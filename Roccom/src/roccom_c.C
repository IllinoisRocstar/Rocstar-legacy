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
// $Id: roccom_c.C,v 1.19 2008/12/06 08:43:25 mtcampbe Exp $

/** \file roccom_c.C
 *  This file contains the wrapper routines for the C binding
 *  of Roccom implementation.
 *  @see roccom_c.h
 */
/* Author: Xiangmin Jiao */

#include "Roccom_base.h"
#include "roccom_assertion.h"
#include <cstdarg>

// Include C++ implementation
#define C_ONLY
#include "roccom_c.h"
#include "roccom_c++.h" 

USE_COM_NAME_SPACE

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

// Blocking function calls
void COM_call_function( const int wf, int argc, ...) {
  COM_assertion_msg( argc <= Function::MAX_NUMARG, "Too many arguments") ;

  int i;
  void *args[Function::MAX_NUMARG];
  va_list   ap;
  
  va_start( ap, argc);
  for ( i=0; i<argc; ++i) {
    args[i] = va_arg( ap, void*);
  }
  va_end( ap);
  COM_get_roccom()->call_function( wf, argc, args);
}

// Non-blocking function calls
void COM_icall_function( const int wf, int argc, ...) {
  COM_assertion_msg( argc <= Function::MAX_NUMARG, "Too many arguments") ;

  int i;
  void *args[Function::MAX_NUMARG];
  va_list   ap;

  va_start( ap, argc);
  for ( i=0; i<argc-1; ++i) {
    args[i] = va_arg( ap, void*);
  }
  int *status = va_arg( ap, int *);
  va_end( ap);
  COM_get_roccom()->icall_function( wf, argc, args, status); 
}

void COM_get_attribute( const char *wa_str, char *loc, 
			int *type, int  *size, char *u_str,  int u_len) 
{
  std::string unit;
  COM_get_roccom()->get_attribute( wa_str, loc, type, size, &unit);
  if ( u_str && u_len) {
    int len=unit.size(), n=std::min(len, int(u_len));
    std::copy( unit.c_str(), unit.c_str()+n, u_str);
    std::fill_n( u_str+1+n, std::max(0,int(u_len-len)), 0);
  }
}


#ifdef __cplusplus
}
#endif






