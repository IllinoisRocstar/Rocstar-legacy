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
// $Id: roccom_assertion.h,v 1.6 2008/12/06 08:43:24 mtcampbe Exp $

/** \file roccom_assertion.h
 *   This file contains a set of routines for error assertion. 
 * @see assertion.C
 */

#ifndef __ROCCOM_ASSERTION_H
#define __ROCCOM_ASSERTION_H

#include "roccom_basic.h"

COM_BEGIN_NAME_SPACE

///  Behavior of failures.
enum Failure_behaviour { ABORT, EXIT, EXIT_WITH_SUCCESS, CONTINUE };

/// Function type for error handlers.
typedef void(*Failure_function)
  (const char*, const char*, const char*, int, const char*);
/// Default error handler.
void assertion_fail( const char*, const char*, int, const char*);

/// Controls the behavior when an assertion fails.
Failure_behaviour
set_error_behaviour(Failure_behaviour eb);
/// Sets the handler for assertion-failures.
Failure_function
set_error_handler( Failure_function handler);

/** Error checking utility similar to the assert macro of the C language.
 *  If NDEBUG is not defined, then it will invoke the error handler set by
 *  set_error_handler, and then perform the action according to the 
 *  Failure_behaviour set by set_error_behaviour. The default error handler
 *  is to print the assertion expression, file name and line number, and
 *  the default error behavior is to abort. If NDEBUG is defined, this
 *  macro has no effect.
 */
#ifndef NDEBUG
#define COM_assertion(EX) \
   ((EX)?((void)0): ::COM::assertion_fail( # EX , __FILE__, __LINE__, 0))
#define COM_assertion_msg(EX,msg) \
   ((EX)?((void)0): ::COM::assertion_fail( # EX , __FILE__, __LINE__, msg))
#else
#define COM_assertion(EX)     ((void)0)
#define COM_assertion_msg(EX,msg)     ((void)0)
#endif

COM_END_NAME_SPACE

#endif // __ROCCOM_ASSERTION_H






