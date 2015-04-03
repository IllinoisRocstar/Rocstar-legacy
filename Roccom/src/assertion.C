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
// $Id: assertion.C,v 1.12 2008/12/06 08:43:25 mtcampbe Exp $

/** \file assertion.C
 * This file contains the implementation for handling assertion failures.
 * @see roccom_assertion.h
 */
/* Author: Xiangmin Jiao
 * Created:  Jan. 10, 2001
 * Last modified: May 8, 2001
 */

#include "roccom_assertion.h"
#include <cstdlib>
#include <iostream>
#include <cstdio>

COM_BEGIN_NAME_SPACE

#ifndef DOXYGEN_SHOULD_SKIP_THIS

// standard error handlers
// -----------------------
static
void
_standard_error_handler( const char* what,
			 const char* expr,
			 const char* file,
			 int         line,
			 const char* msg )
{
  std::cerr << "ROCCOM error: " << what << " violation!" << std::endl
         << "Expr: " << expr << std::endl
         << "File: " << file << std::endl
         << "Line: " << line << std::endl;
    if ( msg != 0)
        std::cerr << "Explanation:" << msg << std::endl;
}


// default handler settings
// ------------------------
static Failure_function
_error_handler = _standard_error_handler;
static Failure_behaviour _error_behaviour   = ABORT;

// failure functions
// -----------------
void
assertion_fail( const char* expr,
		const char* file,
		int         line,
		const char* msg )
{
    extern void printStackBacktrace();

    (*_error_handler)("assertion", expr, file, line, msg);
    printStackBacktrace(); // Print stack backtrace
    switch (_error_behaviour) {
    case ABORT:
        abort();
    case EXIT:
        exit(1);  // EXIT_FAILURE
    case EXIT_WITH_SUCCESS:
        exit(0);  // EXIT_SUCCESS
    case CONTINUE:
        ;
    }
}

Failure_behaviour
set_error_behaviour(Failure_behaviour eb)
{
    Failure_behaviour result = _error_behaviour;
    _error_behaviour = eb;
    return result;
}


// error handler set functions
// ---------------------------
Failure_function
set_error_handler( Failure_function handler)
{
    Failure_function result = _error_handler;
    _error_handler = handler;
    return( result);
}

#endif

COM_END_NAME_SPACE






