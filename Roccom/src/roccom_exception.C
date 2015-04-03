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
// $Id: roccom_exception.C,v 1.13 2008/12/06 08:43:25 mtcampbe Exp $

#include "roccom_exception.h"
#include <iostream>
#include <cstdio>

#if defined(__gnu_linux__) && defined(__i386__)
#define TRACEBACK 1 // Define TRACEBACK on Linux and Apple
#endif

#ifdef TRACEBACK
#include <execinfo.h> /* for backtrace (GNU glibc header) */
#endif

COM_BEGIN_NAME_SPACE

/// Print the stack backtrace.
void printStackBacktrace() {
#if !defined( __CHARMC__) && defined(TRACEBACK)
  const int max_stack=64;
  void *stackPtrs[max_stack];

  int levels=backtrace(stackPtrs,max_stack);
  char **symbols=backtrace_symbols(stackPtrs, levels);

  const int nSkip=2;
  std::printf( "\nStack Backtrace:\n");
  for ( int i=nSkip; i<levels; i++) {
    std::printf( " [%d] %s\n", i-nSkip, symbols[i]);
  }
#endif
}

std::ostream &operator<<( std::ostream& os, const COM_exception &ex) {
  os << "ROCCOM: " << (ex.ierr<1000?"WARNING ":"ERROR ")
     << ex.ierr << ": ";
  std::string msg;
  switch (ex.ierr) {
  case COM_WARN_DUP_WINDOW:    
    msg = "Duplicated window"; break;
  case COM_WARN_DUP_ATTRIBUTE: 
    msg = "Duplicated attribute"; break;
  case COM_WARN_DUP_FUNCTION:  
    msg = "Duplicated function"; break;
  case COM_WARN_DUP_FUNC:        
    msg = "Duplicated function"; break;
  case COM_WARN_EMPTY_MESH:      
    msg = "Empty mesh"; break;
  case COM_WARN_MODULE_LOADED:
    msg = "Module has already been loaded"; break;

  case COM_ERR_MODULE_NOTLOADED:      
    msg = "Module was never loaded"; break;
  case COM_ERR_COULD_OPENLIB:      
    msg = "Could not open library"; break;
  case COM_ERR_COULD_FINDSYM:      
    msg = "Could not find symbol"; break;

  case COM_ERR_WAS_INITIALIZED:    
    msg = "Roccom was already initialized"; break;
  case COM_ERR_WASNOT_INITIALIZED: 
    msg = "Roccom was not initialized"; break;

  case COM_ERR_INIT_DONE_PANEMAP:  
    msg = "Pane map has changed but input argument claims not"; break;
  case COM_ERR_WINDOW_NOTEXIST:  
    msg = "Window does not exist"; break;
  case COM_ERR_PANE_NOTEXIST:
    msg = "Pane does not exist"; break;
  case COM_ERR_ATTRIBUTE_NOTEXIST: 
    msg = "Attribute does not exist"; break;
  case COM_ERR_FUNCTION_NOTEXIST:  
    msg = "Function does not exist"; break;
  case COM_ERR_UNKNOWN_KEYWORD:    
    msg = "Unknown keyword"; break;
  case COM_ERR_UNKNOWN_DATATYPE:
    msg = "Got an unknown data type"; break;
  case COM_ERR_INVALID_ATTRIBUTE_NAME: 
    msg = "Invalid attribute name"; break;

  case COM_ERR_ATTRIBUTE_INITIALIZED: 
    msg = "Attribute was already intialized"; break;
  case COM_ERR_ATTRIBUTE_NOT_ALLOCATED: 
    msg = "Attribute was not allocated by Roccom"; break;
  case COM_ERR_ATTRIBUTE_CONST: 
    msg = "A const array cannot be modified, and a non-const reference to it\n\
           cannot be obtained."; break;
  case COM_ERR_ATTRIBUTE_ALLOCATED: 
    msg = "Attribute has already been allocated by Roccom"; break;
  case COM_ERR_ALLOC_STRUCTURED:
    msg = "Connectivity for structured meshes cannot be Allocated"; break;
  case COM_ERR_INVALID_DIMENSION:  
    msg = "Invalid number of components or dimensions"; break;
  case COM_ERR_INVALID_SIZE:
    msg = "Invalid size. Cannot be negative or larger than capacity, \n\
   and the number of ghosts cannot be greater than the total number of items."; break;
  case COM_ERR_INVALID_CAPACITY:
    msg = "Invalid capacity. Cannot be smaller than the registered \n\
           size or be zero when setting a nonnull array"; break;
  case COM_ERR_INVALID_STRIDE:
    msg = "Invalid stride. Must be either 1 or >= the number of components"; break;
  case COM_ERR_INVALID_ADDR: 
    msg = "Invalid address. Probably is trying to register a local/stack variable."; break;

  case COM_ERR_F90FUNC:
    msg = "Only F90 functions can accept F90 pointers"; break;
  case COM_ERR_UNKNOWN_INTENT:     
    msg = "Unknown intention of argument"; break;
  case COM_ERR_TOO_MANY_ARGS:      
    msg = "Too many arguments"; break;
  case COM_ERR_TOO_FEW_ARGS:       
    msg = "Too few argument"; break;
  case COM_ERR_INVALID_FUNCTION_HANDLE:      
    msg = "Received an invalid function handle"; break;
  case COM_ERR_INVALID_ATTRIBUTE_HANDLE:       
    msg = "Received an invalid attribute handle"; break;
  case COM_ERR_INVALID_WINDOW_HANDLE:       
    msg = "Received an invalid window handle"; break;
  case COM_ERR_INCOMPATIBLE_ATTRS:
    msg = "Incompatible attributes"; break;
  case COM_ERR_INCOMPATIBLE_TYPES: 
    msg = "Incompatible data types"; break;

  case COM_ERR_NOT_A_WINDOW_ATTRIBUTE:
    msg = "Not a window attribute"; break;
  case COM_ERR_NOT_A_POINTER:
    msg = "Trying to nullify an attribute that is not a pointer"; break;
  case COM_ERR_IMMUTABLE:
    msg = "Immutable attributes cannot be passed to mutable arguments"; break;
  case COM_ERR_NULL_POINTER:
    msg = "Unexpected null pointer"; break;
  case COM_ERR_OUT_OF_MEMORY:      
    msg = "Out of memory"; break;
  case COM_ERR_INDEX_OUT_OF_BOUNDS:      
    msg = "Index out of bounds"; break;

  case COM_ERR_PARENT_SMALLER:
    msg = "Parent window does not have all the panes in child window"; break;
  case COM_ERR_CHANGE_INHERITED:
    msg = "Trying to change the meta-data of an inherited attribute"; break;
  case COM_ERR_RESIZE:
    msg = "Trying to resize an inherited or user-allocated attribute"; break;
  case COM_ERR_INHERIT_MESH_LATE:
    msg = "Mesh must be used/cloned before creating any attribute"; break;
  case COM_ERR_INHERIT_ST_NG:
    msg = "Inheritance w/o ghost is not supported for structured meshes"; break;
  case COM_ERR_MIXED_GHOST:
    msg = "Ghost elements of unstructured mesh must be at the end"; break;
  case COM_ERR_GHOST_NODES:
    msg = "The number of ghost nodes must be <= the total number of nodes"; break;
  case COM_ERR_GHOST_ELEMS:
    msg = "The number of ghost elements must be <= the total number of elements"; break;
  case COM_ERR_GHOST_LAYERS:
    msg = "The number of ghost layers must be <= half of each dimension"; break;
  case COM_ERR_APPEND_ARRAY:
    msg = "Appending array is supported only for window and pane attributes without ghosts"; break;
  case COM_UNKNOWN_ERROR:
  default: 
    msg = "Unknow error"; 
  }

  os << msg << "\nROCCOM: Error encountered when processing " 
     << ex.msg << std::endl << "ROCCOM: ";
  return os;
}

std::string
append_frame_info( const std::string &s, const char *frame,
		   const char *file, int line) {
  char buf[1000];
  
  std::sprintf( buf, "\nROCCOM: In %s of file %s at line %d",
		frame, file, line);
  return s+buf;
}

COM_END_NAME_SPACE






