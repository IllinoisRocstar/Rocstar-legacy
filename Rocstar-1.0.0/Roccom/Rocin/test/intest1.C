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
// $Id: intest1.C,v 1.11 2008/12/06 08:43:20 mtcampbe Exp $

/** A test program that reads in given input HDF files using Rocin and 
 *  write out  all the contents into one HDF file using Rocout with 
 *  a given file prefix. */

#include "Rocin.h"
#include "Roccom_base.h"
#include "roccom.h"
#include "roccom_devel.h"

#include <iostream>
#include <cstring>
#include <string>

COM_EXTERN_MODULE( Rocin);
COM_EXTERN_MODULE( Rocout);

using namespace std;

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 3 || argc > 3 ) {
    std::cout << "Usage: To test in serial: \n\t" << argv[0] 
	      << " <input HDF file|Rocin control file> <winname>\n"
	      << "To test in parallel: \n\t" << argv[0]
	      << " -com-mpi <Rocin control file> <winname>\n"
	      << std::endl;
    // Note: -com-mpi option will be eaten away by COM_init.
    exit(-1);
  }

  const char *HDF_in  = argv[1];
  const string winname(argv[2]);

  COM_set_profiling(1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");

  //===== Read in using Rocin
  int IN_read;
  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");

  const char *lastdot=std::strrchr( HDF_in, '.');
  if ( lastdot && std::strcmp( lastdot, ".txt")==0) {
    IN_read = COM_get_function_handle( "IN.read_by_control_file");
  }
  else {
    IN_read = COM_get_function_handle( "IN.read_windows");    
  }

  COM_call_function( IN_read, HDF_in, winname.c_str());

  int NEW_all;
  NEW_all = COM_get_attribute_handle ((winname+".all").c_str());

  COM_call_function( IN_obtain, &NEW_all, &NEW_all);

  //===== Write out using Rocout
  int OUT_write = COM_get_function_handle( "OUT.write_attribute");
  COM_call_function( OUT_write, winname.c_str(), 
 		     &NEW_all, winname.c_str(), "000");

  COM_print_profile("", "");
  COM_finalize();
}






