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
// $Id: param_test.C,v 1.3 2008/12/06 08:43:20 mtcampbe Exp $
// $Author: mtcampbe $
// $Date: 2008/12/06 08:43:20 $

// $Log: param_test.C,v $
// Revision 1.3  2008/12/06 08:43:20  mtcampbe
// Updated license.
//
// Revision 1.2  2008/11/19 22:16:34  mtcampbe
// Added Illinois Open Source License/Copyright
//
// Revision 1.1  2004/11/03 20:12:45  palexand
// Initial versions.
//

/** A test program that reads in a given parameter file and writes 
 *  out the resulting parameter window into one HDF file using 
 *  Rocout with a given file prefix. */

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
	      << " <parameter file> <winname>\n"
	      << std::endl;
    exit(-1);
  }

  const char *FILE_in  = argv[1];
  const string winname(argv[2]);

  COM_set_profiling(1);
  COM_set_verbose(10);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");

  //===== Read in parameter file using Rocin
  int IN_param = COM_get_function_handle( "IN.read_parameter_file");

  COM_call_function(IN_param, FILE_in, winname.c_str());
  int PARAM_all = COM_get_attribute_handle ((winname+".all").c_str());

  COM_window_init_done(winname.c_str());

  //===== Write out using Rocout
  int OUT_write = COM_get_function_handle( "OUT.write_attribute");
  COM_call_function( OUT_write, winname.c_str(), 
 		     &PARAM_all,winname.c_str(), "000");

  COM_print_profile("", "");
  COM_finalize();
}






