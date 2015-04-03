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
// $Id: param_test.C,v 1.5 2008/12/06 08:43:23 mtcampbe Exp $
// $Author: mtcampbe $
// $Date: 2008/12/06 08:43:23 $

// $Log: param_test.C,v $
// Revision 1.5  2008/12/06 08:43:23  mtcampbe
// Updated license.
//
// Revision 1.4  2008/11/19 22:16:38  mtcampbe
// Added Illinois Open Source License/Copyright
//
// Revision 1.3  2005/06/18 22:26:10  jiao
// Fixed bug in casting char*
//
// Revision 1.2  2004/11/08 16:38:34  palexand
// Added a constant, WINDOW_EXISTS, to make it easier to switch between the
// case where the parameter window exists prior to calling
// Rocin::read_parameter_file, and the case where the parameter window doesn't
// exist.
//
// Revision 1.1  2004/11/04 20:03:31  palexand
// Initial versions.  Supports Rocout::write_parameter_file(...) in serial.
//
// Revision 1.1  2004/11/03 20:12:45  palexand
// Initial versions.
//

/** A test program that reads in a given parameter file using Rocin
 *  and writes out the resulting parameter window into a new 
 *  parameter file using Rocout.
*/

// Set WINDOW_EXISTS to 1 to test behavior if the parameter
// window exists prior to calling Rocin::read_parameter_file.
// Doing so will register a window with one attribute of
// the correct parameter format, "burnrate", and all other
// parameters in the parameter file will be discarded.
// If WINDOW_EXISTS is set to 0, then no parameter window
// will exist prior to calling Rocin::read_parameter_file,
// so all correctly formatted (option,value) pairs are registered.
#define WINDOW_EXISTS 0

#include "Rocin.h"
#include "Rocout.h"
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
	      << " <parameter file in> <parameter file out>\n"
	      << std::endl;
    exit(-1);
  }

  const char *FILE_in  = argv[1];
  const char *FILE_out = argv[2]; 
  const string winname("parameters");
    //const string winname(argv[2]);

  COM_set_profiling(1);
  //COM_set_verbose(10);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");
  int IN_param = COM_get_function_handle( "IN.read_parameter_file");
  int OUT_param = COM_get_function_handle( "OUT.write_parameter_file");  

  //===== Add some non parameter formatted attributes to the window
  // a parameter formatted attribute is a windowed COM_CHAR attribute
  // with 1 component
  void* addr;

  if(WINDOW_EXISTS){
    COM_new_window(winname.c_str());
    COM_new_attribute( (winname+"."+"burnrate").c_str(),'w',COM_CHAR,1,"");
    COM_set_size( (winname+"."+"burnrate").c_str(),0,6,0);
    COM_resize_array( (winname+"."+"burnrate").c_str(),0,&addr);
    ((char*&)addr) = "three";
    
    COM_new_attribute( (winname+"."+"notwindowed").c_str(),'p',COM_CHAR,1,"");
    COM_set_size( (winname+"."+"notwindowed").c_str(),0,12,0);
    COM_resize_array( (winname+"."+"notwindowed").c_str(),0,&addr);
    ((char*&)addr) = "notwindowed";
    
    COM_new_attribute( (winname+"."+"wrongtype").c_str(),'w',COM_INT,1,"");
    COM_set_size( (winname+"."+"wrongtype").c_str(),0,1,0);
    COM_resize_array( (winname+"."+"wrongtype").c_str(),0,&addr);
    ((int*&)addr)[1] = 1;
    
    COM_new_attribute( (winname+"."+"toomanycomps").c_str(),'w',COM_CHAR,2,"");
    COM_set_size( (winname+"."+"toomanycomps").c_str(),0,13,0);
    COM_resize_array( (winname+"."+"toomanycomps").c_str(),0,&addr);
    ((char*&)addr) = "toomanycomps";
    
    COM_window_init_done(winname.c_str());
  }

  //===== Read in parameter file using Rocin
  COM_call_function(IN_param, FILE_in, (winname).c_str());

  //===== Write out using Rocout
  COM_call_function( OUT_param, FILE_out, (winname).c_str());

  COM_print_profile("", "");
  COM_finalize();
}






