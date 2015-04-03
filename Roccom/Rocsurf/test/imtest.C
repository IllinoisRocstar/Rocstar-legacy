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
// $Id: imtest.C,v 1.6 2008/12/06 08:43:24 mtcampbe Exp $

#include "roccom.h"
#include "surfbasic.h"
#include "IM_Reader.h"
#include <iostream>

using namespace std;

COM_EXTERN_MODULE( Rocout);
COM_EXTERN_MODULE( Rocsurf);
COM_EXTERN_MODULE( Rocblas);

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc != 3) {
    std::cout << "Usage:\n\tTo run in serial: " << argv[0] 
	      << " <surffile> <hdffile> " << endl;
    std::cout << "\n\tTo run in parallel: <mpirun-command> " << argv[0] 
	      << " -com-mpi <Rocin control file> <hdfoutput-prefix> " << endl;
    exit(-1);
  }

  std::cout << "Reading surface mesh file \"" << argv[1] << '"' << endl;

  std::string fname(argv[1]), wname;
  string::size_type n0 = fname.find_last_of( "/");

  if ( n0 != std::string::npos) 
    fname = fname.substr( n0+1, fname.size());

  string::size_type ni;
  ni = fname.find_first_of( ".:-*[]?\\\"\'0123456789");
  COM_assertion_msg(ni, "File name must start with a letter");

  if ( ni == std::string::npos) {
    wname = fname;
  }
  else {
    while (fname[ni-1]=='_') --ni; // Remove the '_' at the end.
    wname = fname.substr( 0, ni);
  }

  std::cout << "Creating window \"" << wname << '"' << endl;

  // Read in IM/HDF format
  int err = IM_Reader().read_winmesh( argv[1], wname); 
  COM_assertion( err>=0);

  // Allocate memory for normals
  const string nrmls = wname+".normals";
  COM_new_attribute(nrmls.c_str(), 'n', COM_DOUBLE, 3, "");
  COM_resize_array( nrmls.c_str());

  // Allocate an element and a nodal attribute for testing elements_to_nodes
  const string eval = wname+".eval";
  COM_new_attribute(eval.c_str(), 'e', COM_DOUBLE, 1, "m");
  COM_resize_array( eval.c_str());
  
  const string nval = wname+".nval";
  COM_new_attribute(nval.c_str(), 'n', COM_DOUBLE, 1, "m");
  COM_resize_array( nval.c_str());
  
  // Allocate memory for two window attributes
  int *vs_int;
  const string win1 = wname+".win1";
  COM_new_attribute( win1.c_str(), 'w', COM_INT, 1, "");
  COM_set_size( win1.c_str(), 0, 1);
  COM_resize_array( win1.c_str(), 0, &(void*&)vs_int);
  vs_int[0] = 1;

  double *vs_dbl;
  const string win2 = wname+".win2";
  COM_new_attribute( win2.c_str(), 'w', COM_DOUBLE, 3, "m");
  COM_set_size( win2.c_str(), 0, 1);
  COM_resize_array( win2.c_str(), 0, &(void*&)vs_dbl);
  vs_dbl[0] = 1; vs_dbl[1] = 2; vs_dbl[2] = 3;

  const string rks = wname+".franks";
  COM_new_attribute( rks.c_str(), 'n', COM_INT, 1, "");
  COM_resize_array( rks.c_str(), 0);

  COM_window_init_done( wname.c_str());
  int pmesh_hdl = COM_get_attribute_handle( (wname+".pmesh").c_str());

  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocout, "OUT");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocsurf, "SURF");

  int SURF_init = COM_get_function_handle( "SURF.initialize");
  COM_call_function( SURF_init, &pmesh_hdl);

  cout << "Testing feature detection..." << endl;
  int SURF_detect = COM_get_function_handle( "SURF.detect_features");
  COM_call_function( SURF_detect, &pmesh_hdl);

  int SURF_export = COM_get_function_handle( "SURF.export_franks");
  int franks = COM_get_attribute_handle( (wname+".franks").c_str());
  COM_call_function( SURF_export, &franks);

  cout << "Testing elements-to-nodes..." << endl;

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocblas, "BLAS");
  int copy_scalar = COM_get_function_handle( "BLAS.copy_scalar");
  int eval_hdl = COM_get_attribute_handle( eval.c_str());
  double one=1;
  COM_call_function( copy_scalar, &one, &eval_hdl);

  int SURF_e2n = COM_get_function_handle( "SURF.elements_to_nodes");
  int nval_hdl = COM_get_attribute_handle( nval.c_str());
  const int *scheme; COM_get_array_const( "SURF.E2N_ONE", 0, &scheme);
  COM_call_function( SURF_e2n, &eval_hdl, &nval_hdl, &pmesh_hdl, scheme);

  std::cout << "Computing normals..." << endl;

  int SURF_normal = COM_get_function_handle( "SURF.compute_normals");
  int nrmls_hdl = COM_get_attribute_handle( nrmls.c_str());

  COM_call_function( SURF_normal, &pmesh_hdl, &nrmls_hdl);

  std::cout << "Output window into file..." << endl;

  // Output normals
  int OUT_set = COM_get_function_handle( "OUT.set_option");
  int OUT_write = COM_get_function_handle( "OUT.write_attribute");

  COM_call_function( OUT_set, "mode", "w");
  int all_hdl = COM_get_attribute_handle( (wname+".all").c_str());
  COM_call_function( OUT_write, argv[2], &all_hdl, 
		     (char*)wname.c_str(), "000");
  
  // Output new normals
  COM_call_function( OUT_set, "mode", "a");
  COM_call_function( SURF_normal, &pmesh_hdl, &nrmls_hdl);
  COM_call_function( OUT_write, argv[2],  &all_hdl, 
		     (char*)wname.c_str(), "001");

  COM_finalize();
}






