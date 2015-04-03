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
// $Id: pcommtest.C,v 1.4 2008/12/06 08:43:21 mtcampbe Exp $

#include "roccom.h"
#include "mapbasic.h"
#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 2) {
    std::cout << "Usage: " << argv[0] 
	      << " <out_hdf_file> " << endl;
    exit(-1);
  }

  std::cout << "Reading mesh file \"" << argv[1] << '"' << endl;

  std::string wname("surf");
//   std::string fname(argv[1]), wname;
//   string::size_type n0 = fname.find_last_of( "/");

//   if ( n0 != std::string::npos) 
//     fname = fname.substr( n0+1, fname.size());

//   string::size_type ni;
//   ni = fname.find_first_of( ".:-*[]?\\\"\'0123456789");
//   COM_assertion_msg(ni, "File name must start with a letter");

//   if ( ni == std::string::npos) {
//     wname = fname;
//   }
//   else {
//     while (fname[ni-1]=='_') --ni; // Remove the '_' at the end.
//     wname = fname.substr( 0, ni);
//   }

  std::cout << "Creating window \"" << wname << '"' << endl;

  // Read in HDF format
  COM_load_module( "Rocin", "IN");
  std::cout << "Reading window " << endl;
  int IN_read = COM_get_function_handle( "IN.read_window");
  //COM_call_function( IN_read, argv[1], wname.c_str());
  COM_call_function( IN_read, 
  		     "test/surf000[01].hdf", 
  		     wname.c_str());

  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");
  int mesh_hdl = COM_get_attribute_handle((wname+".mesh").c_str());
  std::cout << "Obtaining the mesh " << endl;
  COM_call_function( IN_obtain, &mesh_hdl, &mesh_hdl);

  // Change the memory layout to contiguous.
  std::cout << "Resizing the attribute arrays " << endl;
  COM_resize_array( (wname+".all").c_str(), 0, NULL, 0);

  // Delete all attributes and leave alone the mesh.
  //std::cout << "deleting the attribute" << endl;
  //COM_delete_attribute( (wname+".atts").c_str());

  int npanes; 
  int* pane_ids;
  COM_get_panes( wname.c_str(), &npanes, &pane_ids);
  COM_assertion( npanes>=0);
  
  std::cout<< "Labeling nodes with pane ids" << endl;
  COM_new_attribute("surf.pane_ids",'n', COM_FLOAT, 1, "empty");
  COM_resize_array("surf.pane_ids");
 
  int nitems;
  float *ptr;
  for(int j = 0; j <npanes; ++j){
    COM_get_size("surf.pane_ids", pane_ids[j], &nitems);
    std::cout << "  pane[" << j << "] has " << nitems << " items " << endl;
    COM_get_array("surf.pane_ids", pane_ids[j], &ptr);
    for(int k =0; k<nitems; ++k){
      ptr[k] = pane_ids[j];
    }
  }
  int pid_hdl = COM_get_attribute_handle("surf.pane_ids");

  COM_load_module( "Rocmap", "MAP");

  std::cout << "Performing an average-reduction on shared nodes." << endl;
  int MAP_average_shared = COM_get_function_handle( "MAP.reduce_average_on_shared_nodes");
  COM_call_function( MAP_average_shared, &pid_hdl);
  
  std::cout << "Updating ghost nodes." << endl;
  int MAP_update_ghost = COM_get_function_handle( "MAP.update_ghosts");
  COM_call_function( MAP_update_ghost, &pid_hdl);

  std::cout << "Finalizing the window" << endl;
  COM_window_init_done( wname.c_str());

  std::cout << "loading Rocout" << endl;
  COM_load_module("Rocout", "OUT");

  std::cout << "Output window into file..." << endl;

  // Output new mesh
  int OUT_set = COM_get_function_handle( "OUT.set_option");
  int OUT_write = COM_get_function_handle( "OUT.write_attribute");

  COM_call_function( OUT_set, "mode", "w");
  int all_hdl = COM_get_attribute_handle( (wname+".all").c_str());
  COM_call_function( OUT_write, argv[1], &all_hdl, 
		     (char*)wname.c_str(), "0000");
  
  COM_finalize();
}






