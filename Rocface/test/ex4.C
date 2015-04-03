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
// $Id: ex4.C,v 1.26 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include "commpi.h"

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);

using namespace std;

extern int read_obj( std::istream &is, std::vector<double> &coors,
		     std::vector<int> &elems);

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 7) {
    std::cout << "Usage: " << argv[0] 
	      << " <quad_prefix> <quad_num_blocks> <quad_suffix> "
	      << " <tri_prefix> <tri_num_blocks> <tri_suffix>" << std::endl;
    exit(-1);
  }

  COM_set_verbose( 1);
  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");

  MPI_Comm comm = MPI_COMM_WORLD;
  const int comm_rank=0;
  const int comm_size=1;
 
  const char *quad_prefix = argv[1];
  const int   quad_num_blocks=atoi(argv[2]);
  const char *quad_suffix = argv[3];
  assert( quad_num_blocks <= 5);

  const char *tri_prefix = argv[4];
  const int   tri_num_blocks=atoi(argv[5]);
  const char *tri_suffix = argv[6];
  assert( tri_num_blocks <= 5);

  std::vector<double>  quad_mesh_coors[5];
  std::vector<int>     quad_mesh_elems[5];
  std::vector<double>  quad_mesh_nd[5];
  std::vector<double>  quad_mesh_fd[5];

  for ( int i=0; i<quad_num_blocks; ++i) {
    if ( i%comm_size==comm_rank) {
      char fname[100];
      std::sprintf( fname, "%s%d%s", quad_prefix, i+1, quad_suffix);
      std::ifstream is( fname);
      assert( is != NULL);
      int n = read_obj( is, quad_mesh_coors[i], quad_mesh_elems[i]);
      assert( n==4);
      
      quad_mesh_nd[i] = quad_mesh_coors[i];
      quad_mesh_fd[i].resize( quad_mesh_elems[i].size()/4);
//       evaluate_pressure( quad_mesh_coors[i], quad_mesh_elems[i], 
// 			 quad_mesh_fd[i], 4, 1);
    }
  }

  COM_new_window("quad");

  COM_new_attribute("quad.disp", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("quad.press", 'e', COM_DOUBLE, 1, "Pascal");
  
  for ( int i=0; i<quad_num_blocks; ++i) {
    if ( i%comm_size==comm_rank) {
      COM_set_size( "quad.nc", i+1, quad_mesh_coors[i].size()/3);
      COM_set_array( "quad.nc", i+1, &quad_mesh_coors[i][0]);
      COM_set_size( "quad.:q4:", i+1, quad_mesh_elems[i].size()/4);
      COM_set_array( "quad.:q4:", i+1, &quad_mesh_elems[i][0]);
      
      COM_set_array( "quad.disp", i+1, &quad_mesh_nd[i][0]);
      COM_set_array( "quad.press", i+1, &quad_mesh_fd[i][0]);
    }
  }
  COM_window_init_done( "quad");
    
  std::vector<double>  tri_mesh_coors[5];
  std::vector<int>     tri_mesh_elems[5];
  std::vector<double>  tri_mesh_nd[5];
  std::vector<double>  tri_mesh_fd[5];

  COM_new_window("tri");
  COM_new_attribute("tri.disp", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("tri.press", 'e', COM_DOUBLE, 1, "Pascal");

  for ( int i=0; i<tri_num_blocks; ++i) {
    if ( i%comm_size==comm_rank) {
      char fname[100];
      std::sprintf( fname, "%s%d%s", tri_prefix, i+1, tri_suffix);
      ifstream is( fname); assert( is != NULL);
      int n = read_obj( is, tri_mesh_coors[i], tri_mesh_elems[i]);
      assert( n==3 || n==6);
      
      tri_mesh_nd[i] = tri_mesh_coors[i];
      tri_mesh_fd[i].resize( tri_mesh_elems[i].size()/3,1);
      
//       evaluate_pressure( tri_mesh_coors[i], tri_mesh_elems[i], 
// 			 tri_mesh_fd[i], 3, 1);

      COM_set_size( "tri.nc", i+1, tri_mesh_coors[i].size()/3);
      COM_set_array( "tri.nc", i+1, &tri_mesh_coors[i][0]);
      char *buf = fname;
      std::sprintf( buf, "tri.:t%d:", n);
      COM_set_size( buf, i+1, tri_mesh_elems[i].size()/n);
      COM_set_array( buf, i+1, &tri_mesh_elems[i][0]);
      
      COM_set_array("tri.disp", i+1, &tri_mesh_nd[i][0]);
      COM_set_array("tri.press", i+1, &tri_mesh_fd[i][0]);
    }
  }

  COM_window_init_done( "tri");

  int tri_mesh = COM_get_attribute_handle( "tri.mesh");
  int quad_mesh = COM_get_attribute_handle( "quad.mesh");
  int RFC_clear = COM_get_function_handle( "RFC.clear_overlay");
  int RFC_write = COM_get_function_handle( "RFC.write_overlay");

  if ( comm_size == 1) {    
    int RFC_overlay = COM_get_function_handle( "RFC.overlay");


    COM_call_function( RFC_overlay, &tri_mesh, &quad_mesh, &comm);
    COM_call_function( RFC_write, &tri_mesh, &quad_mesh, "tri", "quad", "HDF");
    COM_call_function( RFC_clear, "tri", "quad");
  }

  int RFC_read = COM_get_function_handle( "RFC.read_overlay");
  COM_call_function( RFC_read, &tri_mesh, &quad_mesh, &comm,
		     "tri", "quad", "HDF");

  char prefix1[100], prefix2[100];
  std::sprintf(prefix1, "tri_%d", comm_rank);
  std::sprintf(prefix2, "quad_%d", comm_rank);

  if ( argc > 7) {
    COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");

    std::sprintf(prefix1, "tri_fields%d", comm_rank);
    int OUT_write = COM_get_function_handle( "OUT.write_attribute");
    int OUT_set = COM_get_function_handle( "OUT.set_option");
    int tri_all = COM_get_attribute_handle( "tri.all");
    int quad_press = COM_get_attribute_handle( "quad.press");

    COM_call_function( OUT_write, prefix1, &tri_all, "tri", "001");
    std::sprintf(prefix2, "quad_fields%d", comm_rank);
    COM_call_function( OUT_write, prefix2, &quad_press, "quad", "001");

    int RFC_transfer = COM_get_function_handle( "RFC.least_squares_transfer");
    int tri_press = COM_get_attribute_handle( "tri.press");
    COM_call_function( RFC_transfer, &tri_press, &quad_press);
    COM_call_function( RFC_transfer, &quad_press, &tri_press);

    int tri_disp = COM_get_attribute_handle( "tri.disp");
    int quad_disp = COM_get_attribute_handle( "quad.disp");
    COM_call_function( RFC_transfer, &quad_disp, &tri_disp);
    COM_call_function( RFC_transfer, &tri_disp, &quad_disp);

    COM_call_function( OUT_set, "mode", "a");
    COM_call_function( OUT_write, prefix1, &tri_all, "tri", "002");
    std::sprintf(prefix2, "quad_fields%d", comm_rank);
    COM_call_function( OUT_write, prefix2, &quad_press, "quad", "002");
  
    COM_call_function( RFC_clear, "tri", "quad");
  }

  COM_delete_window( "tri");
  COM_delete_window( "quad");

  COM_print_profile( "", "");

  COM_finalize();
}






