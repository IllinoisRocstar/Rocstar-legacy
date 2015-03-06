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
// $Id: ex3.C,v 1.26 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstring>
#include <string>
#include "commpi.h"

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);

using namespace std;

extern int read_obj( std::istream &is, vector<double> &coors, 
		     vector<int> &elems);
extern void read_ij( std::istream &is, vector<double> &coors, int dims[2]);

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 7) {
    std::cout << "Usage: " << argv[0] 
	      << " <str_prefix> <str_num_blocks> <str_suffix> "
	      << " <tri_prefix> <tri_num_blocks> <tri_suffix>" << std::endl;
    exit(-1);
  }

  COM_set_verbose( 1);
  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");

  MPI_Comm comm = MPI_COMM_WORLD;
  const int comm_rank=0;
  const int comm_size=1;
 
  const char *str_prefix = argv[1];
  const int   str_num_blocks=atoi(argv[2]);
  const char *str_suffix = argv[3];
  assert( str_num_blocks <= 5);

  const char *tri_prefix = argv[4];
  const int   tri_num_blocks=atoi(argv[5]);
  const char *tri_suffix = argv[6];
  assert( tri_num_blocks <= 5);

  std::vector<double>  str_mesh_coors[5];
  std::vector<double>  str_mesh_nd[5];
  std::vector<double>  str_mesh_npress[5];
  std::vector<double>  str_mesh_fpress[5];
  int  dims[5][2];

  for ( int i=0; i<str_num_blocks; ++i) {
    if ( i%comm_size==comm_rank) {
      char fname[100];
      std::sprintf( fname, "%s%d%s", str_prefix, i+1, str_suffix);
      std::ifstream is( fname);
      assert( is != NULL);
      read_ij( is, str_mesh_coors[i], dims[i]);
      
      str_mesh_nd[i] = str_mesh_coors[i];
      str_mesh_npress[i].resize( dims[i][0]*dims[i][1]*3,3);
      str_mesh_fpress[i].resize( (dims[i][0]-1)*(dims[i][1]-1)*3,3);
    }
  }

  COM_new_window("str");

  COM_new_attribute("str.coor", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("str.npress", 'n', COM_DOUBLE, 3, "Pa");
  COM_new_attribute("str.fpress", 'e', COM_DOUBLE, 3, "Pa");
  
  for ( int i=0; i<str_num_blocks; ++i) {
    if (  i%comm_size==comm_rank) {
      COM_set_array( "str.:st2:", i+1, dims[i]);
      COM_set_array( "str.nc", i+1, &str_mesh_coors[i][0]);
      
      COM_set_array( "str.coor", i+1, &str_mesh_nd[i][0]);
      COM_set_array( "str.npress", i+1, &str_mesh_npress[i][0]);
      COM_set_array( "str.fpress", i+1, &str_mesh_fpress[i][0]);
    }
  }
  COM_window_init_done( "str");
    
  std::vector<double>  tri_mesh_coors[5];
  std::vector<int>     tri_mesh_elems[5];
  std::vector<double>  tri_mesh_nd[5];
  std::vector<double>  tri_mesh_npress[5];
  std::vector<double>  tri_mesh_fpress[5];

  for ( int i=0; i<tri_num_blocks; ++i) {
    if ( i%comm_size==comm_rank) {
      char fname[100];
      std::sprintf( fname, "%s%d%s", tri_prefix, i+1, tri_suffix);
      ifstream is( fname); assert( is != NULL);
      read_obj( is, tri_mesh_coors[i], tri_mesh_elems[i]);
      
      tri_mesh_nd[i] = tri_mesh_coors[i];
      tri_mesh_npress[i].resize( tri_mesh_coors[i].size(),3);
      tri_mesh_fpress[i].resize( tri_mesh_elems[i].size(),3);
    }
  }

  COM_new_window("tri");
  COM_new_attribute("tri.coor", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("tri.npress", 'n', COM_DOUBLE, 3, "Pa");
  COM_new_attribute("tri.fpress", 'e', COM_DOUBLE, 3, "Pa");

  for ( int i=0; i<tri_num_blocks; ++i) {
    if ( i%comm_size==comm_rank) {
      COM_set_size( "tri.nc", i+1, tri_mesh_coors[i].size()/3);
      COM_set_array( "tri.nc", i+1, &tri_mesh_coors[i][0]);
      COM_set_size( "tri.:t3:", i+1, tri_mesh_elems[i].size()/3);
      COM_set_array( "tri.:t3:", i+1, &tri_mesh_elems[i][0]);

      COM_set_array("tri.coor", i+1, &tri_mesh_nd[i][0]);
      COM_set_array("tri.npress", i+1, &tri_mesh_npress[i][0]);
      COM_set_array("tri.fpress", i+1, &tri_mesh_fpress[i][0]);
    }
  }
  COM_window_init_done( "tri");

  int tri_mesh = COM_get_attribute_handle( "tri.mesh");
  int str_mesh = COM_get_attribute_handle( "str.mesh");
  int RFC_clear = COM_get_function_handle( "RFC.clear_overlay");
  int RFC_write = COM_get_function_handle( "RFC.write_overlay");

  if ( comm_size == 1) {
    int RFC_overlay = COM_get_function_handle( "RFC.overlay");

    COM_call_function( RFC_overlay, &tri_mesh, &str_mesh, &comm);
    COM_call_function( RFC_write, &tri_mesh, &str_mesh, "tri", "str", "HDF");
    COM_call_function( RFC_clear, "tri", "str");
  }

  int RFC_read = COM_get_function_handle( "RFC.read_overlay");
  COM_call_function( RFC_read, &tri_mesh, &str_mesh, &comm,
		     "tri", "str", "HDF");

  char prefix1[100], prefix2[100];
  std::sprintf(prefix1, "tri_%d", comm_rank);
  std::sprintf(prefix2, "str_%d", comm_rank);

  if ( argc>7) {
    COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");
    std::sprintf(prefix1, "tri_coor%d", comm_rank);
    int OUT_write = COM_get_function_handle( "OUT.write_attribute");
    int OUT_set = COM_get_function_handle( "OUT.set_option");
    int tri_coor = COM_get_attribute_handle( "tri.coor");
    int str_coor = COM_get_attribute_handle( "str.coor");

    COM_call_function( OUT_set, "mode", "w");
    COM_call_function( OUT_write, prefix1, &tri_coor, "tri", "000");
    std::sprintf(prefix2, "str_coor%d", comm_rank);
    COM_call_function( OUT_write, prefix2, &str_coor, "str", "000");

    int RFC_transfer = COM_get_function_handle( "RFC.least_squares_transfer");
    COM_call_function( RFC_transfer, &tri_coor, &str_coor);
    COM_call_function( RFC_transfer, &str_coor, &tri_coor);

    COM_call_function( OUT_set, "mode", "a");
    COM_call_function( OUT_write, prefix1, &tri_coor, "tri", "001", "a");
    COM_call_function( OUT_write, prefix2, &str_coor, "str", "001", "a");

    int tri_npress = COM_get_attribute_handle( "tri.npress");
    int str_fpress = COM_get_attribute_handle( "str.fpress");
    std::sprintf(prefix1, "tri_press%d", comm_rank);
    std::sprintf(prefix2, "str_press%d", comm_rank);

    COM_call_function( OUT_set, "mode", "w");
    COM_call_function( OUT_write, prefix1, &tri_npress, "tri", "000");
    COM_call_function( OUT_write, prefix2, &str_fpress, "str", "000");

    int tri_fpress = COM_get_attribute_handle( "tri.fpress");
    int str_npress = COM_get_attribute_handle( "str.npress");
    COM_call_function( RFC_transfer, &str_npress, &tri_fpress);
    COM_call_function( RFC_transfer, &tri_fpress, &str_npress);

    COM_call_function( OUT_set, "mode", "a");
    COM_call_function( OUT_write, prefix1, &tri_fpress, "tri", "001", "a");
    COM_call_function( OUT_write, prefix2, &str_npress, "str", "001", "a");
    COM_call_function( RFC_clear, "tri", "str");
  }
   
  COM_delete_window( "tri");
  COM_delete_window( "str");

  COM_print_profile( "", "");

  COM_finalize();
}






