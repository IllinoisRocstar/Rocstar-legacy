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
// $Id: ex5.C,v 1.21 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstring>
#include <string>
#include <cassert>
#include "commpi.h"

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);

using namespace std;

extern int read_obj( std::istream &is, vector<double> &coors, 
		     vector<int> &elems);

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 7) {
    std::cout << "Usage: " << argv[0] 
	      << " <t1_prefix> <t1_num_blocks> <t1_suffix> <t2_prefix>"
	      << " <t2_num_blocks> <t2_suffix>" << std::endl;
    exit(-1);
  }

  COM_set_verbose( 1);
  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");

  MPI_Comm comm = MPI_COMM_WORLD;
  const int comm_rank=0;
  const int comm_size=1;
 
  const char *t1_prefix = argv[1];
  const int   t1_num_blocks=atoi(argv[2]);
  const char *t1_suffix = argv[3];
  assert( t1_num_blocks <= 5);

  std::vector<double>  t1_mesh_coors[5];
  std::vector<int>     t1_mesh_elems[5];

  for ( int i=0; i<t1_num_blocks; ++i) if ( i%comm_size==comm_rank) {
    char fname[100];
    std::sprintf( fname, "%s%d%s", t1_prefix, i+1, t1_suffix);
    std::ifstream is( fname);
    assert( is != NULL);
    read_obj( is, t1_mesh_coors[i], t1_mesh_elems[i]);
  }

  COM_new_window("tri1");
  for ( int i=0; i<t1_num_blocks; ++i) if ( i%comm_size==comm_rank) {
    COM_set_size( "tri1.nc", i+1, t1_mesh_coors[i].size()/3);
    COM_set_array( "tri1.nc", i+1, &t1_mesh_coors[i][0]);
    COM_set_size( "tri1.:t3:", i+1, t1_mesh_elems[i].size()/3);
    COM_set_array( "tri1.:t3:", i+1, &t1_mesh_elems[i][0]);
  }
  COM_window_init_done( "tri1");

  const char *t2_prefix = argv[4];
  const int   t2_num_blocks=atoi(argv[5]);
  const char *t2_suffix = argv[6];
  assert( t2_num_blocks <= 5);

  std::vector<double>  t2_mesh_coors[5];
  std::vector<int>     t2_mesh_elems[5];

  for ( int i=0; i<t2_num_blocks; ++i) if ( i%comm_size==comm_rank) {
    char fname[100];
    std::sprintf( fname, "%s%d%s", t2_prefix, i+1, t2_suffix);
    std::ifstream is( fname);
    assert( is != NULL);
    read_obj( is, t2_mesh_coors[i], t2_mesh_elems[i]);
  }
  COM_new_window("tri2");
  for ( int i=0; i<t2_num_blocks; ++i) if ( i%comm_size==comm_rank) {
    COM_set_size( "tri2.nc", i+1, t2_mesh_coors[i].size()/3);
    COM_set_array( "tri2.nc", i+1, &t2_mesh_coors[i][0]);
    COM_set_size( "tri2.:t3:", i+1, t2_mesh_elems[i].size()/3); 
    COM_set_array( "tri2.:t3:", i+1, &t2_mesh_elems[i][0]);
 }
  COM_window_init_done( "tri2");

  int tri1_mesh = COM_get_attribute_handle( "tri1.mesh");
  int tri2_mesh = COM_get_attribute_handle( "tri2.mesh");
  int RFC_clear = COM_get_function_handle( "RFC.clear_overlay");
  int RFC_write = COM_get_function_handle( "RFC.write_overlay");
    
  if ( comm_size==1) {
    int RFC_overlay = COM_get_function_handle( "RFC.overlay");

    COM_call_function( RFC_overlay, &tri1_mesh, &tri2_mesh, &comm);
    COM_call_function( RFC_write, &tri1_mesh, &tri2_mesh, "tri1", "tri2", "HDF" );
    COM_call_function( RFC_clear, "tri1", "tri2");
  }
  
  int RFC_read = COM_get_function_handle( "RFC.read_overlay");
  COM_call_function( RFC_read, &tri1_mesh, &tri2_mesh, &comm,
		     "tri1", "tri2", "HDF");

  char prefix1[100], prefix2[100];
  std::sprintf(prefix1, "tri1_%d", comm_rank);
  std::sprintf(prefix2, "tri2_%d", comm_rank);
  
  COM_call_function( RFC_clear, "tri1", "tri2");

  COM_delete_window( "tri1");
  COM_delete_window( "tri2");

  COM_print_profile( "", "");

  COM_finalize();
}






