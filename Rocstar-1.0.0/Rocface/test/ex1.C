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
// $Id: ex1.C,v 1.20 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <iostream>
#include <vector>
#include <cstdio>
#include <cassert>

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);

using namespace std;

// ==== Routines for creating mesh information
void init_structured_mesh( double coors[][3], int nrow, int ncol, 
			   int rank, int nproc) {
  // consider the processors as a 2*(nproc/2) grid
  int proc_col=nproc;
  if (nproc%2 ==0) {
    proc_col=nproc/2;
  }
  else {
    proc_col=nproc;
  }
  int row=rank/proc_col, col=rank%proc_col;
  
  const double width=100., length=100.;
  
  for (int i=0; i<nrow; ++i) {
    for (int j=0; j<ncol; ++j) {
      coors[i*ncol+j][0]=col*length+length/(ncol-1)*j;
      coors[i*ncol+j][1]=row*width+width/(nrow-1)*i;
      coors[i*ncol+j][2]=0.01;
    }
  }
}

void init_unstructure_mesh( double coors[][3], int elmts[][4], 
			    int nrow, int ncol, int rank, int nproc) {
  // consider the processors as a 2*(nproc/2) grid
  int proc_col=nproc;
  if (nproc%2 ==0) {
    proc_col=nproc/2;
  }
  else {
    proc_col=nproc;
  }

  int row=rank/proc_col, col=rank%proc_col;

  const double width=100., length=100.;

  for (int i=0; i<nrow; ++i) {
    for (int j=0; j<ncol; ++j) {
      coors[i*ncol+j][0]=col*length+length/(ncol-1)*j;
      coors[i*ncol+j][1]=row*width+width/(nrow-1)*i;
      coors[i*ncol+j][2]=0;
    }
  }

  // computes elmts
  for (int i=0; i<nrow-1; ++i) {
    for (int j=0; j<ncol-1; ++j) {
      elmts[i*(ncol-1)+j][0]=i*ncol+j+1;
      elmts[i*(ncol-1)+j][1]=i*ncol+j+ncol+1;
      elmts[i*(ncol-1)+j][2]=i*ncol+j+ncol+2;
      elmts[i*(ncol-1)+j][3]=i*ncol+j+2;
    }
  }
}

const int total_npanes=4;
const int nrow=4, ncol=3;
const int num_nodes=nrow*ncol;
const int num_elmts=(nrow-1)*(ncol-1);

double  coors_s[total_npanes][num_nodes][3];
double  coors_f[total_npanes][nrow*ncol][3];
double  velcts_s[total_npanes][num_nodes][3];
double  velcts_f[total_npanes][nrow*ncol][3];
double  disps_s[total_npanes][num_nodes][3];
double  disps_f[total_npanes][nrow*ncol][3];

double  burnr_f[3][total_npanes][(nrow-1)*(ncol-1)];
double  burnr_s[3][total_npanes][(nrow-1)*(ncol-1)];

int  elmts[total_npanes][num_elmts][4];


int main(int argc, char *argv[]) {
  const int comm_rank=0;
  const int comm_size=1; 
  const int npanes=total_npanes, first_pane_id=1;
 
  if ( comm_rank == 0)
    std::cout << "Initializing Roccom and Rocface" << std::endl;
  COM_init( &argc, &argv);
  // COM_set_verbose( 1);
  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");

  if ( comm_rank == 0)
    std::cout << "Creating window \"unstr\"" << std::endl;
  COM_new_window("unstr");
  COM_new_attribute("unstr.velo", 'n', COM_DOUBLE, 3, "m/s");
  COM_new_attribute("unstr.disp", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("unstr.burnr", 'e', COM_DOUBLE, 3, "m/s");

  for ( int pid=first_pane_id-1; pid<first_pane_id-1+npanes; ++pid) {
    init_unstructure_mesh( coors_s[pid], elmts[pid], 
			   nrow, ncol, pid, total_npanes);

    COM_set_size( "unstr.nc", pid+1, num_nodes);
    COM_set_array( "unstr.nc", pid+1, &coors_s[pid][0][0]);
    COM_set_size( "unstr.:q4:", pid+1, num_elmts);
    COM_set_array( "unstr.:q4:", pid+1, &elmts[pid][0][0]);
    
    COM_set_array( "unstr.velo", pid+1, &velcts_s[pid][0][0]);
    COM_set_array( "unstr.disp", pid+1, &disps_s[pid][0][0]);

    COM_set_array("unstr.burnr", pid+1, &burnr_s[0][pid][0], 1);
  }
  COM_window_init_done( "unstr");

  if ( comm_rank == 0)
    std::cout << "Creating window \"str\"" << std::endl;
  COM_new_window("str");
  COM_new_attribute("str.velo", 'n', COM_DOUBLE, 3, "m/s");
  COM_new_attribute("str.disp", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("str.burnr", 'e', COM_DOUBLE, 3, "m/s");

  for ( int pid=first_pane_id-1; pid<first_pane_id-1+npanes; ++pid) {
    init_structured_mesh(coors_f[pid], nrow, ncol, pid, total_npanes);
    
    int dims[2] = { ncol, nrow}; // Use Fortran convention
    COM_set_array("str.:st2:", pid+1, &dims[0]);
    COM_set_array("str.nc", pid+1, &coors_f[pid][0][0]);

    COM_set_array("str.velo", pid+1, &velcts_f[pid][0][0]);
    COM_set_array("str.disp", pid+1, &disps_f[pid][0][0]);

    COM_set_array("str.burnr", pid+1, &burnr_f[0][pid][0], 1);
  }

  COM_window_init_done( "str");

  for ( int p=first_pane_id-1; p<first_pane_id-1+npanes; ++p) {
    for (int i=0; i<nrow*ncol; ++i) {
      disps_s[p][i][0] = disps_f[p][i][0] = coors_f[p][i][0];  
      disps_s[p][i][1] = disps_f[p][i][1] = coors_f[p][i][1];  
      disps_s[p][i][2] = disps_f[p][i][2] = coors_f[p][i][2];
      velcts_f[p][i][0] = velcts_s[p][i][0] = i;  
      velcts_f[p][i][1] = velcts_s[p][i][1] = i;  
      velcts_f[p][i][2] = velcts_s[p][i][2] = i; 
    }
    for (int i=0; i<(nrow-1)*(ncol-1); ++i) {
      burnr_s[0][p][i] = burnr_f[0][p][i] = i;  
      burnr_s[1][p][i] = burnr_f[1][p][i] = i;  
      burnr_s[2][p][i] = burnr_f[2][p][i] = i;  
    }
  }

  int str_mesh = COM_get_attribute_handle( "str.mesh");
  int unstr_mesh = COM_get_attribute_handle( "unstr.mesh");
  int RFC_clear = COM_get_function_handle( "RFC.clear_overlay");

  const char *format="HDF";

  int RFC_write = COM_get_function_handle( "RFC.write_overlay");
  if ( comm_size == 1) { // Perform mesh overlay.
    int RFC_overlay = COM_get_function_handle( "RFC.overlay");

    std::cout << "Overlaying meshes..." << std::endl;
    COM_call_function( RFC_overlay, &str_mesh, &unstr_mesh);
    COM_call_function( RFC_write, &str_mesh, &unstr_mesh, "str", "unstr", format);
    COM_call_function( RFC_clear, "str", "unstr");
  }

  int RFC_read = COM_get_function_handle( "RFC.read_overlay");
  COM_call_function( RFC_read, &str_mesh, &unstr_mesh, NULL,
		     "str", "unstr", format);

  char prefix1[100], prefix2[100];
  std::sprintf(prefix1, "str%d", comm_rank);
  std::sprintf(prefix2, "unstr%d", comm_rank);

  if ( argc>1) {
    if ( comm_rank == 0)
      std::cout << "Perform data transfer..." << std::endl;
    std::sprintf(prefix1, "str_disp%d", comm_rank);
    std::sprintf(prefix2, "unstr_disp%d", comm_rank);

    COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");
    
    int OUT_write = COM_get_function_handle( "OUT.write_attribute");
    int OUT_set = COM_get_function_handle( "OUT.set_option");
    int str_disp = COM_get_attribute_handle( "str.disp");
    int unstr_disp = COM_get_attribute_handle( "unstr.disp");
    COM_call_function( OUT_write, prefix1, &str_disp, "str", "000");
    COM_call_function( OUT_write, prefix2, &unstr_disp, "unstr", "000");

    int RFC_transfer = COM_get_function_handle( "RFC.least_squares_transfer");
    COM_call_function( RFC_transfer, &str_disp, &unstr_disp);
    COM_call_function( RFC_transfer, &unstr_disp, &str_disp);
    
    COM_call_function( OUT_set, "mode", "a");
    COM_call_function( OUT_write, prefix1, &str_disp, "str", "001");
    COM_call_function( OUT_write, prefix2, &unstr_disp, "unstr", "001");

    COM_call_function( RFC_clear, "str", "unstr");
    COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");
  }

  COM_delete_window( "str");
  COM_delete_window( "unstr");

  COM_print_profile( "", "");

  COM_finalize();
}






