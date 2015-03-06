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
// $Id: surftest2.C,v 1.3 2008/12/06 08:43:24 mtcampbe Exp $

#include "roccom.h"
#include "surfbasic.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstring>
#include <cassert>

using namespace std;

// ==== Routines for creating mesh information
void init_structured_mesh( double coors[][3], int nrow, int ncol, 
			   int rank, int nproc, int check=1) {
  // consider the processors as a 2*(nproc/2) grid
  int proc_col=nproc;
  if (nproc%2 ==0) {
    proc_col=nproc/2;
  }
  else {
    proc_col=nproc;
  }
  int row=rank/proc_col, col=rank%proc_col;

  const double width=300./(nrow-1), length=300./(ncol-1);

  double x0 = col*300, y0=row*300, y1=y0;
  for (int i=0; i<nrow; ++i) {
    double x1 = x0;
    for (int j=0; j<ncol; ++j) {
      if ( check<0) 
	assert( coors[i*ncol+j][0]==x1 && coors[i*ncol+j][1]==y1 &&
		coors[i*ncol+j][2]==0.5);
      else {
	coors[i*ncol+j][0]=x1*check;
	coors[i*ncol+j][1]=y1*check;
	coors[i*ncol+j][2]=0.5;
      }
      x1 += length;
    }
    y1 += width;
  }
}

void init_unstructure_mesh( double coors[][3], int elmts[][4], int nrow, 
                            int ncol, int rank, int nproc, int check=1) {
  // consider the processors as a 2*(nproc/2) grid
  int proc_col=nproc;
  if (nproc%2 ==0) {
    proc_col=nproc/2;
  }
  else {
    proc_col=nproc;
  }

  int row=rank/proc_col, col=rank%proc_col;

  const double width=300./(nrow-1), length=300./(ncol-1);

  double x0 = col*300, y0=row*300, y1=y0;
  for (int i=0; i<nrow; ++i) {
    double x1 = x0;
    for (int j=0; j<ncol; ++j) {
      if ( check<0) 
	assert( coors[i*ncol+j][0]==x1 && coors[i*ncol+j][1]==y1 &&
		coors[i*ncol+j][2]==0.0);
      else {
	coors[i*ncol+j][0]=x1*check;
	coors[i*ncol+j][1]=y1*check;
	coors[i*ncol+j][2]=0.0;
      }
      x1 += length;
    }
    y1 += width;
  }

  // computes elmts
  for (int i=0; i<nrow-1; ++i) {
    for (int j=0; j<ncol-1; ++j) {
      if ( check<0) {
	assert( elmts[i*(ncol-1)+j][0]==i*ncol+j+1 &&
		elmts[i*(ncol-1)+j][1]==i*ncol+j+ncol+1 &&
		elmts[i*(ncol-1)+j][2]==i*ncol+j+ncol+2 &&
		elmts[i*(ncol-1)+j][3]==i*ncol+j+2);
      }
      else {
	elmts[i*(ncol-1)+j][0]=check*i*ncol+j+1;
	elmts[i*(ncol-1)+j][1]=check*i*ncol+j+ncol+1;
	elmts[i*(ncol-1)+j][2]=check*i*ncol+j+ncol+2;
	elmts[i*(ncol-1)+j][3]=check*i*ncol+j+2;
      }
    }
  }
}

int get_comm_rank( MPI_Comm comm) {
  int rank;
  int ierr = MPI_Comm_rank( comm, &rank); COM_assertion( ierr == 0);
  return rank;
}

int get_comm_size( MPI_Comm comm) {
  int size;
  int ierr = MPI_Comm_size( comm, &size); COM_assertion( ierr == 0);
  return size;
}

int main(int argc, char *argv[]) {
  const int nproc=4;
  const int nrow=40, ncol=30;
  const int num_nodes=nrow*ncol;
  const int num_elmts=(nrow-1)*(ncol-1);

  double  coors_s[nproc][num_nodes][3];
  int     elmts[nproc][num_elmts][4];

  MPI_Init( &argc, &argv);
  COM_init( &argc, &argv);

  MPI_Comm comm = MPI_COMM_WORLD;
  const int comm_rank=get_comm_rank( comm);
  const int comm_size=get_comm_size( comm);
 
  int vb = (argc>1) ? atoi(argv[1]) : 1;
  if ( comm_rank == 0) COM_set_verbose(vb );

  COM_load_module("Rocout", "OUT");

  if ( comm_rank == 0) cout << "Creating window \"unstr\"" << endl;
  COM_new_window("unstr");
  COM_new_attribute("unstr.normals", 'n', COM_DOUBLE, 3, "m");

  for ( int pid=0; pid<nproc; ++pid) if (pid%comm_size==comm_rank) {
    init_unstructure_mesh( coors_s[pid], elmts[pid], 
			   nrow, ncol, pid, nproc);
    COM_set_size( "unstr.nc", pid+1, num_nodes);
    COM_set_array( "unstr.nc", pid+1, &coors_s[pid][0][0]);
    COM_set_size( "unstr.:q4:", pid+1, num_elmts);
    COM_set_array( "unstr.:q4:", pid+1, &elmts[pid][0][0]);
  }
  COM_resize_array( "unstr.atts");
  COM_window_init_done( "unstr");

  int mesh = COM_get_attribute_handle_const( "unstr.mesh");
  int normals = COM_get_attribute_handle( "unstr.normals");

  COM_load_module( "Rocsurf", "SURF");
  int SURF_init = COM_get_function_handle( "SURF.initialize");
  COM_call_function( SURF_init, &mesh);

  int SURF_normal = COM_get_function_handle( "SURF.compute_normals");
  COM_call_function( SURF_normal, &mesh, &normals);

  if ( comm_rank==0)
    cout << "Output normals into file..." << endl;
  int OUT_write = COM_get_function_handle( "OUT.write_attribute");
  int unstr_all = COM_get_attribute_handle( "unstr.all");

  COM_call_function( OUT_write, "unstr_all", &unstr_all, "unstr", "000");
  COM_delete_window( "unstr");

  // Test structured mesh
  double  coors_f[nproc][nrow*ncol][3];

  if ( comm_rank == 0) cout << "Creating window \"str\"" << endl;
  COM_new_window( "str");
  COM_new_attribute("str.normals", 'n', COM_DOUBLE, 3, "m/s");

  for ( int pid=0; pid<nproc; ++pid) if (pid%comm_size==comm_rank) {
    init_structured_mesh(coors_f[pid], nrow, ncol, pid, nproc);
    
    COM_set_size("str.nc", pid+1, nrow*ncol);
    COM_set_array("str.nc", pid+1, &coors_f[pid][0][0]);
    int dims[2] = { ncol, nrow}; // Use Fortran convention
    COM_set_array("str.:st2:", pid+1, &dims[0]);
  }
  
  COM_resize_array( "str.atts");
  COM_window_init_done( "str");

  mesh = COM_get_attribute_handle( "str.mesh");
  normals = COM_get_attribute_handle( "str.normals");

  COM_call_function( SURF_init, &mesh);
  COM_call_function( SURF_normal, &mesh, &normals);


  if ( comm_rank==0)
    cout << "Output normals into file..." << endl;
  int str_all = COM_get_attribute_handle( "str.all");

  COM_call_function( OUT_write, "str_all", &str_all, "str", "000");
  COM_delete_window( "str");

  // COM_unload_module("Rocout");   // Unload Rocout

  COM_finalize();
  MPI_Finalize();
}






