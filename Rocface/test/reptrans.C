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
// $Id: reptrans.C,v 1.9 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <iostream>
#include <cstring>
#include <cstdlib>
#include "../Rocsurf/test/IM_Reader.h"

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocblas);
COM_EXTERN_MODULE( Rocin);
COM_EXTERN_MODULE( Rocout);

bool is_local_solid( int pid, int comm_rank, int comm_size) {
  return ( pid % comm_size == comm_rank);
}

bool is_local_fluid( int pid, int comm_rank, int comm_size) {
  return ( (pid/100) % comm_size == comm_rank);
}

using namespace std;

int main(int argc, char *argv[]) {
  MPI_Init( &argc, &argv);
  COM_init( &argc, &argv);

  MPI_Comm comm = MPI_COMM_WORLD;
  int size, rank;
  MPI_Comm_size( comm, &size);
  MPI_Comm_rank( comm, &rank);

  if ( argc < 4) {
    if ( rank==0) {
      std::cout << "Usage: " << argv[0] << " #iter #startiter scheme\n"
		<< "Schemes: 0: interpolation\n\t 1: weighted residual" 
		<< "\n\t 2: virtual surface" << std::endl;
    }
    exit(-1);
  }

  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocblas, "BLAS");

  int OUT_write = COM_get_function_handle( "OUT.write_attribute");
  int IN_read = COM_get_function_handle( "IN.read_window");
  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");
  int BLAS_copy = COM_get_function_handle( "BLAS.copy");
  int BLAS_add_scalar = COM_get_function_handle( "BLAS.add_scalar");
  int BLAS_sub = COM_get_function_handle( "BLAS.sub");
  int BLAS_nrm2 = COM_get_function_handle( "BLAS.nrm2");

  int RFC_read = COM_get_function_handle( "RFC.read_overlay_sdv");
  int RFC_transfer = COM_get_function_handle( "RFC.least_squares_transfer");
  int RFC_interpolate = COM_get_function_handle( "RFC.interpolate");
  int RFC_set_verbose = COM_get_function_handle( "RFC.set_verbose");

  // Read in the meshes and common refinement
  const string wnames[2] = {string("solid"), string("fluid")};

  double nrms_buf[3];
  for ( int k=0; k<2; ++k) {
    COM_new_window( wnames[k].c_str());
    IM_Reader(comm).read_mesh((wnames[k]+".im").c_str(), wnames[k],
			      k?&is_local_solid:&is_local_fluid);

    COM_new_attribute( (wnames[k]+".fs").c_str(), 'n', COM_DOUBLE, 3, "m");
    COM_new_attribute( (wnames[k]+".errors").c_str(), 'n', COM_DOUBLE, 3, "m");

    COM_resize_array( (wnames[k]+".fs").c_str());
    COM_resize_array( (wnames[k]+".errors").c_str());

    COM_new_attribute( (wnames[k]+".nrms").c_str(), 'w', COM_DOUBLE, 3, "m");
    COM_set_array( (wnames[k]+".nrms").c_str(), 0, nrms_buf);
    COM_window_init_done( wnames[k].c_str());
  }

  int w1_mesh = COM_get_attribute_handle( (wnames[0]+".mesh").c_str());
  int w2_mesh = COM_get_attribute_handle( (wnames[1]+".mesh").c_str());

  COM_call_function( RFC_read, &w1_mesh, &w2_mesh, &comm);

  // Initialize the values to be transferred
  int w1_nc = COM_get_attribute_handle( (wnames[0]+".nc").c_str());
  int w1_fs = COM_get_attribute_handle( (wnames[0]+".fs").c_str());
  int w1_fs1 = COM_get_attribute_handle( (wnames[0]+".1-fs").c_str());
  int w1_err = COM_get_attribute_handle( (wnames[0]+".errors").c_str());
  int w1_err1 = COM_get_attribute_handle( (wnames[0]+".1-errors").c_str());
  int w1_nrms = COM_get_attribute_handle( (wnames[0]+".nrms").c_str());

  double *nrms=NULL; 
  COM_get_array( (wnames[0]+".nrms").c_str(), 0, &(void*&)nrms); 
  COM_assertion( nrms);

  int w2_nc = COM_get_attribute_handle( (wnames[1]+".nc").c_str());
  int w2_fs = COM_get_attribute_handle( (wnames[1]+".fs").c_str());
  
  int niter=atoi(argv[1]); 

  int initial = 0;
  char buf[10];
  
  if ( argc>2) {
    initial = atoi(argv[2]); 
    if (initial<0) initial = 0;
  }
  std::sprintf( buf, "%05d", initial);
  
  if (initial == 0) {
    double one=1.0;
    // Initialize the values of solid.fs
    COM_call_function( BLAS_copy, &w1_nc, &w1_fs);
    COM_call_function( BLAS_add_scalar, &w1_fs1, &one, &w1_fs1);

    COM_call_function( BLAS_copy, &w2_nc, &w2_fs);

    COM_call_function( OUT_write, (wnames[0]+buf).c_str(),
		       &w1_fs, wnames[0].c_str(), buf);
    COM_call_function( OUT_write, (wnames[1]+buf).c_str(),
		       &w2_fs, wnames[1].c_str(), buf);
  }
  else {
    COM_call_function( IN_read, (wnames[0]+buf).c_str(),
		       &w1_fs, wnames[0].c_str(), buf);
    COM_call_function( IN_obtain, &w1_mesh, &w1_mesh);
    COM_call_function( IN_obtain, &w1_fs, &w1_fs);

    COM_call_function( IN_read, (wnames[1]+buf).c_str(),
		       &w2_fs, wnames[1].c_str(), buf);
    COM_call_function( IN_obtain, &w2_mesh, &w2_mesh);
    COM_call_function( IN_obtain, &w2_fs, &w2_fs);
  }

  COM_set_profiling_barrier( RFC_transfer, comm);
  COM_set_profiling_barrier( RFC_interpolate, comm);
  COM_set_profiling( 1);

  int scheme=0;
  if ( argc>3) {
    scheme = atoi(argv[3]); 
  }
  std::sprintf( buf, "%05d", initial);
  
  int nextdump=1;

  for ( int i=initial+1; i<=niter; ++i) {
    int verbose=(i==nextdump), noop=0, two=2;
    double half=0.5, tol=1.e-15;

    COM_call_function( RFC_set_verbose, &noop);
    if ( scheme>0) {
      COM_call_function( RFC_transfer, &w1_fs, &w2_fs, &half, &two, &tol);
      COM_call_function( RFC_set_verbose, &verbose);
      COM_call_function( RFC_transfer, &w2_fs, &w1_fs, &half, &two, &tol);
    }
    else {
      COM_call_function( RFC_interpolate, &w1_fs, &w2_fs);
      COM_call_function( RFC_set_verbose, &verbose);
      COM_call_function( RFC_interpolate, &w2_fs, &w1_fs);
    }
  
    if ( i==niter || i==nextdump) {
      double neg1=-1;
      // Compute errors in 2-norm
      COM_call_function( BLAS_sub, &w1_fs, &w1_nc, &w1_err);
      COM_call_function( BLAS_add_scalar, &w1_err1, &neg1, &w1_err1);
      COM_call_function( BLAS_nrm2, &w1_err, &w1_nrms);
      std::cout << "Error norms are: " << nrms[0] << ' ' << nrms[1] 
		<< ' ' << nrms[2] << std::endl;

      std::sprintf( buf, "%05d", i+1);
      COM_call_function( OUT_write, (wnames[0]+buf).c_str(),
			 &w1_fs, wnames[0].c_str(), buf);
      COM_call_function( OUT_write, (wnames[1]+buf).c_str(),
			 &w2_fs, wnames[1].c_str(), buf);
      nextdump+=nextdump;
    }
  }

  COM_print_profile( "", "");

  COM_finalize();
  MPI_Finalize();  
}






