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
// $Id: rfctest.C,v 1.6 2009/10/08 20:35:10 mtcampbe Exp $

#include <iostream>
#include <cstring>
#include <cstdlib>

#include "roccom.h"
#include "../Rocsurf/test/IM_Reader.h"

COM_EXTERN_MODULE( Rocin);
COM_EXTERN_MODULE( Rocout);
COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocblas);
COM_EXTERN_MODULE( Rocmap);

using namespace std;

void read_file( const char *fname, const string &wname, double alpha) {
  char *lastdot=strrchr( const_cast<char *>(fname), '.');

  COM_new_window( wname.c_str());
  // Read in HDF files or a Rocin control file
  std::cout << "Reading file " << fname << "..." << std::endl;

  // Read in HDF format
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
    
  int IN_read;
  // Read in HDF format using Rocin::read_window or ::read_by_control_file 
  if ( strcmp( lastdot, ".hdf")==0)
    IN_read = COM_get_function_handle( "IN.read_window");
  else
    IN_read = COM_get_function_handle( "IN.read_by_control_file");

  MPI_Comm comm_null = MPI_COMM_NULL;
  std::string bufwin("bufwin");
  COM_call_function( IN_read, fname, bufwin.c_str(), &comm_null);
  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");

  // Check whether bcflag exists. If so, retain only the panes with flag<=1.
  int bcflag = COM_get_attribute_handle((bufwin+".bcflag").c_str());
  if (bcflag > 0) {
    // Read in bcflags.
    COM_call_function( IN_obtain, &bcflag, &bcflag);
    
    // Obtain the IDs of the panes of the window
    int npanes, *pane_ids;
    COM_get_panes( bufwin.c_str(), &npanes, &pane_ids);
    
    // Loop through the panes to remove those with bcflag >1.
    for ( int i=0; i<npanes; ++i) {
      int *flag;
      COM_get_array( (bufwin+".bcflag").c_str(), pane_ids[i], &flag);
      if ( flag==NULL || *flag>1)
	COM_delete_pane( bufwin.c_str(), pane_ids[i]);
    }

    // remove buffers.
    COM_free_buffer( &pane_ids);
  }

  // Remove all attributes except for the mesh
  COM_delete_attribute(  (bufwin+".atts").c_str());

  // Read in the mesh.
  int buf_mesh = COM_get_attribute_handle((bufwin+".mesh").c_str());
  COM_call_function( IN_obtain, &buf_mesh, &buf_mesh);
  COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  
  std::cout << "Obtained window " << wname
	    << " from file " << fname << std::endl;

  // Change the memory layout to contiguous.
  COM_clone_attribute( (wname+".mesh").c_str(), (bufwin+".mesh").c_str(), 0);
  COM_delete_window( bufwin.c_str());
}

using namespace std;

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 3) {
    std::cerr << "Usage: " << argv[0]
	      << " <HDF|RocinControlFile1> <HDF|RocinControlFile2> [<out_prefix>] [<RocfaceControlFile>]\n\n"
              << "\t<HDF|RocinControl File1> specifies the files for the first window\n"
              << "\t<HDF|RocinControl File2> specifies the files for the second window\n"
              << "\t<out_prefix> specifies a prefix for output files. \n\t\tDefault is the current directory\n"
	      << "\t<RocfaceControlFile> specifies a file name for Rocface control parameters. \n"
              << "\nExample:\t" 
              << argv[0] << " \"ifluid*.hdf\" \"isolid*.hdf\" " << "\n\t\t"
              << argv[0] << " Rocflo/Rocin/\"ifluid*.txt\" Rocfrac/Rocin/\"isolid*.txt\" Rocman/RocfloRocfrac/" << "\n\t\t"
              << std::endl;
    exit(-1);
  }

  if ( argc >= 2 && strcmp( argv[1], "-h") == 0) {
    std::cout << "Usage: " << argv[0] << " [-v]" 
	      << std::endl;
    exit(-1);
  }

  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocblas, "BLAS");
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocmap, "MAP");

  string       fnames[2] = {string(argv[1]), string(argv[2])};
  const string wnames[2] = {string("solid"), string("fluid")};
  
  for ( int k=0; k<2; ++k) {
    read_file( fnames[k].c_str(), wnames[k], 1.);

    COM_new_attribute( (wnames[k]+".flux").c_str(), 'e', COM_DOUBLE, 3, "m/s");
    COM_new_attribute( (wnames[k]+".velo").c_str(), 'n', COM_DOUBLE, 3, "m/s");

    COM_resize_array( (wnames[k]+".atts").c_str());
    COM_window_init_done( wnames[k].c_str());
  }

  int w1_mesh = COM_get_attribute_handle( (wnames[0]+".mesh").c_str());
  int w2_mesh = COM_get_attribute_handle( (wnames[1]+".mesh").c_str());

  int RFC_read = COM_get_function_handle( "RFC.read_overlay");
  MPI_Comm comm=MPI_COMM_WORLD;
  COM_call_function( RFC_read, &w1_mesh, &w2_mesh, &comm);

  int OUT_write = COM_get_function_handle( "OUT.write_attribute");

  int BLAS_copy_scalar = COM_get_function_handle( "BLAS.copy_scalar");
  int w1_nc = COM_get_attribute_handle( (wnames[0]+".nc").c_str());
  int w1_flux = COM_get_attribute_handle( (wnames[0]+".flux").c_str());
  int w1_velo = COM_get_attribute_handle( (wnames[0]+".velo").c_str());

  int w2_nc = COM_get_attribute_handle( (wnames[1]+".nc").c_str());
  int w2_flux = COM_get_attribute_handle( (wnames[1]+".flux").c_str());
  int w2_velo = COM_get_attribute_handle( (wnames[1]+".velo").c_str());
  
  double zero=0.0;
  double dt = 1.e-5;
  char buf[10];
  
  int initial = 0;
  if ( argc>2) {
    initial = atoi(argv[2]); 
    if (initial<0) initial = 0;
  }
  std::sprintf( buf, "%05d", initial);
  
  int MAP_compute_face_normals = 
    COM_get_function_handle( "MAP.compute_element_normals");
  int MAP_compute_nodal_normals = 
    COM_get_function_handle( "MAP.compute_nodal_normals");

  if (initial == 0) {
    int MAP_init = COM_get_function_handle( "MAP.initialize");
    COM_call_function( MAP_init, &w1_nc, &comm);
    COM_call_function( MAP_compute_face_normals, &w1_flux);
    COM_call_function( BLAS_copy_scalar, &zero, &w2_flux);

    COM_call_function( MAP_compute_nodal_normals, &w1_velo);
    COM_call_function( BLAS_copy_scalar, &zero, &w2_velo);

    COM_call_function( OUT_write, (wnames[0]+buf).c_str(),
		       &w1_flux, wnames[0].c_str(), buf);
    COM_call_function( OUT_write, (wnames[1]+buf).c_str(),
		       &w2_flux, wnames[1].c_str(), buf);
  }
  else {
    int IN_read = COM_get_function_handle( "IN.read_window");
    int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");

    COM_call_function( IN_read, (wnames[0]+buf).c_str(),
		       &w1_flux, wnames[0].c_str(), buf);
    COM_call_function( IN_obtain, &w1_mesh, &w1_mesh);
    COM_call_function( IN_obtain, &w1_flux, &w1_flux);

    COM_call_function( IN_read, (wnames[1]+buf).c_str(),
		       &w2_flux, wnames[1].c_str(), buf);
    COM_call_function( IN_obtain, &w2_mesh, &w2_mesh);
    COM_call_function( IN_obtain, &w2_flux, &w2_flux);
  }

  int RFC_transfer = COM_get_function_handle( "RFC.least_squares_transfer");
  int RFC_interpolate = COM_get_function_handle( "RFC.interpolate");
  COM_set_profiling_barrier( RFC_transfer, comm);
  COM_set_profiling_barrier( RFC_interpolate, comm);
  COM_call_function( RFC_transfer, &w1_flux, &w2_flux);
  COM_call_function( RFC_transfer, &w2_flux, &w1_flux);
  COM_set_profiling( 1);

  for ( int i=initial; i<1; ++i) {
    COM_call_function( MAP_compute_face_normals, &w1_flux);
    COM_call_function( BLAS_copy_scalar, &zero, &w2_flux);
    
    COM_call_function( RFC_transfer, &w1_flux, &w2_flux);
    COM_call_function( RFC_transfer, &w2_flux, &w1_flux);
    COM_call_function( RFC_interpolate, &w1_velo, &w2_velo);
    COM_call_function( RFC_interpolate, &w2_velo, &w1_velo);
    COM_call_function( RFC_transfer, &w1_velo, &w2_velo);
    COM_call_function( RFC_transfer, &w2_velo, &w1_velo);
    
    int BLAS_axpy_scalar = COM_get_function_handle( "BLAS.axpy_scalar");
    COM_call_function( BLAS_axpy_scalar, &dt, &w1_velo, &w1_nc, &w1_nc);
    COM_call_function( BLAS_axpy_scalar, &dt, &w2_velo, &w2_nc, &w2_nc);
    
    //     if ( i%1000 == 999) {
    if ( false) {
      std::sprintf( buf, "%05d", i+1);
      COM_call_function( OUT_write, (wnames[0]+buf).c_str(),
			 &w1_flux, wnames[0].c_str(), buf);
      COM_call_function( OUT_write, (wnames[1]+buf).c_str(),
			 &w2_flux, wnames[1].c_str(), buf);
    }
  }

  COM_print_profile( "", "");

  COM_finalize();
}






