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
// $Id: surfextractor.C,v 1.9 2009/10/08 20:35:10 mtcampbe Exp $

#include <iostream>
#include <cstring>
#include <cstdlib>
#include <vector>
#include <iomanip>
#include <sstream>
#include <fstream>

#include "roccom.h"

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);
COM_EXTERN_MODULE( Rocin);
COM_EXTERN_MODULE( Rocblas);
COM_EXTERN_MODULE( Rocsurf);

using namespace std;

void read_file( const char *fname, const string &wname, double alpha) {
  char *lastdot=strrchr( const_cast<char *>(fname), '.');
  int rank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  COM_new_window( wname.c_str());
  // Read in HDF files or a Rocin control file
  if(!rank)
    std::cout << "Reading file " << fname << "..." << std::endl;

  // Read in HDF format
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  
  int IN_read;
  // Read in HDF format using Rocin::read_window or ::read_by_control_file 
  if ( strcmp( lastdot, ".hdf")==0)
    IN_read = COM_get_function_handle( "IN.read_window");
  else
    IN_read = COM_get_function_handle( "IN.read_by_control_file");
  
  MPI_Comm comm_null = MPI_COMM_WORLD;
  std::string bufwin("bufwin");
  COM_call_function( IN_read, fname, bufwin.c_str(), &comm_null);
  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");

  int all = COM_get_attribute_handle((bufwin+".all").c_str());
  COM_call_function(IN_obtain,&all,&all);
  //  int buf_mesh = COM_get_attribute_handle((bufwin+".mesh").c_str());
  //  COM_call_function( IN_obtain, &buf_mesh, &buf_mesh);
  //  int bcflag = COM_get_attribute_handle((bufwin+".bcflag").c_str());
  //  if(bcflag > 0)
  //    COM_call_function(IN_obtain,&bcflag,&bcflag);
  
  //  if (bcflag > 0) {
    // Read in bcflags.
  //    COM_call_function( IN_obtain, &bcflag, &bcflag);
    
    // Obtain the IDs of the panes of the window
  //    int npanes, *pane_ids;
  //    COM_get_panes( bufwin.c_str(), &npanes, &pane_ids);
    
    // Loop through the panes to remove those with bcflag >1.
  //    for ( int i=0; i<npanes; ++i) {
  //      int *flag;
  //      COM_get_array( (bufwin+".bcflag").c_str(), pane_ids[i], &flag);
  //      if ( flag==NULL || *flag>1)
  //	COM_delete_pane( bufwin.c_str(), pane_ids[i]);
  //    }
    
    // remove buffers.
  //    COM_free_buffer( &pane_ids);
  //  }
  if(!rank)
    std::cout << "Recovering t = 0 positions." << std::endl;
  COM_clone_attribute( (wname+".mesh").c_str(),   (bufwin+".mesh").c_str(),   1);
  COM_clone_attribute( (wname+".nc_t0").c_str(),  (bufwin+".nc_t0").c_str(),  1);
  COM_clone_attribute( (wname+".bcflag").c_str(), (bufwin+".bcflag").c_str(), 1);
  COM_window_init_done(wname);
  COM_delete_window( bufwin.c_str());
  int *srcpane_ids;
  int npanes;
  std::vector<int> pane_id; 
  COM_get_panes( wname.c_str(), &npanes, &srcpane_ids);
  pane_id.resize(npanes);
  for(int i = 0;i < npanes;i++)
    pane_id[i] = srcpane_ids[i];
  // These are no longer necessary as we've duped the info into
  // a locally allocated array
  COM_free_buffer( &srcpane_ids);
  for(int p = 0;p < npanes;p++){
    //    int *flag;
    //    COM_get_array((wname+".bcflag").c_str(),pane_id[p],&flag);
    //    if(flag && *flag < 2){
    void *src_ptr = NULL;
    int src_std = 0;
    int src_cap = 0;
    void *trg_ptr = NULL;
    int trg_std = 0;
    int trg_cap = 0;
    COM_get_array((wname+".nc_t0").c_str(),pane_id[p],&src_ptr,&src_std,&src_cap);
    COM_get_array((wname+".nc").c_str(),pane_id[p],&trg_ptr,&trg_std,&trg_cap);
    if(src_ptr && trg_ptr && (trg_std*trg_cap == src_std*src_cap) && src_std > 1 && src_cap > 1)
      memcpy(trg_ptr,src_ptr,sizeof(double)*src_std*src_cap);
    else
      if(!rank)
	std::cout << "Warning: Not copying nc_t0 for Pane(" << pane_id[p] 
		  << ") src_ptr(" << src_ptr << ") src_std(" << src_std 
		  << ") src_cap(" << src_cap << ") trg_ptr(" << trg_ptr 
		  << ") trg_std(" << trg_std << ") trg_cap(" << trg_cap 
		  << ")" << endl;
  }
  //}
  COM_window_init_done(wname);
  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocout,"Rocout");
  int OUT_set_option = COM_get_function_handle( "Rocout.set_option");
  std::string rankstr("0");
  COM_call_function( OUT_set_option, "rankwidth", rankstr.c_str());
  std::ostringstream Ostr;
  Ostr << "surf0_" << setw(5) << setfill('0') << rank+1;
  int whand = COM_get_function_handle("Rocout.write_attribute");
  all = COM_get_attribute_handle((wname+".all"));
  COM_call_function(whand,Ostr.str().c_str(),&all,wname.c_str(),"");
  std::ofstream Ouf;
  string controlfilename(Ostr.str() + "_in.txt");
  Ouf.open(controlfilename.c_str());
  Ouf << "@Proc: " << rank << endl
      << "@Files: " << "surf0_" << setw(5) 
      << setfill('0') << rank+1 << ".hdf" << endl;
  Ouf.clear();
  Ouf << "@Panes: ";
  std::vector<int>::iterator pii = pane_id.begin();
  while(pii != pane_id.end())
    Ouf << *pii++ << " ";
  Ouf << endl;
  Ouf.close();
  
  COM_UNLOAD_MODULE_STATIC_DYNAMIC(Rocout,"Rocout");

}

int main(int argc, char *argv[]) {
  MPI_Init(&argc,&argv);
  COM_init( &argc, &argv);
  
  if ( argc < 2) {
    std::cerr << "Usage: " << argv[0]
	      << " <HDF|RocinControlFile1> " << std::endl
              << "\t<HDF|RocinControl File1> specifies the file(s) from which to extract the time 0 mesh.\n"
              << std::endl;
    exit(-1);
  }

  COM_set_profiling( 1);

  string     fname(argv[1]);
  string     wname;
  // Discard the directory name and suffix to obtain a window name.
  string::size_type n0 = fname.find_last_of( "/");
  std::string fname0;
  if (n0 != std::string::npos)
    fname = fname.substr( n0+1, fname.size());

  string::size_type ni;
  ni = fname.find_first_of( ".:_-*[]?\\\"\'0123456789");
  COM_assertion_msg(ni, "File name must start with a letter");

  if ( ni == std::string::npos) {
    wname = fname;
    fname.append(".hdf"); // Append the .hdf suffix to the file name.
  }
  else {
    if ( fname[ni] == '_' && (fname[ni+1] == 's' || fname[ni+1] == 'f'))
      ni += 2;
    wname = fname.substr( 0, ni);
  }
  read_file( fname.c_str(), wname, 1.); 

  COM_finalize();
  MPI_Finalize();
  return 0;
}







