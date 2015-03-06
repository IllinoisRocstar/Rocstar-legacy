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
// $Id: surfjumper.C,v 1.6 2009/10/08 20:35:10 mtcampbe Exp $

#include <iostream>
#include <iomanip>
#include <cstring>
#include <cstdlib>
#include <sstream>
#include <fstream>

#include "roccom.h"
#include "../Rocsurf/test/IM_Reader.h"

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);
COM_EXTERN_MODULE( Rocblas);
COM_EXTERN_MODULE( Rocsurf);

using namespace std;

void read_file( const char *fname, const string &wname, double alpha,bool with_ghost) {
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

  // Pass MPI_COMM_NULL to Rocin so that the rank becomes a wildcard.
  MPI_Comm comm_null = MPI_COMM_WORLD;
  std::string bufwin("bufwin");
  COM_call_function( IN_read, fname, bufwin.c_str(), &comm_null);
  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");

  int buf_all = COM_get_attribute_handle((bufwin+".all").c_str());
  COM_call_function( IN_obtain, &buf_all, &buf_all);
  COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");
  
  if(!rank)
    std::cout << "Obtained window " << wname
	      << " from file " << fname << std::endl;

  // Change the memory layout to contiguous.
  if(with_ghost)
    COM_clone_attribute( (wname+".all").c_str(), (bufwin+".all").c_str(), 1);
  else
    COM_clone_attribute( (wname+".all").c_str(), (bufwin+".all").c_str(), 0);

  COM_delete_window( bufwin.c_str());
}

int main(int argc, char *argv[]) {
  MPI_Init(&argc,&argv);
  COM_init( &argc, &argv);
  int rank= 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  if(!rank){
    if ( argc < 3) {
      std::cerr << "Usage: " << argv[0]
		<< " <src Rocin control> <targ Rocin control> [<out_prefix>] [<RocfaceControlFile>]\n\n"
		<< "\t<src Rocin control>  specifies the Rocin control file for the old window\n"
		<< "\t<targ Rocin control> specifies the Rocin control file for the new window\n"
		<< "\t<out_prefix> specifies a prefix for output files. \n\t\tDefault is the current directory\n"
		<< "\t<RocfaceControlFile> specifies a file name for Rocface control parameters. \n"
		<< "\nExample:\t" 
		<< argv[0] << " Rocflo/Rocin/\"ifluid*.txt\" Rocfrac/Rocin/\"isolid*.txt\" Rocman/RocfloRocfrac/" << "\n\t\t"
		<< std::endl;
      exit(-1);
    }
  }
    
  COM_set_profiling( 1);
    
  string     fnames[3];
  fnames[0]=(string(argv[1]));
  fnames[1]=(string(argv[2]));
  fnames[2]=(string(argv[2]));
  string     pre = (argc>3)?argv[3]:"";
  // Append '/' to pre if not there
  if ( !pre.empty() && pre[pre.size()-1] != '/') pre.append("/");
  
  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");
  
  int RFC_readcntr = COM_get_function_handle( "RFC.read_control_file");
  int RFC_transfer = COM_get_function_handle("RFC.least_squares_transfer");
  int RFC_read = COM_get_function_handle( "RFC.read_overlay");

  if ( argc>4) {
    if(!rank)
      std::cout << "Reading Rocface control file..." << std::endl;
    COM_call_function( RFC_readcntr, argv[4]);
    if(!rank)
      std::cout << "Finished reading Rocface control file." << std::endl;
  }
    
  string     wnames[3];
  for ( int k=0; k<3; ++k) {
    // Discard the directory name and suffix to obtain a window name.
    string::size_type n0 = fnames[k].find_last_of( "/");
    
    std::string fname;
    if ( n0 == std::string::npos) fname=fnames[k]; 
    else fname = fnames[k].substr( n0+1, fnames[k].size());
    
    string::size_type ni;
    ni = fname.find_first_of( ".:_-*[]?\\\"\'0123456789");
    COM_assertion_msg(ni, "File name must start with a letter");
    
    if ( ni == std::string::npos) {
      wnames[k] = fname;
      fnames[k].append(".hdf"); // Append the .hdf suffix to the file name.
      }
    else {
      if ( fname[ni] == '_' && (fname[ni+1] == 's' || fname[ni+1] == 'f'))
	ni += 2;
      wnames[k] = pre+fname.substr( 0, ni);
    }
    COM_assertion_msg( k==0 || wnames[0]!=wnames[1],
		       "Two input files must have different alphabetic prefix");
    
    if(k == 2)
      wnames[2] = "new_surf";
    read_file( fnames[k].c_str(), wnames[k], 1.,k>1);
    COM_window_init_done( wnames[k].c_str());
  }
  string srcwin(wnames[0]);
  string trgwin(wnames[1]);
  string r_trgwin(wnames[2]);
  int srcmesh = COM_get_attribute_handle( (wnames[0]+".mesh").c_str());
  int trgmesh = COM_get_attribute_handle( (wnames[1]+".mesh").c_str());
  std::vector<int> pane_id;
    
  if(!rank)
    std::cout << "Reading mesh overlay..." << std::endl;
  MPI_Comm comm = MPI_COMM_WORLD;
  COM_call_function( RFC_read, &srcmesh, &trgmesh, &comm,"src","trg","HDF");
  
  int num_attributes;
  string names;
  COM_get_attributes( srcwin.c_str(),&num_attributes,names);
  char loc;
  COM_Type comtype;
  int ncomp;
  std::string unit;
  istringstream Istr(names);
  for(int i = 0;i < num_attributes;i++){
    string aname;
    Istr >> aname;
    COM_get_attribute(srcwin+"."+aname,&loc,&comtype,&ncomp,&unit);
    if((loc == 'e' || loc == 'n') && comtype == COM_DOUBLE){
      if(!rank)
	cout << "Transferring attribute: " << aname << " on " 
	     << (loc == 'e' ? "elements" : "nodes") << "." << endl; 
      COM_resize_array((srcwin+"."+aname).c_str());
      COM_new_attribute((trgwin+"."+aname).c_str(),(char)loc,
			COM_DOUBLE,(int)ncomp,unit.c_str());
      COM_new_attribute((r_trgwin+"."+aname).c_str(),(char)loc,
			COM_DOUBLE,(int)ncomp,unit.c_str());
      COM_resize_array((r_trgwin+"."+aname).c_str());
      COM_resize_array((trgwin+"."+aname).c_str());
      int src_ahdl  = COM_get_attribute_handle((srcwin+"."+aname).c_str());
      int trg_ahdl  = COM_get_attribute_handle((trgwin+"."+aname).c_str());
      COM_call_function( RFC_transfer, &src_ahdl, &trg_ahdl);
      int *srcpane_ids;
      int npanes;
      COM_get_panes( trgwin.c_str(), &npanes, &srcpane_ids);
      pane_id.resize(npanes);
      for(int i = 0;i < npanes;i++)
	pane_id[i] = srcpane_ids[i];
      // These are no longer necessary as we've duped the info into
      // a locally allocated array
      COM_free_buffer( &srcpane_ids);
      for(int p = 0;p < npanes;p++){
	void *src_ptr = NULL;
	int src_std = 0;
	int src_cap = 0;
	void *trg_ptr = NULL;
	int trg_std = 0;
	int trg_cap = 0;
	COM_get_array((trgwin+"."+aname).c_str(),pane_id[p],&src_ptr,&src_std,&src_cap);
	//	COM_assertion_msg((src_ptr && (src_std*src_cap >= 0)),"Source array missing or of 0 size.");
	COM_get_array((r_trgwin+"."+aname).c_str(),pane_id[p],&trg_ptr,&trg_std,&trg_cap);
	//	COM_assertion_msg((trg_ptr && (trg_std*trg_cap >= 0)),"Target array missing or of 0 size.");
	if(src_ptr && trg_ptr && (trg_std*trg_cap*src_std*src_cap >=0))
	  memcpy(trg_ptr,src_ptr,sizeof(double)*src_std*src_cap);
      }
    }
  } 
  
  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocout,"Rocout");
  int OUT_set_option = COM_get_function_handle( "Rocout.set_option");
  std::string rankstr("0");
  COM_call_function( OUT_set_option, "rankwidth", rankstr.c_str());
  std::ostringstream Ostr;
  Ostr << "new_surf_" << setw(5) << setfill('0') << rank+1;
  int whand = COM_get_function_handle("Rocout.write_attribute");
  int all = COM_get_attribute_handle((r_trgwin+".all"));
  COM_call_function(whand,Ostr.str().c_str(),&all,r_trgwin.c_str(),"");
  std::ofstream Ouf;
  string controlfilename(Ostr.str() + "_in.txt");
  Ouf.open(controlfilename.c_str());
  Ouf << "@Proc: " << rank << endl
      << "@Files: " << "new_surf_" << setw(5) 
      << setfill('0') << rank+1 << ".hdf" << endl;
  Ouf.clear();
  Ouf << "@Panes: ";
  std::vector<int>::iterator pii = pane_id.begin();
  while(pii != pane_id.end())
    Ouf << *pii++ << " ";
  Ouf << endl;
  Ouf.close();
  
  COM_UNLOAD_MODULE_STATIC_DYNAMIC(Rocout,"Rocout");
  COM_UNLOAD_MODULE_STATIC_DYNAMIC(Rocface,"RFC");
  
  COM_print_profile( "", "");
  
  COM_finalize();
  MPI_Finalize();
}






