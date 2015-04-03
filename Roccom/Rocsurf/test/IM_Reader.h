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
// $Id: IM_Reader.h,v 1.10 2008/12/06 08:43:23 mtcampbe Exp $

#ifndef IM_READER_H
#define IM_READER_H

#include "../Rocsurf/include/surfbasic.h"
#include <vector>
#include <string>
#include <fstream>
#include <cstdlib>
#include <cstdio>

COM_EXTERN_MODULE( Rocin);

// This class provides an interface for reading in a surface mesh 
// from the ".im" file.
class IM_Reader {
#ifndef isfinite /* this is a macro under Intel CC 8.0 */
  bool isfinite( double x) { return x>-HUGE_VAL && x<HUGE_VAL; }
#endif
public:
  // Constructor from an MPI communicator and a scale factor.
  explicit IM_Reader( MPI_Comm comm=MPI_COMM_WORLD, double a=1) : _alpha(a) {
    int flag; MPI_Initialized(&flag);
    if ( !flag || comm == MPI_COMM_NULL) {
      _rank = 0; _size = 1;
    }
    else {
      MPI_Comm_rank( comm, &_rank);
      MPI_Comm_size( comm, &_size);
    }
  }

  typedef bool (*Func_ptr)(int pane_id, int comm_rank, int comm_size);

  int read_winmesh( const char *fname, const std::string &wname, 
		    bool del=true) {

    const char *lastdot=std::strrchr( fname, '.');
    if ( lastdot && (std::strcmp( lastdot, ".hdf")==0 ||
		     std::strcmp( lastdot, ".cgns")==0 ||
		     std::strcmp( lastdot, ".txt")==0)) {
      // Read in HDF format
      COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");

      int IN_read;
      if ( std::strcmp( lastdot, ".hdf") == 0 || 
	   std::strcmp( lastdot, ".cgns") == 0)
	IN_read = COM_get_function_handle( "IN.read_window");
      else
	IN_read = COM_get_function_handle( "IN.read_by_control_file");

      COM_call_function( IN_read, fname, wname.c_str());

      int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");
      int mesh_hdl = COM_get_attribute_handle((wname+".mesh").c_str());
      COM_call_function( IN_obtain, &mesh_hdl, &mesh_hdl);
      COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocin, "IN");

      // Change the memory layout to contiguous.
      COM_resize_array( (wname+".mesh").c_str(), 0, NULL, 0);

      if ( del) {
	// Delete all attributes and leave alone the mesh.
	COM_delete_attribute( (wname+".atts").c_str());
      }

      int npanes; COM_get_panes( wname.c_str(), &npanes, NULL);
      return npanes;
    }
    else if (!lastdot || std::strcmp( lastdot, ".im")) {
      std::cerr << "Unknown file format with suffix " << lastdot << std::endl;
      return -1;
    }
    return read_mesh( fname, wname, NULL);
  }

  // Assuming the window has been created, read in the local panes from
  // given file and return the number of local panes.
  int read_mesh( const char *fname, const std::string &wname, 
		 Func_ptr is_local) {
    std::ifstream is( fname); 
    if ( is == NULL) {
      std::cerr << "Error: Could not open file " << fname << std::endl;
      exit(-1);
    }

    // Create the window if not yet exist
    int h=COM_get_window_handle(wname.c_str());
    if ( h<=0) COM_new_window( wname.c_str());

    std::cout << "Reading file " << fname << std::endl;

    get_nextline( is, buf);
    int num_panes, mesh_type;
    char t;

    std::sscanf( buf, "%d%c%d", &num_panes, &t, &mesh_type);
    if ( num_panes<=0) {
      std::cerr << "Error: The number of panes must be positive." << std::endl;
      exit(-1);
    }

    int n=0;
    switch (mesh_type) {
    case 2: // Structured mesh
      for ( int i=0; i<num_panes; ++i) {
	get_nextline( is, buf);
	int pid;
	std::sscanf( buf, "%d", &pid);
	if ( pid<=0) {
	  std::cerr << "Error: Found nonpositive pane id " << pid << std::endl;
	  exit(-1);
	}

	bool local=(!is_local) || is_local( pid, _rank, _size);
	n+=local;
	read_pane_ij( is, wname, pid, local);
      }
      break;
    case 3: // Unstrured mesh with 3-node triangles
    case 4: // Unstrured mesh with 4-node quadrilaterals
    case 6: // Unstrured mesh with 6-node triangles
      for ( int i=0; i<num_panes; ++i) {
	get_nextline( is, buf);
	int pid;
	std::sscanf( buf, "%d", &pid);
	if ( pid<=0) {
	  std::cerr << "Error: Found nonpositive pane id " << pid << std::endl;
	  exit(-1);
	}

	bool local=(!is_local) || is_local( pid, _rank, _size);
	n+=local;
	read_pane_uns( is, wname, pid, mesh_type, local);
      }
      break;
    case 5: // Mixed mesh with 3-node triangles and 4-node quadrilaterals
      for ( int i=0; i<num_panes; ++i) {
	get_nextline( is, buf);
	int pid;
	std::sscanf( buf, "%d", &pid);
	if ( pid<=0) {
	  std::cerr << "Error: Found nonpositive pane id " << pid << std::endl;
	  exit(-1);
	}

	bool local=(!is_local) || is_local( pid, _rank, _size);
	n+=local;
	read_pane_mixed( is, wname, pid, local);
      }
      break;
    default:
      std::cerr << "Error: File: " << fname 
		<< " has unknown mesh type" << mesh_type << std::endl;
      exit(-1);
    }
    std::cout << "Finished reading file " << fname << std::endl;
    return n;
  }

private:
  void get_nextline( std::istream &is, char *str) {
    char  accpet[]={'0','1','2','3','4','5','6','7','8','9',
		    '.','+','-','e','E'};

    do { 
      if ( is.eof()) {
	std::cerr << "Error: Unexpected EOF" << std::endl;
	exit(-1);
      }
      is.getline( str, MAXLEN); 
    } while ( str[0]=='#' || strpbrk( str,accpet)==NULL);
  }

  // Read in coordinates of a pane into a window if local
  void read_pane_coors( std::istream &is, const std::string &wname,
			int pid, int n, bool local) {
    SURF::Point_3<double>  *coors=NULL;

    if ( local) {
      COM_set_size( (wname+".nc").c_str(), pid, n);
      COM_allocate_array( (wname+".nc").c_str(), pid, &(void*&)coors);
    }

    for ( int i=0; i<n; ++i) {
      get_nextline( is, buf);
      if ( local) {
	std::sscanf( buf, "%lf %lf %lf", &coors[i][0], &coors[i][1], &coors[i][2]);
	if (!isfinite( coors[i][0]) || !isfinite(coors[i][1]) ||
	    !isfinite( coors[i][2])) {
	  std::cerr << "Error: Got invalid coordinates " << coors[i] 
		    << " for node " << i+1  << " on pane " << pid 
		    << " of window " << wname << std::endl;
	  exit(-1);
  	}
      }
    }

    if ( local && _alpha!=1.)
      for ( int i=0; i<n; ++i) {
	coors[i][0]*=_alpha; coors[i][1]*=_alpha; coors[i][2]*=_alpha;
      }
  }

  // Read in element connectivity of a pane into a window if local
  void read_pane_elems( std::istream &is, const std::string &wname, int pid, 
			int num_elems, int nodes_per_elem, bool local){
    int *elems;

    const char *types[] = {".:t3:", ".:q4:", "", ".:t6:"};
    if (local) {
      COM_set_size( (wname+types[nodes_per_elem-3]).c_str(), pid, num_elems);
      COM_allocate_array( (wname+types[nodes_per_elem-3]).c_str(), 
			  pid, &(void*&)elems);
    }

    for ( int i=0; i<num_elems; ++i) {
      get_nextline( is, buf);
      if (local) {
	int *p = &elems[i*nodes_per_elem];
	switch ( nodes_per_elem) {
	case 3: std::sscanf( buf, "%d %d %d", p, p+1, p+2); break;
	case 4: std::sscanf( buf, "%d %d %d %d", p, p+1, p+2, p+3); break;
	case 6: std::sscanf( buf, "%d %d %d %d %d %d", p, p+1, p+2, p+3, p+4, p+5); break;
	default: assert(false); abort();
	}
      }
    }
  }

  // Read in a structured pane into a window if local
  void read_pane_ij( std::istream &is, const std::string &wname,
		     int pid, bool local) {
    get_nextline( is, buf);
    int dims[2];
    std::sscanf( buf, "%d %d", &dims[0], &dims[1]);
    if ( dims[0]<0 && dims[1]<0) {
      std::cerr << "Error: Negative mesh dimension " << std::endl;
      exit(-1);
    }

    COM_set_array( (wname+".:st2:").c_str(), pid, dims);
    read_pane_coors( is, wname, pid, dims[0]*dims[1], local);
  }
  
  // Read in an unstructured pane into a window if local
  void read_pane_uns( std::istream &is, const std::string &wname,
		      int pid, int nodes_per_elem, bool local) {
    int num_nodes, num_elmes;

    get_nextline( is, buf);
    std::sscanf( buf, "%d %d", &num_nodes, &num_elmes);
    if ( num_nodes<0 || num_elmes<0) {
      std::cerr << "Error: Negative node or element size" << std::endl;
      exit(-1);
    }

    read_pane_coors( is, wname, pid, num_nodes, local);
    read_pane_elems( is, wname, pid, num_elmes, nodes_per_elem, local);
  }

  // Read in a mixed pane into a window if local
  void read_pane_mixed( std::istream &is, const std::string &wname,
			int pid, bool local) {
    int num_nodes, num_tris, num_quads;
    get_nextline( is, buf);
    std::sscanf( buf, "%d %d %d", &num_nodes, &num_tris, &num_quads);
    if ( num_nodes<0 && num_tris<0 || num_quads<0) {
      std::cerr << "Error: Negative node or element size" << std::endl;
      exit(-1);
    }

    read_pane_coors( is, wname, pid, num_nodes, local);
    read_pane_elems( is, wname, pid, num_tris, 3, local);
    read_pane_elems( is, wname, pid, num_quads, 4, local);
  }

private:
  enum  {MAXLEN=255};
  char  buf[MAXLEN+1];
  double _alpha;
  int    _rank, _size;
};

#endif






