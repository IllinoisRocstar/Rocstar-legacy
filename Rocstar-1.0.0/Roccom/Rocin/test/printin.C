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
/**
 Use Rocin to read an HDF file, and print out
 a human-readable text listing of everything in the file.
 
 Orion Sky Lawlor, olawlor@acm.org, 2004/05/07
*/
#include "Rocin.h"
#include "roccom.h"

#include <stdio.h>
#include <iostream>
#include <cstring>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <sstream>
#include <fstream>

// #define DEBUG_FILE_DUMP "/turing/home/jnorris/temp/"

/// Convert COM datatype to human-readable string.
std::string COM_get_type(int type) {
  switch (type) {
  case COM_DOUBLE: return "COM_DOUBLE";
  case COM_DOUBLE_PRECISION: return "COM_DOUBLE_PRECISION";
  case COM_INT: return "COM_INT";
  case COM_INTEGER: return "COM_INTEGER";
  case COM_FLOAT: return "COM_FLOAT";
  case COM_BYTE: return "COM_BYTE";
  case COM_REAL: return "COM_REAL";
  case COM_CHAR: return "COM_CHAR";
  case COM_VOID: return "COM_VOID";
  };
  char buf[100];
  std::sprintf(buf,"COM datatype %d",type);
  return buf;
}

/** Print a terse human-readable description of this window to this ostream. 
  Does not print a header.
*/
void COM_print_window(const char *wName, std::ostream &ostr) {
  std::string w(wName); w+=".";
  
  // Obtain the list of panes
  int np, *pane_ids;
  COM_get_panes( wName, &np, &pane_ids);
  
  int  nnodes = 0;  // total number of nodes
  int  nelems = 0;  // total number of elems

  // Loop through the panes to find the meshes
  for ( int i=0; i<np; ++i) {
    int nconn;    // Number of connectivity tables
    char *cnames; // Names of connectivity tables separated by space
    ostr << "Pane "<<pane_ids[i]<< " ";

    // Obtain the connectivity tables
    COM_get_connectivities( wName, pane_ids[i], &nconn, &cnames);

    if ( nconn == 1 && strncmp(cnames,":st",3)==0) { // Structured mesh
      int ndims;     // Number of dimensions
      int nglayers; // Number of ghost layers.
      COM_get_size( (w+cnames).c_str(), pane_ids[i], &ndims, &nglayers);

      // Obtain the dimensions (must be a const array) of the pane and set them
      const int *dims;
      COM_get_array_const((w+cnames).c_str(), pane_ids[i], &dims);

      ostr << "Structured mesh: " 
	   << ndims << "-D (" << nglayers << " ghost layers);"
	   <<" grid is " << dims[0];
      nnodes = dims[0];
      nelems = std::max(1, dims[0] - 1);
      if ( ndims>1) {
        ostr << "x" << dims[1];
        nnodes *= dims[1];
        nelems *= std::max(1, dims[1] - 1);
      }
      if ( ndims>2) {
        ostr << "x" << dims[2];
        nnodes *= dims[2];
        nelems *= std::max(1, dims[2] - 1);
      }
      ostr << std::endl;
    }
    else { // Unstructured mesh
      int  ngnodes; // Number of ghost nodes

      // Obtain the size of nodes
      COM_get_size((w+"nc").c_str(), pane_ids[i], &nnodes, &ngnodes);
      ostr << "Unstructured mesh: " 
                << nnodes << " nodes (" << ngnodes << " ghosts)";
      
      // Obtain the sizes of connectivity tables
      if ( nconn>0) {
	std::istringstream is( cnames);
	for ( int c=0; c<nconn; ++c) {
	  std::string cname;
	  is >> cname;
	  int ne, ng;
	  COM_get_size((w+cname).c_str(), pane_ids[i], &ne, &ng);
          nelems += ne;
	  if (nconn>1)
	    ostr << "\n    Connectivity table " << c << " has ";
	  else ostr<<" / ";
	  ostr << ne << " elements (" << ng << " ghosts)";
	}
      }
      ostr<<std::endl;

      // free the buffer of cnames
      COM_free_buffer( &cnames);
    }
  }
  ostr<<std::endl;

  // Obtain the list of attributes
  int na;      // Number of attributes
  char *atts;  // names of attributes separated by spaces
  COM_get_attributes( wName, &na, &atts);

  std::istringstream is(atts);
  for ( int i=0; i<na; ++i) {
    // Obtain the attribute name
    std::string aname;  is >> aname; 
    char loc;
    int  type, ncomp;
    std::string units;

    COM_get_attribute( (w+aname).c_str(), &loc, &type, &ncomp, &units);
    ostr << "" << aname << " ("<<units<<")  " << ncomp << " x " << COM_get_type(type) <<"   ";

    int nitems;
    if ( loc == 'w') { /* window attribute */
      int ng;
      COM_get_size((w+aname).c_str(), 0, &nitems, &ng);
      ostr << "    Window: " << nitems << " items ("<<ng<<" ghosts)";
    }
    else if (loc=='p') { /* panel attribute */
      for ( int i=0; i<np; ++i) {
	  // Obtain the size for a panel attribute.
	  int ng;
	  COM_get_size((w+aname).c_str(), pane_ids[i], &nitems, &ng);
	  ostr << "\n    Panel("<<pane_ids[i]<<"): "<<nitems<<" items ("<<ng<<" ghosts)";
      }
    }
    else if (loc=='e') { /* element attribute */
      nitems = nelems;
      ostr << "    Element";
    }
    else if (loc=='n') { /* node attribute */
      nitems = nnodes;
      ostr << "    Node";
    } 
    else {
      ostr << "  (Unrecognized attribute location "<<loc<<")";
    }
    ostr << std::endl;

#ifdef DEBUG_FILE_DUMP
    std::ofstream fout(( DEBUG_FILE_DUMP + aname + ".txt").c_str());
    const void* data;
    int strd, cap;
    for (int c=0; c<ncomp; ++c) {
      if (ncomp == 1)
        COM_get_array_const((w + aname).c_str(), pane_ids[i], &data, &strd,
                            &cap);
      else
        COM_get_array_const((w + (char)('1' + c) + '-' + aname).c_str(),
                            pane_ids[i], &data, &strd, &cap);
      if (data == NULL)
        fout << "NULL\n##################################################\n";
      else {
        int j;
        switch (type) {
          case COM_CHAR:
          case COM_CHARACTER:
            for (j=0; j<nitems; ++j) {
              fout << (int)((const char*)data)[j];
              if ((j + 1) % 8 == 0)
                fout << '\n';
              else
                fout << ' ';
            }
            break;
          case COM_INT:
          case COM_INTEGER:
            for (j=0; j<nitems; ++j) {
              fout << ((const int*)data)[j];
              if ((j + 1) % 8 == 0)
                fout << '\n';
              else
                fout << ' ';
            }
            break;
          case COM_FLOAT:
          case COM_REAL:
            for (j=0; j<nitems; ++j) {
              fout << ((const float*)data)[j];
              if ((j + 1) % 8 == 0)
                fout << '\n';
              else
                fout << ' ';
            }
            break;
          case COM_DOUBLE:
          case COM_DOUBLE_PRECISION:
            for (j=0; j<nitems; ++j) {
              fout << ((const double*)data)[j];
              if ((j + 1) % 8 == 0)
                fout << '\n';
              else
                fout << ' ';
            }
            break;
        }
        fout << "\n##################################################\n";
      }
    }
    fout.close();
#endif // DEBUG_FILE_DUMP
  }

  // Free buffers for pane ids and attribute names
  COM_free_buffer( &pane_ids);
  COM_free_buffer( &atts);
}

COM_EXTERN_MODULE( Rocin);

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);
  
  if (argc<2) {
    std::cerr<<"Usage: printtest <hdf file to print>\n";
    std::exit(1);
  }

  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocin, "IN");
  
  int IN_read = COM_get_function_handle( "IN.read_windows");
  int IN_obtain = COM_get_function_handle( "IN.obtain_attribute");

  COM_set_verbose(0);
  COM_set_profiling(0);

  const char *hdf_in  = argv[1];

  const char *win_in = "random_window";
  std::string w(win_in); w+=".";

  COM_call_function( IN_read, hdf_in, win_in, "");
  
  int IN_all   = COM_get_attribute_handle((w+"all").c_str());
  COM_call_function(IN_obtain, &IN_all, &IN_all);

  printf("Analysis of file '%s':\n",hdf_in);
  COM_print_window(win_in,std::cout);

  COM_finalize();
  return 0;
}






