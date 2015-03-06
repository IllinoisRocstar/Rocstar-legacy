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
// $Id: plot3d.C,v 1.7 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstring>
#include <string>
#include <cstdlib>
#include <cmath>
#include <cassert>
#include "commpi.h"

COM_EXTERN_MODULE( Rocface);

using namespace std;

class Point_3 { 
  typedef double Type;
public:
  Point_3() {}
  Point_3( Type a, Type b, Type c) : _x(a), _y(b), _z(c) {}
  Type &operator[](const int i) { return (&_x)[i]; }
  const Type &operator[](const int i) const { return (&_x)[i]; }

protected:
  Type _x, _y, _z; 
};

// Read in a nstructed mesh
void read_plot3d( istream &is, vector<vector<Point_3> > &coors, 
		  vector< pair<int,int> > &dims) {
  int ngrid;
  is >> ngrid;

  dims.resize(ngrid);
  for ( int i=0; i<ngrid; ++i) {
    int t;
    is >> dims[i].first >> dims[i].second >> t;
  }

  coors.resize( ngrid);
  for ( int i=0; i<ngrid; ++i) {
    coors[i].resize( dims[i].first*dims[i].second);

    for ( int l=0; l<3; ++l) {
      for ( int j=0, nj=coors[i].size(); j<nj; ++j)
	is >> coors[i][j][l];
    }
  }
}

int main(int argc, char *argv[]) {
  MPI_Init( &argc, &argv);
  COM_init( &argc, &argv);

  if ( argc < 3) {
    std::cout << "Usage: " << argv[0] 
	      << " file1 file2 [<out_path>] " 
	      << " [<scale_1>] [<scale_2>]" << std::endl;
    exit(-1);
  }

  COM_set_verbose( 1);
  COM_set_profiling( 1);

  const string wnames[2] = {string(argv[1], string( argv[1]).find(".")), 
			    string(argv[2], string( argv[2]).find("."))};

  vector< vector<Point_3> >    coors[2];

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");

  for ( int k=0; k<2; ++k) {
    COM_new_window( wnames[k].c_str());

    ifstream fi( argv[k+1]); 
    vector<pair< int, int> >  dims;
    read_plot3d( fi, coors[k], dims);

    for ( int i=0, ni=dims.size(); i<ni; ++i) {
      COM_set_array( (wnames[k]+".:st2:").c_str(), i+1, &dims[i].first);
      COM_set_array( (wnames[k]+".nc").c_str(), i+1, &coors[k][i][0]);
    }

    COM_window_init_done( wnames[k].c_str());
  }

  MPI_Comm comm = MPI_COMM_WORLD;

  int RFC_overlay = COM_get_function_handle( "RFC.overlay");
  int RFC_write = COM_get_function_handle( "RFC.write_overlay");

  int tri1_mesh = COM_get_attribute_handle( (wnames[0]+".mesh").c_str());
  int tri2_mesh = COM_get_attribute_handle( (wnames[1]+".mesh").c_str());

  COM_call_function( RFC_overlay, &tri1_mesh, &tri2_mesh, &comm, "./");
  COM_call_function( RFC_write, &tri1_mesh, &tri2_mesh);

  COM_print_profile( "", "");

  COM_finalize();
  MPI_Finalize();  
}






