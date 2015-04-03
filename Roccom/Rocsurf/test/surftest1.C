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
// $Id: surftest1.C,v 1.4 2008/12/06 08:43:24 mtcampbe Exp $

#include "surfbasic.h"
#include "roccom.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cstring>

COM_EXTERN_MODULE( Rocsurf);
COM_EXTERN_MODULE( Rocout);

using namespace std;

struct Four_tuple {
  Four_tuple() {}
  Four_tuple( int a, int b, int c, int d) : v1(a), v2(b), v3(c), v4(d) {}
  int v1, v2, v3, v4;
};

int main(int argc, char **argv) {
  COM_init( &argc, &argv);

  if ( argc < 3) {
    cout << "Usage: " << argv[0] << " nrow ncol" << endl;
    exit(-1);
  }

  COM_set_verbose(1);
 
  const int n_row=atoi(argv[1]), n_col=atoi(argv[2]);

  const double row_max=double(n_row-1.)/2., col_max=double(n_col-1.)/2.;

  vector<SURF::Point_3<double> >  pnts(n_row*n_col);
  vector<Four_tuple >            elems((n_row-1)*(n_col-1));

  const double pi_by_2 = asin(1.);

  for (int i=0; i<n_row; ++i)
    for (int j=0; j<n_col; ++j) {
      double sin_1 = sin((i+row_max)/(n_row-1)*pi_by_2);
      double sin_2 = sin((j+col_max)/(n_col-1)*pi_by_2);
      double cos_1 = cos((i+row_max)/(n_row-1)*pi_by_2);
      double cos_2 = cos((j+col_max)/(n_col-1)*pi_by_2);

      pnts[i*n_col+j] = SURF::Point_3<double>( cos_1*sin_2, cos_2, sin_1*sin_2);
    }

  for (int i=0; i<n_row-1; ++i)
    for (int j=0; j<n_col-1; ++j)
      elems[i*(n_col-1)+j] = Four_tuple(i*n_col+j+1, (i+1)*n_col+j+1,
					(i+1)*n_col+j+2, i*n_col+j+2);
  vector<SURF::Vector_3<double> >  nrms( pnts.size());

  cout << "Creating window \"quad1\"..." << endl;
  
  COM_new_window("quad1");
  COM_new_attribute("quad1.normals", 'n', COM_DOUBLE, 3, "m");
  COM_new_attribute("quad1.evals", 'e', COM_DOUBLE, 3, "m");
  COM_new_attribute("quad1.nvals", 'n', COM_DOUBLE, 3, "m");

  COM_set_size( "quad1.nc", 1, pnts.size());
  COM_set_array( "quad1.nc", 1, &pnts[0]);
  COM_set_size( "quad1.:q4:", 1, elems.size());
  COM_set_array( "quad1.:q4:", 1, &elems[0]);

  COM_set_array("quad1.normals", 1, &nrms[0]);
  COM_resize_array( "quad1.evals");
  COM_resize_array( "quad1.nvals");
  COM_window_init_done( "quad1");

  double *evals_p; COM_get_array( "quad1.evals", 1, &(void*&)evals_p);
  for ( int i=0, n=3*elems.size(); i<n; ++i) evals_p[i] = 1.;

  int mesh = COM_get_attribute_handle_const( "quad1.mesh");
  int normals = COM_get_attribute_handle( "quad1.normals");
  int evals = COM_get_attribute_handle( "quad1.evals");
  int nvals = COM_get_attribute_handle( "quad1.nvals");

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocsurf, "SURF");
  int SURF_init = COM_get_function_handle( "SURF.initialize");
  COM_call_function( SURF_init, &mesh);

  int SURF_normal = COM_get_function_handle( "SURF.compute_normals");
  COM_call_function( SURF_normal, &mesh, &normals);

  int SURF_e2n = COM_get_function_handle( "SURF.elements_to_nodes");
  int scheme = SURF::E2N_ANGLE;
  COM_call_function( SURF_e2n, &evals, &nvals, &mesh, &scheme);
  
  cout << "Output normals into file..." << endl;

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocout, "OUT");
  int OUT_write = COM_get_function_handle( "OUT.write_attribute");
  int quad1_all = COM_get_attribute_handle( "quad1.all");

  char prefix1[100];  
  sprintf(prefix1, "quad1_all");
  COM_call_function( OUT_write, prefix1, &quad1_all, "quad1", "000");

  COM_delete_window( "quad1");

  COM_finalize();
}






