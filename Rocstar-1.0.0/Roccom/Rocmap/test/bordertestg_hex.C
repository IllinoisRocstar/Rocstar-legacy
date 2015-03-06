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
#include "roccom.h"
#include <iostream>
#include <sstream>

using namespace std;

COM_EXTERN_MODULE( Rocmap);
COM_EXTERN_MODULE( Rocout);

// ==== Routines for creating mesh information
void init_unstructure_mesh( double coors[24][3], int elmts[6][8]) {
  coors[0][0] = 0.0;
  coors[0][1] = 0.0;
  coors[0][2] = 0.0;
  coors[1][0] = 0.5;
  coors[1][1] = 0.0;
  coors[1][2] = 0.0;
  coors[2][0] = 1.0;
  coors[2][1] = 0.0;
  coors[2][2] = 0.0;
  coors[3][0] = 1.0;
  coors[3][1] = 0.5;
  coors[3][2] = 0.0;  
  coors[4][0] = 0.5;
  coors[4][1] = 0.5;
  coors[4][2] = 0.0;
  coors[5][0] = 0.0;
  coors[5][1] = 0.5;
  coors[5][2] = 0.0;
  coors[6][0] = 0.0;
  coors[6][1] = 1.0;
  coors[6][2] = 0.0;
  coors[7][0] = 0.5;
  coors[7][1] = 1.0;
  coors[7][2] = 0.0;
  coors[8][0] = 1.0;
  coors[8][1] = 1.0;
  coors[8][2] = 0.0;
  coors[9][0] = 0.0;
  coors[9][1] = 0.0;
  coors[9][2] = 1.0;
  coors[10][0] = 0.5;
  coors[10][1] = 0.0;
  coors[10][2] = 1.0;
  coors[11][0] = 1.0;
  coors[11][1] = 0.0;
  coors[11][2] = 1.0;
  coors[12][0] = 1.0;
  coors[12][1] = 0.5;
  coors[12][2] = 1.0;
  coors[13][0] = 0.5;
  coors[13][1] = 0.5;
  coors[13][2] = 1.0;
  coors[14][0] = 0.0;
  coors[14][1] = 0.5;
  coors[14][2] = 1.0;
  coors[15][0] = 0.0;
  coors[15][1] = 1.0;
  coors[15][2] = 1.0;
  coors[16][0] = 0.5;
  coors[16][1] = 1.0;
  coors[16][2] = 1.0;
  coors[17][0] = 1.0;
  coors[17][1] = 1.0;
  coors[17][2] = 1.0;
  coors[18][0] = 1.5;
  coors[18][1] = 0.0;
  coors[18][2] = 0.0;
  coors[19][0] = 1.5;
  coors[19][1] = 0.5;
  coors[19][2] = 0.0;
  coors[20][0] = 1.5;
  coors[20][1] = 1.0;
  coors[20][2] = 0.0;
  coors[21][0] = 1.5;
  coors[21][1] = 0.0;
  coors[21][2] = 1.0;
  coors[22][0] = 1.5;
  coors[22][1] = 0.5;
  coors[22][2] = 1.0;
  coors[23][0] = 1.5;
  coors[23][1] = 1.0;
  coors[23][2] = 1.0;

  elmts[0][0] = 1;
  elmts[0][1] = 2;
  elmts[0][2] = 5;
  elmts[0][3] = 6;
  elmts[0][4] = 10;
  elmts[0][5] = 11;
  elmts[0][6] = 14;
  elmts[0][7] = 15;

  elmts[1][0] = 2;
  elmts[1][1] = 3;
  elmts[1][2] = 4;
  elmts[1][3] = 5;
  elmts[1][4] = 11;
  elmts[1][5] = 12;
  elmts[1][6] = 13;
  elmts[1][7] = 14;

  elmts[2][0] = 5;
  elmts[2][1] = 4;
  elmts[2][2] = 9;
  elmts[2][3] = 8;
  elmts[2][4] = 14;
  elmts[2][5] = 13;
  elmts[2][6] = 18;
  elmts[2][7] = 17;

  elmts[3][0] = 6;
  elmts[3][1] = 5;
  elmts[3][2] = 8;
  elmts[3][3] = 7;
  elmts[3][4] = 15;
  elmts[3][5] = 14;
  elmts[3][6] = 17;
  elmts[3][7] = 16;

  elmts[4][0] = 3;
  elmts[4][1] = 19;
  elmts[4][2] = 20;
  elmts[4][3] = 4;
  elmts[4][4] = 12;
  elmts[4][5] = 22;
  elmts[4][6] = 23;
  elmts[4][7] = 13;

  elmts[5][0] = 4;
  elmts[5][1] = 20;
  elmts[5][2] = 21;
  elmts[5][3] = 9;
  elmts[5][4] = 13;
  elmts[5][5] = 23;
  elmts[5][6] = 24;
  elmts[5][7] = 18;

}

int main(int argc, char *argv[]) {
  const int num_nodes = 24, num_elmts = 6;
  const int ghost_nodes = 6, ghost_elements = 2;

  double  coors_s[num_nodes][3];
  int     elmts[num_elmts][8];

  COM_init( &argc, &argv);

  cout << "Creating window \"unstr\"" << endl;
  COM_new_window("unstr");
  COM_new_attribute("unstr.normals", 'e', COM_DOUBLE, 3, "m");
  COM_new_attribute("unstr.centers", 'e', COM_DOUBLE, 3, "m");

  init_unstructure_mesh( coors_s, elmts);
  COM_set_size( "unstr.nc", 1, num_nodes, ghost_nodes);
  COM_set_array( "unstr.nc", 1, &coors_s[0][0]);
  COM_set_size( "unstr.:H8:", 1, num_elmts, ghost_elements);
  COM_set_array( "unstr.:H8:", 1, &elmts[0][0]);
  
  COM_resize_array( "unstr.atts");

  for(int i =0; i < num_nodes; ++i){
    cout << "Coors_s[0][" << i <<"] = [" 
	 << coors_s[i][0] << " , " << coors_s[i][1] << " , " 
	 << coors_s[i][2] << "]" << endl;
  }
  for(int i =0; i < num_elmts; ++i){
    cout << "Element " << i << ": "  
	 << elmts[i][0] << " " << elmts[i][1] << " "
	 << elmts[i][2] << " " << elmts[i][3] << " "
	 << elmts[i][4] << " " << elmts[i][5] << " "
	 << elmts[i][6] << " " << elmts[i][7] << endl;
  }

  // Allocate memory for displacements
  const string disps("unstr.disps");
  char loc;
  int size, type;
  string unit;  
  COM_get_attribute("unstr.nc", &loc, &type, &size, &unit); 
  COM_new_attribute(disps.c_str(), 'n', COM_DOUBLE, 3, unit.c_str());
  COM_resize_array(disps.c_str());
  COM_window_init_done("unstr");
  int mesh_hdl = COM_get_attribute_handle("unstr.mesh");

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocmap, "MAP");
  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocout, "OUT");

  int MAP_compute_pconn = COM_get_function_handle( "MAP.compute_pconn");
  const string pconn = "unstr.pconn";
  int pconn_hdl = COM_get_attribute_handle( pconn.c_str());
  COM_call_function( MAP_compute_pconn, &mesh_hdl, &pconn_hdl);

  COM_finalize();
}






