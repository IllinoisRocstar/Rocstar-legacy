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
// $Id: smf2hdf.C,v 1.3 2008/12/06 08:43:24 mtcampbe Exp $
// Converting a .smf file to .hdf.

#include "SMF_Reader.h"
#include "roccom.h"
#include <iostream>
#include <sstream>

using namespace std;

COM_EXTERN_MODULE(Rocout);

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc < 2) {
    cout << "Usage: " << argv[0] << " <smf_file> [hdf_name] " << endl;
    return -1;
  }

  char *firstdot=strchr( argv[1], '.');
  const string wname(argv[1], firstdot? firstdot-argv[1]: strlen( argv[1]));

  std::cout << "Reading surface mesh file \"" << argv[1] << '"' << endl;

  COM_new_window( wname.c_str());
  SMF_Reader().read_mesh( argv[1], wname);
  COM_window_init_done( wname.c_str());

  // Output mesh
  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocout, "OUT");

  int OUT_write = COM_get_function_handle( "OUT.write_attribute");
  int mesh = COM_get_attribute_handle_const( (wname+".mesh").c_str());


  std::string fname( argc>=3 ? argv[2] : (wname+".hdf").c_str());

  int separate_files = fname.find( ".hdf") == string::npos;
  
  if ( !separate_files) {
    std::cout << "Writing HDF file(s) " << fname << endl;

    COM_call_function( OUT_write, fname.c_str(), &mesh, (char*)wname.c_str(), "000");
  }
  else {
    int npanes, *paneIDs;
    COM_get_panes( wname.c_str(), &npanes, &paneIDs);

    for ( int i=0; i<npanes; ++i) {
      std::ostringstream ostr;

      ostr << fname; 
      ostr.fill('0'); ostr.width(5);
      ostr << paneIDs[i] << ".hdf";

      std::cout << "Writing HDF file(s) " << ostr.str() << endl;

      COM_call_function( OUT_write, ostr.str().c_str(), &mesh, 
			 (char*)wname.c_str(), 
			 "000", NULL, NULL, &paneIDs[i]);
    }

    COM_free_buffer( &paneIDs);
  }

  COM_finalize();
  return 0;
}






