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
#include "Rocin.h"
#include "roccom.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include "SurfaceProjection.h"
#include "Roccom_base.h"

using namespace std;
COM_EXTERN_MODULE( Rocin);

void project_window(const std::string& wName, const std::string& timeStr,
                      const std::string& file_in)
{
  Vec3D p,q;
  double dist;
  double total_dist =0;
  double total_nodes =0;

  // Obtain the list of panes
  Window * w = COM_get_roccom()->get_window_object(wName.c_str());
  vector<Pane*> ps;
  unsigned int pass =0;
  Pane * pn = NULL;
  double * nc;
  int nPanes;
  int* paneIds;

  COM_get_panes(wName.c_str(), &nPanes, &paneIds);
  w->panes(ps);
  cerr <<"Window " <<  wName << " has " << nPanes << " panes." << endl;

 
  for (unsigned int i=0; i<ps.size(); ++i) 
    { 
      pn = ps[i];
      nc = pn->coordinates();
      unsigned int n = pn->size_of_real_nodes();
      total_nodes+=n;
      SurfaceProjector_Pane sp(pn);

      cout << "PANE " << i 
	   <<" projecting " << pn->size_of_real_nodes() << " nodes." 
	   << endl;

      //loop through pane nodes
      for(unsigned int j=0;j < pn->size_of_real_nodes();j++)
	{
	  p[0] = nc[j];
	  p[1] = nc[j+ n];
	  p[2] = nc[j+2*n];

	  
	  //project to surface
	 
	  dist = sp.project_point(p,q);
	  if( dist != 0)
	   {
	     total_dist+=dist; 
	   }
	  else
	    pass++;

	}
    }
  cerr << "RESULTS: " << pass << " of " << pn->size_of_real_nodes() 
       <<" points projected to themselves, average error = " << total_dist/total_nodes << endl;
  
  COM_free_buffer(&paneIds);
} 

int main(int argc, char* argv[])
{
  COM_init(&argc, &argv);
 
  if (argc != 2)
    {
      std::cerr << "Usage:project_test <hdf file>" << std::endl;
      return 1;
    }

  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocin, "IN");
  
  int IN_obtain = COM_get_function_handle("IN.obtain_attribute");

  COM_set_verbose(0);
  COM_set_profiling(0);

  std::string file_in(argv[1]);
  std::string win_in(file_in);
  std::string::size_type st = win_in.find_last_of('/');
  if (st != std::string::npos)
    win_in.erase(0, st+1);
  st = win_in.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
  if (st != std::string::npos)
    win_in.erase(st);

  int  len = 15;
  char timeStr[16] = "";

  
  std::cout << "Reading HDF file(s) " << file_in
	    << " into window " << win_in << std::endl;
  
  int IN_read = COM_get_function_handle("IN.read_window");
  COM_call_function(IN_read, file_in.c_str(), win_in.c_str(), NULL, NULL,
		    timeStr, &len);
  
  cerr << "Finished reading HDF file into window\n";
  int IN_all = COM_get_attribute_handle((win_in + ".all").c_str());
  COM_call_function(IN_obtain, &IN_all, &IN_all);

  std::cout << "Projecting surface mesh to itself" << std::endl;
  project_window(win_in, timeStr, file_in);
  std::cout << "Done" << std::endl;

  COM_finalize();
  return 0;
}






