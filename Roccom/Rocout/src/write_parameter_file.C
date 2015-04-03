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
#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>

#include "Rocout.h"
#include "roccom_c++.h"

using namespace std;
#ifndef DOXYGEN_SHOULD_SKIP_THIS
USE_COM_NAME_SPACE
#endif

void Rocout::write_parameter_file( const char* file_name, 
				 const char* window_name, 
				 const MPI_Comm *comm){

  // Check to see if the window exists
  COM::Roccom_base * rbase = COM_get_roccom();
  int whandle = COM_get_window_handle(window_name);
  COM::Window* param_window = NULL;
   if(whandle >0)
    param_window = rbase->get_window_object(whandle);

  COM_assertion_msg(param_window, 
		    "Parameter window does not exist");

  //open parameter file for output
  std::ofstream param_file(file_name);
  COM_assertion_msg(file_name,
  		    "Specified parameter file can't be opened for writing.");

  // Get all the attributes of the window
  std::vector<Attribute*> atts;
  param_window->attributes(atts);

  // Loop through all the attributes
  // print out those with the correct format.
  for(uint i =0, ni = atts.size(); i<ni; ++i){
    if(atts[i]->location()=='w'
       && atts[i]->data_type() == COM_CHAR
       && atts[i]->size_of_components() == 1){

      std::string option = atts[i]->name();
      std::string value ("");
      value.assign((const char*)atts[i]->pointer(), 
		   atts[i]->size_of_items());

      param_file << option << " = ";

      if(value.find(" ") != std::string::npos)
	param_file << "\"" << value << "\"\n";
      else
	param_file << value << "\n";
    }
  }
} // file closed automatically






