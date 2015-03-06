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


#include "roccom_c++.h"
#include "Rocin.h"

using namespace std;
using namespace COM;

void eat_whitespace_until(std::string& str, const char& c, int start =0);

void get_parameter_list(std::vector<std::pair<std::string,std::string> >& param_list,
			 const char* file_name);

void Rocin::read_parameter_file( const char* file_name,
				 const char* window_name,
				 const MPI_Comm *param_comm){

   // vector of (option,value) pairs
  std::vector<std::pair<std::string,std::string> > param_list;
  get_parameter_list(param_list, file_name);

  // Check to see if the window exists
  //  Roccom_base * rcom= COM_get_roccom();
  COM::Roccom_base * rbase = COM_get_roccom();
  int whandle = COM_get_window_handle(window_name);
  COM::Window* param_window = NULL;
  if(whandle >0)
    param_window = rbase->get_window_object(whandle);
  COM::Attribute* param_att = NULL;

  // if the parameter window already exists, then only read
  // in attributes which exist in the window
  if(param_window){
    for(uint i =0, ni = param_list.size(); i<ni; ++i){

      param_att = param_window->attribute(param_list[i].first);

      if (param_att) {
	COM_assertion_msg( param_att->location()=='w' &&
			   param_att->size_of_components()==1,
			   "The attribute must be a windowed scalar attribute.");
	// Obtain address for the current attribute.
	void *addr = param_att->pointer();
	COM_assertion_msg( addr, "Unexpected null pointer");

	// Map value based on its type.
	switch ( param_att->data_type()) {
	case COM_INTEGER:
	case COM_UNSIGNED:
	case COM_INT: {
	  *(int*)addr = std::atoi( param_list[i].second.c_str());
	  break;
	}
	case COM_DOUBLE_PRECISION:
	case COM_DOUBLE: {
          // replace 'd' or 'D' by 'e'
          char *p = const_cast< char* >(  strpbrk(param_list[i].second.c_str(), "dD") );
          if (p) *p = 'e';
	  *(double*)addr = std::atof( param_list[i].second.c_str());
	  break;
	}
	case COM_FLOAT:
	case COM_REAL: {
          char *p = const_cast< char* >( strpbrk(param_list[i].second.c_str(), "dD") );
          if (p) *p = 'e';
	  *(float*)addr = std::atof( param_list[i].second.c_str());
	  break;
	}
	case COM_CHAR: {
	  int param_size = param_list[i].second.size();
	  COM_assertion_msg( param_size<param_att->size_of_items(),
			     "Attribute does not have enough space");

	  strcpy( (char*)addr, param_list[i].second.c_str());
	  break;
	}
	case COM_BOOL: {
	  int param_size = param_list[i].second.size();
          const char *str = param_list[i].second.c_str();
          char val = str[0];
          if (val == 'T') val = 1;
          if (val == 'F') val = 0;
          if (val == 'Y') val = 1;
          if (val == 'N') val = 0;
          if (val == '1') val = 1;
          if (val == '0') val = 0;
	  COM_assertion_msg((val == 1 || val==0),
			     "Invalid boolean value (T or F)");

          *(char *)addr = val;
	  break;
        }
	default:
	  COM_assertion_msg(false, "Unsupported data type.");
	}
      }
    }
  }
  // if the parameter window doesn't exist, then create it
  // and populate it will all of the option value pairs
  else{
    if(param_comm)
      COM_new_window(window_name, *param_comm);
    else
      COM_new_window(window_name);
    std::string wname(window_name);
    for(uint i =0, ni = param_list.size(); i<ni; ++i){
      COM_new_attribute( (wname+"."+param_list[i].first).c_str(),
			 'w', COM_CHAR, 1, "");
      int param_size = param_list[i].second.size();
      COM_set_size( (wname+"."+param_list[i].first).c_str(),
		    0, param_size+1,0);
      void *addr;
      COM_resize_array( (wname+"."+param_list[i].first).c_str(), 0, &addr);
      strcpy( (char*)addr, param_list[i].second.c_str());
    }
    // Create an empty pane so that the window will be written out by Rocout
    COM_set_size((wname+".nc").c_str(),1,0);
    COM_window_init_done(window_name);
  }
}

void eat_whitespace_until(std::string& str, const char& c, int start){
  for(int i=start, ni=str.size(); i<ni&&str[i]!=c; ++i){
    if(str[i] == ' ' || str[i] == '\t'){
      str.erase(i,1);
      --i;
    }
  }
}

void get_parameter_list(std::vector<std::pair<std::string,std::string> >& param_list,
			 const char* file_name){

  //open parameter file for input
  std::ifstream param_file(file_name);
  COM_assertion_msg(param_file,
  		    "Specified parameter file not found.");

  string cur_line, option, value;
  string::size_type cur_pos;
  // get one line of the file at a time
  while (getline(param_file,cur_line)){

    cur_pos = cur_line.find('#');
    if(cur_pos != string::npos)
      cur_line.erase(cur_pos, cur_line.size()-cur_pos);

    // remove whitespace up to first '='
    eat_whitespace_until(cur_line, (char)61);

    // If the line only contained whitespace and comments,
    // go to the next line.
    if(cur_line.size()==0)
      continue;

    // Now find the first '='
    cur_pos = cur_line.find_first_of("=");
    COM_assertion_msg(cur_pos != string::npos  && cur_pos !=0,
  		      (std::string("Parameter file '")
                       + file_name + "' has an invalid line.").c_str());

    option.assign(cur_line,0,cur_pos);
    value.assign(cur_line,cur_pos+1,string::npos);

    //eat all whitspace in the value up to the first '"'
    eat_whitespace_until(value, '"');

    //if the value has quotes, then make sure they are only at
    //beginning and ending of the string
    cur_pos = value.find('"');
    if(cur_pos != string::npos){
      COM_assertion_msg(cur_pos == 0,
			"Parameter file has invalid value format");
      value.erase(0,1);
      cur_pos = value.rfind('"');
      // This should get rid of any whitespace after the last quotation mark
      eat_whitespace_until(value, '"', (int)cur_pos+1);
      COM_assertion_msg(cur_pos == value.size()-1,
			"Parameter file has invalid value format");
      value.erase(cur_pos,1);
      COM_assertion_msg(value.find('"') == string::npos,
			"Parameter file has invalid value format");
    }

    // quotations are never allowed in the option
    cur_pos = option.find_first_of("\"");
    COM_assertion_msg(cur_pos == string::npos,
		      "Unexpected quotation mark found in an option.");

    param_list.push_back(std::make_pair(option,value));
  }

}


