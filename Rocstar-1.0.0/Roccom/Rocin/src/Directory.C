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
#include <string>
#include <vector>
#include <iostream>
#include <sys/types.h>
#include <dirent.h>

#include "Directory.H"

Directory::Directory(const std::string &path)
{
  _good = false;
  _dir = NULL;
  _path.assign(path);
  if(open(path)){
    std::cerr << "Directory::Error: Could not open " << path 
	      << " as a directory." << std::endl;
    _good = false;
  }
  else
    close();
}

Directory::~Directory()
{
  // Take the default
}

Directory::operator void* ()
{
  return(_good ? this : NULL);
}

bool 
Directory::operator ! ()
{
  return(!_good);
}

void 
Directory::close()
{
  if(_good)
    closedir(_dir);
}

int 
Directory::open(const std::string &path)
{
  //  if(_good){
    //    this->close();
  _path = path;
    //  }
  if(path.empty())
    return(1);
  if(!(_dir = opendir(path.c_str())))
    return(1);
  _path = path;
  _good = true;
  struct dirent *entry;
  // Skip . and ..
  entry = readdir(_dir);
  entry = readdir(_dir);
  while((entry = readdir(_dir)) != NULL)
    this->push_back(entry->d_name);
  return(0);
}







