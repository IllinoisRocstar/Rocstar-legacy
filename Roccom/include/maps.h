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
// $Id: maps.h,v 1.6 2008/12/06 08:43:24 mtcampbe Exp $

/** \file maps.h
 * Contains declaration of mappings for module, window, function, attribute.
 * @see Roccom_base.C
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_MAPS_H__
#define __ROCCOM_MAPS_H__

#include "roccom_basic.h"
#include "roccom_exception.h"
#include <list>

COM_BEGIN_NAME_SPACE

/// Supports mapping from names to handles and vice-versa for 
/// a module, window, function, or attribute.
template <class Object>
class Roccom_map {
  typedef std::vector<Object>         I2O; ///< Mapping from indices to objects
  typedef std::map<std::string, int>  N2I; ///< Mapping from names to indices
public:
  typedef Object                      value_type;

  Roccom_map() {}
  ~Roccom_map() {}
  
  /// Insert an object into the table.
  int add_object( std::string name, Object t, 
		  bool is_const=false) throw(COM_exception);

  /// Remove an object from the table.
  void remove_object( std::string name, 
		      bool is_const=false) throw(COM_exception);

  /// whether the object mutable
  bool is_immutable(int i) const
  { return names[i].find(" (const)")!=std::string::npos; }

  /// Access an object using its handle.
  const Object &operator[](int i) const throw(COM_exception) {
    if ( i>=(int)i2o.size()) throw COM_exception( COM_UNKNOWN_ERROR);
    return i2o[i];
  }

  Object &operator[](int i) throw(COM_exception) {
    if ( i>=(int)i2o.size()) throw COM_exception( COM_UNKNOWN_ERROR);
    return i2o[i];
  }

  /// Name of the object
  const std::string &name( int i) const { return names[i]; }

  int size() const { return names.size(); }

  std::pair<int,Object *> find( const std::string &name, bool is_const=false) {
    N2I::iterator it=n2i.find( is_const?name+"(const)":name);
    if ( it == n2i.end()) 
      return std::pair<int,Object *>(-1,NULL);
    else
      return std::pair<int,Object *>(it->second, &i2o[ it->second]);
  }
  
  std::vector<std::string> get_names() { return names; }
protected:
  I2O    i2o;                      ///< Mapping from index to objects
  N2I    n2i;                      ///< Mapping from names to indices
  std::list<int> salvaged;         ///< List of salvaged indices.
  std::vector<std::string>  names; ///< Name of the objects
};

template <class Object>
int Roccom_map<Object>::add_object( std::string name, Object t, 
				    bool is_const) throw(COM_exception)
{
  if (is_const) name.append( " (const)");

  N2I::iterator it = n2i.find(name);
  int i = ( it == n2i.end()) ? -1 : it->second;

  if ( i>0) 
  { i2o[i] = t; }
  else if ( salvaged.empty()) { 
    i=i2o.size(); n2i[name] = i; 
    i2o.push_back( t); names.push_back( name); 
  }
  else { 
    i=salvaged.front(); salvaged.pop_front(); 
    n2i[name] = i; i2o[i] = t; names[i] = name;
  }
  return i; 
}

template <class Object>
void Roccom_map<Object>::remove_object( std::string name, 
					bool is_const) throw(COM_exception) {
  if (is_const) name.append( " (const)");

  N2I::iterator it = n2i.find(name);
  if ( it == n2i.end()) throw COM_exception( COM_UNKNOWN_ERROR);
  salvaged.push_back( it->second);
  n2i.erase(it);
}

class Function;

/// A map functions. Supports quickly finding a function object
/// from a function handle. Also contains timing information of
/// functions to support profiling.
class Function_map : protected Roccom_map<Function*> {
  typedef Roccom_map<Function*>        Base;
public:
  /// Insert a function into the table.
  int add_object( const std::string &n, Function *t) {
    unsigned int i = Roccom_map<Function*>::add_object( n, t);
    if ( i+1>verbs.size()) {
      verbs.resize(i+1, false);
      wtimes_self.resize(i+1, 0.);
      wtimes_tree.resize(i+1, 0.);
      counts.resize(i+1, 0);
    }
    return i;
  }

  using Base::name;
  using Base::operator[];
  using Base::size;

  std::vector<char>         verbs;         ///< Whether verbose is on
  std::vector<double>       wtimes_self;   ///< Accumulator of wall-clock time spent by itself excluding functions called by it.
  std::vector<double>       wtimes_tree;   ///< Accumulator of wall-clock time spent by itself and those functions called by it
  std::vector<int>          counts;        ///< Counts of the number of calls
};

COM_END_NAME_SPACE

#endif






