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

/** \file Pane_node_accessors.h
 * Some definitions of classes for accessing data in a mesh.
 */
/* Author: T.J. Alumbaugh
 * Date:   October 19, 2004
 */

#ifndef _PANE_NODE_ACCESSORS_H_
#define _PANE_NODE_ACCESSORS_H_

#include "roccom.h"
#include <cassert>
#include <algorithm>

COM_BEGIN_NAME_SPACE

class Pane_node_enumerator {
public:
  Pane_node_enumerator() : _pane(NULL){}

  /// Constructor for an element in a structured or an unstructured mesh.
  /// If conn==NULL, then i is an element index local to the pane. 
  /// If conn!=NULL, then i is an element index local to the connectivity.
  Pane_node_enumerator( const Pane *pane, int i,
			   const Connectivity *conn=NULL);

  /// Go to the next element within the connectivity tables of a pane.
  int next();

  /// Get the dimension of the base pane.
  int dimension() const { return _pane->dimension(); }

  /// Get the local id of the element within the pane.
  int id() const {
	  return _node_num;
  }

  // Obtain a reference to the pane
  inline const Pane* pane() const { return _pane; }
  
protected:
  const Pane         *_pane;   // Owner pane.
  const Connectivity *_conn;
  bool position_allowed3D();
  bool position_allowed2D();
  int	_node_num;	//pane level id of node
  int   ni;
  int   nj;
  int   nk;
  int   _buffer;
  int   _size_of_nodes;
  void set_first() {
	  _node_num = ni*nj*_buffer + ni*_buffer + _buffer+1;
  }

};


COM_END_NAME_SPACE

#endif // _PANE_NODE_ENUMERATOR_H_
// EOF






