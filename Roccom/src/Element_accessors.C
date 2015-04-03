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
// $Id: Element_accessors.C,v 1.11 2008/12/06 08:43:25 mtcampbe Exp $

#include "Element_accessors.h"
#include "roccom_assertion.h"
#include <cstdlib>

COM_BEGIN_NAME_SPACE

Element_node_enumerator::
Element_node_enumerator( const Pane *pane, int i,
			 const Connectivity *conn)
  : _pane(pane), _conn( _pane->is_structured()?NULL:
			(conn==NULL?pane->connectivity(i):conn)) 
{
  if ( _pane->is_unstructured()) {
    if ( conn == NULL && _conn != NULL) i -= _conn->index_offset()+1;
    else --i;

    if ( _conn) {
      COM_assertion( i>=0 && i<std::max(int(_conn->size_of_elements()),1));
      _start = _conn->pointer()+i*_conn->size_of_nodes_pe();
      _res = _conn->size_of_elements() - i;
    }
    else {
      _pane = NULL;
      _conn = NULL;
      _start = NULL;
      _res = -1;
    }
  }
  else if ( _pane->dimension()==2) {
    COM_assertion( i>=1 && i<=int(_pane->size_of_elements()));
    _res = (i-1) % (_pane->size_i()-1);
    _base = i + (i-1) / (_pane->size_i()-1);
  }
  else {
    COM_assertion(false); // Not yet supported.
  }
}

/*! 
 * \param pane a pointer to the owner pane.
 * \param r the row id of the element (starting from 1).
 * \param c the column id of the element (starting from 1).
 */
Element_node_enumerator::Element_node_enumerator(const Pane *pane,
						 const std::pair<int,int> &id)
  : _pane(pane), _conn(NULL) {
  COM_assertion( _pane != NULL && _pane->is_structured());
  COM_assertion( id.first>0 && id.first<=int(_pane->size_i()));
  COM_assertion( id.second>0 && id.second<=int(_pane->size_j()));

  _res = id.first-1;
  _base = (id.second-1)*_pane->size_i()+id.first;
}

void Element_node_enumerator::next() {
  if ( _conn) {
    if ( --_res==0) {
      _conn = _pane->connectivity( _conn->index_offset()+1+
				   _conn->size_of_elements());
      if ( _conn) {
	_start = _conn->pointer();
	_res = _conn->size_of_elements();
      }
      else {
	_pane = NULL;
	_conn = NULL;
	_start = NULL;
	_res = -1;
      }
    }
    else
      _start += _conn->size_of_nodes_pe();
  }
  else if (_pane->dimension()==2) { // Structured mesh
    ++_base; 
    if ( ++_res == int(_pane->size_i()-1)) { ++_base; _res=0; }
  }
  else {
    COM_assertion(false); // Not yet supported.
  }
}

int
Element_node_enumerator::vertex( int lvid, bool level) const {
  if ( _pane->is_structured()) { // Structured mesh
    if ( lvid == _base) return 0;
    if ( lvid == _base+1) return 1;
    const int nr = _pane->size_i();
    if ( lvid == int(_base+nr)) return 3;
    if ( lvid == int(_base+nr+1)) return 2;

    // There is a branch cut in the pane. Resort to more expensive step.
    // Check which boundary lvid is on.
    if ( !level) {
      COM_assertion( nr>2 && _pane->size_j()>2);
      const int c1 = (_base-1)/nr, r1 = (_base-1)%nr;
      const int c2 = (lvid-1)/nr, r2 = (lvid-1)%nr;
      
      if ( std::abs( r1-r2)<=1) {
	const int nc = _pane->size_j(); 
	if ( c2 == 0) return vertex( (nc-1)*nr+r2+1, true);
	if ( c2 == nc-1) return vertex( r2+1, true);
      }
      else {
	COM_assertion( std::abs( c1-c2)<=1);
	if ( r2 == 0) return vertex( (c2+1)*nr, true);
	if ( r2 == nr-1) return vertex( c2*nr+1, true);
      }
    }
    COM_assertion( false); return -1;
  }
  else { // Unstructured
    const int *p=_start;
    for ( int i=0, size=_conn->size_of_nodes_pe(); i<size; ++i,++p) {
      if ( lvid == *p) return i;
    }

    return -1; // The node is not in the table
  }
}

void Element_node_enumerator::get_nodes( std::vector< int > &nodes) {
  int num_nodes = size_of_nodes();
  nodes.resize( num_nodes);

  for (int i=0; i<num_nodes; i++) nodes[i] = (*this)[i];
}

Facet_node_enumerator::
Facet_node_enumerator( const Element_node_enumerator *ene, int k)
  : _ene(ene), _k(k), _ne(0) {
  if ( ene->dimension() == 2) {
    const_cast<int&>(_ne) = 1;
    _fn_list = NULL;
  }
  else {
    switch ( ene->size_of_nodes()) {
    case 4:    // Tetrahedra
    case 10: {
      const_cast<int&>(_ne) = 3;
      _fn_list = _face_node_lists_tets[_k];
      break;
    }
    case 5:    // Pyramids
    case 14: {
      const_cast<int&>(_ne) = (_k==0)? 4 : 3;
      _fn_list = _face_node_lists_pyra[_k];
      break;
    }
    case 6:    // Prisms (Pentahedra)
    case 15:
    case 18: {
      const_cast<int&>(_ne) = (_k<3)? 4 : 3;
      _fn_list = _face_node_lists_pris[_k];
      break;
    }
    case 8:    // Hexahedra
    case 20:
    case 27: {
      const_cast<int&>(_ne) = 4;
      _fn_list = _face_node_lists_hexa[_k];
      break;
    }
    default: COM_assertion_msg(false, "Not supported yet.");
    }
  }
}

int Facet_node_enumerator::size_of_nodes() const {
  if ( _ne == 1)
    return _ene->is_quadratic() ? 3 : 2;
  else {
    int nn = _ne; // Number of nodes per face

    if ( _ene->is_quadratic()) {
      int num_nodes = _ene->size_of_nodes();
      nn += _ne + (num_nodes == 18 && _k<3 || num_nodes == 27);
    }
    return nn;
  }
}

int Facet_node_enumerator::operator[]( int i) const  {
  if ( _ne > 1) 
    return (*_ene)[_fn_list[i]];
  else {
    if ( i<2) return (*_ene)[(_k+i)%_ene->size_of_edges()];
    else return (*_ene)[_ene->size_of_edges()+_k];
  }
}

void Facet_node_enumerator::
get_nodes( std::vector< int > &nodes, bool quad_ret) {
  int nn = quad_ret ? size_of_nodes() : size_of_corners();
  nodes.resize( nn);

  if ( _ne == 1) {
    int num_edges = _ene->size_of_edges(); 
    // Retrun the nodes on each edge.
    nodes[0] = (*_ene)[(_k)%num_edges];
    nodes[1] = (*_ene)[(_k+1)%num_edges];

    if (nn>2) nodes[2] = (*_ene)[num_edges+_k];
  } 
  else {
    for ( int i=0; i<nn; ++i) nodes[i] = (*_ene)[_fn_list[i]];
  }
}

const int Facet_node_enumerator::_face_node_lists_tets[4][6] =
{ {0, 2, 1, 6, 5, 4}, { 0, 1, 3, 4, 8, 7}, 
  {1, 2, 3, 5, 9, 8}, {2, 0, 3, 6, 7, 9}};

const int Facet_node_enumerator::_face_node_lists_pyra[5][8] =
{ {0,3,2,1,8,7,6,5}, {0,1,4,5,10,9,-1,-1}, {1,2,4,6,11,10,-1,-1},
  {2,3,4,7,12,11,-1,-1}, {3,0,4,8,9,12,-1,-1}};

const int Facet_node_enumerator::_face_node_lists_pris[5][9] =
{ {0,1,4,3,6,10,12,9,15}, {1,2,5,4,7,11,13,10,16}, {2,0,3,5,8,9,14,11,17},
  {0,2,1,8,7,6,-1,-1,-1}, {3,4,5,12,13,14,-1,-1,-1}};

const int Facet_node_enumerator::_face_node_lists_hexa[6][9] =
{ {0,3,2,1,11,10,9, 8, 20}, {0,1,5,4,8, 13,16,12,21},
  {1,2,6,5,9, 14,17,13,22}, {2,3,7,6,10,15,18,14,23},
  {0,4,7,3,12,19,15,11,24}, {4,5,6,7,16,17,18,19,25}};

COM_END_NAME_SPACE






