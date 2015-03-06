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
// $Id: Dual_connectivity.C,v 1.17 2008/12/06 08:43:21 mtcampbe Exp $

#include <algorithm>

#include "Dual_connectivity.h"

MAP_BEGIN_NAMESPACE

Pane_dual_connectivity::Pane_dual_connectivity( const COM::Pane *p, 
						bool with_ghost) 
  : _pane(*p), _with_ghost(with_ghost) {
  
  if ( p->dimension()==2 && p->is_structured())
    construct_connectivity_str_2();
  else {
    assert( !p->is_structured());
    construct_connectivity_unstr();
  }
}

void Pane_dual_connectivity::incident_elements( int node_id, 
						std::vector<int>& elists) {
  COM_assertion( node_id > 0); // Node id start from 1.
  elists.clear();
  elists.insert( elists.begin(), &_eids[_offsets[node_id-1]], &_eids[_offsets[node_id]]);
}

void Pane_dual_connectivity::construct_connectivity_str_2() {
  COM_assertion_msg( _with_ghost || _pane.size_of_ghost_layers()==0,
		     "Not yet implemented for structured mesh with ghosts");

  int n=_with_ghost ? _pane.size_of_nodes() : _pane.size_of_real_nodes();
  { // First, count the number of incident elements of each .
    std::vector<char> nielems(n);
    std::fill_n( nielems.begin(), n, 4);
  
    int ni=_pane.size_i(), nj=_pane.size_j();
  
    std::fill_n( nielems.begin(), ni, 2);
    std::fill_n( nielems.begin()+ni*(nj-1), ni, 2);
  
    for ( int i=ni; i<ni*(nj-1); i+=ni)
      nielems[i] = nielems[i+ni-1] = 2;

    nielems[0] = nielems[ni-1] = 1;
    nielems[ni*nj-ni] = nielems[ni*nj-1] = 1;
  
    // Now construct a vector for storing the offsets
    _offsets.clear(); _offsets.resize(n+1, 0);

    for ( int i=1; i<=n; ++i) _offsets[i] = _offsets[i-1]+nielems[i-1];
    _eids.resize( _offsets[n]);
  }

  { // Fill in the connectivity table
    const int nn = 4;
    Element_node_enumerator_str_2 ene( &_pane, 1);
    for ( int i=1,size=_pane.size_of_elements(); i<=size; ++i, ene.next())
      for ( int k=0; k<nn; ++k) 
	_eids[ _offsets[ene[k]-1]++] = i;
    
    // Recover _offsets
    for ( int i=n; i>0; --i) _offsets[i] = _offsets[i-1];
    _offsets[0] = 0;
  }
}

void Pane_dual_connectivity::construct_connectivity_unstr() {

  int nnodes, nelems;

  if ( _with_ghost) {
    nnodes=_pane.size_of_nodes();
    nelems=_pane.size_of_elements();
  }
  else {
    nnodes=_pane.size_of_real_nodes();
    nelems=_pane.size_of_real_elements();
  }
  
  { // First, count the number of incident elements of each node.
    std::vector<char> nielems(nnodes);
    std::fill_n( nielems.begin(), nnodes, 0);

    Element_node_enumerator_uns ene( &_pane, 1);
    for ( int i=1; i<=nelems; ++i, ene.next())
      for ( int k=0, nn=ene.size_of_nodes(); k<nn; ++k) 
	++nielems[ ene[k]-1];
    
    // Now construct a vector for storing the offsets
    _offsets.clear(); _offsets.resize(nnodes+1);
    _offsets[0] = 0;
    for ( int i=1; i<=nnodes; ++i) _offsets[i] = _offsets[i-1]+nielems[i-1];

    _eids.resize( _offsets[nnodes]);
  }

  { // Fill in the connectivity table
    Element_node_enumerator_uns ene( &_pane, 1);
    for ( int i=1; i<=nelems; ++i, ene.next())
      for ( int k=0, nn=ene.size_of_nodes(); k<nn; ++k) 
	_eids[ _offsets[ene[k]-1]++] = i;

    // Recover _offsets
    for ( int i=nnodes; i>0; --i) _offsets[i] = _offsets[i-1];
    _offsets[0] = 0;
  }
}

MAP_END_NAMESPACE






