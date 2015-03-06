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
// $Id: RFC_Window_transfer.C,v 1.15 2008/12/06 08:43:29 mtcampbe Exp $

//===============================================================
// Implementaiton of RFC_Window_transfer and RFC_Pane_transfer.
// Author: Xiangmin Jiao
// Date:   June. 15, 2001
//===============================================================

#include "RFC_Window_transfer.h"

RFC_BEGIN_NAME_SPACE

RFC_Pane_transfer::RFC_Pane_transfer( COM::Pane *b, int c)
  : Base( b, c), _window( NULL), _to_recv( NULL) {}
RFC_Pane_transfer::~RFC_Pane_transfer() {}

// Constructor and deconstructors
RFC_Window_transfer::RFC_Window_transfer( COM::Window *b, int c, 
					  MPI_Comm com, 
					  const char *pre, 
					  const char *format) 
  : Base(b,c,com), _buf_dim(0), _comm( com), _replicated(false), 
    _prefix( pre==NULL?b->name():pre), _IO_format( get_sdv_format(format)) {

  std::vector< Pane*> pns; panes(pns);
  std::vector< Pane*>::iterator pit=pns.begin(), piend=pns.end();
  for ( ; pit != piend; ++pit) {
    (*pit)->_window = this;
  }

  // Initialize pane_map
  int nprocs = comm_size();
  _num_panes.resize( nprocs,int(0));

  const COM::Window::Proc_map &proc_map = _base->proc_map();
  
  for ( COM::Window::Proc_map::const_iterator
	  it=proc_map.begin(), iend=proc_map.end(); it!=iend; ++it) {
    int p=it->second;
    _pane_map[ it->first] = std::pair<int,int>(p, _num_panes[p]++);
  }
}

RFC_Window_transfer::~RFC_Window_transfer() {
  while ( !_replic_panes.empty()) {
    delete _replic_panes.begin()->second;
    _replic_panes.erase( _replic_panes.begin());
  }
}

RFC_Pane_transfer &
RFC_Window_transfer::pane( const int pid) {
  Pane_set::iterator pit = _pane_set.find( pid);
  if ( pit != _pane_set.end()) {
    return reinterpret_cast<RFC_Pane_transfer&>(*pit->second);
  }
  else {
    return *_replic_panes.find( pid)->second; 
  }
}

const RFC_Pane_transfer &
RFC_Window_transfer::pane( const int pid) const {
  Pane_set::const_iterator pit = _pane_set.find( pid);
  if ( pit != _pane_set.end()) {
    return reinterpret_cast<const RFC_Pane_transfer&>(*pit->second);
  }
  else {
    return *_replic_panes.find( pid)->second; 
  }
}

// Initialize buffer spaces for data transfer
void RFC_Window_transfer::
init_nodal_buffers( const Nodal_data &nd, int size, bool init_emm) {
  _buf_dim = nd.dimension();

  // Loop through the panes
  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer&)*pi->second;

    pane._buffer.resize( size);
      
    for ( int i=0; i<size; ++i) 
      pane._buffer[i].resize(pane._base->size_of_nodes()*_buf_dim);
      
    int n=pane.size_of_faces();

    if ( !init_emm) continue;

    pane._emm_offset.resize( n+1);
    // Initialize space for the mass matrix
    pane._emm_offset[0] = 0;
    
    int k=0;
    // Loop through the connectivity table to mark their nodes as false.
    if ( pane._base->is_structured()) {
      for ( int i=0; i<n; ++i, ++k) {
	pane._emm_offset[k+1] = pane._emm_offset[k] +  16;
      }
    }
    else {
      std::vector< const COM::Connectivity*>  elems;
      pane._base->elements( elems);
      
      for ( std::vector<const COM::Connectivity*>::const_iterator
	      it = elems.begin(); it != elems.end(); ++it) {
	const int nn = (*it)->size_of_nodes_pe();
	for ( int i=1,size=(*it)->size_of_elements(); i<=size; ++i, ++k) {
	  pane._emm_offset[k+1] = pane._emm_offset[k] +  nn*nn;
	}
      }
    }
    
    pane._emm_buffer.resize( pane._emm_offset[n], 0);
  }
}

Nodal_data RFC_Window_transfer::
nodal_buffer( int index) {
  RFC_assertion( index >= 0);
  return Nodal_data( -index-1, _buf_dim);
}

void RFC_Window_transfer::
delete_nodal_buffers() {
  // Loop through the panes to remove the buffer spaces
  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer &)*pi->second;

    free_vector( pane._buffer);
    free_vector( pane._emm_offset);
    free_vector( pane._emm_buffer);
  }
}

void RFC_Window_transfer::
set_tags( const COM::Attribute *tag) {
  // Loop through the panes to set the tags
  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer &)*pi->second;

    pane._to_recv = ( tag == NULL) ? (int *)NULL : 
      (int *)(pane._base->attribute( tag->id())->pointer());
  }
}

// Initialize buffer spaces for data transfer
void RFC_Window_transfer::
init_facial_buffers( const Facial_data &nd, int size) {
  _buf_dim = nd.dimension();

  // Loop through the panes
  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer&)*pi->second;
 
    pane._buffer.resize( size);

    for ( int i=0; i<size; ++i) 
      pane._buffer[i].resize(pane.size_of_faces()*_buf_dim);
  }
}

Facial_data RFC_Window_transfer::
facial_buffer( int index) {
  RFC_assertion( index >= 0);
  return Facial_data( -index-1, _buf_dim);
}

void RFC_Window_transfer::
delete_facial_buffers() {
  // Loop through the panes to remove the buffer spaces
  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer &)*pi->second;

    free_vector( pane._buffer);
  }
}

void 
RFC_Window_transfer::incident_panes( std::vector<int> &pane_ids) {
  std::set<int>  ids;

  // Loop through the panes to detect incident panes
  for (Pane_set::iterator pi=_pane_set.begin(); pi != _pane_set.end(); ++pi) {
    RFC_Pane_transfer &pane = (RFC_Pane_transfer &)*pi->second;

    for ( int i=0, size=pane._subface_counterparts.size(); i<size; ++i) {
      ids.insert( pane._subface_counterparts[i].pane_id);
    }
    for ( int i=0, size=pane._subnode_counterparts.size(); i<size; ++i) {
      RFC_assertion(ids.find( pane._subnode_counterparts[i].pane_id) != ids.end());
    }
  }

  pane_ids.resize(0); pane_ids.reserve( ids.size());
  pane_ids.insert( pane_ids.end(), ids.begin(), ids.end());
}

RFC_END_NAME_SPACE






