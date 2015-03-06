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
// $Id: RFC_Window_base_IO_binary.C,v 1.5 2008/12/06 08:43:28 mtcampbe Exp $

#include "RFC_Window_base.h"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <cassert>

RFC_BEGIN_NAME_SPACE

inline void
write(std::ostream& os, const int t) {
  os.write((char*)&t, sizeof(int));
}

inline void
read(std::istream& is, int& t) {
  is.read((char*)&t, sizeof(int));
}

inline void
swap_endian( short int &t) {
  char *c = (char*)&t;
  std::swap( c[0],c[1]);
}

inline void
swap_endian( int &t) {
  char *c = (char*)&t;
  std::swap( c[0],c[3]); std::swap( c[1],c[2]);
}

inline void
swap_endian( float &t) {
  char *c = (char*)&t;
  std::swap( c[0],c[3]); std::swap( c[1],c[2]);
}

inline void
swap_endian( double &t) {
  char *c = (char*)&t;
  std::swap( c[0],c[7]); std::swap( c[1],c[6]);
  std::swap( c[2],c[5]); std::swap( c[3],c[4]);
}

// Write the subdivision in binary format.
void RFC_Pane_base::
write_binary( std::ostream &os) const {
  RFC_assertion( sizeof(int)==4);
  RFC_assertion( sizeof(Real)==8);

  write( os, int(1));  // For detecting big or small endian
  Real  three = 3.;    // Version number (New version changed on 04/11/04)
  os.write( (char *)&three, sizeof(Real));

  // Write out color and the id of the pane.
  write( os, _color); write( os, id());
  
  // #nodes and #faces
  write( os, size_of_nodes()); write( os, size_of_faces());

  // Nodal coordinates of the pane
  const Real *p = _base->coordinates();
  os.write( (const char*)p, 3*_base->size_of_nodes()*sizeof(Real));

  if ( _base->is_structured()) {
    write( os, 1); 
    write( os, 2); write( os, 1); 
    write( os, _base->size_i()); write( os, _base->size_j()); 
  }
  else {
    std::vector< const COM::Connectivity*>  elems;
    _base->elements( elems);
    write( os, elems.size());
    // Output sizes of the faces
    for ( std::vector<const COM::Connectivity*>::const_iterator
	    it = elems.begin(); it != elems.end(); ++it) {
      write( os, (*it)->size_of_nodes_pe()); 
      write( os, (*it)->size_of_elements());

      const int *e = (*it)->pointer();
      const int nn = (*it)->size_of_nodes_pe();
      os.write( (const char*)e, (*it)->size_of_elements()*nn*sizeof(int));
    }
  }

  // Pane connectivity of input mesh
  write( os, _b2v_table.size());
  for ( B2v_table::const_iterator 
	  it=_b2v_table.begin(), iend=_b2v_table.end();	it != iend; ++it) {
    const B2v &b2v = it->second;

    int n=b2v.size();
    write( os, it->first); write( os, n);
    os.write( (const char*)&b2v[0], n*sizeof(b2v[0]));
  }
  write ( os, _v2b_table.size());
  for ( V2b_table::const_iterator
	  it = _v2b_table.begin(), iend=_v2b_table.end(); it!=iend; ++it) {
    write( os, it->first); 
    write( os, it->second.first);
    write( os, it->second.second);
  }

  write(os, _subnode_parents.size()); write(os, _subfaces.size());

  // Output subnodes. 
  {
    int n=_subnode_parents.size(); 
    RFC_assertion( sizeof(_subnode_parents[0])==8);
    os.write( (const char*)&_subnode_parents[0], 
	      n*sizeof(_subnode_parents[0]));
    os.write( (const char*)&_subnode_nat_coors[0], 
	      n*sizeof(_subnode_nat_coors[0]));
    os.write( (const char*)&_subnode_counterparts[0], 
	      n*sizeof(_subnode_counterparts[0]));
  }

  // Output subfaces.
  {
    int n = _subfaces.size();
    os.write( (const char*)&_subfaces[0], 
	      n*sizeof(_subfaces[0]));
    os.write( (const char*)&_subface_parents[0], 
	      n*sizeof(_subface_parents[0]));
    os.write( (const char*)&_subface_counterparts[0], 
	      n*sizeof(_subface_counterparts[0]));
  }

  // Offsets of sub-faces.
  RFC_assertion( int(_subface_offsets.size()) == int(size_of_faces())+1);
  os.write( (const char*)&_subface_offsets[0],
	    sizeof(int)*_subface_offsets.size());
  
  write( os, _subnode_parents.size()+_subfaces.size()); // Checksum
}

// Read in RFC_Pane_base in binary format.
void RFC_Pane_base::
read_binary( std::istream &is, std::vector <int> *b2v_all, COM::Pane *pn_in) {

  COM_Pane_friend *pn = (COM_Pane_friend*)pn_in;
  RFC_assertion( sizeof(int)==4);
  RFC_assertion( sizeof(Real)==8);

  int t1, t2;
  
  read( is, t1); // For detecting big or small endian
  bool need_swap = (t1 != 1);
  if ( need_swap) swap_endian( t1);
  RFC_assertion( t1 == 1);
  Real version;
  is.read( (char*)&version, sizeof(Real));
  if ( need_swap) swap_endian( version);
  RFC_assertion( version == 3.);

  // Read in the color and the id of the pane.
  read( is, t1); read( is, t2); 
  if ( need_swap) { swap_endian( t1); swap_endian( t2); }
  
  // #nodes and #faces
  read( is, t1); read( is, t2);
  if ( need_swap) { swap_endian( t1); swap_endian( t2); }
  if ( pn != NULL) pn->set_size( pn->attribute(COM::COM_NC), t1, 0);
  else 
    RFC_assertion( t1 == size_of_nodes() && t2 == size_of_faces());
  _subface_offsets.resize( t2);

  // Nodal coordinates of the pane
  {
    int n=3*_base->size_of_nodes();
    std::vector< Real> buf( n);
    is.read( (char*)&buf.front(), n*sizeof(Real));
  }

  // Read in number of element types.
  int ntypes, is_structured=false;
  read( is, ntypes); if ( need_swap) { swap_endian( ntypes); }
  for ( int i=0; i<ntypes; ++i) {
    read( is, t1); read( is, t2);
    if ( need_swap) { swap_endian( t1); swap_endian( t2); }

    if ( t1 == 2) { // Structured mesh
      is_structured=true;
      RFC_assertion( t2==1);
      int dims[2];
      read( is, dims[0]); read( is, dims[1]);
      if ( need_swap) { swap_endian( dims[0]); swap_endian( dims[1]); }
      if ( pn != NULL) {
	int *t = &dims[0];
	COM::Connectivity *conn = pn->connectivity( ":st2:");
	pn->reinit_conn( conn, COM::Pane::OP_SET, &t, 0, 0);
      }
    }
    else { // Unstructured mesh
      int n=t1*t2;
      if ( pn) {
	int *buf=NULL;
	std::string elem;
	// Create a new Connectivity object in the Pane
	switch (t1) {
	case 3: elem=":t3:"; break;
	case 4: elem=":q4:"; break;
	case 6: elem=":t6:"; break;
	case 8: elem=":q8:"; break;
	case 9: elem=":q9:"; break;
	default: RFC_assertion(false);
	}
	// Insert a connectivity
	COM::Connectivity *conn=pn->connectivity( elem);
	pn->set_size( conn, t2, 0);
	pn->reinit_conn( conn, COM::Pane::OP_RESIZE, &buf, 0, 0);

	is.read( (char*)buf, n*sizeof(int));
	if ( need_swap) for ( int j=0; j<n; ++j) swap_endian( buf[j]);
      }
      else {
	int t;
	for ( int i=0; i<n; ++i) is.read( (char*)&t, sizeof(int));
      }
    }
  }

  // Pane connectivity of input mesh
  read( is, t1); t2 = size_of_nodes();
  if ( need_swap) { swap_endian( t1); }
  if ( b2v_all) b2v_all->reserve(t1);
  for ( int i=0; i<t1; ++i) {
    int pane_id, n;
    read( is, pane_id); read( is, n);
    if ( need_swap) { swap_endian( pane_id); swap_endian( n); }
    B2v &b2v = _b2v_table[pane_id];
    b2v.resize( n);
    is.read( (char*)&b2v[0], n*sizeof(b2v[0]));
    if ( need_swap) for ( int j=0; j<n; ++j) swap_endian( b2v[j]);
    RFC_assertion_code( for ( int j=0; j<n; ++j) 
			RFC_assertion( b2v[j]>=1 && b2v[j]<=t2));
    if ( b2v_all)  { 
      b2v_all->push_back( pane_id); b2v_all->push_back( n); 
      b2v_all->insert( b2v_all->end(), b2v.begin(), b2v.end());
    }
  }

  read( is, t1); if ( need_swap) { swap_endian( t1); }
  for ( int i=0; i<t1; ++i) {
    int n1,n2,n3;
    read( is, n1);  if ( need_swap) { swap_endian( n1); }
    read( is, n2);  if ( need_swap) { swap_endian( n2); }
    read( is, n3);  if ( need_swap) { swap_endian( n3); }

    _v2b_table[n1] = std::make_pair( n2, n3);
  }

  read( is, t1); read( is, t2);
  if ( need_swap) { swap_endian( t1); swap_endian( t2); }
  _subnode_parents.resize( t1);
  _subnode_nat_coors.resize( t1);
  _subnode_counterparts.resize( t1);
  _subfaces.resize( t2);
  _subface_parents.resize( t2);
  _subface_counterparts.resize( t2);

  // Read in the subnodes.
  int n=_subnode_parents.size(); 
  RFC_assertion( sizeof(_subnode_parents[0])==8);
  is.read( (char*)&_subnode_parents[0], n*sizeof(_subnode_parents[0]));
  if ( need_swap)
    for ( int j=0; j<n; ++j) {
      swap_endian( _subnode_parents[j].face_id);
      swap_endian( _subnode_parents[j].edge_id);
    }

  is.read( (char*)&_subnode_nat_coors[0], 
	   n*sizeof(_subnode_nat_coors[0]));
  if ( need_swap)
    for ( int j=0; j<n; ++j) {
      swap_endian( _subnode_nat_coors[j][0]);
      swap_endian( _subnode_nat_coors[j][1]);
    }

  is.read( (char*)&_subnode_counterparts[0], 
	   n*sizeof(_subnode_counterparts[0]));
  if ( need_swap)
    for ( int j=0; j<n; ++j) {
      swap_endian( _subnode_counterparts[j].pane_id);
      swap_endian( _subnode_counterparts[j].node_id);
    }

  n = t2;
  // Read in the subfaces.
  is.read( (char*)&_subfaces[0], n*sizeof(_subfaces[0]));
  if ( need_swap)
    for ( int j=0; j<n; ++j) {
      swap_endian( _subfaces[j][0]);
      swap_endian( _subfaces[j][1]);
      swap_endian( _subfaces[j][2]);
    }
  is.read( (char*)&_subface_parents[0], n*sizeof(_subface_parents[0]));
  if ( need_swap)
    for ( int j=0; j<n; ++j) {
      swap_endian( _subface_parents[j]);
    }
  is.read( (char*)&_subface_counterparts[0], 
	   t2*sizeof(_subface_counterparts[0]));
  if ( need_swap)
    for ( int j=0; j<n; ++j) {
      swap_endian( _subface_counterparts[j].pane_id);
      swap_endian( _subface_counterparts[j].face_id);
    }

  // Read in the offset of sub-faces.
  _subface_offsets.resize( size_of_faces()+1);
  n = _subface_offsets.size();
  is.read( (char*)&_subface_offsets[0], sizeof(int)*n);
  if ( need_swap) for ( int j=0; j<n; ++j) swap_endian( _subface_offsets[j]);

  RFC_assertion( _subface_offsets[size_of_faces()] == 
		 int(_subfaces.size()));

  read( is, t1);
  if ( need_swap) { swap_endian( t1); }
  RFC_assertion( t1 ==int(_subnode_parents.size()+_subfaces.size())); // Checksum

  comp_nat_coors();
}


RFC_END_NAME_SPACE






