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
// $Id: Pane_boundary.C,v 1.31 2008/12/06 08:43:21 mtcampbe Exp $

#include <map>
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <sstream>

#include "Pane_boundary.h"

MAP_BEGIN_NAMESPACE

void Pane_boundary::
determine_border_nodes( std::vector<bool> &is_border,
			std::vector<bool> &is_isolated,
			std::vector<Facet_ID > *b,
			int ghost_level) throw(int) {
  if ( _pane.dimension() == 2) {
    int ng;
    int *ng_ptr = ghost_level ? &ng : (int*)NULL;

    if ( _pm) {
      _pm->get_borders( is_border, is_isolated, b, ng_ptr);
    }
    else
      Simple_manifold_2( &_pane, NULL, ghost_level).
	get_borders( is_border, is_isolated, b, ng_ptr);
  }
  else {
	
    determine_border_nodes_3( is_border, b, ghost_level);
    determine_isolated_nodes( is_isolated, ghost_level);
  }
}

// A helper class for comparing two faces.
struct Four_tuple {
  Four_tuple( int a, int b, int c, int d) 
    :  _a(a), _b(b), _c(c), _d(d) {}
  int &operator[](int i) { return (&_a)[i]; }
  const int &operator[](int i) const { return (&_a)[i]; }
  bool operator<( const Four_tuple &x) const {
    return _a<x._a || (_a==x._a && (_b<x._b || _b==x._b && 
				    (_c<x._c || _c==x._c && _d<x._d)));
  }
private:
  int _a, _b, _c, _d;
};

typedef std::map< Four_tuple, Facet_ID>  Corners2Face_Map;

// Check whether a given face exists in the facemap. If so, remove the
// face from the map; otherwise, insert it into the map. This function
// uses a vector of Corners2Face_Map to reduce worst-case time complexity.
static void
check_face_unique( const Four_tuple &face_corners,
		   const Facet_ID   &fid, 
		   std::vector< Corners2Face_Map > &c2f_vec,
		   int &num_external_face, int &counter) {

  // Sort the node ids of the face
  Four_tuple sorted_ns( face_corners);
  std::sort( &sorted_ns[0], &sorted_ns[4]);

  // Check whether the sorted node-list is in vec2facemap_vec 
  // by trying to insert it
  Corners2Face_Map &c2f = c2f_vec[sorted_ns[1]-1];

  std::pair<Corners2Face_Map::iterator, bool> 
    ans = c2f.insert( std::make_pair(sorted_ns, fid));

  // If the node-list already exists, then remove it from vecFaceMap
  if ( !ans.second) {
    counter++;
    c2f.erase( ans.first); 
    --num_external_face; 
  }
  // Otherwise, insertion succeeded.
  else ++num_external_face;
}

void Pane_boundary::
determine_border_nodes_3( std::vector<bool> &is_border,
			  std::vector<Facet_ID > *b, 
			  int ghost_level ) throw(int) {
  int num_nodes, num_elmts;

  if (ghost_level == 0){
    num_nodes=_pane.size_of_real_nodes();
    num_elmts=_pane.size_of_real_elements();
  } 
  else {
    COM_assertion_msg(ghost_level > 0, "Ghost level must be positive");
    num_nodes=_pane.size_of_nodes();
    num_elmts=_pane.size_of_elements();
  }

  // Initialize is_border to false for all nodes.
  is_border.clear(); is_border.resize( num_nodes, false);

  // Handle structured meshes
  if ( _pane.is_structured()) {

    const int ni=_pane.size_i(), nj=_pane.size_j(), nk=_pane.size_k();

    // s = the first index on the level above the bottom
    // t = the first index on the top level
    // h = an offset for the first index on a layer with ghosts
    // w = the length of the i dimension with ghosts
    // buffer = number of layers we 'skip' for determining borders

    int buffer = _pane.size_of_ghost_layers() - ghost_level;
    buffer = buffer < 0 ? 0 : buffer;
    int s = ni*nj;
    int t = ni*nj*(nk-1);
    int h = buffer*ni + buffer;
    int w = ni - 2*buffer;

    //bottom
    for ( int j=s*(buffer) + h, j_end = s*(1+buffer) - ni*buffer; j< j_end; j += ni){
	std::fill_n( is_border.begin() +j, w, true);
    }
    //top
    for ( int j= t+h-s*buffer, end = s*nk - s*buffer- ni*buffer; j< end; j += ni){
	std::fill_n( is_border.begin() +j, w, true);
    }
  
    //left and right 
    for ( int j= s*(1+buffer)+h, stop = t-s*buffer+h; j< stop; j += ni*nj){
	std::fill_n( is_border.begin() +j, w, true);
	std::fill_n( is_border.begin() +j + ni*((nj-1) - 2*buffer), w, true);
    }

    //upper and lower
    for ( int j= s*(1+buffer)+h; j< t-s*buffer+h; j += ni*nj){
	for (int k=ni, k_end = ni*(nj-(1+2*buffer)); k< k_end; k += ni){
	    is_border[j+k] = true;
	    is_border[j+k + (ni - (2*buffer+ 1)) ] = true;
	}
    }

    return;
  }

  // Now consider unstructured panes
  std::vector< Corners2Face_Map>  c2f_vec(num_nodes);
  int num_external_face=0;


  // Determine all border faces
  Element_node_enumerator ene( &_pane, 1);
  int counter = 0;
  int count2 = 0;

  for (int i=1; i<=num_elmts; ++i, ene.next()) {
  //for (int i=1; i<=6; ++i, ene.next()) {
    for (int j=0, nf=ene.size_of_faces(); j<nf; ++j ){
      Facet_node_enumerator fne( &ene, j) ;

      Four_tuple ns( fne[0], fne[1], fne[2], fne.size_of_edges()>3?fne[3]:-1);
      count2++;
      check_face_unique( ns, Facet_ID(i, j), c2f_vec, num_external_face, counter);
    }
  }

  // Mark all nodes of the border faces as border nodes, and 
  // insert their edges into b.
  if ( b) 
  { b->clear(); b->reserve( num_external_face); }

  std::vector<int> nodes; nodes.reserve(9);

  for (int i=0; i<num_nodes; ++i){
    Corners2Face_Map::const_iterator vit = c2f_vec[i].begin(); 
    Corners2Face_Map::const_iterator v_end = c2f_vec[i].end();

    for ( ; vit != v_end; ++vit) {
      // Get all nodes of the face into vector nodes.
      const Facet_ID &fid = vit->second;
      Element_node_enumerator_uns ene( &_pane, fid.eid());
      Facet_node_enumerator fne( &ene, fid.lid());
      fne.get_nodes( nodes, true);

      for ( int i=0, n=nodes.size(); i<n; ++i)
	is_border[ nodes[i] - 1 ] = true;
      
      if (b) b->push_back( fid);
    }
  }
  
}

void Pane_boundary::
determine_isolated_nodes( std::vector<bool> &is_isolated,
			  int ghost_level) throw(int) {
  // Initialize to true
  is_isolated.clear();

  int nnodes = ghost_level ? 
    _pane.size_of_nodes() : _pane.size_of_real_nodes();

  if ( _pane.is_structured()) {
    // A structured mesh have no isolated nodes.
    is_isolated.resize( nnodes, false);
    return;
  }

  is_isolated.resize( nnodes, true);

  // Loop through the connectivity table to mark their nodes as false.
  Element_node_enumerator_uns ene(&_pane, 1);
  int nelems = ghost_level ?
    _pane.size_of_elements() : _pane.size_of_real_elements();
  for ( int i=0,size=nelems; i<size; ++i, ene.next()) {
    for ( int j=ene.size_of_nodes()-1; j>=0; --j)
      is_isolated[ ene[j]-1] = false;
  }
}

inline double square( double x) { return x*x; }

double Pane_boundary::
min_squared_edge_len( const std::vector<Facet_ID > &be) throw(int) {
  const COM::Attribute *attr = _pane.attribute( COM::COM_NC);

  double sql = HUGE_VAL;

  // Loop through all the edges of border facets to compute the
  // shortest squared edge length.
  std::vector<Facet_ID >::const_iterator it;
  for ( it=be.begin(); it!=be.end(); ++it) {
    Element_node_enumerator ene( &_pane, it->eid());
    Facet_node_enumerator fne( &ene, it->lid());
    int ne = fne.size_of_corners();

    // Loop through all edges of the facet.
    for ( int j=0; j<fne.size_of_edges(); ++j) {
      int n1 = fne[j], n2 = fne[(j+1)%ne];
      const double *x1 = (const double*)attr->get_addr( n1-1, 0);
      const double *x2 = (const double*)attr->get_addr( n2-1, 0);

      double sqdiff;
      if ( attr->stride() >= 3) {
	// Contiguous layout of coordinates
	sqdiff = square(x1[0]-x2[0])+square(x1[1]-x2[1])+square(x1[2]-x2[2]);
      }
      else {
	// This supports R^2 points.
	sqdiff=square(x1[0]-x2[0]);
	for ( int i=1, d=attr->size_of_components(); i<d; ++i) {
	  sqdiff += square((const double*)attr->get_addr( n1-1, i)-
			   (const double*)attr->get_addr( n2-1, i));
	}
      }
      
      sql = std::min( sql, sqdiff);
    }
  }

  return sql;
}

void Pane_boundary::determine_borders( const COM::Attribute *mesh,
				       COM::Attribute *isborder,
				       int ghost_level) {
  COM_assertion_msg( mesh, "Unexpected NULL pointer");
  COM_assertion_msg( isborder, "Unexpected NULL pointer");
  COM_assertion_msg( COM_compatible_types( isborder->data_type(), COM_INT),
		     "Border-list must have integer type");
  COM_assertion_msg( isborder->is_nodal() == 1,
		     "Border-list must have integer type");

  std::vector<COM::Pane*> panes;
  isborder->window()->panes( panes);

  for ( int i=0, n=panes.size(); i<n; ++i) {
    Pane_boundary pb( panes[i]);
    COM::Attribute *bdl_pane = panes[i]->attribute(isborder->id());

    std::vector<bool> is_border_bitmap, is_isolated;
    pb.determine_border_nodes( is_border_bitmap, is_isolated, 
			       NULL, ghost_level);

    int *ptr = (int *)bdl_pane->pointer();
    int nj=0;
    if (ghost_level == 0)
      nj=panes[i]->size_of_real_nodes();
    else {
      COM_assertion_msg(ghost_level > 0, "Ghost level must be positive");
      nj=panes[i]->size_of_nodes();
    }
    for ( int j=0; j<nj; ++j) {
      ptr[j] = is_border_bitmap[j];
    }
  }
}

MAP_END_NAMESPACE






