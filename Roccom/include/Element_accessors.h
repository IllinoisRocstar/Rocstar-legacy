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
// $Id: Element_accessors.h,v 1.12 2008/12/06 08:43:24 mtcampbe Exp $

/** \file Enumerator_accessors.h
 * Some definitions of classes for accessing data in a mesh.
 */
/* Author: Xiangmin Jiao
 * Date:   Mar. 12, 2002
 */

#ifndef _ELEMENT_ACCESSORS_H_
#define _ELEMENT_ACCESSORS_H_

#include "roccom.h"
#include <cassert>
#include <algorithm>

COM_BEGIN_NAME_SPACE

/** An adaptor for enumerating node IDs of an element.
 */
class Element_node_enumerator {
public:
  Element_node_enumerator() : _pane(NULL), _conn(NULL), _start(NULL), _res(0){}

  /// Constructor for an element in a structured or an unstructured mesh.
  /// If conn==NULL, then i is an element index local to the pane. 
  /// If conn!=NULL, then i is an element index local to the connectivity.
  Element_node_enumerator( const Pane *pane, int i,
			   const Connectivity *conn=NULL);
  
  /// A constructor for an element in a structured mesh.
  Element_node_enumerator( const Pane *pane,
			   const std::pair<int,int> &id);

  /// Go to the next element within the connectivity tables of a pane.
  void next();

  /// Obtain the type of the element.
  int type() const { 
    if ( _conn) return _conn->element_type(); 
    else if ( _pane->dimension()==2) return Connectivity::ST2;
    else return Connectivity::ST3;
  }

  /// Check whether the element is quadratic.
  int is_quadratic() const { 
    if ( _conn) return _conn->is_quadratic(); 
    else return false;
  }

  /// Number of nodes per element.
  int size_of_nodes() const {
    if ( _conn) return _conn->size_of_nodes_pe();
    if ( _pane->dimension()==2) return 4;
    return 8;
  }

  /// Number of corners per element.
  int size_of_corners() const {
    if ( _conn) return _conn->size_of_corners_pe();
    if ( _pane->dimension()==2) return 4;
    return 8;
  }

  /// Get a vector of all of the nodes in the element
  void get_nodes( std::vector< int > &nodes);

  /// Number of edges per element.
  int size_of_edges() const  {
    if ( _conn) return _conn->size_of_edges_pe();
    if ( _pane->dimension()==2) return 4;
    return 12;
  }

  /// Number of faces per element.
  int size_of_faces() const  {
    if ( _conn) return _conn->size_of_faces_pe();
    if ( _pane->dimension()==2) return 1;
    return 6;
  }

  /// Get the dimension of the base pane.
  int dimension() const { return _pane->dimension(); }

  /// Get the local id of the element within the pane.
  int id() const {
    if ( _pane->is_structured()) {
      return _base - (_base-1) / _pane->size_i();
    }
    else {
      return _conn->index_offset() + 1 +
	(_start - _conn->pointer()) / _conn->size_of_nodes_pe();
    }
  }

  //! Get the vertex index of an vertex within the element.
  int vertex( int lvid, bool level=false) const;

  /** Obtain the pane-scope node ID from its element-scope index.
   * \param i the element-scope index of a node (starting from 0).
   */
  inline int operator[]( int i) const  {
    if ( _conn) // Unstructured mesh
      return _start[i];
    else { // Structured mesh; Only support 2-D mesh now.
      int t=_base;
      if ( i>1) t+=_pane->size_i();
      return ( i==1 || i==2)? t+1 : t;
    }
  }

  // Obtain a reference to the pane
  inline const Pane* pane() const { return _pane; }
  
protected:
  const Pane         *_pane;   // Owner pane.
  const Connectivity *_conn;
  union {
    const int             *_start;  // For unstructured mesh
    int                   _base;    // For structured mesh
  };
  int                     _res;     // For structured or unstructured mesh
};

/** Optimized version for 2-D structured meshes */ 
class Element_node_enumerator_str_2 : public Element_node_enumerator {
public:
  Element_node_enumerator_str_2() {}
  Element_node_enumerator_str_2( const Pane *pane, int i)
    : Element_node_enumerator( pane, i) {}

  Element_node_enumerator_str_2( const Pane *pane, 
			       const std::pair<int,int> &id)
    : Element_node_enumerator( pane, id) {}

  void next() 
  { ++_base; if ( ++_res == int(_pane->size_i()-1)) { ++_base; _res=0; } }
  
  int operator[]( int i) const  {
    int t=_base;
    if ( i>1) t+=_pane->size_i();
    return ( i==1 || i==2)? t+1 : t;
  }
};

/** Optimized version for unstructured meshes */ 
class Element_node_enumerator_uns : public Element_node_enumerator {
public:
  Element_node_enumerator_uns() {}
  Element_node_enumerator_uns( const Pane *pane, int i,
			       const Connectivity *conn=NULL)
    : Element_node_enumerator( pane, i, conn) {}

  void next() {
    if ( --_res==0) {
      _conn = _pane->connectivity( _conn->index_offset()+1+
				   _conn->size_of_elements());
      if ( _conn) {
	_start = _conn->pointer();
	_res = _conn->size_of_elements();
      }
      else {
	_start = NULL;
	_res = -1;
      }
    }
    else
      _start += _conn->size_of_nodes_pe();
  }
  
  int operator[]( int i) const  { return _start[i]; }
  int size_of_nodes()   const   { return _conn->size_of_nodes_pe();  }
};


/** Adaptor for enumerating node IDs of a facet of an element.
 */
class Facet_node_enumerator {
public:
  // Constructor from an element and a local face ID (starting from 0).
  Facet_node_enumerator( const Element_node_enumerator *ene, int k) ;
  
  // Get the number of the nodes of a facet.
  int size_of_nodes() const;

  // Get the number of the corners of a facet.
  int size_of_corners() const { return (_ne == 1) ? 2 : _ne ; }

  // Get the number of the edges of a facet.
  int size_of_edges() const { return _ne;  }

  // Get a list of node IDs of the given facet.
  void get_nodes( std::vector< int > &nodes, bool quad_ret);

  // Get the node ID of a particular node within a facet.
  int operator[]( int i) const;

protected:
  static const int _face_node_lists_tets[4][6];
  static const int _face_node_lists_pyra[5][8];
  static const int _face_node_lists_pris[5][9];
  static const int _face_node_lists_hexa[6][9];

  const Element_node_enumerator * const _ene;
  const int _k;
  const int _ne;        // Number of edges. 1 in 2-D.
  const int *_fn_list;  // NULL in 2-D and pointer in 3-D.
};

/// This is a helper class for accessing nodal data.
template <class Value>
class Element_node_vectors_k_const {
 public:
  enum { MAX_NODES=9};
  typedef unsigned int       Size;

  /// Default constructor.
  Element_node_vectors_k_const() : _attr(NULL)
  { std::fill_n( _vs, 9, reinterpret_cast<Value*>(NULL)); }
  
  /// initialize the accessor with a pointer and a specific stride.
  void set( const Value *p, Element_node_enumerator &ene, int strd) {
    COM_assertion( ene.size_of_nodes() <= MAX_NODES);
    _attr = NULL;
    for ( int i=ene.size_of_nodes()-1; i>=0; --i)
      _vs[i] = &p[(ene[i]-1)*strd];
  }

  /// initialize the accessor from an attribute assiciated with a pane.
  void set( const Attribute *a, Element_node_enumerator &ene) {
    COM_assertion( a->is_nodal() && a->pane()->id());
    COM_assertion( sizeof(Value)==COM_get_sizeof( a->data_type(),1));

    int ncomp = a->size_of_components();
    int strd = a->stride();
    int nn = ene.size_of_nodes();
    if ( ncomp<=strd) { 
      const Value *p = reinterpret_cast<const Value*>(a->pointer());
      _attr = NULL; 
      for ( int i=0; i<nn; ++i) _vs[i] = &p[(ene[i]-1)*strd];
    }
    else {
      _attr = a;
      for ( int i=0; i<nn; ++i) _offsets[i] = ene[i]-1;
    }
  }

  // Access a vector.
  const Value &operator()( int i, int j) const {
    if ( _attr) {
      COM_assertion( _attr->size_of_components()>1);
      const Attribute *aj = _attr+j+1;
      return reinterpret_cast<const Value*>
	(aj->pointer())[_offsets[i]*aj->stride()];
    }
    else
      return _vs[i][j];
  }

  const Value &operator()(int i) const { return operator()(i,0); }

  const Value &operator[](int i) const { return operator()(i,0); }

protected:
  const Attribute *_attr;
  union {
    int                 _offsets[MAX_NODES];  // Used if _attr is not NULL
    const Value        *_vs[MAX_NODES];       // Used if _attr is NULL
  };
};

template <class Value>
class Element_node_vectors_k : public Element_node_vectors_k_const<Value> {
  typedef Element_node_vectors_k_const<Value>    Base;
public:
  Element_node_vectors_k() : Base() {}

  void set( Value *p, Element_node_enumerator &ene, int strd) 
  { Base::set( p, ene, strd); }

  void set( Attribute *a, Element_node_enumerator &ene) 
  { Base::set( a, ene); }

  Value &operator()(int i, int j) const 
  { return const_cast<Value&>(Base::operator()(i,j)); }

  Value &operator()(int i) const 
  { return const_cast<Value&>(Base::operator()(i,0)); }

  Value &operator[](int i) const 
  { return const_cast<Value&>(Base::operator()(i,0)); }
};

/// This is a helper class for accessing elemental data.
template <class Value>
class Element_vectors_k_const {
 public:
  typedef unsigned int       Size;

  /// Default constructor.
  Element_vectors_k_const() : _attr(NULL), _vs(NULL) {}
  
  /// initialize the accessor with a pointer and a specific stride.
  void set( const Value *p, Element_node_enumerator &ene, int strd) {
    _attr = NULL;
    _vs = &p[(ene.id()-1)*strd];
  }

  /// initialize the accessor from an attribute assiciated with a pane.
  void set( const Attribute *a, Element_node_enumerator &ene) {
    COM_assertion( a->is_elemental() && a->pane()->id());
    COM_assertion( sizeof(Value)==COM_get_sizeof( a->data_type(),1));

    int ncomp = a->size_of_components();
    int strd = a->stride();
    if ( ncomp<=strd) { 
      const Value *p = reinterpret_cast<const Value*>(a->pointer());
      _attr = NULL; 
      _vs = &p[(ene.id()-1)*strd];
    }
    else {
      _attr = a;
      _offset = ene.id()-1;
    }
  }

  // Access a vector.
  const Value &operator()( int i, int j) const {
    if ( _attr) {
      COM_assertion( _attr->size_of_components()>1);
      const Attribute *aj = _attr+j+1;
      return reinterpret_cast<const Value*>
	(aj->pointer())[_offset*aj->stride()];
    }
    else
      return _vs[j];
  }

  const Value &operator()(int i) const { return operator()(i,0); }

  const Value &operator[](int i) const { return operator()(i,0); }

protected:
  const Attribute *_attr;
  union {
    int                 _offset;   // Used if _attr is not NULL
    const Value        *_vs;       // Used if _attr is NULL
  };
};

template <class Value>
class Element_vectors_k : public Element_vectors_k_const<Value> {
  typedef Element_vectors_k_const<Value>    Base;
public:
  Element_vectors_k() : Base() {}

  void set( Value *p, Element_node_enumerator &ene, int strd) 
  { Base::set( p, ene, strd); }

  void set( Attribute *a, Element_node_enumerator &ene) 
  { Base::set( a, ene); }

  Value &operator()(int i, int j) const 
  { return const_cast<Value&>(Base::operator()(i,j)); }

  Value &operator()(int i) const 
  { return const_cast<Value&>(Base::operator()(i,0)); }

  Value &operator[](int i) const 
  { return const_cast<Value&>(Base::operator()(i,0)); }
};

COM_END_NAME_SPACE

#endif // _ELEMENT_ENUMERATOR_H_
// EOF






