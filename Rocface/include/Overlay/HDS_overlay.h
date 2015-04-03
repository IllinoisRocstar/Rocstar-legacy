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
// $Id: HDS_overlay.h,v 1.10 2008/12/06 08:43:27 mtcampbe Exp $

//==============================================
//  This file contains definition of the halfedge data structure for 
//     the overlay algorithm.
//
//  Author: Xiangmin Jiao
//  Optimized: Apr. 4, 2002
//==============================================

#ifndef HDS_OVERLAY_H
#define HDS_OVERLAY_H

#include "rfc_basic.h"
#include "In_place_list_n.h"
#include <iterator>

RFC_BEGIN_NAME_SPACE

class Vertex_overlay;
class Facet_overlay;
class Halfedge_overlay;
class RFC_Pane_overlay;

class Vertex_overlay {
  // defines the maximal vertex functionality including halfedge pointer
  // and a template parameter for the point.
protected:
  Halfedge_overlay *hdg;
  RFC_Pane_overlay *pn;

public:
  typedef Vertex_overlay  Self;
  typedef Tag_true        Supports_vertex_point;
  typedef Tag_true        Supports_vertex_halfedge;
  typedef Point_3         Point;
  
  Vertex_overlay() : hdg(NULL), pn(NULL) {}
  explicit Vertex_overlay( const Point&) : hdg(NULL), pn(NULL) {}
  ~Vertex_overlay() {  RFC_assertion_code(hdg=NULL); }

  // Use default copy constructor and copy assignment operator.
  // Vertex_overlay( const Vertex_overlay &v) hdg(v.hdg), pn(v.pn) {}
  // Self &operator=( const Self &v)
  // { hdg = v.hdg; pn = v.pn; return *this; }
  
  RFC_Pane_overlay       *pane() { return pn; }
  const RFC_Pane_overlay *pane() const { return pn; }
  void set_pane( RFC_Pane_overlay *p) { pn = p; }

  Halfedge_overlay*       halfedge()         { return hdg;}
  const Halfedge_overlay* halfedge() const   { return hdg;}
  void  set_halfedge( Halfedge_overlay* h)   { hdg = h;}

  const Point& point() const;
};

// type declaration for Halfedge_overlay
class Halfedge_overlay {
public:
  typedef Halfedge_overlay          Self;
  typedef Vertex_overlay            Vertex;
  typedef Facet_overlay             Facet;

  typedef Tag_true        Supports_halfedge_prev;
  typedef Tag_true        Supports_halfedge_vertex;
  typedef Tag_true        Supports_halfedge_facet;  

protected:
  Halfedge_overlay *nxt;
  char             *prv;
  Facet_overlay    *f;
  Vertex_overlay   *v;

public:
  Halfedge_overlay() : nxt(NULL), prv(NULL), f(NULL), v(NULL) {}

  // Use default copy constructor and copy assignment operator.
  // Halfedge_overlay( const Halfedge_overlay &h) 
  //   : nxt(h.nxt), prv(h.prv), f(h.f), v(f.v) {}
  // Self &operator=( const Self &h)
  //   { nxt=h.nxt; prv(h.prv), f = h.f; v=h.v; return *this; }
  
  Vertex_overlay*       vertex()       { return v;}
  const Vertex_overlay* vertex() const { return v;}
  // the incident vertex.

  void  set_opposite( Halfedge_overlay* h) 
  { if ( h>this) set_primary(); else unset_primary(); }
  void  set_next( Halfedge_overlay* h)     { nxt = h; }
  void  set_prev( Halfedge_overlay* h)     
  { prv = ((char*)h)+is_primary(); RFC_assertion( prev()==h); }
  void  set_vertex( Vertex_overlay* _v)    { v = _v;}
  void  set_facet( Facet_overlay* _f)      { f = _f; }

  Halfedge_overlay*       opposite()       { return is_primary() ? this+1 : this-1; }
  const Halfedge_overlay* opposite() const { return is_primary() ? this+1 : this-1; }
  Halfedge_overlay*       next()           { return nxt;}
  const Halfedge_overlay* next() const     { return nxt;}

  Halfedge_overlay*       prev()       
  { return (Halfedge_overlay*)(prv-is_primary()); }
  const Halfedge_overlay* prev() const 
  { return (Halfedge_overlay*)(prv-is_primary()); }

  // the facet to the left.
  Facet_overlay* facet() { return f; }
  const Facet_overlay* facet() const { return f;}
  bool  is_border() const            { return f==0; }

  //====== The following functions are added for the overlay algorithm
  // the origin of the halfedge
  Vertex_overlay*       origin() { return opposite()->vertex(); }
  const Vertex_overlay* origin() const { return opposite()->vertex(); }
  Vertex_overlay*       destination() { return vertex(); }
  const Vertex_overlay* destination() const { return vertex(); }
private:
  bool is_primary() const { return (prv-(char*)0)&1; }
  void set_primary()   { if ( !is_primary()) prv+=1; }
  void unset_primary() { if ( is_primary())  prv-=1; }
};

class Facet_overlay {
protected:
  Halfedge_overlay *hdg;
public:
  typedef Facet_overlay   This;
  typedef Tag_false       Supports_facet_plane;
  typedef Tag_false       Supports_facet_normal;
  typedef Tag_true        Supports_facet_halfedge;
  typedef void*           Plane;
  typedef void*           Normal;

  Facet_overlay() : hdg(0) {}
  ~Facet_overlay() { RFC_assertion_code(hdg=NULL); }

  // Use default copy constructor and copy assignment operator.
  // Facet_overlay( const Facet_overlay &f) : hdg(f.hdg) {}
  // This &operator=(const This &f) { hdg = f.hdg; return *this; }

  Halfedge_overlay*       halfedge()       { return hdg;}
  const Halfedge_overlay* halfedge() const { return hdg;}
  void set_halfedge( Halfedge_overlay* h)  { hdg = h;}
};

RFC_END_NAME_SPACE

#include <CGAL/Halfedge_data_structure_using_vector.h>

RFC_BEGIN_NAME_SPACE

// HDS_overlay inherits the implementation of 
// CGAL::Halfedge_data_structure_using_vector and overwrites
// the methods new_vertex, and normalize_border.
// Note that this class is neither copy-constructible nor assignable.
class HDS_overlay : private CGAL::Halfedge_data_structure_using_vector
< Vertex_overlay, Halfedge_overlay, Facet_overlay> {
public:
  typedef HDS_overlay                           This;
  typedef CGAL::Halfedge_data_structure_using_vector< Vertex_overlay, 
    Halfedge_overlay, Facet_overlay>            Base;

  // Inherit the following from CGAL::Halfedge_data_structure_using_vector.
  using Base::Vertex; 
  using Base::Halfedge;
  using Base::Facet;
  using Base::Difference;
  using Base::Size;
  using Base::Point;

  using Base::Supports_halfedge_facet;
  using Base::Supports_vertex_halfedge;
  using Base::Supports_halfedge_prev;
  using Base::Supports_halfedge_vertex;
  using Base::Supports_facet_halfedge;
  using Base::Supports_vertex_point;
  using Base::Supports_facet_plane;
  using Base::Supports_facet_normal;
  using Base::Supports_removal;

  using Base::iterator_category;
  using Base::Vertex_iterator;
  using Base::Halfedge_iterator;
  using Base::Facet_iterator;

  using Base::Vertex_const_iterator;
  using Base::Halfedge_const_iterator;
  using Base::Facet_const_iterator;

  using Base::reserve;
  using Base::size_of_vertices;
  using Base::size_of_halfedges;
  using Base::size_of_facets;
  using Base::capacity_of_vertices;
  using Base::capacity_of_halfedges;
  using Base::capacity_of_facets;
  using Base::bytes;
  using Base::bytes_reserved;
  using Base::vertices_begin;
  using Base::vertices_end;
  using Base::halfedges_begin;
  using Base::halfedges_end;
  using Base::facets_begin;
  using Base::facets_end;

  using Base::new_edge;
  using Base::new_facet;
  using Base::delete_all;
  using Base::vertex_pop_back;
  using Base::edge_pop_back;
  using Base::facet_pop_back;

  using Base::size_of_border_halfedges;
  using Base::size_of_border_edges;
  using Base::border_halfedges_begin;

  // Constructors
  explicit HDS_overlay( RFC_Pane_overlay *p) : pn(p) {}
  HDS_overlay( RFC_Pane_overlay *p, const HDS_overlay &hds)
    : Base( hds), pn(p) { 
    for ( Vertex_iterator it=vertices_begin(); it!=vertices_end(); ++it) 
      it->set_pane( p);
  }

  // We need no destructor because memory is not dynamically allocated.

  RFC_Pane_overlay *pane() { return pn; }
  const RFC_Pane_overlay *pane() const { return pn; }

  // Perform the standard normalization and also 
  // make every border vertex point to its incident border halfedge.
  void normalize_border_vertices();

  // Insertion
//
// The following operations overwrites the ones in 
// CGAL::Halfedge_data_structure_using_vector to initialize the hds fields
// in Vertex_overlay, Halfedge_overlay and Facet_overlay.
  Vertex* new_vertex() {
              RFC_assertion( vertices.size() < vertices.capacity());
              vertices.push_back( Vertex()); vertices.back().set_pane(pn);
              return & (vertices.back());
  }

  Vertex* new_vertex( const Vertex* v) {
              RFC_assertion( vertices.size() < vertices.capacity());
              vertices.push_back( *v); vertices.back().set_pane(pn);
              return & (vertices.back());
  }

  Vertex* new_vertex( const Point& p) {
              RFC_assertion( vertices.size() < vertices.capacity());
              vertices.push_back( Vertex( p)); vertices.back().set_pane(pn);
              return & (vertices.back());
  }


protected:
  RFC_Pane_overlay *pn;

private:
  // Explicitly disable the following functions.
  HDS_overlay();
  HDS_overlay( const HDS_overlay&);
  This &operator=( const HDS_overlay&);
};

struct Host_face {
  Host_face() : _p(0) {};
  Host_face( Halfedge_overlay* h, Parent_type t) 
    : _p( (char*)(h) + (int(t)&3))
  { RFC_assertion( h==halfedge() && t==parent_type()); }
  Halfedge_overlay *halfedge() const 
  { return (Halfedge_overlay*)(_p-parent_type()); }
  Parent_type parent_type() const 
  { return Parent_type(std::distance((char*)NULL,_p)&3); }
  bool operator==( const Host_face &h) const { return _p==h._p; }
  bool operator!=( const Host_face &h) const { return _p!=h._p; }
private:
  char *_p;
};

class INode : private In_place_list_n_base<INode,2> {
public:
  typedef INode                         Self;
  typedef Halfedge_overlay              Halfedge;
  typedef In_place_list_n_base<INode,2> Base;

  using Base::next_link;
  using Base::prev_link;
protected:
  Host_face   bp;     // Blue parent object
  Host_face   gp;     // Green parent object
  Point_2S    b_nc;   // The natrual coordinate in the original blue mesh
  Point_2S    g_nc;   // The natrual coordinate in the original green mesh
  int         _id;

public:
  INode() : _id(-1) {}
  ~INode() { RFC_assertion_code(bp=gp=Host_face()); }
    
  bool operator==( const INode &x) 
  { return ( b_nc == x.b_nc && g_nc == x.g_nc && bp == x.bp && gp == x.gp); }
  
  const Point_2S &nat_coor( const int color) const {
    if ( color == BLUE) return b_nc;
    else { RFC_assertion(color == GREEN); return g_nc; }
  }

  void nat_coor( const int color, Point_2 &p) const {
    if ( color == BLUE) { p[0]=b_nc[0]; p[1]=b_nc[1]; }
    else { RFC_assertion(color == GREEN); p[0]=g_nc[0]; p[1]=g_nc[1]; }
  }

  void set_parent( Halfedge* h, const Point_2& p,  int color) {
    if ( color == BLUE) { 
      bp = Host_face( h, pt(p)); b_nc[0]=p[0]; b_nc[1]=p[1]; 
    }
    else {
      RFC_assertion( color == GREEN); 
      gp = Host_face( h, pt(p)); g_nc[0]=p[0]; g_nc[1]=p[1]; 
    }
  }

  int id() const { return _id; }
  void set_id( int i)  { _id = i; }

  Parent_type parent_type( const int color) const {
    if ( color == BLUE) { return bp.parent_type(); }
    else { RFC_assertion( color == GREEN); return gp.parent_type(); }
  }
  Parent_type blue_parent_type()  const { return bp.parent_type(); }
  Parent_type green_parent_type() const { return gp.parent_type(); }

  Halfedge *halfedge( const int color) const {
    if ( color == BLUE) { return bp.halfedge(); }
    else { RFC_assertion( color == GREEN); return gp.halfedge(); }
  }
  Halfedge* blue_halfedge()  const { return bp.halfedge(); }
  Halfedge* green_halfedge() const { return gp.halfedge(); }

private:
  Parent_type pt( const Point_2 &p) {
    if ( p[1] != 0) return PARENT_FACE;
    else if ( p[0] != 0) return PARENT_EDGE;
    else return PARENT_VERTEX;
  }
};

RFC_END_NAME_SPACE

#endif






