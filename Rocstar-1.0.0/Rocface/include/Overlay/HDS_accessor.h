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

// $Id: HDS_accessor.h,v 1.9 2008/12/06 08:43:27 mtcampbe Exp $

//=======================================================================
//  This file contains the definition of an accessor of the halfedge data
//     structure (HDS). The overlay algorithms uses this accessor to access 
//     information of HDS instead of directly using member functions of HDS.
//  Note that the accessor is not meant to substitute 
//     Halfedge_data_structure_decorator, which supports functions to
//     create/modify the HDS. HDS_accessor mainly supports read operations 
//     for the HDS structure, with few overlay-related access operations.
//
//  Author: Xiangmin Jiao
//  Last modified: Feb. 09, 2001
//=======================================================================

#ifndef RFC_HDS_ACCESSOR_H
#define RFC_HDS_ACCESSOR_H

#include "rfc_basic.h"
#include "RFC_Window_overlay.h"

RFC_BEGIN_NAME_SPACE

template < class _MP=Tag_false>
class HDS_accessor {
public:

// TYPES
// ----------------------------------
  typedef HDS_overlay              HDS;
  typedef Vertex_overlay           Vertex;
  typedef Halfedge_overlay         Halfedge;
  typedef Facet_overlay            Facet;

  // Point needed for Vertex constructor for efficiency reasons.
  typedef typename HDS::Point      Point;
  typedef Vector_3                 Vector;

// The following types are equal to either `Tag_true' or `Tag_false',
// dependant whether the named feature is supported or not.

  typedef _MP                                    Supports_multiple_panes;

// CREATION
// ----------------------------------

  // HDS_accessor() {}

// Access Functions
// ----------------------------------

  Halfedge* get_halfedge( Vertex* v) const {
    // returns the incident halfedge of a vertex v. If supports connected
    // panes, return the primary copy of the halfedge.
     return get_halfedge( v, Supports_multiple_panes());
  }

  Vertex* get_vertex( Halfedge* h) const {
    // returns the incident vertex of h. If supports connected
    // panes, return the primary copy of the vertex.
    return get_vertex( h, Supports_multiple_panes());
  }

  Vertex* get_origin( Halfedge* h) const {
    // returns the incident vertex of h. If supports connected
    // panes, return the primary copy of the vertex.
    return get_vertex( get_opposite(h,Tag_false()));
  }

  Vertex* get_destination( Halfedge* h) const {
    // returns the incident vertex of h. If supports connected
    // panes, return the primary copy of the vertex.
    return get_vertex( h);
  }

  Halfedge* get_opposite( Halfedge *h) const {
    // Get the opposite halfedge of h. If supports connected
    // panes, return the primary copy of the opposite halfedge.
    return get_opposite( h, Supports_multiple_panes());
  }

  inline Halfedge* get_prev( Halfedge* h) const 
  { return (Halfedge*)get_prev( (const Halfedge*)h); }

  inline Halfedge* get_next( Halfedge* h) const 
  { return (Halfedge*)get_next( (const Halfedge*)h); }

  Facet* get_facet( Halfedge* h) const {
    // returns the incident facet of h.
    return h->facet();
  }

  Halfedge* get_halfedge( Facet* f) const {
    // returns the incident halfedge of f. 
    // Note that the halfedge is necessarily a primary copy.
    return f->halfedge();
  }

  Vertex* get_primary( Vertex *v) const
  { return v?get_primary( v, Supports_multiple_panes()):v; }

  HDS_overlay *get_hds( Vertex *v)   const { return v->pane()->hds(); }
  HDS_overlay *get_hds( Halfedge *h) const { return get_hds(h->vertex()); }

  RFC_Pane_overlay *get_pane( Vertex *v)   const { return v->pane(); }
  RFC_Pane_overlay *get_pane( Halfedge *h) const { return get_pane(h->vertex()); }

  Halfedge* get_prev_around_origin(Halfedge* h) const {
    // returns the previous halfedge around h->origin(). 
    return get_opposite( get_prev( h));
  }

  Halfedge* get_next_around_origin(Halfedge* h) const {
    // returns the next halfedge around h->origin().
    return get_next( get_opposite( h));
  }

  Halfedge* get_prev_around_destination(Halfedge* h) const {
    // returns the previous halfedge around h->destination().
    return get_prev( get_opposite( h));
  }

  Halfedge* get_next_around_destination(Halfedge* h) const {
    // returns the next halfedge around h->destination().
    return get_opposite( get_next( h));
  }

// Const Access Functions
// ----------------------------------
  bool is_border( const Halfedge *h) const 
  { return is_border( h, Supports_multiple_panes()); }
  bool is_border( const Halfedge *h, Tag_false) const 
  { return h->is_border(); }
  bool is_border( const Halfedge *h, Tag_true) const 
  { return get_pane(h)->is_physical_border(h); }

  bool is_border( const Vertex *v) const 
  { return is_border( v, Supports_multiple_panes()); }
  // Precondition for is_border( v) is HDS  boundary vertices normalized.
  bool is_border( const Vertex *v, Tag_false) const 
  { return v->halfedge()->is_border(); }
  bool is_border( const Vertex *v, Tag_true) const { 
    if ( !v->halfedge()->is_border()) return false;
    const Halfedge *h = get_halfedge( v), *h0=h;
    do {
      if (h->is_border()) return true;
    } while ( ( h=get_next_around_destination( h)) != h0);
    return false; 
  }

  bool is_on_feature( const Vertex *v) const
  { const Vertex *vprim = get_primary( v);
    return get_pane(vprim)->is_on_feature(vprim); 
  }

  bool is_feature_0( const Vertex *v) const
  { const Vertex *vprim = get_primary( v);
    return get_pane(vprim)->is_feature_0(vprim); 
  }

  bool is_feature_1( const Halfedge *h) const
  { return get_pane(h)->is_feature_1(h); }

  bool is_primary( const Vertex *v) const { return get_pane(v)->is_primary(v);}

  const Vertex* get_primary( const Vertex *v) const
  { return v?get_primary( v, Supports_multiple_panes()):v; }

  const Halfedge* get_halfedge( const Vertex* v) const {
    // returns the incident halfedge of a vertex v. If supports connected
    // panes, return the primary copy of the halfedge.
    return get_halfedge( v, Supports_multiple_panes());
  }

  const Vertex* get_vertex( const Halfedge* h) const {
    // returns the incident vertex of h. If supports connected
    // panes, return the primary copy of the vertex.
    return get_vertex( h, Supports_multiple_panes());
  }

  const Vertex* get_origin( const Halfedge* h) const {
    // returns the incident vertex of h. If supports connected
    // panes, return the primary copy of the vertex.
    return get_vertex( get_opposite(h,Tag_false()));
  }

  const Vertex* get_destination( const Halfedge* h) const {
    // returns the incident vertex of h. If supports connected
    // panes, return the primary copy of the vertex.
    return get_vertex( h);
  }

  const Halfedge* get_opposite( const Halfedge *h) const {
    // Get the opposite halfedge of h. If supports connected
    // panes, return the primary copy of the opposite halfedge.
    return get_opposite( h, Supports_multiple_panes());
  }

  const Halfedge* get_prev( const Halfedge* h) const {
    const Halfedge *prev = h->prev();
    if ( !is_border( h)) return prev;

    RFC_assertion_code( int count=0);
    while ( !is_border(prev)) {
      RFC_assertion( prev->is_border());
      prev = get_counterpart(prev)->opposite()->prev();
      RFC_assertion( is_border(prev) || h != get_counterpart(prev));
      RFC_assertion( ++count<100);
    }
    return prev;
  }

  const Halfedge* get_next( const Halfedge* h) const {
    const Halfedge *next = h->next();
    if ( !is_border( h)) return next;

    RFC_assertion_code( int count=0);
    while ( !is_border(next)) {
      RFC_assertion( next->is_border());
      next = get_counterpart(next)->opposite()->next();
      RFC_assertion( is_border(next) || h != get_counterpart(next));
      RFC_assertion( ++count<100);
    }
    return next;
  }

  const Facet* get_facet( const Halfedge* h) const {
    // returns the incident facet of h.
    return h->facet();
  }
  
  const Halfedge* get_halfedge( const Facet* f) const {
    // returns the incident halfedge of f. 
    // Note that the halfedge is necessarily a primary copy.
    return f->halfedge();
  }

  const HDS_overlay *get_hds( const Vertex *v)   const { return v->pane()->hds(); }
  const HDS_overlay *get_hds( const Halfedge *h) const { return get_hds(h->vertex()); }

  const RFC_Pane_overlay *get_pane(const Vertex *v)   const 
  { return v->pane();}
  const RFC_Pane_overlay *get_pane(const Halfedge *h) const 
  { return get_pane(h->vertex());}

  const Halfedge* get_prev_around_origin( const Halfedge* h) const {
    // returns the previous halfedge around h->origin(). 
    return get_opposite( get_prev( h));
  }

  const Halfedge* get_next_around_origin( const Halfedge* h) const {
    // returns the next halfedge around h->origin().
    return get_next( get_opposite( h));
  }

  const Halfedge* get_prev_around_destination( const Halfedge* h) const {
    // returns the previous halfedge around h->destination().
    return get_prev( get_opposite( h));
  }

  const Halfedge* get_next_around_destination( const Halfedge* h) const {
    // returns the next halfedge around h->destination().
    return get_opposite( get_next(h));
  }

  const Vector_3 &get_normal( const Halfedge *h) const 
  { return get_pane(h)->get_normal(h, h->destination()); }

  const Vector_3 &get_normal( const Halfedge *h, const Vertex *v) const 
  { return get_pane(h)->get_normal(h, v); }

  void mark( Halfedge* h) const
  { get_pane(h)->mark( h); }
  void unmark( Halfedge* h) const
  { get_pane(h)->unmark( h); }
  bool marked(  const Halfedge* h) const 
  { return get_pane(h)->marked( h); }

// Implementing These Functions.
// ====================================================
// Access Functions
// ----------------------------------

  Halfedge* get_halfedge( Vertex* v,Tag_false) const { return v->halfedge(); }
  Halfedge* get_halfedge( Vertex* v,Tag_true) const {
    Halfedge *h = v->halfedge();
    return h->is_border() ? get_counterpart( h) : h;
  }

  Vertex* get_vertex( Halfedge* h, Tag_false) const { return h->vertex(); }
  Vertex* get_vertex( Halfedge* h, Tag_true)  const
  { return get_primary( h->vertex()); }

  Halfedge* get_opposite( Halfedge* h, Tag_false) const 
  { return h->opposite(); }
  Halfedge* get_opposite( Halfedge* i, Tag_true)  const {
    Halfedge *h = i->opposite();
    return h->is_border() ? get_counterpart( h) : h;
  }

  Vertex* get_primary( Vertex *v, Tag_false) const { return v; }
  Vertex* get_primary( Vertex *v, Tag_true) const 
  { return get_pane(v)->get_primary(v); }

// Const Access Functions
// ----------------------------------

  const Halfedge* get_halfedge( const Vertex* v, Tag_false) const  
  { return v->halfedge(); }
  const Halfedge* get_halfedge( const Vertex* v, Tag_true) const {
    const Halfedge *h = v->halfedge();
    return h->is_border() ? get_counterpart( h) : h;
  }

  const Vertex* get_vertex( const Halfedge* h, Tag_false) const 
  { return h->vertex(); }
  const Vertex* get_vertex( const Halfedge* h, Tag_true)  const
  { return get_primary( h->vertex()); }

  const Halfedge* get_opposite( const Halfedge* h, Tag_false) const 
  { return h->opposite(); }
  const Halfedge* get_opposite( const Halfedge* i, Tag_true)  const  {
    const Halfedge *h = i->opposite();
    return h->is_border() ? get_counterpart( h) : h;
  }

  const Vertex* get_primary( const Vertex *v, Tag_false) const 
  { return v; }
  const Vertex* get_primary( const Vertex *v, Tag_true) const 
  { return v?get_pane(v)->get_primary(v):v; }

  void set_parent( INode *i, Halfedge *h, const Point_2 &p, int color) {
    Real x=p[0];  RFC_assertion( x>=0. && x<=1.);
    
    if ( x == 1) { h=get_opposite(h); x = 0; }
    if ( is_border( h)) {
      if (x==0) h = h->opposite()->next();
      else { h = h->opposite(); x=1.-x; }
      RFC_assertion( !h->is_border());
    }
    if ( x!=p[0])
      i->set_parent( h, Point_2(x,p[1]), color);
    else 
      i->set_parent( h, p, color);
  }

  INode *get_inode(  Vertex *v) const
  { RFC_assertion( is_primary( v)); return get_pane( v)->get_inode( v); }
  const INode *get_inode(  const Vertex *v) const 
  { RFC_assertion( is_primary( v)); return get_pane( v)->get_inode( v); }
  void set_inode(  Vertex *v, INode *i) const
  { RFC_assertion( is_primary( v)); get_pane(v)->set_inode(v,i); }

  INode_list &get_inode_list(  Halfedge *h) const
  { return get_pane(h)->get_inode_list(h); }

  const INode_list &get_inode_list(  const Halfedge *h) const
  { return get_pane(h)->get_inode_list(h); }

  INode *get_buffered_inode(  Halfedge *h, int tag) const {
    return get_pane(h)->get_buffered_inode( h, tag);
  }
  void set_buffered_inode( Halfedge *h, int tag, INode *i) const {
    get_pane(h)->set_buffered_inode( h, tag, i);
  }
protected:
  Halfedge *get_counterpart( Halfedge *h) const 
  { return get_pane(h)->get_counterpart( h); }
  const Halfedge *get_counterpart( const Halfedge *h) const  
  { return get_pane(h)->get_counterpart( h); }
};

RFC_END_NAME_SPACE

#endif // RFC_CHDS_DECORATOR_H //
// EOF //






