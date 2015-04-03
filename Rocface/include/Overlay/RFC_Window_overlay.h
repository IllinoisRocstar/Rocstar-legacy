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
// $Id: RFC_Window_overlay.h,v 1.22 2008/12/06 08:43:27 mtcampbe Exp $

//===============================================================
// This file defines RFC_Pane_overlay and RFC_Window_overlay based on
//    RFC_Pane_base and RFC_Window_base respectively.
// Author: Xiangmin Jiao
// Creation date:   Dec. 21, 2001
//===============================================================

#ifndef RFC_WINDOW_H
#define RFC_WINDOW_H

#include "RFC_Window_base.h"
#include "HDS_overlay.h"
#include <CGAL/Polyhedron_incremental_builder_3.h>
#include <map>
#include <list>
#include "In_place_list_n.h"
#include <cmath>
#include "commpi.h"

#ifndef HUGE_VALF
#define HUGE_VALF 1e+36F
#endif

RFC_BEGIN_NAME_SPACE

class INode;
template <class Tag> class HDS_accessor;
class RFC_Window_overlay;
typedef In_place_list_n< INode, false>  INode_list;

// RFC_Pane_overlay is based on RFC_Pane_base with the extension of 
//    a halfedge structure for storing element connectivity and
//    extra attribute for storing normals of all vertices.
class RFC_Pane_overlay : public RFC_Pane_base {
public:
  typedef RFC_Pane_overlay                               Self;
  typedef RFC_Pane_base                                  Base;
  typedef Vertex_overlay                                 Vertex;
  typedef Halfedge_overlay                               Halfedge;
  typedef Facet_overlay                                  Facet;
  typedef HDS_overlay                                    HDS;
  typedef CGAL::Halfedge_data_structure_decorator< HDS>  HDS_decorator;
  typedef CGAL::Polyhedron_incremental_builder_3<HDS>    HDS_builder;

  friend class RFC_Window_overlay;

  RFC_Pane_overlay( COM::Pane *b, int color);
  virtual ~RFC_Pane_overlay() {}

  RFC_Window_overlay *window() { return _window; }
  const RFC_Window_overlay *window() const { return _window; }

  HDS &hds() { return _hds; }
  const HDS &hds() const { return _hds; }

  // Functions for supporting the DCEL data structure
  int get_index( const Vertex *v) const
  { return v - (const Vertex*)&*_hds.vertices_begin(); }
  int get_index( const Halfedge *h) const 
  { return h - (const Halfedge*)&*_hds.halfedges_begin(); }
  int get_index( const Facet *f) const
  { return f - (const Facet*)&*_hds.facets_begin(); }

  int get_lid( const Vertex *v) const
  { return v - (const Vertex*)&*_hds.vertices_begin() + 1; }
  int get_lid( const Facet *f) const
  { return f - (const Facet*)&*_hds.facets_begin() + 1; }

  Vertex *get_vertex_from_id( int i)
  { RFC_assertion(i>0); return &_hds.vertices_begin()[ i-1]; }
  const Vertex *get_vertex_from_id( int i) const
  { RFC_assertion(i>0); return &_hds.vertices_begin()[ i-1]; }

  // If a vertex is a border vertex, it can have instances in multiple
  // panes. The instance in the pane with smallest id is the primary copy.
  bool is_primary( const Vertex *v) const {
    return !v->halfedge()->is_border() || 
      _primaries[get_border_index(v->halfedge())]==v; 
  }

  // Get the primary copy of v.
  const Vertex *get_primary( const Vertex *v) const {
    RFC_assertion( v->halfedge());
    // Precondition: v is a border vertex and v is not an isolated node
    if ( !v->halfedge()->is_border()) return v;
    else return _primaries[get_border_index(v->halfedge())];
  }
  Vertex *get_primary( Vertex *v) 
  { return const_cast<Vertex*>( get_primary( (const Vertex*)v)); }

  const Point_3 &get_point( int id) const 
  { return Base::get_point(id); }
  const Point_3 &get_point( const Vertex *v) const 
  { return Base::get_point( get_index(v),0); }

  const Halfedge *get_counterpart( const Halfedge *h) const {
    if ( !h->is_border()) return h;
    RFC_assertion( h->vertex()->pane()==this);
    return _cntrs[get_border_index(h)];
  }

  Halfedge *get_counterpart(Halfedge *h)  
  { return const_cast<Halfedge*>( get_counterpart( (const Halfedge*)h)); }

  /** Determine whether an edge is on the physical boundary of the window.
   *  A edge is on a physical boundary if it is on the pane boundary and
   *  it does not have a counterpart in any other pane.
   */
  bool is_physical_border( const Halfedge *h) const {
    if ( !h->is_border()) return false;

    RFC_assertion( h->vertex()->pane()==this);
    return _cntrs[ get_border_index(h)] == h;
  }

  // Obtaining the normal direction of a vertex
  Vector_3& get_normal( int v)                 
  { RFC_assertion(v>0); return _nrmls[v-1]; }
  const Vector_3& get_normal( int v) const     
  { RFC_assertion(v>0); return _nrmls[v-1]; }
  void set_normal( int v, const Vector_3 &vec)
  { RFC_assertion(v>0); _nrmls[v-1] = vec; }

  // Get the normal of a vertex.
  Vector_3 &get_normal( Halfedge *h, Vertex *v);
  Vector_3 &get_normal( Halfedge *h, int i)
  { return get_normal( h, get_vertex_from_id(i)); }
  Vector_3 &get_tangent( Halfedge *h, Vertex *v);

  const Vector_3 &get_normal( const Halfedge *h, const Vertex *v) const;
  const Vector_3 &get_normal( const Halfedge *h, int i) const
  { return get_normal( h, get_vertex_from_id(i)); }
  const Vector_3 &get_tangent( const Halfedge *h, const Vertex *v) const;

  void add_tangent( Halfedge *h, const Vector_3 &v) {
    int &ind = _f_t_index[ get_index(h)];
    if ( ind < 0) { ind = _f_tngts.size(); _f_tngts.push_back( v); }
    else _f_tngts[ind] += v;
  }

  INode *get_inode( const Vertex *v) const
  { RFC_assertion( is_primary(v)); return _v_nodes[ get_index(v)]; }
  void set_inode( Vertex *v, INode *i) 
  { RFC_assertion( is_primary(v)); _v_nodes[ get_index(v)] = i; }
  INode_list &get_inode_list( Halfedge *h) 
  { return _e_node_list[ get_index(h)]; }
  const INode_list &get_inode_list( const Halfedge *h) const
  { return _e_node_list[ get_index(h)]; }

  INode *get_buffered_inode( Halfedge *h, int tag) const {
    int i=get_index(h);
    if ( _e_marks[i] == tag) return _e_node_buf[i];
    else return NULL;
  }
  void set_buffered_inode( Halfedge *h, int tag, INode *inode) {
    int i=get_index(h);
    _e_marks[i] = tag;
    _e_node_buf[i] = inode;
  }

  void mark( Halfedge *h)   { _e_marks[get_index(h)] = true; }
  void unmark( Halfedge *h) { _e_marks[get_index(h)] = false; } 
  bool marked( const Halfedge *h) const { return _e_marks[get_index(h)]; } 

  bool is_on_feature( const Vertex *v) const { return _is_on_f[ get_index(v)]; }
  bool is_feature_0( const Vertex *v) const { return _is_f_0[ get_index(v)]; }
  bool is_feature_1( const Halfedge *h) const { return _is_f_1[get_index(h)]; }

  // ===========================
  // Routines for building the subdivision
  const int &size_of_subfaces() const { return _size_of_subfaces; }
  int  &size_of_subfaces() { return _size_of_subfaces; }

  //! Allocate memory space for the arrays for storing the subdivision.
  void allocate_subnodes( int num_sn) {
    _subnode_parents.resize( num_sn);
    _subnode_nat_coors.resize( num_sn);
    _subnode_counterparts.resize( num_sn);
  }
  
  //! Allocate memory space for the arrays for storing the subdivision.
  void allocate_subfaces( const std::vector< int> &cnts) {
    int nf = size_of_faces();  RFC_assertion( nf == int(cnts.size()));
    _subface_offsets.resize( nf+1);
    _subface_offsets[0] = 0;
    for ( int i=1; i<=nf; ++i) {
      _subface_offsets[i] = _subface_offsets[i-1]+cnts[i-1];
    }
    
    int nsf=_subface_offsets[nf];
    _subfaces.resize( nsf);
    _subface_parents.resize( nsf);
    _subface_counterparts.resize( nsf);
  }

  //! Insert all the infomation related to a subface into the database.
  void insert_subface( int idx, int plid, const int *lids, 
		       const Edge_ID *eids, const Point_2 *nc,
		       int rp_id, int cnt, const int *rids);

protected:
  //=========================================================
  // The following functions are for constructing the internal 
  // data structure of RFC_Pane.
  //=========================================================
  // Construct the HDS data structure.  
  void build_hds();

  // Evaluating normals for all vertices within a pane.
  void evaluate_normals();

  // Determing counterparts of local border halfedges.
  void determine_counterparts();

  void unmark_alledges( );

  //=========== Functions for supporting feature detection
  void mark_ridge( Halfedge *h);

  const float &get_cos_face_angle( const Halfedge *h) const
  { return _fd_1[ get_index( h)>>1]; }
  float &get_cos_face_angle( Halfedge *h)
  { return _fd_1[ get_index( h)>>1]; }
  void init_face_angle() 
  { _fd_1.clear(); _fd_1.resize( _hds.size_of_halfedges()>>1, HUGE_VALF); }

  const float& get_angle_defect( const Vertex *v) const 
  { return _ad_0[ get_index( v)]; }
  float& get_angle_defect( Vertex *v)
  { return _ad_0[ get_index( v)]; }
  void init_angle_defect() 
  { _ad_0.clear(); _ad_0.resize( _hds.size_of_vertices(), HUGE_VALF); }

  const float& get_cos_edge_angle( const Vertex *v) const 
  { return _ea_0[ get_index( v)]; }
  float& get_cos_edge_angle( Vertex *v) 
  { return _ea_0[ get_index( v)]; }
  void reset_cos_edge_angle( Vertex *v) 
  { _ea_0[ get_index( v)] = HUGE_VALF; }
  void init_edge_angle() 
  { _ea_0.clear(); _ea_0.resize( _hds.size_of_vertices(), HUGE_VALF); }

  void set_strong_edge( Halfedge *h) { _is_f_1[ get_index(h)] = true; }
  void unset_strong_edge( Halfedge *h) { _is_f_1[ get_index(h)] = false; }
  void set_feature_0( Vertex *v)   { _is_f_0[ get_index(v)] = true; }
  void unset_feature_0( Vertex *v) { _is_f_0[ get_index(v)] = false; }
  void set_on_feature( Vertex *v)  { _is_on_f[ get_index(v)] = true; }
  void unset_on_feature( Vertex *v)  { _is_on_f[ get_index(v)] = false; }

  //=========== Functions for supporting the overlay algorithm
  void create_overlay_data();
  void delete_overlay_data();

  void construct_bvpair2edge();
  int get_border_index( const Halfedge *h) const {
    const Halfedge *t = &*hds().border_halfedges_begin();
    int i = h - t; RFC_assertion(i>=0 && (i&1)==1);
    return i>>1;
  }

protected:
  // Data member 
  HDS                                   _hds;
  RFC_Window_overlay                   *_window;    // Point to parent window.

  std::map< std::pair<int,int>, Halfedge*>  _bv2edges;
  std::vector<Halfedge*>  _cntrs;    // Counterparts
  std::vector<Vertex*>    _primaries;  // Primaries of border vertices

  // Extra attributes for the normals of the vertices.
  std::vector<Vector_3>  _nrmls;    // Normals.

  std::vector<Vector_3>  _f_nrmls;  // Normals for vertices on 1-features
                                     // Each entry coorresponds to a halfedge
                                     // on 1-features, storing the 
                                     // normal/binormal of its destination.
  std::vector< int>       _f_n_index;// Indices for the halfedges in _f_nrmls.


  std::vector< Vector_3> _f_tngts;
  std::vector< int>       _f_t_index; // Map fron halfedges to indices of _f_b_index

  // Extra data members for features
  std::vector< float>    _ad_0;     // Angle defect at vertices
  std::vector< float>    _ea_0;     // Edge angle at vertices
  std::vector< float>    _fd_1;     // Face angle at edges

  std::vector< bool>      _is_f_0;
  std::vector< bool>      _is_on_f;
  std::vector< bool>      _is_f_1;

  // Data for supporting overlay algorithm.
  std::vector<INode*>     _v_nodes;     // INodes at vertices
  std::vector<INode*>     _e_node_buf;  // INode buffer for edges
  std::vector<INode_list> _e_node_list; // INodes on edges
  std::vector<int>        _e_marks;     // Marks for edges

  int                           _size_of_subfaces;
  std::vector< const INode*>    _subnodes;
  static HDS_accessor<Tag_true>   acc;
};

/// A window is a collection of panes.
class RFC_Window_overlay : public RFC_Window_derived<RFC_Pane_overlay> {
public:
  typedef RFC_Window_overlay                        Self;
  typedef RFC_Window_derived<RFC_Pane_overlay>      Base;
  typedef Vertex_overlay                            Vertex;
  typedef Halfedge_overlay                          Halfedge;
  typedef Facet_overlay                             Facet;

  RFC_Window_overlay( COM::Window *b, int color, const char *pre=NULL);
  virtual ~RFC_Window_overlay();

  /** @defgroup pio Public Interface for Overlay
   * @{
   */
  bool is_same_node( Vertex *v1, Vertex *v2);
  void create_overlay_data();
  void delete_overlay_data();

  void determine_counterparts();
  void unmark_alledges();

  const  Halfedge *get_an_unmarked_halfedge() const;
  Halfedge *get_an_unmarked_halfedge();
  /** @} end of pio */

  /** @defgroup ntc Normal and Tangent Computation
   * @{
   */
public:
  void evaluate_normals();

  void reduce_coordinates_to_all();

  void print_features();
protected:
  void reduce_normals_to_all( MPI_Op);

  Vector_3 get_tangent( const Halfedge *h)
  { return h->destination()->point()-h->origin()->point(); }
  /** @} end of ntc */

public:
  /** @defgroup pifd Public Interface for Feature Detection
   * @{
   */
  class Feature_0 {
  public:
    Feature_0() : _v(0) {}
    explicit Feature_0( Vertex *v) : _v(v) {}
    const Point_3 &point() const { return _v->point(); }
    Vertex *vertex() { return _v; }
    const Vertex *vertex() const { return _v; }
//    bool operator<( const Feature_0 &f) {
    bool operator<( const Feature_0 &f) const {
      return _v->point()<f._v->point();
    }
  private:
    Vertex  *_v;
  };
  struct Feature_1 : public std::list<Halfedge*> {
    typedef std::list<Halfedge*>      Base;
    Feature_1() {}
    template <class Iter>
    Feature_1( Iter it1, Iter it2) : Base(it1, it2) {}
    Bbox_3   bbox;
  };
  typedef std::list< Feature_0>             Feature_list_0;
  typedef std::list< Feature_1>             Feature_list_1;

  void detect_features();
  bool snap_on_features() const { return _snap_on_features; }
  Feature_list_0 &flist_0() { return _f_list_0; }
  Feature_list_1 &flist_1() { return _f_list_1; }
  const Feature_list_0 &flist_0() const { return _f_list_0; }
  const Feature_list_1 &flist_1() const { return _f_list_1; }
  /** @} end of pifd */

  /** @addtogroup fdp Feature-Detection Primitives 
   * @{
   */
protected:
  float comp_angle_defect( Vertex *v);
  float cos_face_angle( Halfedge *h, Halfedge *hopp);
  float cos_edge_angle( const Halfedge *h1, const Halfedge *h2);
  float cos_edge_angle( const Feature_1 &f1, Feature_1::const_iterator it,
			bool isloop);
  bool  is_strong_ad( Vertex *v);
  bool  is_rstrong_ea( const Feature_1 &f1, 
		       Feature_1::const_iterator hprev, 
		       Feature_1::const_iterator hnext, 
		       float cos_ea, bool isloop);
  /** @} end of fdp */

  /** @addtogroup mf1 Manipulation of Strong Curves 
   * @{
   */
protected:
  bool check_false_strong_1( Feature_1 &);
  void subdiv_feature_curve( const Feature_1 &f1, 
			     Feature_list_1 &new_flist, int &dropped);
  void merge_features_1( Vertex *v, Feature_1 &f1, Feature_1 &f2);
public:
  void remove_feature_1( Feature_1 &f);
  /** @} end of mf1 */

  /** @addtogroup mf0 Manipulation of Strong Vertices
   * @{
   */
protected:
  void identify_features_0();
public:
  Feature_list_0::iterator remove_feature_0( Feature_list_0::iterator i);
  /** @} end of mf0 */

  /** @addtogroup fdh Feature-Detection Helpers
   * @{
   */
private:
  void init_feature_parameters();
  void set_feature_0( Vertex *v) const 
  { v->pane()->set_feature_0( v); }
  void set_on_feature( Vertex *v) const 
  { v->pane()->set_on_feature( v); }

  bool is_feature_0( const Vertex *v) const 
  { return v->pane()->is_feature_0(v); }
  bool is_on_feature( const Vertex *v) const 
  { return v->pane()->is_on_feature(v); }
  bool is_feature_1( const Halfedge *h) const 
  { return h->vertex()->pane()->is_feature_1(h); }
  void unset_strong_edge( Halfedge *h) const 
  { h->vertex()->pane()->unset_strong_edge(h); }
  /** @} end of fdh */

  /** @addtogroup io Output Routines
   * @{
   */
  void dump_strong_edges(const std::vector< std::pair<float, Halfedge*> > &,
			 const std::vector< std::pair<float, Halfedge*> > &);

  /** @} end of io */

  /** Miscellaneous Helpers */ 
  void normalize( Vector_3 &v)
  { if (v != Vector_3(0,0,0)) v = v / std::sqrt( v*v); }
private:
  float                 _cos_uf, _cos_lf, _rf, _cos_weakend;
  float                 _ud, _ld, _rd;
  float                 _cos_ue, _cos_le, _re;
  int                   _min_1f_len;
  int                   verb;

  std::string           out_pre;
  Feature_list_0        _f_list_0;
  Feature_list_1        _f_list_1;
  std::map<Vertex*,int> _f0_ranks;
  bool                  _long_falseness_check;
  bool                  _strong_ended;
  // Whether to snap blue features onto green features
  bool                  _snap_on_features;

  static const float              r2d;
  static HDS_accessor<Tag_true>   acc;
};

RFC_END_NAME_SPACE

#endif






