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
// $Id: RFC_Window_base.h,v 1.30 2008/12/06 08:43:26 mtcampbe Exp $

//===============================================================
// This file defines RFC_Window_base and RFC_Pane_base. The class
//      RFC_Window_overlay, RFC_Pane_overlay, RFC_Window_transfer, 
//      and RFC_Pane_transfer are implemented based on these two.
// Author: Xiangmin Jiao
// Date:   06/14/2001
//===============================================================

#ifndef RFC_WINDOW_BASE_H
#define RFC_WINDOW_BASE_H

#include "rfc_basic.h"
#include "roccom_devel.h"
#include "Tuple.h"
#include "Vector_n.h"
#include <set>
#include <map>
#include <fstream>
#include "../Rocmap/include/Pane_communicator.h"
#include "Element_accessors.h"

RFC_BEGIN_NAME_SPACE

//! A global ID of a node.
/*! 
  \related RFC_Pane_base
  Each node is identified by a pair of integers: the pane id of its
  owner, and its local id within the pane.
 */
struct Node_ID {
  //! A dummy constructor.
  Node_ID() {}
  //! Constructor from two integers.
  /*! 
    \param p the id of the owner pane.
    \param v the local id within the pane.
  */
  Node_ID( int p, int v) : pane_id( p), node_id( v) {}
  int pane_id;        //!< the id of the owner pane.
  int node_id;      //!< the local id within the pane starting from 1.

  //! Equal operator.
  bool operator==( const Node_ID &v) const 
  { return pane_id == v.pane_id && node_id == v.node_id; }
  //! Less-than operator.
  bool operator<( const Node_ID &v) const {
    return (pane_id < v.pane_id ||
	    pane_id == v.pane_id && node_id < v.node_id); 
  }
};
  
//! A global ID of a face.
/*! 
  \related RFC_Pane_base
  Each face is identified by a pair of integers: the pane id of its
  owner, and its local id of within the pane.
 */
struct Face_ID { 
  //! A dummy constructor.
  Face_ID() {}
  //! Constructor from two integers.
  /*! 
    \param p the id of the owner pane.
    \param f the local id within the pane.
  */
  Face_ID( int p, int f) : pane_id( p), face_id( f) {}

  int pane_id;        //!< the id of the owner pane.
  int face_id;        //!< the local id within the pane starting from 1.

  //! Equal operator.
  bool operator==( const Face_ID &v) const 
  { return pane_id == v.pane_id && face_id == v.face_id; }
  //! Less-than operator.
  bool operator<( const Face_ID &v) const {
    return (pane_id < v.pane_id ||
	    pane_id == v.pane_id && face_id < v.face_id); 
  }
};

class RFC_Window_base;
using MAP::Element_node_enumerator;

template < class _Pane> class RFC_Window_derived;
//! The base implementation of RFC_Pane.
/*! 
  RFC_Pane_base is built on top of COM::Pane, which encapsulates the 
  nodal coordinates, element connectivity, and attributes of a pane.
  RFC_Pane_base contains a reference to a COM::Pane object, and also
  stores a subdivision of a pane, and the pane connectivities for
  the nodes in the input mesh. Each node and each face in the 
  subdivision has a counterpart in another window, and RFC_Pane_base 
  also stores the ids of counterparts.

  If a node is a border node, it can have multiple instances, and
  the one with the smallest global id among non-isolated-node 
  instances is the primary copy.

  For each pair of adjacent panes, we define a numbering system for 
  their coincident nodes, which are numbered from 0 to the number 
  of coincident nodes minus one. We refer to these ids as 
  boundary ids. The number system in the adjacent pane is said to define
  the remote boundary ids corresponding to this pane.
  
  We use two data structures to store pane connectivity information:
  The first one, V2b_table, stores mapping from node ids (within this 
  pane) to the remote boundary ids of their primary copoes. V2b 
  is a mapping from node ids to a pair <pane id, REMOTE boundary id>. 
  The second data structure is B2v_table, which is a mapping from pane 
  ids to B2v objects, where a B2v object is a dynamic
  array each of whose entries contains a local node id and whose
  indices correspond to the local boundary ids for these nodes. 
  Therefore, a B2v objects contains a mapping from local boundary ids 
  to local node ids for a specific adjacent pane, and there is a B2v
  object for every adjacent pane. Note that it is possible that a pane
  is adjacent to itself in the case where a "branch-cut" exists, and 
  in this case V2b_table can contain an entry for the pane itself.
  
  To find the node id of the primary copy for a local node, one
  must use the local V2b_table and the remote B2v_table. For
  example, to locate the node id in pane p for a local node v,
  one could use 
  pane(v2b_table[v].first).b2v_table[my_pane_id][v2b_table[v].second].
  One should, however, check that v2b_table.find(v)!=v2b_table.end()
  and v2b_table[v].find(p.id())!=v2b_table[v].end().
  
  \sa RFC_Window_base RFC_Window_derived
*/
class RFC_Pane_base {
public:
  typedef RFC_Pane_base                                  Self;
  typedef COM::Pane                                      Base;
  typedef COM::Attribute                                 Attribute;
  typedef Base::Size                                     Size;
  typedef RFC::Real                                      Real;

  friend class RFC_Window_base;
  template <class _Pane> friend class RFC_Window_derived;

  //! A local ID of an edge.
  /*! Each edge is identified by a pair of integers: the local face id and
    a unique id of the edge within a face.
  */
  struct Edge_ID { 
    //! A dummy constructor.
    Edge_ID() {}
    //! Constructor from two integers.
    /*! 
      \param f local face id.
      \param e edge id within the pane
    */
    Edge_ID( int f, int e) : face_id( f), edge_id(e) {}
 
    int face_id;    //!< local face id.
    int edge_id;    //!< edge id within the face.
    
    //! Less-than operator.
    bool operator<( const Edge_ID &v) const {
      return (face_id < v.face_id ||
	      face_id == v.face_id && edge_id < v.edge_id); 
    }
  };

  // Pane connectivity informaiton.
  typedef std::pair<int,int>  V2b;
                           //!< Pane id and local boundary id.
  typedef std::map<int, V2b>  V2b_table;
                           //!< From node id to its corresponding map.
  typedef std::vector<int>    B2v; 
                           //!< From local boundary ids to node ids
  typedef std::map<int, B2v>  B2v_table;
                           //!< From pane id to its corresponding mapping.

  //! A constructor
  RFC_Pane_base( Base *b, int color);
  void init();

  //! The default destructor
  virtual ~RFC_Pane_base() {}

  //! The id of its base COM::Pane object.
  Base *base() { return _base; }
  const Base *base() const { return _base; }
  int id() const { return _base->id(); }

  //! Is this pane a master copy?
  bool is_master()    const { return _is_master; }
  //! Is this pane not a master copy?
  bool is_replicate() const { return !_is_master; }
  //! Does this pane contain quadratic elements?
  bool is_quadratic() const { return _quadratic; }
  //! Is a give node on the boundary of the pane?
  bool is_border_node( int i) const 
  { RFC_assertion( i>=1 && i<= size_of_nodes()); return _is_border[i-1]; }
  //! Is a give node an isolated node not belong to any element?
  bool is_isolated_node( int i) const 
  { RFC_assertion( i>=1 && i<= size_of_nodes()); return _is_isolated[i-1]; }

  //! Report whether the two given nodes are coincident.
  bool coincident( int i, int j) const;

  //! The total number of nodes in the pane.
  int size_of_nodes() const { return _base->size_of_nodes(); }
  //! The total number of faces in the pane.
  int size_of_faces()    const { return _base->size_of_elements(); }

  // ! Obtain the number of isolated nodes in the pane
  int size_of_isolated_nodes() const { return _n_isolated; }
  // ! Obtain the number of border nodes in the pane
  int size_of_border_nodes() const { return _n_border; }

  //! Get the edge id within a given face.
  Edge_ID get_edge_id( const int face_lid, const int vertex_lid) const;

  //! The total number of nodes in the subdivision of the pane.
  int size_of_subnodes() const { return _subnode_parents.size(); }
  //! The total number of faces in the subdivision of the pane.
  int size_of_subfaces() const  { return _subfaces.size(); }

  //! Get the address of a given attribute with id i.
  Real *pointer( int i)
  { return (Real*)_base->attribute(i)->pointer(); }
  //! \overload
  const Real *pointer( int i) const
  { return (Real*)_base->attribute(i)->pointer(); }

  //! Is the node with given local id a primary one?
  bool is_primary_node( const int vid) const;

  //! Get total number of primary nodes contained in the pane.
  int size_of_primary_nodes() const;

  //! Get the physical coordinates of the node with given local id.
  const Point_3 &get_point( int id) const {
    RFC_assertion( sizeof( Point_3) == 3*sizeof( Real));
    return *reinterpret_cast<const Point_3*>(_base->coordinates()+3*(id-1));
  }
  const Point_3 &get_point( int id, int) const {
    RFC_assertion( sizeof( Point_3) == 3*sizeof( Real));
    return *reinterpret_cast<const Point_3*>(_base->coordinates()+3*id);
  }
  const Real *coordinates() const { return _base->coordinates(); }
  Point_3 get_point_of_subnode( int id) const;

  //! Get the bounding box of the pane.
  Bbox_3 get_bounding_box() const;

  //! The color of the parent window (BLUE or GREEN).
  int color() const { return _color; }

  //! Determine the parent type of a subnode of given tyep.
  int parent_type_of_subnode( int) const;

  //! Get the local parent node id of a given subnode.
  int get_parent_node( int) const;

  //! Get the local parent face id of a given subface.
  int get_parent_face( int id) const { return _subface_parents[ id-1]; }

  /*! Normailize the natural coordinates.
   * \param idx the index of an edge of an element.
   *  \param e the number of edges fo an element
   *  \param nc the natural of a point in the element which where the edge
   *            idx is its first edge.
   */
  template <class Point>
  void normalize_nat_coor( int idx, int e, Point &nc) const {
    if( e == 3) {
      switch( idx) {
      case 0: return; 
      case 1: nc = Point( 1.-nc[0]-nc[1], nc[0]); return;
      case 2: nc = Point( nc[1], 1.-nc[0]-nc[1]); return;
      default: RFC_assertion( false);
      }
    }
    else {
      switch( idx) {
      case 0: return; 
      case 1: nc = Point( 1-nc[1], nc[0]); return;
      case 2: nc = Point( 1-nc[0], 1-nc[1]); return; 
      case 3: nc = Point( nc[1], 1-nc[0]); return;
      default: RFC_assertion( false);
      }
    }
  }

  void
  get_host_element_of_subnode( int i, Element_node_enumerator &ene,
			       Point_2 &nc) const {
    int f=_subnode_parents[i-1].face_id;
    if ( ene.pane() != _base || ene.id() != f) 
      ene = Element_node_enumerator( _base, f);
    const Point_2S &p=_subnode_normalized_nc[i-1]; 
    nc[0]=p[0]; nc[1]=p[1];
  }

  void
  get_host_element_of_subface( int i, Element_node_enumerator &ene) const {
    int f=_subface_parents[i-1];
    if ( ene.pane() != _base || ene.id() != f) 
      ene = Element_node_enumerator( _base, f);
  }

  //! Take a subface id and a local subnode id, return the natual 
  //! coordinates of the subnode within the parent element.
  void get_nat_coor_in_element( const int eid, const int lid, Point_2 &nc) const {
    const Point_2S &p=_subface_nat_coors[eid-1][lid];
    nc[0] = p[0]; nc[1]=p[1];
  }

  //! Take a subnode id and an element, return the natrual coordinates
  //! within the element.
  void get_nat_coor_in_element( const int sn_id, 
				Element_node_enumerator &ene, Point_2 &nc) const;

  //! Get the connectivity object and the id within 
  //! the connectivity object for a given element.
  std::pair< const COM::Connectivity*, int> get_element( int face_id) const {
    const int* p = std::lower_bound( &*_elem_offsets.begin(),
				     &*_elem_offsets.end(), face_id)-1;
    return std::make_pair( _elem_conns[p-&_elem_offsets[0]],
			   face_id-_elem_offsets[*p]);
  }
  const Node_ID &get_subnode_counterpart( int i) const { 
    RFC_assertion( i>=1 && i<=(int)_subnode_counterparts.size());
    return _subnode_counterparts[i-1];
  }
  const Face_ID &get_subface_counterpart( int i) const { 
    RFC_assertion( i>=1 && i<=(int)_subface_counterparts.size());
    return _subface_counterparts[i-1];
  }

  int get_lvid( const Element_node_enumerator &ene, 
		const int v) const;

protected:
  //! Compute the natural coordinates.
  void comp_nat_coors();

  //! Build pane connectivity.
  void build_v2b_from_b2v( const RFC_Window_base *w);

protected:
  // Used by read_binary and read_rocin to create a pane
  class COM_Pane_friend : protected COM::Pane {
  protected:
    COM_Pane_friend();
  public:
    using COM::Pane::attribute;
    using COM::Pane::connectivity;
    using COM::Pane::set_size;
    using COM::Pane::reinit_conn;
  };

  //=========== IO Functions
  void write_tec_ij( std::ostream &, const COM::Attribute *a=0) const;
  void write_tec_tri( std::ostream &, const COM::Connectivity &,
		      const COM::Attribute *a=0) const;
  void write_tec_mixed( std::ostream&, 
			const std::vector< const COM::Connectivity*>&,
			const COM::Attribute *a=0) const;
  void write_tec_sub( std::ostream &) const;
  void write_tec_ascii( std::ostream &os, const COM::Attribute *attr=0) const;

  // Write in native binary format
  void write_binary( std::ostream &os) const;

  // Read in native binary format.
  void read_binary( std::istream &is, 
		    std::vector <int> *b2v_all=NULL, COM::Pane *p=NULL);
  
  // Register the attributes for the pane for output using Rocout.
  void register_sdv_attributes( const std::string &wname) const;
  
  /** Read in using Rocin.
   * \param sdv_wname subdivision window name
   * \param parent_wname parent window name
   * \param p parent pane ID base */
  void read_rocin( const std::string &sdv_wname, 
		   const std::string &parent_wname="", 
		   COM::Pane *p=NULL);

protected:
  // Data members
  Base                             *_base;      //!< Reference to its base object.
  bool                              _is_master; //!< Is the pane a master copy?
  bool                              _quadratic; //!< Does it contain quadratic elements?

  std::vector< bool>                _is_border;  //!< Is a node on border?
  std::vector< bool>                _is_isolated; //!< Is a node isolated?
  int                               _n_border, _n_isolated;

  V2b_table                         _v2b_table;  //!< \sa V2b_table
  B2v_table                         _b2v_table;  //!< \sa B2v_table

  // Information about the subnodes contained in the pane
  std::vector< Edge_ID>             _subnode_parents;
                        //!< Edge ids of parents.
  std::vector< Point_2S>            _subnode_nat_coors;
                        //!< Natual coordinates in the parent face.
  std::vector< Point_2S >           _subnode_normalized_nc;
                        //!< Natual coordinates in the parent face.
  std::vector< int>                 _subnode_subID;
                        //!< Sub-node ID of nodes in the pane.

  std::vector< Node_ID>             _subnode_counterparts;   
                        //!< Ids of counterparts of subnodes.

  // Information about the subfaces contained in the pane
  std::vector< Three_tuple<int> >   _subfaces;

  std::vector< int >                _subface_parents;
                        //!< Face ids of the parents of the subfaces.
  std::vector< int>                  _subface_offsets;
                        //!< Offsets of first subfaces contained in a face.
  std::vector< Face_ID>             _subface_counterparts;
                        //!< Ids of counterparts of faces.

  // Natural coordiates of the subnodes of a subface in its parent face
  std::vector< Three_tuple<Point_2S> >   _subface_nat_coors;
                        //!< Element connectivity of the subfaces.
private:
  int                                    _color;     //!< Is a node on border?

  std::vector< const COM::Connectivity*> _elem_conns;
  std::vector< int>                      _elem_offsets;

private:
  // Disable the following functions.
  RFC_Pane_base();
  RFC_Pane_base( const Self&);
  Self &operator=( const Self&);
};

//================ Filed data access operators ===================
//! An const adaptor for accessing nodal coordinates of a pane.
/*! \sa RFC_Pane_base */
class Nodal_coor_const {
public:
  typedef Point_3               Value;              //!<
  typedef Point_3               Value_nonconst;     //!<
  typedef const Point_3         Value_const;        //!<
  typedef unsigned int          Size;               //!<

  //! Get the coordinates of a point in \p p with local id \i.
  Value_const& get_value( const RFC_Pane_base *p, int i) const {
    return p->get_point(i);
  }
  //! Get the coordinates of a point in \p p with local id \i.
  Value_const& get_value( const Real *p, int i) const {
    return *reinterpret_cast<Value_const*>(p+3*(i-1));
  }
};


// Adpator for element-wise data container. 
//  This wrapper provides an interface to access data through the
//  element-wise local indices.
template < class _Cont, class _Enum>
class Field {
public:
  typedef typename _Cont::Value          Value;
  typedef typename _Cont::Value_nonconst Value_nonconst;
  typedef typename _Cont::Value_const    Value_const;
  typedef unsigned int                   Size;
private:
  _Cont             &_cont;
  Real              *_p;
  const _Enum       &_enum;
public:
  Field( _Cont & cont, Real *p, const _Enum &ids) 
    : _cont(cont), _p(p), _enum(ids) {}
  Size dimension() const { return _cont.dimension(); }
  Value_const operator[]( int i) const
  { return _cont.get_value(_p, _enum[i]); }
  Value_nonconst operator[]( int i)
  { return _cont.get_value(_p, _enum[i]); }
};

template < class _Cont, class _Enum>
class Field< const _Cont, _Enum> {
public:
  typedef typename _Cont::Value          Value;
  typedef typename _Cont::Value_nonconst Value_nonconst;
  typedef typename _Cont::Value_const    Value_const;
  typedef unsigned int                   Size;
private:
  const _Cont             &_cont;
  const Real              *_p;
  const _Enum             &_enum;
public:
  Field( const _Cont & cont, const Real *p, const _Enum &ids) 
    : _cont(cont), _p(p), _enum(ids) {}
  Size dimension() const { return _cont.dimension(); }
  Value_const operator[]( int i) const
  { return _cont.get_value(_p, _enum[i]); }
};

//! A window is a collection of panes.
/*!
  \sa RFC_Pane_base
*/
class RFC_Window_base {
public:
  typedef RFC_Window_base                  Self;
  typedef COM::Window                      Base;
  typedef COM::Attribute                   Attribute;
  typedef RFC_Pane_base                    Pane;
  typedef std::map< int, Pane*>            Pane_set;

  //! Construct an object with base \p b and color \p c. \sa init
  RFC_Window_base( Base *b, int c, MPI_Comm);

  //! Default destructor.
  virtual ~RFC_Window_base();

  //! The name of the window.
  std::string name() const { return _base->name(); }

  const RFC_Pane_base & pane( const int pid) const {
    Pane_set::const_iterator pit = _pane_set.find( pid);
    RFC_assertion( pit != _pane_set.end());
    return *pit->second;
  }

  RFC_Pane_base &pane( const int pid) {
    Pane_set::iterator pit = _pane_set.find( pid);
    RFC_assertion( pit != _pane_set.end());
    return *pit->second;
  }

  //! Get a reference to the base COM::Window object.
  const COM::Window *base() const { return _base; }

  //! The color of the window for overlay or for data transfer (BLUE or GREEN).
  int color() const { return _color; }

  //! Get the total number of nodes contained the window. The coincident
  //! nodes are counted more than once.
  int size_of_nodes() const;

  //! Get the total number of faces contained the window.
  int size_of_faces() const;

  //! Get the total number of nodes contained the window. The coincident
  //! nodes are counted once.
  int size_of_primary_nodes() const;

  //! Get the bounding box of the window.
  Bbox_3 get_bounding_box() const;
  
  //! Number of local panes in the window.
  int size_of_panes() const { return _pane_set.size(); }

  //! Retrieve an attribute object from the base using the attribute name.
  const Attribute *attribute( const char *f) const 
  { return _base->attribute(f); }

  void export_window( RFC_Window_base *) const;

  //! Ouptut a mesh in Tecplot format.
  void write_tec_ascii( const char *prefix) const;
  //! Ouptut a subdivision of a mesh in Tecplot format.
  void write_tec_sub( const char *prefix) const;

  //! New attributes 
  void new_sdv_attributes( const std::string &wname) const;

  //! Read in local panes in native binary format or using Rocin.
  void read_sdv( const char *prefix, const char *format=NULL);

  //! Write out panes in native binary format or using Rocout.
  void write_sdv( const char* prefix, const char *format=NULL) const;

  //! Build the pane connectivity table.
  void build_pc_tables();

protected:
  enum { SDV_BINARY, SDV_HDF, SDV_CGNS};

  // Obtain the file name for a specific pane.
  static std::string get_sdv_fname( const char *prefix, int pane_id, 
				    const int format=SDV_BINARY);

  // Get the basename of the prefix without the directory part and suffix.
  static const char *get_prefix_base( const char *prefix);

  // Convert from string into the code.
  static int get_sdv_format( const char *format);

  // Prefix to be added in front of buffer windows.
  static const char *_bufwin_prefix;

protected:  // Data members
  Base           *_base;       //!< A reference to its base COM::Window object.
  Pane_set        _pane_set;   //!< The set of panes contained in the window.
  int             _verbose;
  int             _color;

  MAP::Pane_communicator  _map_comm;

private:
  // Disable the following functions.
  RFC_Window_base();
  RFC_Window_base(const Self&);
  Self &operator=( const Self&);
};

//! Reusable implementation for derived class of RFC_Window_base.
/*! This class provides some template implementation for some useful
 *    functions that depends on the specific type of the panes. One
 *    usually should derive subclasses of RFC_Window_base by inherit
 *    from this class, but it is not required.
 */
template <class _Pane>
class RFC_Window_derived : public RFC_Window_base {
protected:
  typedef RFC_Window_derived<_Pane>               Self;
  typedef RFC_Window_base                         Base;
  typedef _Pane                                   Pane;

  //! A constructor.
  RFC_Window_derived( COM::Window *b, int c, MPI_Comm comm) : Base(b,c, comm) 
  { init(); }
  //! A destructor.
  virtual ~RFC_Window_derived();
  void init();

public:
  //! Get a reference to the pane with give id \p pid.
  const Pane &pane( const int pid) const;
  //! \overload
  Pane &pane( const int pid);

  //! Get a handle to the primary copy of a node contained in the pane \p p with local id \p vid.
  std::pair< Pane*, int> get_primary( Pane *p, int vid);

  //! \overload
  std::pair< const Pane*, int> get_primary( const Pane *p, int vid) const;

  //! Get a vector of local panes contained in the window.
  void panes( std::vector<Pane*> &ps);
  //! \overload
  void panes( std::vector<const Pane*> &ps) const;
};

template < class _Pane>
RFC_Window_derived<_Pane>::~RFC_Window_derived() {}

template < class _Pane>
void
RFC_Window_derived<_Pane>::init() {
  std::vector< COM::Pane*>  ps;
  _base->panes( ps);   

  for (std::vector< COM::Pane*>::const_iterator 
	 it=ps.begin(); it!=ps.end(); ++it) {
    // Insert a pane with given id
    (_pane_set[(*it)->id()] = new Pane(*it, color()))->init();
  }
}

template < class _Pane>
const _Pane &
RFC_Window_derived<_Pane>::pane( const int pid) const {
  Pane_set::const_iterator pit = _pane_set.find( pid);
  RFC_assertion( pit != _pane_set.end());
  return reinterpret_cast<const _Pane&>(*pit->second);
}

template < class _Pane>
_Pane &
RFC_Window_derived<_Pane>::pane( const int pid) {
  Pane_set::iterator pit = _pane_set.find( pid);
  RFC_assertion( pit != _pane_set.end());
  return reinterpret_cast<_Pane&>(*pit->second);
}

template < class _Pane>
void 
RFC_Window_derived<_Pane>::panes( std::vector<Pane*> &ps) {
  const_cast<Self*>(this)->panes( (std::vector<const Pane*>&)ps);
}

template < class _Pane>
void 
RFC_Window_derived<_Pane>::panes( std::vector<const Pane*> &ps) const {
  ps.resize(0);
  ps.reserve( _pane_set.size());
  Pane_set::const_iterator it=_pane_set.begin();
  for ( ; it != _pane_set.end(); ++it) 
    ps.push_back( reinterpret_cast<const Pane*>(it->second));
}

template < class _Pane>
std::pair< _Pane*, int> 
RFC_Window_derived<_Pane>::get_primary( Pane *p, int vid) {
  RFC_Pane_base::V2b_table::iterator it;
  
  //  points to a border halfedge.
  if ( !p->is_border_node( vid) || 
       (it = p->_v2b_table.find( vid))==p->_v2b_table.end()) {
    RFC_assertion( !p->is_isolated_node( vid));
    return std::make_pair(p, vid);
  }

  RFC_Pane_base::V2b &v2b = it->second;

  Pane &p2 =  pane( v2b.first);
  Node_ID v2( v2b.first, p2._b2v_table.find(p->id())->second[v2b.second]);

  RFC_assertion( v2 < Node_ID( p->id(), vid) || p->is_isolated_node(vid));
  RFC_assertion( !p2.is_isolated_node( v2.node_id));
  return std::make_pair( &p2, v2.node_id);
}

template < class _Pane>
std::pair< const _Pane*, int> 
RFC_Window_derived<_Pane>::get_primary( const Pane *p, int vid) const {
  return const_cast<Self*>(this)->get_primary( const_cast<Pane*>(p), vid);
}

RFC_END_NAME_SPACE

#endif






