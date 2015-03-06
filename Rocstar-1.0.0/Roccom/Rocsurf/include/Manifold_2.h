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
// $Id: Manifold_2.h,v 1.18 2008/12/06 08:43:23 mtcampbe Exp $

//=======================================================================
//  This file defines the data structures for accessing a surface mesh.
//  The data structures mimic the halfedge data structure, but with the
//  extension to support partitioned meshes. Typically, the user should 
//  use Node and Halfedge to access the entities of a surface mesh.
//
//  Author: Xiangmin Jiao
//  Last modified: Oct 11, 2004
//=======================================================================

#ifndef __MANIFOLD_2_H_
#define __MANIFOLD_2_H_

#include "surfbasic.h"
#include "../Rocmap/include/Simple_manifold_2.h"

namespace MAP { class Pane_communicator; };

SURF_BEGIN_NAMESPACE

using MAP::Simple_manifold_2;

class Window_manifold_2;
class Halfedge;
class Node;
typedef MAP::Facet_ID Edge_ID;

enum Access_Mode { REAL_PANE, WHOLE_PANE, ACROSS_PANE };

/** This class implements a data structure for 2-manifold on each pane.
 *  It extends Simple_manifold_2 by adding data and function members
 *  to support partitioned meshes. */
class Pane_manifold_2 : public Simple_manifold_2 {
  friend class Window_manifold_2;
  friend class Node;
  friend class Halfedge;
public:
  /// Default constructors
  Pane_manifold_2();

  /// Constructor from a pane
  explicit Pane_manifold_2( const COM::Pane *p, 
			    const Pane_manifold_2 *parent=NULL);

  /// Destructor
  ~Pane_manifold_2();

  /** Get the counterpart of an edge in the ACROSS_PANE mode, i.e. its 
   *  correponding edge in an adjacent pane. If the edge is on the 
   *  physical boundary, then its counterpart is 
   *  Halfedge(NULL,Edge_ID(), ACROSS_PANE). */
  inline const Halfedge &get_counterpart( Edge_ID eID) const;

  /** Obtain normal of incident face of border edge. */
  inline const Vector_3<Real> &get_bd_normal( Edge_ID) const;

  /** Obtain flag of incident face of border edge. */
  inline int get_bd_flag( Edge_ID) const;

  /** Obtain flag of opposite halfedge of a border halfedge. */
  inline bool get_bdedge_flag( Edge_ID) const;

  /// Obtain the opposite edge in a given access mode
  inline Halfedge get_opposite_edge( Edge_ID, Access_Mode mode) const;

  /// Obtain the previous edge in a given access mode
  Halfedge get_prev_edge( Edge_ID, Access_Mode mode) const;

  /// Obtain the next edge in a given access mode
  Halfedge get_next_edge( Edge_ID, Access_Mode mode) const;

  /** Obtains a Node object corresponding to the primary copy of the given 
   *  node. If the mode is not ACROSS_PANE, then the primary copy is always
   *  the node itself. If the mode is ACROSS_PANE, then the incident edge of
   *  the primary copy of the node must be a border edge, and the primary copy
   *  of an interior node is the one with smallest ID among shared nodes. */
  inline Node get_primary( int vID, Access_Mode mode) const;

  /// Determines whether a given node is a primary copy.
  inline bool is_primary( int vID, Access_Mode mode) const;

  /// Is the edge on the boundary of a given mode?
  inline bool is_border_edge( Edge_ID eID, Access_Mode mode) const;

  /// Is the given edge a physical border edge?
  inline bool is_physical_border_edge( Edge_ID eID) const
  { return eID.is_border() && _cnt_pn[get_border_edgeID(eID)-1]==0; }

  /// Number of primary nodes of the pane for a given mode. In ACROSS_PANE 
  /// mode, the shared nodes will be counted only if they are primary copies.
  int size_of_nodes( Access_Mode mode) const;

  /// Number of primary edges of the pane in a given mode. In ACROSS_PANE
  /// mode, the shared edges will be counted only if they are primary copies.
  int size_of_edges( Access_Mode mode) const;

  /// Obtain the number of faces.
  int size_of_faces( Access_Mode mode) const {
    switch ( mode) {
    case REAL_PANE:
    case ACROSS_PANE: return size_of_real_faces(); 
    default: return Simple_manifold_2::size_of_faces();
    }
  }

  /// Obtain the number of triangles.
  int size_of_triangles( Access_Mode mode) const {
    switch ( mode) {
    case REAL_PANE:
    case ACROSS_PANE: return size_of_real_triangles(); 
    default: return Simple_manifold_2::size_of_triangles();
    }
  }

  /// Obtain the number of quadrilaterals.
  int size_of_quadrilaterals( Access_Mode mode) const {
    switch ( mode) {
    case REAL_PANE:
    case ACROSS_PANE: return size_of_real_quadrilaterals(); 
    default: return Simple_manifold_2::size_of_quadrilaterals();
    }
  }

protected:
  /// Convert from edge-based pconn to a ghost-node-style pconn
  /// for a pane attribute.
  void convert_pconn_edge2ghost( const COM::Attribute *pconn_e, 
				 COM::Attribute *pconn_g);


  /// Obtain the border node index (equal to ID of incident border edge). 
  /// Return -1 if not a border edge.
  int get_bnode_index( int vID) const {
    Edge_ID eID = get_incident_real_edge( vID);

    if ( !eID.is_border()) return -1;

    int bid = eID.eid()-1;
    if ( bid>=int(_beIDs.size())) return bid-_size_ghost_borders;
    else return bid; 
  }

  /* The following data memters are for storing the counterpart of each
   * edge on pane boundaries, for the across-pane access mode. */
  std::vector<int>             _cnt_pn;

  //< Stores pane-id of the counterparts of border edges.
  //< If owner pane is remote, then save the negative ID. If there is no
  //< counterpart (i.e., edge is on physical boundary), then save 0.

  std::vector<Halfedge>        _bd_cnt;   //< Counterpart edges in ACROSS_PANE mode
  std::vector<Vector_3<Real> > _bd_nrms;  //< Normal of opposite face
  std::vector<int >            _bd_flgs;  //< Flag of opposite face
  std::vector<char >           _bd_bm;    //< Bitmap of opposite edge
  std::vector<Node>            _nd_prm;   //< Primary nodes in ACROSS_PANE mode.
};

/** This class implements a data structure for 2-manifold over a whole
 *  window, which can be composed of multiple panes. It supports different
 *  modes in accessing the nodes and halfedges of the window. */
class Window_manifold_2 {
public:
  typedef std::vector< Pane_manifold_2>::iterator PM_iterator;
  typedef std::vector< Pane_manifold_2>::const_iterator PM_const_iterator;

  /// Default constructor
  Window_manifold_2() : _buf_window(NULL), _cc(NULL), _pconn_nb(0) {}

  /// Construct a Window_manifold_2 from a given window.
  explicit Window_manifold_2(const COM::Attribute *pmesh)
    : _buf_window(NULL), _cc(NULL), _pconn_nb(0) { init(pmesh); }

  ~Window_manifold_2();

  /** \name Buffer window management
   *  \{
   */
  /// Obtain the underlying window object.
  COM::Window *window() { return _buf_window; }
  const COM::Window *window() const { return _buf_window; }

  //\}

  /** \name Pane management
   *  \{
   */
  /// Obtain a Pane_manifold_2 from a pane ID
  Pane_manifold_2 *get_pane_manifold( int pid)
  { return &_pms[ _pi_map[pid]]; }

  /// Obtain a const Pane_manifold_2 from a pane ID
  const Pane_manifold_2 *get_pane_manifold( int pid) const
  { return &_pms[ _pi_map.find(pid)->second]; }

  /// Obtain the number of panes.
  int size_of_panes() const { return _pms.size(); }

  /// Obtain an iterator to the first pane manifold of the window.
  PM_iterator pm_begin() { return _pms.begin(); }
  PM_const_iterator pm_begin() const { return _pms.begin(); }

  /// Obtain an iterator to the past-the-last pane manifold of the window.
  PM_iterator pm_end()   { return _pms.end(); }
  PM_const_iterator pm_end() const { return _pms.end(); }

  //\}

  /** \name Communication
   *  \{
   */
  /// Initialize a pane communicator. Must be called before any operation
  /// involving interprocess communication.
  void init_communicator();

  /// Perform reduction over a given nodal attributes.
  void reduce_on_shared_nodes( COM::Attribute *attr, MPI_Op op);

  //\}

  /** \name Utilities
   *  \{
   */
  /** Compute the normals at nodes or faces, depending on the type of the
   *  attribute normals. For nodal normals, use one of the following 
   *  weighting schemes:
   *  E2N_ONE:   Same weight for all incident faces.
   *  E2N_AREA:  Area of incident faces.
   *  E2N_ANGLE: Angle of the incident faces at the node.
   */
  void compute_normals( COM::Attribute *normals, 
			int scheme = E2N_ANGLE,
			bool to_normalize = true);
  
  /** Update the normals for border faces. */
  void update_bd_normals( const COM::Attribute *normals,
			  bool to_normalize = false);

  /** Update the flags for border faces. */
  void update_bd_flags( const COM::Attribute *flags);

  /** Update bitmap for border edges. */
  void update_bdedge_bitmap( const COM::Attribute *bitmap);

  /** Accumulate the values for border faces */
  void accumulate_bd_values( const COM::Attribute *vals);

  // Compute mean-curvature normals and their Laplace-Beltrami operator.
  void compute_mcn( COM::Attribute *mcn_in, COM::Attribute *lbmcn_in);

  /** Convert element values to nodal values using weighted averaging.
   *  The weighting scheme can be user-defined (E2N_USER, given by ews), 
   *  one for each element (E2N_ONE, default), area of the element (E2N_AREA),
   *  or angle at each node (E2N_ANGLE). 
   *  If nws is present, then output weights into nws.
   *  If tosum is true, then compute weighted sum instead of weighted average.
   */
  void elements_to_nodes( const COM::Attribute *evals,
			  COM::Attribute *nvals,
			  const int scheme = E2N_ONE,
			  const COM::Attribute *ews = NULL,
			  COM::Attribute *nws = NULL,
			  const int tosum=false);

  /// Obtain shortest edge length of incident edges of each node or element.
  void shortest_edge_length( COM::Attribute *lens);

  /// Perturb the given mesh along its normal direction by length equal
  /// to a random number between -range and range times shortest edge length
  void perturb_mesh( double range);
  //\}

  /// Obtain the number of nodes (shared nodes are counted only once
  int size_of_nodes( Access_Mode mode) const;

  /// Obtain the number of faces.
  int size_of_faces( Access_Mode mode) const;

  /// Obtain the number of triangles.
  int size_of_triangles( Access_Mode mode) const;

  /// Obtain the number of quadrilaterals.
  int size_of_quadrilaterals( Access_Mode mode) const;
  
  /// Obtain the number of edges.
  int size_of_edges( Access_Mode mode) const;

  /// Obtain the number of pconn blocks.
  int pconn_nblocks() const { return _pconn_nb; }

  /// Create a serial window (with single pane) from the current window
  void serialize_window( COM::Window *outwin) const;

protected:
  /** \name Helper
   */
  /// Initialize the manifold by inheriting the given mesh.
  /// Called by constructors of Window_manifold_2.
  void init( const COM::Attribute *pmesh);

  /// Determine the counterparts of pane-border edges for across-pane access.
  void determine_counterparts( const COM::Attribute *pconn_e);
  
  /// Determine the primary nodes for across-pane access.
  void determine_primaries( const COM::Attribute *pconn_n);

  /// Obtain shortest edge length of incident edges of each element.
  static void compute_shortest_edgelen_elements( COM::Attribute *lens);

  /// Obtain shortest edge length of incident edges of each nodes.
  void compute_shortest_edgelen_nodes( COM::Attribute *lens);

  /** Compute nodal normals using one of the following weighting schemes:
   *  E2N_ONE, E2N_AREA, and E2N_ANGLE, and E2N_USER. 
   *  \seealso compute_normals()
   */
  void compute_nodal_normals( COM::Attribute *nrm,
			      int scheme = E2N_ANGLE,
			      bool to_normalize = true,
			      const COM::Attribute *weights = NULL);

  // Assign nodal IDs. Called by serialize_window()
  void assign_global_nodeIDs( std::vector<std::vector<int> > &gids) const;

  //\}

  /** \name Data members
   */
public:
  static MPI_Op OP_MAX, OP_MIN, OP_SUM, OP_PROD, OP_MAXABS, OP_DIFF,
    OP_BOR, OP_BAND, OP_LOR, OP_LAND;

protected:
  // A buffer window created by the Manifold class by inheriting from
  // a user window when the init() function is called. in general, 
  // one must inherit a user attribute onto this window.
  COM::Window                     *_buf_window;
  std::vector< Pane_manifold_2>    _pms;
  // Create a mapping from pane ids to the corresponding indices in _pms
  std::map< int, int>              _pi_map;
  MAP::Pane_communicator          *_cc;       // Pane communicator.
  int                              _pconn_nb; // Number of blocks of pconn
  //\}
};

/** This class encapsulate a node over a window manifold. */
class Node {
public:
  /// Default constructor
  Node() : _pm(NULL), _vID(0), _mode( ACROSS_PANE) {}

  /// Construct a Node object from a pane manifold and its node ID
  Node( const Pane_manifold_2 *pm, int vid, Access_Mode mode) 
    : _pm( pm), _vID( vid), _mode( mode) {}

  /** Get an incident halfedge originated from the node. If the node is on
   *  the physical boundary, the halfedge must be a physical border edge. */
  inline Halfedge halfedge() const;

  /// Obtain the pane manifold that owns the edge.
  const Pane_manifold_2 *pane_manifold() const { return _pm; }

  /// Obtain the owner Pane_manifold_2 of the node
  const COM::Pane *pane() const 
  { if ( _pm) return _pm->pane(); else return NULL; }

  /// Obtain the node ID within its Pane_manifold_2.
  int  id() const { return _vID; }

  /// Obtain the coordinates of a node
  const Point_3<Real> &point() const 
  { return (const Point_3<Real>&)_pm->pane()->coordinates()[ 3*_vID-3]; }
  
  /// Obtain the address of an attribute associated with the node.
  /// Note: This function can be slow O(log(#panes)) if the 
  /// attribute is not on the current pane.
  const void *addr( const COM::Attribute *attr) const;

  /// Obtain the attribute on the parent pane of the node. It takes 
  /// O(log(#panes)) time.
  const COM::Attribute *attr( const COM::Attribute *a) const { 
    COM_assertion_msg( a, "Unexpected NULL pointer");
    return a->window()->pane( _pm->pane()->id()).attribute(a->id()); 
  }

  /// Is the node on the physical boundary?
  inline bool is_border() const;

  /// Is the node isolated?
  bool is_isolated() const { return _pm->is_isolated_node( _vID); }

  /// Is the node a primary?
  bool is_primary() const { return _pm->is_primary( _vID, _mode); }

  /// Get the primary copy of the node.
  Node get_primary() const { return _pm->get_primary( _vID, _mode); }

  /// Check whether the current node coincide with the given node.
  bool coincide( const Node &n) const
  { return _pm->get_primary( _vID, _mode) == n._pm->get_primary( n._vID, n._mode); }

  /// Are two nodes the same?
  bool operator==( const Node &n) const
  { return _pm == n._pm && _vID == n._vID; }

  /// Are two nodes different?
  bool operator!=( const Node &n) const
  { return _pm != n._pm || _vID != n._vID; }

  /// Does the current node has a smaller ID then the given node?
  bool operator<( const Node &n) const {
    return _pm->pane()->id() < n._pm->pane()->id() ||
      _pm == n._pm && _vID < n._vID; 
  }

protected:
  const Pane_manifold_2 *_pm;      // Reference to Pane_manifold_2
  int                    _vID;     // Node id
  Access_Mode            _mode;    // Access mode
};

/** This class encapsulate a halfedge over a window manifold. */
class Halfedge {
public:
  Halfedge() : _pm(NULL) {}
  /// Construct a Halfedge object from a pane manifold and its edge ID
  Halfedge( const Pane_manifold_2 *pm, Edge_ID e, Access_Mode mode) 
    : _pm( pm), _eID( e), _mode(mode) {}

  /// Get the ID of the opposite edge of a given edge
  Halfedge opposite() const 
  { return _pm->get_opposite_edge( _eID, _mode); }

  /// Get the previous halfedge of its owner element. If the current edge
  /// is a border edges, it returns the previous border edge along 
  /// the boundary.
  Halfedge prev() const { return _pm->get_prev_edge( _eID, _mode); }

  /// Get the next halfedge of its owner element. If the current edge
  /// is a border edges, it returns the next border edge along 
  /// the boundary.
  Halfedge next() const { return _pm->get_next_edge( _eID, _mode); }

  /// Obtain the (primary copy of the) origin of the edge
  Node   origin() const
  { return _pm->get_primary( _pm->get_origin( _eID), _mode); }

  /// Obtain the (primary copy of the) destination of the edge
  Node   destination() const
  { return _pm->get_primary( _pm->get_destination( _eID), _mode); }

  /// Is the edge a border edge?
  bool is_border() const 
  { return _pm->is_border_edge( _eID, _mode); }

  /// Is the edge on the physical boundary?
  bool is_physical_border() const 
  { return _pm->is_physical_border_edge( _eID); }

  /// Get the tangent of the given halfedge.
  Vector_3<Real> tangent() const
  { return destination().point()-origin().point(); }

  /// Get the normal of the incident facet.
  Vector_3<Real> normal() const;

  /// Get the normal of the deformed shape (coordinates plus given 
  /// displacements) of the incident facet of the halfedge.
  Vector_3<Real> deformed_normal( const COM::Attribute *disp) const;

  /// Get the edge center of the given halfedge.
  void edge_center( Point_3<Real> &mid) const 
  { edge_average( _pm->pane()->attribute( COM::COM_NC), 
		  reinterpret_cast<Vector_3<Real> &>(mid)); }

  /// Get the face center of the incident face of a given halfedge. 
  /// If this is a border halfedge, then return its midpoint.
  void face_center( Point_3<Real> &cnt) const 
  { face_average( _pm->pane()->attribute( COM::COM_NC), 
		  reinterpret_cast<Vector_3<Real> &>(cnt)); }

  /// Get the avarage value of the nodes of a given halfedge. Value must
  /// have operators + and * defined.
  template < class Value>
  void edge_average( const COM::Attribute *attr, Value &val) const {
    val = 0.5*(*reinterpret_cast<const Value *>(destination().addr(attr))+
	       *reinterpret_cast<const Value *>(origin().addr(attr))); 
  }
  
  /// Get the average value of the nodes of the incident face of a given 
  /// halfedge. If this is a border halfedge, then return its edge averagd.
  /// Value must have operators + and * defined.
  template < class Value>
  void face_average( const COM::Attribute *attr,  Value &val) const {
    // If this is a border halfedge, then return its edge center.
    if ( is_border()) { edge_average( attr, val); return; }
  
    // Otherwise, compute the center
    val = Value(0);
    
    int count=0;
    Halfedge h=*this, h0=h;

    do {
      val += *reinterpret_cast<const Value*>(h.origin().addr( attr));
      ++count;
    } while ( (h=h.next()) != h0);
    
    val/=count;
  }
  
  /// Return the dihedral angle at the edge. If the edge is a border edge,
  /// then return pi.
  double dihedral_angle() const;

  /// Obtain the pane manifold that owns the edge.
  const Pane_manifold_2 *pane_manifold() const { return _pm; }

  /// Obtain the owner Pane_manifold_2 of the node
  const COM::Pane *pane() const 
  { if ( _pm) return _pm->pane(); else return NULL; }

  /// Obtain the ID of the edge.
  Edge_ID id() const { return _eID; }

  /// Obtain the address of an attribute associated with its bounded element.
  /// Note: This function can be slow O(1) if the attribute is not on the
  /// current pane, and is even slower (O(log(#panes)) if not owned by the
  /// owner window of the pane.
  const void *addr(  const COM::Attribute *attr) const;

  /// Obtain the attribute on the parent pane of the node.
  const COM::Attribute *attr( const COM::Attribute *a) const { 
    COM_assertion_msg( a, "Unexpected NULL pointer");
    return a->window()->pane( _pm->pane()->id()).attribute(a->id()); 
  }

  /// Are two halfedges the same?
  bool operator==( const Halfedge &h) const 
  { return _pm==h._pm && _eID==h._eID; }

  /// Are two halfedges different?
  bool operator!=( const Halfedge &h) const 
  { return _pm!=h._pm || _eID!=h._eID; }

  /// Does the current halfedge has a smaller ID then the given one?
  bool operator<( const Halfedge &h) const {
    int pid1=_pm->pane()->id(), pid2=h._pm->pane()->id();
    return pid1<pid2 || pid1==pid2 && _eID<h._eID; 
  }

protected:
  const Pane_manifold_2 *_pm;      // Reference to Pane_manifold_2
  Edge_ID                _eID;     // Edge ID
  Access_Mode            _mode;    // Access mode
};

const Halfedge &Pane_manifold_2::get_counterpart( Edge_ID eID) const 
{
  COM_assertion( eID.is_border());
  return _bd_cnt[ eID.eid()-1];
}

const Vector_3<Real> &Pane_manifold_2::get_bd_normal( Edge_ID eID) const {
  COM_assertion_msg( eID.is_border() && !_bd_nrms.empty(),
		     "get_bd_normal must be called on border edges and be called after update_bd_normals() has been called.");
  return _bd_nrms[ eID.eid()-1];
}

int Pane_manifold_2::get_bd_flag( Edge_ID eID) const {
  COM_assertion_msg( eID.is_border() && !_bd_flgs.empty(),
		     "get_bd_flag must be called on border edges and be called after update_bd_flags() has been called.");
  return _bd_flgs[ eID.eid()-1];
}

bool Pane_manifold_2::get_bdedge_flag( Edge_ID eID) const {
  COM_assertion_msg( eID.is_border() && !_bd_bm.empty(),
		     "get_bdedge_flag must be called on border edges and be called after update_bdedge_bitmap() has been called.");
  return _bd_bm[ eID.eid()-1];
}

// Obtain the opposite edge in a given access mode
Halfedge Pane_manifold_2::
get_opposite_edge( Edge_ID eID, Access_Mode mode) const {
  Edge_ID opp;
  switch ( mode) {
  case REAL_PANE:  
    opp = get_opposite_real_edge( eID); break;
  case WHOLE_PANE: 
    opp = Simple_manifold_2::get_opposite_edge( eID); break;
  case ACROSS_PANE: {
    opp=get_opposite_real_edge( eID);
   
    if (  opp.is_border()) {
      Halfedge h = get_counterpart(opp);
      if ( h.pane_manifold()) return h;
    }

    break;
  }
  default:  COM_assertion_msg(false, "Should never reach here");
  }
  return Halfedge( this, opp, mode);
}

Node Pane_manifold_2::get_primary( int vID, Access_Mode mode) const 
{
  if ( mode != ACROSS_PANE) return Node(this, vID, mode);

  int bid = get_bnode_index( vID);

  COM_assertion( bid<int(_nd_prm.size()));
  if ( bid >=0 && _nd_prm[ bid].pane_manifold())
    return _nd_prm[ bid];
  else
    return Node( this, vID, ACROSS_PANE);
}

bool Pane_manifold_2::is_primary( int vID, Access_Mode mode) const 
{
  if ( mode != ACROSS_PANE) return true;

  int bid = get_bnode_index( vID);

  COM_assertion( bid<int(_nd_prm.size()));
  return bid <0 || !_nd_prm[ bid].pane_manifold();
}

// Is the edge on the boundary of a given mode?
bool Pane_manifold_2::is_border_edge( Edge_ID eID, Access_Mode mode) const 
{ 
  switch ( mode) {
  case REAL_PANE:  
    return is_real_border_edge( eID);
  case WHOLE_PANE: 
    // For whole pane, an edge is border only if it is pure real border edge.
    return is_pure_real_border_edge( eID);
  case ACROSS_PANE: 
    return eID.is_border() && get_counterpart( eID).pane_manifold() == NULL;
  default:  COM_assertion_msg(false, "Should never reach here");
    return false;
  }
}

bool Node::is_border() const { return halfedge().is_border(); }

Halfedge Node::halfedge() const 
{
  Edge_ID eID;

  switch ( _mode) {
  case REAL_PANE:
    eID = _pm->get_incident_real_edge( _vID); break;
  case WHOLE_PANE:
    eID = _pm->get_incident_edge( _vID); break;
  default: {
    eID =_pm->get_incident_real_edge( _vID);
    // the current node (vertex) is in general the origin of the edge or 
    // the edge center, unless the node is isolated. 

    if ( eID.is_border()) {
      Halfedge h=_pm->get_counterpart( eID);
      if ( h.pane_manifold()) return h;
    }
  }
  }

  return Halfedge(_pm, eID, _mode); 
}

/// Get the face normal of a point in the element incident on h.
Vector_3<Real> get_normal( const Halfedge &h, const Vector_2<Real> &nc);

/// Get the normal of the deformed shape (coordinates plus given 
/// displacements) of the incident facet of a halfedge.
Vector_3<Real> get_deformed_normal( const Halfedge &h, 
				    const Vector_2<Real> &nc, 
				    const COM::Attribute *disp);

/// Get the tangent of the given halfedge.
inline Vector_3<Real> get_tangent( const Halfedge &h) 
{ return h.tangent(); }


SURF_END_NAMESPACE

#endif






