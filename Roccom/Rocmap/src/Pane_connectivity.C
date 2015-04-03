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
// $Id: Pane_connectivity.C,v 1.35 2008/12/06 08:43:21 mtcampbe Exp $

#include <set>
#include <map>
#include <cassert>
#include <algorithm>
#include <iostream>

#include "kdtree_d.h"
#include "Pane_connectivity.h"
#include "Pane_boundary.h"

MAP_BEGIN_NAMESPACE

class Point_3_ref : protected Point_3<Real> {
public:
  Point_3_ref() : offs_(-1) {}
  Point_3_ref( Real x, Real y, Real z) : Point_3<Real>(x,y,z), offs_(-1) {}
  Point_3_ref( const Point_3<Real> &r, int k) : Point_3<Real>(r), offs_(k) {}

  using Point_3<Real>::operator[];

  int offset() const 
  { assert( offs_>1); return offs_; }
  
private:
  int offs_;
};

class KD_tree_3 : 
  public CGAL::Kdtree_d<CGAL::Kdtree_interface<Point_3<Real> > > 
{
  typedef CGAL::Kdtree_d<CGAL::Kdtree_interface<Point_3<Real> > > Base;
public:
  KD_tree_3() : Base(3) {}
};

class KD_tree_pntref_3 : 
  public CGAL::Kdtree_d<CGAL::Kdtree_interface<Point_3_ref > > 
{
  typedef CGAL::Kdtree_d<CGAL::Kdtree_interface<Point_3_ref > > Base;
public:
  KD_tree_pntref_3() : Base(3) {}
};

typedef std::pair<int,int>                      pair_int;
typedef std::pair<int,int>                      Node_ID;

Pane_connectivity::
Pane_connectivity( const COM::Attribute *mesh, MPI_Comm c) 
  : _win(mesh->window()), _mani2(NULL),
    _comm( COMMPI_Initialized()?c:MPI_COMM_NULL) 
{
  _win->panes( const_cast<std::vector<const COM::Pane*>&>(_panes));
}

Pane_connectivity::
Pane_connectivity( const Simple_manifold_2 **mani2, MPI_Comm c) 
  : _win( mani2[0]->pane()->window()), _mani2(mani2),
    _comm( COMMPI_Initialized()?c:MPI_COMM_NULL) 
{
  _win->panes( const_cast<std::vector<const COM::Pane*>&>(_panes));
}

static void convert_nodelist( const std::vector<bool> &bs,
			      std::vector<int> &nodes) {
  nodes.clear();
  const int s = bs.size();
  for ( int i=0; i<s; ++i)
    if ( bs[i]) nodes.push_back( i+1);
}

void Pane_connectivity::
collect_nodes( const std::vector<std::vector<int> > &ns,
	       std::vector<int> &nodes, 
	       std::vector<Point_3> &pnts, 
	       bool for_facet) {
  int count=0;
  for (unsigned int i=0; i<ns.size(); ++i) count+=ns[i].size();

  // Collect the nodes
  nodes.clear(); nodes.reserve( count+2*ns.size());
  pnts.clear();  pnts.reserve( count+2*ns.size());

  for ( int i=0, s=_panes.size(); i<s; ++i) {
    nodes.push_back( _panes[i]->id());
    nodes.push_back( ns[i].size());

    nodes.insert( nodes.end(), ns[i].begin(), ns[i].end());

    pnts.push_back( Point_3(HUGE_VAL,HUGE_VAL,HUGE_VAL));
    Point_3 &bnd_min = pnts.back();
    pnts.push_back( Point_3(-HUGE_VAL,-HUGE_VAL,-HUGE_VAL));
    Point_3 &bnd_max = pnts.back();

    const COM::Attribute *attr = _panes[i]->attribute( COM::COM_NC);
    const int d = attr->size_of_components();

    for (std::vector<int>::const_iterator it=ns[i].begin(), iend=ns[i].end();
	 it!=iend;++it) {
      Point_3 p(0,0,0);
      if ( !for_facet) {
	int j=*it-1;

	for ( int k=0; k<d; ++k)
	  p[k] = *(const double*)attr->get_addr( j, k);
      }
      else {
	Facet_ID eID=(Facet_ID&)(*it);
	Element_node_enumerator ene( _panes[i], eID.eid());
	int vdst=(ene[eID.lid()+1==ene.size_of_edges()?0:eID.lid()+1]);
	int j1=ene[eID.lid()]-1, j2=vdst-1;

	for ( int k=0; k<d; ++k) {
	  p[k] = 0.5*(*(const double*)attr->get_addr( j1, k) +
		      *(const double*)attr->get_addr( j2, k));
	}
      }

      pnts.push_back( p);

      for (int k=0; k<3; ++k) {
	bnd_min[k] = std::min( bnd_min[k], p[k]);
	bnd_max[k] = std::max( bnd_max[k], p[k]);
      }
    }
  }
}

void Pane_connectivity::
collect_points( const std::vector<std::vector<int> > &ns,
		std::vector<Point_3> &pnts) {
  int count=0;
  for (unsigned int i=0; i<ns.size(); ++i) count+=ns[i].size();
  pnts.clear();  pnts.reserve( count);
  
  for ( int i=0, s=_panes.size(); i<s; ++i) {
    const COM::Attribute *attr = _panes[i]->attribute( COM::COM_NC);
    const int d = attr->size_of_components();

    for (std::vector<int>::const_iterator it=ns[i].begin(), iend=ns[i].end();
	 it!=iend;++it) {
      Point_3 p(0,0,0);
      int j=*it-1;
      for ( int k=0; k<d; ++k)
	p[k] = *(const double*)attr->get_addr( j, k);

      pnts.push_back( p);
    }
  }
}


// Determine the real nodes that coincide with given isolated points.
void Pane_connectivity::
determine_coisolated_nodes( const COM::Pane &pn, 
			    std::vector<Point_3> &pnts,
			    const double tol, 
			    std::vector<bool> &is_co,
			    KD_tree_3 *tree) throw(int) {

  int n=pn.size_of_real_nodes();
  is_co.clear(); is_co.resize( n, false);

  // Build a kd-tree from the points in pnts if not give at input
  KD_tree_3 ktree_local;

  if ( tree == NULL) {
    ktree_local.build( pnts);
    tree = &ktree_local;
  }

  // Define a temporary buffer for holding the matching points.
  std::vector< Point_3>  outList; outList.reserve(4);

  // Loop through the nodes in the current pane to determine coisolated nodes
  const COM::Attribute *attr = pn.attribute( COM::COM_NC);
  const int d = attr->size_of_components();

  for ( int j=0; j<n; ++j) {
    Point_3 q(0,0,0);
    for ( int k=0; k<d; ++k)
      q[k] = *(const double*)attr->get_addr( j, k);

    Point_3 lb(q.x()-tol, q.y()-tol, q.z()-tol);
    Point_3 ub(q.x()+tol, q.y()+tol, q.z()+tol);
    KD_tree_3::Box box( lb, ub, 3);

    // If found match in the tree, then the current node is coisolated.
    outList.clear();

    tree->search( std::back_inserter( outList), box);

    if ( outList.size()) is_co[j]=true;
  }
}

double Pane_connectivity::
get_local_boundary_nodes( std::vector<int> &nodes, 
			  std::vector<Point_3> &pnts, 
			  bool for_facet) throw(int) {

  std::vector< std::vector<int> >   ns(_panes.size());
  std::vector< std::vector<int> >   iso_ns(_panes.size());

  double sql = HUGE_VAL;
  int iso_local=0;
  for ( int i=0, s=_panes.size(); i<s; ++i) {
    std::vector<bool> is_border;
    std::vector<bool> is_isolated;
    std::vector<Facet_ID>  facets;

    Pane_boundary pb = 
      _mani2 ? Pane_boundary( _mani2[i]) : Pane_boundary( _panes[i]);

    pb.determine_border_nodes( is_border, is_isolated, &facets);
    sql = std::min( sql, pb.min_squared_edge_len( facets));

    if ( !for_facet) {
      convert_nodelist( is_border, ns[i]);
      convert_nodelist( is_isolated, iso_ns[i]);
      
      iso_local += iso_ns[i].size();
    }
    else {
      ns[i].clear();
      ns[i].insert( ns[i].end(), (int*)&*facets.begin(), (int*)&*facets.end());
    }
  }

  // Compute minimum squared edge length.
  double g_sql=sql;
  if ( _comm != MPI_COMM_NULL)
    MPI_Allreduce( &sql, &g_sql, 1, MPI_DOUBLE, MPI_MIN, _comm);

  if ( g_sql==HUGE_VAL) g_sql=0;
  double tol = std::sqrt( g_sql)*0.03;

  if ( !for_facet) {
    std::vector<int> iso_counts;

    // Is there any isolated nodes?
    int comm_size, comm_rank;
    if ( _comm != MPI_COMM_NULL) {
      MPI_Comm_size( _comm, &comm_size);
      MPI_Comm_rank( _comm, &comm_rank);

      iso_counts.resize(comm_size);
      MPI_Allgather(&iso_local, 1, MPI_INT, &iso_counts[0], 1, MPI_INT, _comm);

      iso_local=0;
      for (int i=0; i<comm_size; ++i) iso_local+=iso_counts[i];
    }
    else {
      comm_size=1; 
      comm_rank=0;
    }

    if ( iso_local) { // Determine all nodes incident on isolated nodes.
      collect_points( iso_ns, pnts);
      
      // Collect all isolated nodes onto the processor
      std::vector< Point_3> pnts_g( iso_local);
      
      if ( _comm != MPI_COMM_NULL) {
	for (int i=0; i<comm_size; ++i) 
	  iso_counts[i]*=3;

	std::vector<int> iso_disps(comm_size, 0);
	for (int i=1; i<comm_size; ++i) 
	  iso_disps[i]=iso_disps[i-1]+iso_counts[i-1];

	MPI_Allgatherv( &pnts[0], iso_counts[comm_rank], MPI_DOUBLE,
			&pnts_g[0], &iso_counts[0], &iso_disps[0],
			MPI_DOUBLE, _comm);
      }
      else
	pnts_g = pnts;
    
      if ( pnts_g.size()>0) {
	// Build a kd-tree from the points in pnts_g
	KD_tree_3 ktree;
	ktree.build( pnts_g);

	for ( int i=0, s=_panes.size(); i<s; ++i) {
	  std::vector<bool> is_co;
	  determine_coisolated_nodes( *_panes[i], pnts_g, tol, is_co, &ktree);
	  convert_nodelist( is_co, iso_ns[i]);
	}
      }
    }

    // Merge the lists
    for ( unsigned int i=0; i<ns.size(); ++i) {
      std::vector<int> &nsi = ns[i];
      nsi.insert( ns[i].end(), iso_ns[i].begin(), iso_ns[i].end()); 

      // sort and unique the entries in ns[i]
      std::sort(nsi.begin(), nsi.end());
      std::vector<int>::iterator new_end=std::unique(nsi.begin(), nsi.end());
      nsi.erase( new_end, nsi.end());
    }
  }

  // Copy the nodes from ns into nodes and pnts
  collect_nodes( ns, nodes, pnts, for_facet);
  
  return tol;
}

static void 
make_kd_tree( const std::vector<int> &nodes,
	      const std::vector<Point_3<Real> > &pnts,
	      std::vector<Point_3<Real> > &bbox,
	      KD_tree_pntref_3 &ktree) {
  if ( nodes.empty()) return;
  assert( nodes.size() == pnts.size());
  std::vector< Point_3_ref> b; b.reserve( pnts.size());
  bbox.clear();

  unsigned int count = 0;
  while (count<nodes.size()) {
    bbox.push_back( pnts[count]); 
    bbox.push_back( pnts[++count]);
    for ( int i=0, n = nodes[count++]; i<n; ++i, ++count) {
      b.push_back( Point_3_ref(pnts[count],count));
    }
  }
  ktree.build( b);
}

static bool
intersect_bbox( const Point_3<Real> &bmin1, const Point_3<Real> &bmax1,
		const Point_3<Real> &bmin2, const Point_3<Real> &bmax2,
		Real eps) {
  // check for emptiness ??
  if (bmax1.x()+eps < bmin2.x() || bmax2.x()+eps < bmin1.x())
    return false;
  if (bmax1.y()+eps < bmin2.y() || bmax2.y()+eps < bmin1.y())
    return false;
  if (bmax1.z()+eps < bmin2.z() || bmax2.z()+eps < bmin1.z())
    return false;
  return true;
}

static bool
intersect_bbox( const Point_3<Real> &bmin, const Point_3<Real> &bmax,
		const std::vector<Point_3<Real> > &bbox, Real tol) {
  for ( int i=0, n=bbox.size(); i<n; i+=2) {
    if ( intersect_bbox( bmin, bmax, bbox[i], bbox[i+1], tol))
      return true;
  }
  return false;
}

static void
collect_coincident_nodes( const std::vector<int> &r_nodes, 
			  const std::vector<Point_3<Real> > &r_pnts, 
			  const std::vector<Point_3<Real> > &bbox,
			  KD_tree_pntref_3 &ktree, double tol, 
			  std::vector<int> &nodes, 
			  std::vector<Point_3<Real> > &pnts) {
  unsigned int count = 0;

  while (count<r_nodes.size()) {
    const Point_3<Real> &xmin=r_pnts[count], &xmax=r_pnts[count+1];

    if ( !intersect_bbox( xmin, xmax, bbox, tol))
    { count += 2+r_nodes[count+1]; continue; }
    int pane = r_nodes[count++]; 

    int nn = 0;
    for ( int i=0, n = r_nodes[count++]; i<n; ++i, ++count) {
      const Point_3<Real> &p=r_pnts[count];
      Point_3_ref lb(p.x()-tol, p.y()-tol, p.z()-tol);
      Point_3_ref ub(p.x()+tol, p.y()+tol, p.z()+tol);
      KD_tree_pntref_3::Box box( lb, ub, 3);

      std::vector< Point_3_ref>  outList; outList.reserve(4);
      ktree.search( std::back_inserter( outList), box);
      if ( !outList.empty()) {
	if ( nn==0) {
	  nodes.push_back( -pane); // Use negative for remote panes
	  nodes.push_back( 0);
	  pnts.push_back( xmin); pnts.push_back( xmax);
	}
	nodes.push_back( r_nodes[count]); 
	pnts.push_back( p);
	++nn;
      }
    }
    if ( nn>0) *(nodes.end()-nn-1) = nn;
  }
}


/** Collect the boundary nodes of all panes that are coincident with
 *  the boundary nodes of local panes. 
 *  The output nodes is in the following format:
 *      <+/-pane_id> <n=#of nodes> <local_id 1> ... <local_id n>
 *      <+/-pane_id 2> .... !repeat the above
 *  Note that for remote panes, the negative of pane ids are given.
 *  The local nodes are always listed first.
 *  For points, they are stored as 
 *      minx miny minz maxx maxy maxz x1 y1 z1 x2 y2 z2 ... xn yn zn
 *      ! then repeats for other panes
 *      in consecutive order for all the boundary nodes.
 *  It returns an estimated tolerance for window query.
 */
double Pane_connectivity::
collect_boundary_nodes( std::vector<int>     &nodes,
			std::vector<Point_3> &pnts,
			bool for_facet) throw(int) {

  double tol = get_local_boundary_nodes( nodes, pnts, for_facet);
  if ( _comm == MPI_COMM_NULL) return tol;

  int comm_size, comm_rank;
  MPI_Comm_size( _comm, &comm_size);
  MPI_Comm_rank( _comm, &comm_rank);

  // Gather the size info
  int  ns=nodes.size(); assert( pnts.size()==nodes.size());
  std::vector<int> nss(  comm_size);

  MPI_Allgather( &ns, 1, MPI_INT, &nss[0], 1, MPI_INT, _comm);

  std::vector<MPI_Request> reqs; reqs.reserve( (comm_size-1)*2);

  std::vector<int>     l_nodes = nodes;
  std::vector<Point_3> l_pnts = pnts;
  // Now have each processor broadcast their 
  for ( int i=1; i<comm_size; ++i) {
    const int to_rank = (comm_rank+i)%comm_size;
    MPI_Request req;
    MPI_Isend( &l_nodes[0], nodes.size(), MPI_INT, to_rank,
	       101, _comm, &req); reqs.push_back(req);
    MPI_Isend( &l_pnts[0], 3*pnts.size(), MPI_DOUBLE, to_rank,
	       102, _comm, &req); reqs.push_back(req);
  }

  KD_tree_pntref_3 local_rtree;
  std::vector< Point_3> bbox;
  std::vector< int>     r_nodes, r_nodes_pre = nodes;
  std::vector< Point_3> r_pnts,  r_pnts_pre;
  int from_rank_pre = comm_rank;

  for ( int i=comm_size-1; i>=1; --i) {
    MPI_Request l_reqs[2];
    MPI_Status  l_stats[2];

    const int from_rank = (comm_rank+i)%comm_size;
    r_nodes.resize( nss[from_rank]);
    r_pnts.resize( nss[from_rank]);
    MPI_Irecv( &r_nodes[0], r_nodes.size(), MPI_INT, from_rank,
	       101, _comm, &l_reqs[0]);
    MPI_Irecv( &r_pnts[0], 3*r_pnts.size(), MPI_DOUBLE, from_rank,
	       102, _comm, &l_reqs[1]);

    // Overlap computation with communication.
    if ( i==comm_size-1) {
      make_kd_tree( r_nodes_pre, pnts, bbox, local_rtree);
    }
    else {
      collect_coincident_nodes( r_nodes_pre, r_pnts_pre, bbox, local_rtree,
				tol, nodes, pnts);
    }
    MPI_Waitall( 2, l_reqs, l_stats);
    r_nodes_pre.swap( r_nodes); r_pnts_pre.swap( r_pnts);
    from_rank_pre = from_rank;
  }

  collect_coincident_nodes( r_nodes_pre, r_pnts_pre, bbox, 
			    local_rtree, tol, nodes, pnts);

  std::vector<MPI_Status> stat( reqs.size());
  if (reqs.size())
    MPI_Waitall( reqs.size(), &reqs[0], &stat[0]);

  return tol;
}


/** Create b2v mapping for nodes or edges.
 *  The output is a vector of b2v mapping for the local panes (b2v[i] for
 *      panes[i]). Each mapping has the following format:
 *      <remote_pane_id> <n=#of coincident nodes> <local_id 1> ... <local_id n>
 *      <remote_pane_id 2> .... !repeat the above
 */
void Pane_connectivity::
create_b2map( std::vector< std::vector<int> > &b2v, 
	      bool for_facet) throw(int) {
  
  typedef std::map< int, std::map< int, std::vector<pair_int> > >  B2v_map;
  B2v_map b2v_map;

  //< This is a mapping for pane_1-->pane_2-->node_1-->node2, where
  //<   <pane_1,node_1> and <pane_2,node_2> are two coincident nodes.
  //<   Furthermore, pane_1 is always <= pane_2; If pane_1<=pane_2, then
  //<   node_1<node_2.
  
  try {
    // Create a mapping in b2v_map
    std::vector<int>     nodes, panes;
    std::vector<Point_3> pnts;
  
    // Obtain the boundary nodes of all panes that are coincident with
    // the boundary nodes of local panes.
    double tol = collect_boundary_nodes( nodes, pnts, for_facet);

    panes.resize( nodes.size());
    for (unsigned int count=0; count<nodes.size(); count+=nodes[count+1]+2) {
      std::fill( &panes[count], &panes[count+nodes[count+1]+2], nodes[count]);
    }
    
    std::vector< Point_3>  bbox;
    KD_tree_pntref_3 ktree;
    make_kd_tree( nodes, pnts, bbox, ktree);

    std::vector< bool>  processed(nodes.size());
    std::fill_n( processed.begin(), nodes.size(), false);

    unsigned int count = 0;
    while (count<nodes.size()) {
      int paneid = nodes[count++]; 
      // If the pane is remote (i.e., paneid<0), we stop processing the
      // array because the local ones are always before the remote ones.
      if (paneid<0) break;
      
      for ( int i=0, n=nodes[count++]; i<n; ++i, ++count) {
	if ( processed[count]) continue;

	const Point_3 &p = pnts[count];
	Point_3_ref lb(p.x()-tol, p.y()-tol, p.z()-tol);
	Point_3_ref ub(p.x()+tol, p.y()+tol, p.z()+tol);
	KD_tree_pntref_3::Box box( lb, ub, 3);

	std::vector< Point_3_ref>  outList; outList.reserve(4);
	ktree.search( std::back_inserter( outList), box);
	assert( !outList.empty());

	std::vector< Node_ID> ids; ids.reserve( outList.size());
	for ( std::vector< Point_3_ref>::iterator 
		i=outList.begin(); i!=outList.end(); ++i) {
	  int offset=i->offset();
	  processed[ offset] = true;
	  ids.push_back( Node_ID( panes[offset],nodes[offset]));
	}

	std::vector< Node_ID >::const_iterator id1=ids.begin();
	for ( id1=ids.begin(); id1!=ids.end(); ++id1) {
	  std::vector< Node_ID >::const_iterator id2=id1; ++id2;

	  for ( ; id2!=ids.end(); ++id2) {
	    if ( abs(id1->first) < abs(id2->first) || 
		 abs(id1->first) == abs(id2->first) && 
		 id1->second < id2->second) 
	      b2v_map[id1->first][id2->first].
		push_back(pair_int(id1->second, id2->second));
	    else
	      b2v_map[id2->first][id1->first].
		push_back(pair_int(id2->second, id1->second));
	  }
	}
      }
    }
  }
  catch (int ex) {
    throw ex; 
  }

  // Now copy from b2v_map into b2v_t
  std::map<int, std::vector<int> > b2v_t;
  B2v_map::iterator i, iend;
  for ( i=b2v_map.begin(), iend=b2v_map.end(); i!=iend; ++i) {
    // Try to catch branchcut.
    std::map<int,int>  branchcut;

    B2v_map::mapped_type::iterator j, jend;
    for ( j=i->second.begin(), jend=i->second.end(); j!=jend; ++j) {
      // Sort the ids in the group so that the mapping between the local 
      // boundary ids and the node ids is monotonically increasing on process.
      // with smaller rank. This is useful for correcting correspondence
      // across processors and is also useful for defining primary node as the 
      // one with the smallest <pane_id,node_id> pair (for Rocface). When they
      // are ordered incrementally, the primary copy can be detected locally.
      std::sort( j->second.begin(), j->second.end());
      if ( i->first == j->first) {
	COM_assertion( i->first>0);

	std::vector< int> &b2v_i = b2v_t[i->first];
	b2v_i.push_back( j->first);
	b2v_i.push_back( j->second.size()*2);

	B2v_map::mapped_type::mapped_type::const_iterator k, kend;
	for ( k=j->second.begin(), kend=j->second.end(); k!=kend; ++k) {
	  b2v_i.push_back( k->first);
	  b2v_i.push_back( k->second);
	  branchcut[k->first] = k->second;
	}
      }
      else {
	if ( i->first>0) { // First pane is local
	  std::vector< int> &b2v_i = b2v_t[i->first];
	  b2v_i.push_back( std::abs(j->first));
	  b2v_i.push_back( j->second.size());

	  B2v_map::mapped_type::mapped_type::const_iterator k, kend;
	  for ( k=j->second.begin(), kend=j->second.end(); k!=kend; ++k)
	    b2v_i.push_back( k->first);
	}
	if ( j->first>0) { // Second pane is local
	  std::vector< int> &b2v_j = b2v_t[j->first];
	  b2v_j.push_back( std::abs(i->first));
	  b2v_j.push_back( j->second.size());
	  
	  B2v_map::mapped_type::mapped_type::const_iterator k, kend;
	  for ( k=j->second.begin(), kend=j->second.end(); k!=kend; ++k)
	    b2v_j.push_back( k->second);
	}
      }
    }

    if ( !branchcut.empty()) {
      if ( _win->pane(i->first).is_structured())
	std::cerr << "\nRocmap Info: Got a branchcut in pane " 
		  << i->first << " of window " << _win->name() << std::endl;
      else {
	std::cerr << "\nRocmap Warning: Got duplicate nodes within pane "
		  << i->first << " of window " << _win->name() << std::endl;
	while ( !branchcut.empty()) {
	  std::map<int,int>::iterator it=branchcut.begin();

	  std::cerr << "Rocmap Warning: Duplicate nodes " << it->first 
		    << " and " << it->second << std::endl;

	  branchcut.erase(it);
	}
      }
    }
  }

  b2v_map.clear();

  // Copy from b2v_t into b2v
  b2v.resize(_panes.size());
  Pane_set::const_iterator it=_panes.begin();
  for ( int i=0, s=_panes.size(); i<s; ++i, ++it) {
    b2v[i].swap( b2v_t[(*it)->id()]);
  }
}

/// Create b2v mapping for nodes.
void Pane_connectivity::create_b2map( COM::Attribute *pconn, 
				      bool for_facet) throw(int)
{ 
  std::vector< std::vector<int> > b2v;

  create_b2map( b2v, for_facet); 

  // Loop through the panes to get the connectivity map for each pane.
  for ( int i=0, n=b2v.size(); i!=n; ++i) {
    COM::Attribute *attr;
    attr = const_cast<COM::Attribute*>(_panes[i]->attribute( pconn->id()));

    int size = b2v[i].size() + _pconn_offset;
    attr->set_size( size);
    
    int *addr;
    // If not yet initialized, allocate memory for it.
    if ( !attr->initialized())
      addr = (int*)attr->allocate( 1, size, false);
    else
      addr = (int*)attr->pointer();
      
    int cap = attr->capacity();
    int cpanes = 0; // number of communicating panes.
    int j=0,len=b2v[i].size();
    for(; j+1<len; j+=2+(b2v[i])[j+1]){
      // Count all the panes in b2v
      cpanes ++;
    }
    COM_assertion_msg(j==len, "Invalid communication map");

    // Make sure to output pconn in correct format
    if (_pconn_offset && cap >= 1){
      addr[0] = cpanes;
      std::copy(b2v[i].begin(), b2v[i].begin()+std::min(size-1,cap-1), addr+1);
    }
    else if (cap >= 1)
      std::copy( b2v[i].begin(), b2v[i].begin()+std::min(size,cap),addr);
  }
}

void Pane_connectivity::compute_pconn( COM::Attribute *pconn_n,
				       COM::Attribute *pconn_f) throw(int) { 
  if ( pconn_n) create_b2map( pconn_n, false); 
  if ( pconn_f) create_b2map( pconn_f, true); 
}

void Pane_connectivity::size_of_cpanes( const COM::Attribute *pconn,
					const int *pane_id,
					int *npanes_total, int *npanes_ghost){
  const COM::Window    *win = pconn->window();
  const COM::Attribute *pconn_pn;
  pconn_pn = pconn->window()->pane( *pane_id).attribute( pconn->id());

  // Obtain the address and sizes of the map
  const int *tmp_ptr = (const int *)pconn_pn->pointer();
  const int *ptr_pconn = tmp_ptr ? tmp_ptr+_pconn_offset : tmp_ptr;
  const int len_real = ptr_pconn ? pconn_pn->size_of_real_items()-_pconn_offset : 0;
  const int len_total = ptr_pconn ? pconn_pn->size_of_items()-_pconn_offset : 0;

  COM_assertion_msg( npanes_total, 
		     "Invalid NULL pointer for argument npanes_total");

  *npanes_total = 0;
  int i=0;
  for ( ; i+1<len_real; i+=2+ptr_pconn[i+1]) {
    // Count only the panes existing in the window
    *npanes_total += win->owner_rank( ptr_pconn[i])>=0;
  }
  COM_assertion_msg( i==len_real, "Invalid communication map");

  for ( ; i+1<len_total; i+=2+ptr_pconn[i+1]) {
    // Count only the panes existing in the window
    *npanes_total += win->owner_rank( ptr_pconn[i])>=0;
  }

  // Compute the communicating panes that involve ghost nodes
  if ( npanes_ghost) {
    *npanes_ghost = 0;
    for ( i=len_real; i+1<len_total; i+=2+ptr_pconn[i+1])
      *npanes_ghost += win->owner_rank( ptr_pconn[i])>=0;
  }

  COM_assertion_msg( i==len_total, "Invalid communication map");  
}

int Pane_connectivity::pconn_nblocks( const COM::Attribute *pconn) {
  const COM::Window  *win = pconn->window();

  int nblocks = 0;
  // Loop through all panes
  std::vector<const COM::Pane*> panes; win->panes(panes);
  for ( int p=0, np=panes.size(); p<np; ++p) {
    const COM::Attribute *pconn_pn = panes[p]->attribute( pconn->id());
    
    // Obtain the address and sizes of the map
    const int *pconn_ptr = (const int *)pconn_pn->pointer();

    if ( pconn_ptr==NULL) continue;

    const int len_total = pconn_pn->size_of_items()-_pconn_offset;

    const int len_ghost = pconn_ptr ?  pconn_pn->size_of_ghost_items() : 0;
    if ( len_ghost == 0) { nblocks = std::max( nblocks, 1); continue; }

    pconn_ptr += len_total-len_ghost+_pconn_offset;

    // Skip real nodes
    int index=1, ncpanes=pconn_ptr[0];
    for ( int i=1; i<=ncpanes; ++i, index+=pconn_ptr[index+1]+2);
    // Skip ghost nodes
    ncpanes=pconn_ptr[index++];
    for ( int i=1; i<=ncpanes; ++i, index+=pconn_ptr[index+1]+2);

    nblocks = std::max( nblocks, 2+(index<len_ghost));
  }
  
  // Communicate to get the maximum of nblocks.
  if ( COMMPI_Initialized()) {
    int nblocks_local = nblocks;
    MPI_Allreduce( &nblocks_local, &nblocks, 1, MPI_INT, MPI_MAX,
		   win->get_communicator());
  }

  return nblocks;
}

const int Pane_connectivity::_pconn_offset=1;


MAP_END_NAMESPACE






