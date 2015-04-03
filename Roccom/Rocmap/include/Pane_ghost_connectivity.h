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
// $Id: Pane_ghost_connectivity.h,v 1.5 2008/12/06 08:43:20 mtcampbe Exp $

/** \file Pane_ghost_connectivity.h
 * Utility for constructing pane ghost connecvtivities in parallel.
 */
/* Initial version by  P. Alexander, 3/14/2006
 */

#include <iostream>
#include <iomanip>
#include <algorithm>
#include <deque>
#include "mapbasic.h"
#include "Rocmap.h"
#include "Roccom_base.h"
#include "Pane_connectivity.h"
#include "Pane.h"

MAP_BEGIN_NAMESPACE

class Pane_friend : public COM::Pane {
  explicit Pane_friend( COM::Pane&);
public:
  using COM::Pane::new_attribute;
  using COM::Pane::delete_attribute;
  using COM::Pane::inherit;
  using COM::Pane::set_size;
  using COM::Pane::reinit_attr;
  using COM::Pane::reinit_conn;
  using COM::Pane::connectivity;
};

using COM::Connectivity;
using COM::Attribute;
using COM::Window;
using COM::Pane;

using std::vector;
using std::multimap;
using std::map;
using std::set;
using std::pair;
using std::make_pair;
using std::deque;
using std::string;
using std::cout;
using std::endl;
using std::setw;
using std::setfill;

class Pane_ghost_connectivity {
  
public:
  /// Constructors
  explicit Pane_ghost_connectivity(COM::Window *window){
    _buf_window = window;
    init();
  }
  
  ~Pane_ghost_connectivity(){
    ;
  }
  
  void 
  build_pconn();

  void
  init();
  
  // Get a total ordering of nodes
  void 
  get_node_total_order();

  /** Determine elements/nodes to be ghosted on adjacent panes.
   *
   * Create a buffer of local cells to send to each adjacent pane. Each
   * cell in the buffer will take the form:
   *
   * [cell type][cell nodes in total-ordering format]
   *
   * Element ids are not needed, just use the same order in the RCS and GCR
   * sections of the pconn.
   */   
  void
  get_ents_to_send(vector<vector<vector<int> > > &gelem_lists,
		   vector<vector<map<pair<int,int>,int> > > &nodes_to_send,
		   vector<vector<deque<int> > > &elems_to_send,
		   vector<vector<int> > &comm_sizes);
  
  // Determine # of ghost nodes to receive and map (P,N) to ghost node ids
  // Also determine # ghost elements of each type to receive
  void 
  process_received_data(
			vector<vector<vector<int> > > &recv_info,
			vector<vector<int> > &elem_renumbering,
			vector<vector<map<pair<int,int>,int> > > &nodes_to_recv);
  
  // Take the data we've collected and turn it into the pconn
  // Remember that there are 5 blocks in the pconn:
  // 1) Shared node info - already exists
  //      Simply Copy into the buffer
  // 2) Real Nodes to Send
  //      Data in nodes_to_send.. should be in correct order
  // 3) Ghost Nodes to Receive
  //      Data in nodes_to_recv
  // 4) Real Cells to Send
  //      In elems_to_send
  // 5) Ghost Cells to Receive
  //      Combine elem_renumbering with recv_info
  //
  // Also need to calculate the connectivity tables for the 
  // new ghost elements. Do this while looking through recv_info
  // for GCR
  
  void 
  finalize_pconn(vector<vector<map<pair<int,int>,int> > > &nodes_to_send,
		 vector<vector<map<pair<int,int>,int> > > &nodes_to_recv,
		 vector<vector<deque<int> > > &elems_to_send,
		 vector<vector<int> > &elem_renumbering,
		 vector<vector<vector<int> > > &recv_info);

  // Determine communicating panes for shared nodes.
  void 
  get_cpanes();
  
  // send_gelem_lists
  void 
  send_gelem_lists(vector<vector<vector<int> > > &gelem_lists,
		   vector<vector<vector<int> > > &recv_info,
		   vector<vector<int> > &comm_sizes);
  
  // Send arbitrary amount of data to another pane.
  // send_info = data to send
  // recv_info = buffer for receiving data
  // comm_sizes = amount of data to receive
  void 
  send_pane_info(vector<vector<vector<int> > > &send_info,
		 vector<vector<vector<int> > > &recv_info,
		 vector<vector<int> > &comm_sizes);
  
private:

  void determine_shared_border();

  void 
  mark_elems_from_nodes(std::vector<std::vector<bool> > &marked_nodes,
			std::vector<std::vector<bool> > &marked_elems);
    
private:

  // Is node shared?
  std::vector<std::vector<bool> > _is_shared_node;

  // Is the elem shared?
  std::vector<std::vector<bool> > _is_shared_elem; 

  // List of communicating panes.
  std::vector<std::vector<int> >  _cpanes;
  
  COM::Window* _buf_window;
  
  // Maps element type to element type string
  string _etype_str[COM::Connectivity::TYPE_MAX_CONN];

  // data structures for total node ordering
  vector<vector<int> > _p_gorder;
  Attribute* _w_n_gorder;

  // mapping from total ordering to local node id
  vector<map<pair<int,int>,int> > _local_nodes;

  // pointers to all local panes
  vector<Pane*> _panes;

  // number of local panes
  int _npanes;
};

MAP_END_NAMESPACE






