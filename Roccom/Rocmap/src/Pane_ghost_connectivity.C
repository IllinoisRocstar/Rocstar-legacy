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
// $Id: Pane_ghost_connectivity.C,v 1.8 2009/01/22 23:56:53 gzagaris Exp $

/** \file Pane_ghost_connectivity.C
 * Utility for constructing pane ghost connecvtivities in parallel.
 */
/* Initial version by  P. Alexander, 3/14/2006
 */
#include <cstring>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <algorithm>
#include "Pane_ghost_connectivity.h"
#include "Rocmap.h"
#include "Roccom_base.h"
#include "Pane_connectivity.h"
#include "Pane_communicator.h"
#include "Dual_connectivity.h"
#include "Pane.h"
#include "Element_accessors.h"

MAP_BEGIN_NAMESPACE

typedef vector<vector<int> > pane_i_vector;
typedef vector<int>::iterator i_vector_iter;
typedef vector<set<set<int> > > pane_i_set_set;

void Pane_ghost_connectivity::
init(){

  // Get pointers to all local panes
  _buf_window->panes(_panes);

  _npanes = (int)_panes.size();

  for(int i=0; i<_npanes; ++i){
    _panes[i]->set_ignore_ghost(false);
  }

  // Make sure that we have the shared-node pconn information.
  MAP::Pane_connectivity pc(_buf_window->attribute(COM::COM_MESH),
			    _buf_window->get_communicator());
  
  pc.compute_pconn(_buf_window->attribute(COM::COM_PCONN));
  
  // Determine which nodes are shared
  determine_shared_border();
  
  // Get the list of communicating panes
  get_cpanes();

  // Create data structures for the total node ordering
  _w_n_gorder = 
    _buf_window->new_attribute("n_gorder",'n',COM_INT,1,"");
  _buf_window->resize_array(_w_n_gorder,0);
  _buf_window->init_done();

   _etype_str[Connectivity::ST1]       = ":st1:"; 
  _etype_str[Connectivity::ST2]       = ":st2:";
  _etype_str[Connectivity::ST3]       = ":st3:"; 
  _etype_str[Connectivity::BAR2]      = ":b2:";
  _etype_str[Connectivity::BAR3]      = ":b3:";  
  _etype_str[Connectivity::TRI3]      = ":t3:";
  _etype_str[Connectivity::TRI6]      = ":t6:";  
  _etype_str[Connectivity::QUAD4]     = ":q4:";
  _etype_str[Connectivity::QUAD8]     = ":q8:";  
  _etype_str[Connectivity::QUAD9]     = ":q9:";
  _etype_str[Connectivity::TET4]      = ":T4:";  
  _etype_str[Connectivity::TET10]     = ":T10:";
  _etype_str[Connectivity::PYRIMID5]  = ":P5:";  
  _etype_str[Connectivity::PYRIMID14] = ":P14:";
  _etype_str[Connectivity::PRISM6]    = ":P6:";  
  _etype_str[Connectivity::PRISM15]   = ":P15:";
  _etype_str[Connectivity::PRISM18]   = ":P18:"; 
  _etype_str[Connectivity::HEX8]      = ":B8:";
  _etype_str[Connectivity::HEX20]     = ":B20:"; 
  _etype_str[Connectivity::HEX27]     = ":B27:";
}

void Pane_ghost_connectivity:: 
build_pconn(){

  // Determine the total node ordering
  get_node_total_order();
  
  vector< pane_i_vector > gelem_lists;
  pane_i_vector comm_sizes;
  vector<vector<map<pair<int,int>,int> > > nodes_to_send;
  vector<vector<deque<int> > > elems_to_send;

  get_ents_to_send(gelem_lists,
		   nodes_to_send,
		   elems_to_send,
		   comm_sizes);  
  
  // Communicate calculated ghost information
  vector<pane_i_vector> recv_info;  
  send_gelem_lists(gelem_lists,
			recv_info,
			comm_sizes);
  
  vector<vector<int> > elem_renumbering;
  vector<vector<map<pair<int,int>,int> > > nodes_to_recv;
  process_received_data(recv_info,
			elem_renumbering,
			nodes_to_recv);
  
  finalize_pconn( nodes_to_send,
		  nodes_to_recv,
		  elems_to_send,
		  elem_renumbering,
		  recv_info);
}

// Get a total ordering of nodes in the form of a pair <P,N> where
// P is the "owner pane" and N is the nodes id on the owner pane.
void Pane_ghost_connectivity:: 
get_node_total_order(){

  // Resize per-pane data structures
  _p_gorder.resize(_npanes);
  _local_nodes.resize(_npanes);
  
  // On each node determine P, the pane responsible for numbering the node
  for(int i=0; i < (int)(_npanes); ++i){

    int pane_id = _panes[i]->id();
    int nrnodes = _panes[i]->size_of_real_nodes();

    // Initialize P values of complete ordering to the current pane
    _p_gorder[i].clear();
    _p_gorder[i].resize(nrnodes,pane_id);
    
    // Obtain the pane connectivity of the local pane.
    const Attribute *pconn = _panes[i]->attribute(COM::COM_PCONN);

    // The pconn MAY/MAY not list the number of communicating-panes
    // in the shared nodes section.Handle as though its not there.
    const int *vs 
      = (const int*)pconn->pointer()+MAP::Pane_connectivity::pconn_offset();
    
    // Loop through communicating panes for shared nodes.
    for ( int j=0, index=0, nj=_cpanes[i].size(); 
	  j<nj; ++j, index+=vs[index+1]+2) {

      // Skip panes which the pconn refers to, but which are not 
      // in the current window. May result from partial inheritance.
      while ( _buf_window->owner_rank(vs[index])<0)
	index+=vs[index+1]+2;
      
      // Update P values for the current list of shared nodes
      for(int k=0; k<vs[index+1]; ++k){
	// EDIT
	if(vs[index] > _p_gorder[i][vs[index+2+k]-1])
	  _p_gorder[i][vs[index+2+k]-1] = vs[index];	
      }      
    }
  }
  
  // Set the values of N on nodes for which this pane is responsible
  for(int i=0; i < (int)(_npanes); ++i){
    
    int pane_id = _panes[i]->id();
    int nrnodes = _panes[i]->size_of_real_nodes();
    int n_gorder_id = _w_n_gorder->id();
    
    // There are window-level and pane-level attributes, we need the
    // pane level attributes to obtain a pointer to data.
    Attribute* p_n_gorder = 
      const_cast<Attribute*>(_panes[i]->attribute(n_gorder_id));
    int * n_gorder_ptr = 
      reinterpret_cast<int*>(p_n_gorder->pointer());
    
    for(int j=0; j< nrnodes; ++j){
      if(_p_gorder[i][j] == pane_id)
	n_gorder_ptr[j] = j+1;
      else
	n_gorder_ptr[j] = 0;
    }
  }
  
  // Update shared nodes across all panes using a max reduce operation
  MAP::Pane_communicator pc(_buf_window, _buf_window->get_communicator());
  pc.init(_w_n_gorder);
  pc.begin_update_shared_nodes();
  pc.reduce_on_shared_nodes(MPI_MAX);
  pc.end_update_shared_nodes();  
  
  // Store a mapping from the total node-ordering to the local node id:
  // NOTE: can I make _local_nodes linear and just sort it once?
  for(int i=0; i < (int)(_npanes); ++i){
    
    int nrnodes = _panes[i]->size_of_real_nodes();
    int n_gorder_id = _w_n_gorder->id();

    Attribute* p_n_gorder = 
      const_cast<Attribute*>(_panes[i]->attribute(n_gorder_id));
    int * n_gorder_ptr = 
      reinterpret_cast<int*>(p_n_gorder->pointer());

    for(int j=0; j< nrnodes; ++j)
      _local_nodes[i].
	insert(make_pair(make_pair(_p_gorder[i][j],
					     n_gorder_ptr[j])
			      ,j+1));
  }
}

// Determine elements/nodes to be ghosted on adjacent panes.
void Pane_ghost_connectivity:: 
get_ents_to_send(vector<pane_i_vector > &gelem_lists,
		 vector<vector<map<pair<int,int>,int> > > &nodes_to_send,
		 vector<vector<deque<int> > > &elems_to_send,
		 pane_i_vector &comm_sizes){

  // sets of adjacent elements and nodes
  vector<vector<set<int> > > adj_eset;
  vector<vector<set<int> > > adj_nset;
  set<int>::iterator eset_pos;

  // resize per-local-pane data structures
  gelem_lists.resize(_npanes);
  adj_eset.resize(_npanes);
  adj_nset.resize(_npanes);
  comm_sizes.resize(_npanes);
  nodes_to_send.resize(_npanes);
  elems_to_send.resize(_npanes);

  for(int i=0; i < _npanes; ++i){

    int n_comm_panes = _cpanes[i].size();
    adj_eset[i].resize(n_comm_panes);
    adj_nset[i].resize(n_comm_panes);
    comm_sizes[i].resize(n_comm_panes,0);
    nodes_to_send[i].resize(n_comm_panes);
    elems_to_send[i].resize(n_comm_panes);

    set<int> cur_eset;

    // Obtain the pane connectivity of the local pane.
    const Attribute *pconn = _panes[i]->attribute(COM::COM_PCONN);
    
    MAP::Pane_dual_connectivity dc(_panes[i],0);
    
    const int *vs 
      = (const int*)pconn->pointer() + 
      MAP::Pane_connectivity::pconn_offset();
    
    int vs_size
      = pconn->size_of_real_items() - 
      MAP::Pane_connectivity::pconn_offset();    

    // Loop through communicating panes for shared nodes.
    for ( int j=0, index=0; j<n_comm_panes; ++j, index+=vs[index+1]+2) {
      
      // skiping nonexistent panes to get to next communication pane
      while ( _buf_window->owner_rank(vs[index])<0) {
	index+=vs[index+1]+2;
	COM_assertion_msg( index<=vs_size, "Invalid communication map");
      }		

      for(int k=0; k<vs[index+1]; ++k){
	
	// get elements incident on the shared node:
	vector<int> elist;
	adj_nset[i][j].insert(vs[index+2+k]);
	dc.incident_elements(vs[index+2+k],elist);
	
	// remember elements adj. to this pane
	for(unsigned int ii=0; ii<elist.size(); ++ii)
	  cur_eset.insert(elist[ii]);
      }
      
      adj_eset[i][j] = cur_eset;
      
      // Calculate size of data to send. For every element, send its type 
      // and a list of its nodes in complete ordering format
      for(eset_pos = cur_eset.begin(); eset_pos != cur_eset.end(); ++eset_pos){
	comm_sizes[i][j] += 1 + 2*((_panes[i]->connectivity(*eset_pos))
				   ->size_of_nodes_pe());
      }
      cur_eset.clear();
    }
  }

  int n_gorder_id = _w_n_gorder->id();

  // fill in pane comm data
  for(int i=0; i < _npanes; ++i){

    Attribute* p_n_gorder = 
      const_cast<Attribute*>(_panes[i]->attribute(n_gorder_id));
    int * n_gorder_ptr = 
      reinterpret_cast<int*>(p_n_gorder->pointer());

    gelem_lists[i].resize(_cpanes[i].size());

    // fill in the comm buffers
    for(int j=0, nj = comm_sizes[i].size(); j<nj; ++j){

      int index = 0;

      gelem_lists[i][j].resize(comm_sizes[i][j]);

      for(eset_pos = adj_eset[i][j].begin();
	  eset_pos != adj_eset[i][j].end();
	  ++eset_pos){
	
	vector<int> nodes;
	COM::Element_node_enumerator ene(_panes[i],*eset_pos);
	elems_to_send[i][j].push_back(*eset_pos);
	ene.get_nodes(nodes);

	// store type
	gelem_lists[i][j][index++] = ene.type();
	
	for(int  k=0, nk = ene.size_of_nodes(); k<nk; ++k){
	  // store nodes in (P,N) format
	  int P = _p_gorder[i][nodes[k]-1];
	  int N = n_gorder_ptr[nodes[k]-1];
	  gelem_lists[i][j][index++] = P;
	  gelem_lists[i][j][index++] = N;

	  // Send nodes which aren't shared w/ this processor
	  if(adj_nset[i][j].find(nodes[k]) == adj_nset[i][j].end())
	    nodes_to_send[i][j].insert(make_pair(make_pair(P,N),nodes[k]));
	}
      }
    }
  }

  // We are finished w/ the total ordering at this point, free up some space
  _buf_window->delete_attribute("n_gorder");
  _buf_window->init_done();

  _p_gorder.clear();
}

// Determine # of ghost nodes to receive and map (P,N) to ghost node ids
// Also determine # ghost elements of each type to receive
void Pane_ghost_connectivity:: 
process_received_data(
		      vector<pane_i_vector> &recv_info,
		      vector<vector<int> > &elem_renumbering,
		      vector<vector<map<pair<int,int>,int> > > &nodes_to_recv){

  map<pair<int,int>,int>::iterator pos1, pos2;

  elem_renumbering.resize(_npanes);
  nodes_to_recv.resize(_npanes);

  for(int i=0; i<_npanes; ++i){

    int n_real_nodes = _panes[i]->size_of_real_nodes();
    int next_node_id = n_real_nodes + 1;
    int comm_npanes = recv_info[i].size();
    elem_renumbering[i].resize((int)Connectivity::TYPE_MAX_CONN+1,0);
    nodes_to_recv[i].resize(comm_npanes);

    for(int j=0; j< comm_npanes; ++j){
      
      int recv_size = recv_info[i][j].size();
      int index = 0;
      
      while(index < recv_size){
	int type = recv_info[i][j][index];
	int nnodes = Connectivity::size_of_nodes_pe(type);
	++elem_renumbering[i][type+1];
	
	// Examine element's nodes, labeling those seen for the first time.
	for(int k=1; k<=2*nnodes; k+=2){
	  
	  int P = recv_info[i][j][index+k];
	  int N = recv_info[i][j][index+k+1];
	  
	  pos1 = 
	    nodes_to_recv[i][j].find(make_pair(P,N));
	  
	  pos2 = 
	    _local_nodes[i].find(make_pair(P,N));
	  
	  // If we haven't seen this node at all, give it an id, otherwise
	  // use the existing id	  
	  int cur_node_id = next_node_id;
	  if(pos2 == _local_nodes[i].end())
	    _local_nodes[i].insert(make_pair(make_pair(P,N),next_node_id++));
	  else
	    cur_node_id = pos2->second;
	  
	  // If we haven't seen this node from the current adjacent pane,
	  // and we don't have a local real copy of the node, then 
	  // remember to receive it
	  
	  if(pos1 == nodes_to_recv[i][j].end()
	     && cur_node_id > n_real_nodes)
	    nodes_to_recv[i][j].insert(make_pair(make_pair(P,N),cur_node_id));
	}
	index += 2*nnodes+1;
      }
    }      
  }
}

// Take the data we've collected and turn it into the pconn
// Remember that there are 5 blocks in the pconn:
// 1) Shared node info - already exists
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

void Pane_ghost_connectivity:: 
finalize_pconn(vector<vector<map<pair<int,int>,int> > > &nodes_to_send,
	       vector<vector<map<pair<int,int>,int> > > &nodes_to_recv,
	       vector<vector<deque<int> > > &elems_to_send,
	       vector<vector<int> > &elem_renumbering,
	       vector<pane_i_vector> &recv_info){

  map<pair<int,int>,int>::iterator rns_pos,gnr_pos;
  vector<vector<int> > node_pos;

  // Buff for #elmts to recv from each incident pane
  vector<vector<int> > n_elem;

  // Get the name of the buffer window
  string buf_name(_buf_window->name());
    
  // Save ptrs to conn tables so we don't have to look up 
  // as each element's connectivity is registered
  vector<vector<int*> > conn_ptr(_npanes);

  node_pos.resize(_npanes);
  n_elem.resize(_npanes);

  // Determine buffer space required:
  // 1 (#comm panes) + 2 per adj pane (comm pane id and #entities)
  // + total #entries in entity list (ie node lists for nodes to receive)
  for(int i=0; i< _npanes; ++i){

    int gcr_size =1, rcs_size =1, gnr_size =1, rns_size = 1;
    int n_comm_panes = _cpanes[i].size();
    int pane_id = _panes[i]->id();

    // Real nodes to send
    for(int j=0, nj = nodes_to_send[i].size(); j<nj; ++j)
      rns_size += 2+nodes_to_send[i][j].size();

    // Ghost nodes to receive
    for(int j=0, nj = nodes_to_recv[i].size(); j<nj; ++j)
      gnr_size += 2+nodes_to_recv[i][j].size();

    // Real cells to send
    for(int j=0, nj = (int)elems_to_send[i].size(); j<nj; ++j)
      rcs_size += 2+elems_to_send[i][j].size();

    // Ghost cells to receive
    n_elem[i].resize(n_comm_panes,0);
    for(int j=0, nj = (int)_cpanes[i].size(); j<nj; ++j){
      gcr_size += 2;
      for(int ind=0, size = (int)recv_info[i][j].size(); 
	  ind < size;
	  ind += 1+2*Connectivity::size_of_nodes_pe(recv_info[i][j][ind])){
	gcr_size++;
	n_elem[i][j]++;
      }
    }

    node_pos[i].assign(elem_renumbering[i].begin(),elem_renumbering[i].end());
    
    // Make room for pointers to all potential connectivity tables
    conn_ptr[i].resize(Connectivity::TYPE_MAX_CONN,NULL);

    // Resize connectivity tables
    for(int j=0; j<Connectivity::TYPE_MAX_CONN; ++j){

      int nelems = elem_renumbering[i][j+1];
      elem_renumbering[i][j+1] += elem_renumbering[i][j];

      if(nelems >0){

	const string conn_name = _etype_str[j]+"virtual";

	int nnodes = Connectivity::size_of_nodes_pe(j);	

	// Resize connectivity table and keep a pointer to its buffer
	void* addr;
	_buf_window->set_size(conn_name.c_str(), pane_id, nelems,nelems);
	_buf_window->resize_array(conn_name.c_str(), pane_id, &addr,nnodes,nelems);

	conn_ptr[i][j] = (int*)addr;
	COM_assertion_msg(addr!= NULL, "Could not allocate space for connectivity table");
      }
    }

    // Resize pconn
    Attribute *pconn = NULL;

    pconn = _panes[i]->attribute(COM::COM_PCONN);
    int rsize = pconn->size_of_real_items();
    int gsize = rns_size + gnr_size + rcs_size + gcr_size;

    void *addr;
    _buf_window->set_size("pconn", pane_id, rsize+gsize,gsize);	
    _buf_window->resize_array("pconn",pane_id,&addr);

    pconn = _panes[i]->attribute(COM::COM_PCONN);

    int* pconn_ptr = (int*)addr;

    int rns_ind = rsize;
    int gnr_ind = rns_ind + rns_size;
    int rcs_ind = gnr_ind + gnr_size;
    int gcr_ind = rcs_ind + rcs_size;

    // each block begins w/ # of communicating blocks
    pconn_ptr[rns_ind++] = n_comm_panes;
    pconn_ptr[gnr_ind++] = n_comm_panes;
    pconn_ptr[rcs_ind++] = n_comm_panes;
    pconn_ptr[gcr_ind++] = n_comm_panes;

    // Offset to start of ghost element_ids
    int real_offset = _panes[i]->size_of_real_elements()+1;

    // My current implementation only sends nodes to panes w/ a ghost
    // copy of a local element, so there are the same number of communicating
    // panes in each pconn block.
    // If the code is generalized in the future, there may need to be
    // separate loops for some pconn blocks.
    for(int j=0; j <n_comm_panes; ++j){

      // Write communicating-pane id to buffer
      int comm_pane_id = _cpanes[i][j];
      pconn_ptr[rns_ind++] = comm_pane_id;
      pconn_ptr[gnr_ind++] = comm_pane_id;
      pconn_ptr[rcs_ind++] = comm_pane_id;
      pconn_ptr[gcr_ind++] = comm_pane_id;

      // Write number of enties to buffer
      pconn_ptr[rns_ind++] = nodes_to_send[i][j].size(); 
      pconn_ptr[gnr_ind++] = nodes_to_recv[i][j].size();
      pconn_ptr[rcs_ind++] = elems_to_send[i][j].size(); 
      pconn_ptr[gcr_ind++] = n_elem[i][j];

      // Write entities to ghost pconn buffers
      for(rns_pos = nodes_to_send[i][j].begin();
	  rns_pos != nodes_to_send[i][j].end(); ++rns_pos)
	pconn_ptr[rns_ind++] = rns_pos->second;

      for(gnr_pos = nodes_to_recv[i][j].begin();
	  gnr_pos != nodes_to_recv[i][j].end(); ++gnr_pos)
	pconn_ptr[gnr_ind++] = gnr_pos->second;
      
      for(int k=0, nk = (int)elems_to_send[i][j].size(); k<nk; ++k)
	pconn_ptr[rcs_ind++] = elems_to_send[i][j][k]; 

      // The GCR block is more complicated because we want all ghost elements
      // of a single type to have contiguous element ids, which is required
      // by Roccom if we want to register one connectivity table per type
      int recv_size = recv_info[i][j].size();
      int index = 0;
      while(index < recv_size){

	int elem_type = recv_info[i][j][index];
	int nnodes = Connectivity::size_of_nodes_pe(elem_type);

	// id offset within the correct connectivity table
	int conn_offset = node_pos[i][elem_type]++;

	pconn_ptr[gcr_ind++] = real_offset + elem_renumbering[i][elem_type]++;
	
	// Write out ghost element's nodes
	for(int k=1; k <= 2*nnodes; k+=2){	  

	  map<pair<int,int>,int>::iterator pos;
	  pos = _local_nodes[i].
	    find(make_pair(recv_info[i][j][index+k],
			   recv_info[i][j][index+k+1]));
	  COM_assertion(pos != _local_nodes[i].end());
	  conn_ptr[i][elem_type][nnodes*conn_offset+(k-1)/2] = pos->second;
	}

	index += 2*nnodes+1;
      }
    }
    
    int new_size = _local_nodes[i].size();
    int new_gsize = new_size - _panes[i]->size_of_real_nodes();
    
    // 1) Extend nodal coords to accommodate ghost nodes
    _buf_window->
      set_size("nc", pane_id, new_size, new_gsize);

    _buf_window->resize_array("nc", pane_id, &addr,3);

  }
  // 4) Update ghost nodal coordinates using newly constructed pconn
  MAP::Rocmap::update_ghosts(_buf_window->attribute(COM::COM_NC)); 
}

// Determine communicating panes for shared nodes. Look through pconn
// twice, once to determine the # of communicating panes, and again
// to fill in the properly sized vector .. 

void Pane_ghost_connectivity:: 
get_cpanes(){
  
  // Resize per-local-pane data structures
  _cpanes.resize(_npanes);
  
  for(int i=0; i < (_npanes); ++i){
    
    // Obtain the pconn Attribute of the local pane.
    const Attribute *pconn = _panes[i]->attribute(COM::COM_PCONN);

    // Use the pconn offset and get a pointer to pconn data
    const int *vs =
      (const int*)pconn->pointer()+MAP::Pane_connectivity::pconn_offset();
    int vs_size = 
      pconn->size_of_real_items()-MAP::Pane_connectivity::pconn_offset();    
    
    int n_cpanes = 0;
    for (int j=0, nj=vs_size; j<nj; j+=vs[j+1]+2) {
      if (_buf_window->owner_rank( vs[j]) >=0)
	++n_cpanes;
    }
    _cpanes[i].resize(n_cpanes);
    int cpane_ind = 0;
    for (int j=0, nj=vs_size; j<nj; j+=vs[j+1]+2) {
      if (_buf_window->owner_rank( vs[j]) >=0){
	_cpanes[i][cpane_ind] = vs[j];
	++cpane_ind;
      }
    }
  }
}

void Pane_ghost_connectivity:: 
send_gelem_lists(vector<pane_i_vector> &gelem_lists,
		      vector<pane_i_vector> &recv_info,
		      pane_i_vector &comm_sizes){

  pane_i_vector size_buffer;

  size_buffer.resize(_npanes);
  
  for(int i=0; i < _npanes; ++i)
    size_buffer[i].resize(_cpanes[i].size(),1);
  
  vector<pane_i_vector> send_buffer;
  send_buffer.resize(_npanes);

  // Transfer comm_sizes into send_buffer
  for(int i=0; i < _npanes; ++i){
    send_buffer[i].resize(_cpanes[i].size());
    for(int j=0, nj=_cpanes[i].size(); j<nj; ++j){
      send_buffer[i][j].resize(1);
      send_buffer[i][j][0] = comm_sizes[i][j];
    }
  } 

  // Tell adj. panes how much information I'm sending them
  send_pane_info(send_buffer,
		 recv_info,
		 size_buffer);

  // Transfer received sizes into size_buffer
  for(int i=0; i < _npanes; ++i){
    for(int j=0, nj=_cpanes[i].size(); j<nj; ++j){
      size_buffer[i][j] = recv_info[i][j][0];
    }
  }

  // Send pane comm data
  send_pane_info(gelem_lists,
		 recv_info,
		 size_buffer);  
}

// Send arbitrary amount of data to another pane.
// send_info = data to send
// recv_info = buffer for receiving data
// comm_sizes = amount of data to receive
// cpanes = list of communicating panes
void Pane_ghost_connectivity:: 
send_pane_info(vector<pane_i_vector> &send_info,
	       vector<pane_i_vector> &recv_info,
	       pane_i_vector &comm_sizes){
  
  const Window::Proc_map &proc_map = _buf_window->proc_map();
  int total_npanes = proc_map.size();
  int tag_max = total_npanes*total_npanes;
  recv_info.resize(_npanes);

  // Determine a mapping from user-defined pane ids to a set of 
  // internal pane IDs, which are unique and contiguous across all processes
  // to be used for defining unique tags for MPI messages.
  map<int,int> lpaneid_map;
  map<int,int>::const_iterator it=proc_map.begin();
  for ( int i=0; i<total_npanes; ++i, ++it) 
    lpaneid_map[ it->first] = i;
  
  vector<MPI_Request> reqs_send, reqs_recv;
  int int_size = sizeof(int);
  MPI_Request req;
  MPI_Comm mycomm = _buf_window->get_communicator();
  int myrank = COMMPI_Initialized() ? 
    COMMPI_Comm_rank(mycomm) : 0;
  
  // initiate mpi sends and recieves
  for(int i=0; i< _npanes; ++i){
    
    recv_info[i].resize(_cpanes[i].size());
    int lpid = lpaneid_map.find(_panes[i]->id())->second;
    
    for(int j=0, nj = _cpanes[i].size(); j<nj; ++j){
      
      recv_info[i][j].resize(comm_sizes[i][j],0);

      const int lqid = lpaneid_map.find(_cpanes[i][j])->second;
      int adjrank = _buf_window->owner_rank(_cpanes[i][j]);
      
      int stag = 100 + ((lpid > lqid) ?
			lpid*total_npanes+lqid : lqid*total_npanes+lpid);
      int rtag = stag;
      MPI_Comm comm = mycomm;      
      
      // pane sending to self, copy data directly
      if(myrank == adjrank && lpid == lqid)
	memcpy(&send_info[i][j][0], &recv_info[i][j][0],
		    int_size*send_info[i][j].size());    
      
      // sending to other panes, use COMMPI
      else{
	// process sending to self
	if (myrank == adjrank){
	  // make tags unique
	  if(lpid > lqid)
	    stag += tag_max;
	  else 
	    rtag += tag_max;	
	  comm = MPI_COMM_SELF;
	  adjrank = 0;
	}

	int ierr = 
	  COMMPI_Isend(&send_info[i][j][0],int_size*send_info[i][j].size(),
				MPI_BYTE, adjrank, stag, comm, &req);
	COM_assertion(ierr==0);
	reqs_send.push_back(req);
	
	ierr = COMMPI_Irecv(&recv_info[i][j][0],int_size*comm_sizes[i][j],
			    MPI_BYTE, adjrank, rtag, comm, &req);
	COM_assertion(ierr==0);
	reqs_recv.push_back(req);
      }
    }      
  }
  // wait for MPI communication to finish
  if(mycomm != MPI_COMM_NULL){

    int ierr, index;
    MPI_Status status;

    while(!reqs_recv.empty()){
      ierr = MPI_Waitany(reqs_recv.size(),&reqs_recv[0],
			 &index, &status);
      COM_assertion(ierr == 0);    
      reqs_recv.erase(reqs_recv.begin()+index);
    }

    if(reqs_send.size()){
      ierr = MPI_Waitany(reqs_send.size(),&reqs_send[0],
			 &index, &status);
      COM_assertion(ierr == 0);
      reqs_send.erase(reqs_send.begin()+index);
    }
  } 
}

void Pane_ghost_connectivity::
determine_shared_border(){

  _is_shared_node.resize(_npanes);
  
  //First, get the list of shared nodes.
  for(int i=0; i < (int)(_npanes); ++i){
    // Obtain the pane connectivity of the local pane.
    const COM::Attribute *pconn = _panes[i]->attribute(COM::COM_PCONN);
    // Use the pconn offset
    const int *vs = (const int*)pconn->pointer() + 
      MAP::Pane_connectivity::pconn_offset();
    int vs_size=pconn->size_of_real_items() - 
      MAP::Pane_connectivity::pconn_offset();    

    _is_shared_node[i].resize(_panes[i]->size_of_real_nodes(),0);
    
    // Determine the number of communicating panes for shared nodes.
    int count=0;
    for (int j=0, nj=vs_size; j<nj; j+=vs[j+1]+2) {
      if (_buf_window->owner_rank( vs[j]) >=0) ++count;
    }
    
    int index = 0;
    // Loop through communicating panes for shared nodes.
    for ( int j=0; j<count; ++j, index+=vs[index+1]+2) {
      // We skip the panes that are not in the current window 
      while ( _buf_window->owner_rank(vs[index])<0) {
	index+=vs[index+1]+2;
	COM_assertion_msg( index<=vs_size, "Invalid communication map");
      }	
      // Add shared nodes to the list
      for(int k=0; k<vs[index+1]; ++k){
	_is_shared_node[i][vs[index+2+k]-1] = 1;
      }
    }
  }
  mark_elems_from_nodes(_is_shared_node,_is_shared_elem);
}

void Pane_ghost_connectivity::
mark_elems_from_nodes(std::vector<std::vector<bool> > &marked_nodes,
		      std::vector<std::vector<bool> > &marked_elems){


  marked_elems.clear();
  marked_elems.resize(_npanes);

  //Loop through panes
  for(int i=0; i < (int)(_npanes); ++i){

    marked_elems[i].clear();
    marked_elems[i].resize(_panes[i]->size_of_real_elements(),false);

    // Loop through real elements.
    // Mark for quality check if they contain shared nodes.
    int s_real_elems = _panes[i]->size_of_real_elements();
    std::vector<int> nodes;
    for(int j=1; j<= s_real_elems; ++j){
      COM::Element_node_enumerator ene(_panes[i],j);
      ene.get_nodes(nodes);
      for(int k=0, nk=nodes.size(); k<nk; ++k){
	if (marked_nodes[i][nodes[k]-1])
	  marked_elems[i][j-1] = true;
      }
    }
  }
}

MAP_END_NAMESPACE






