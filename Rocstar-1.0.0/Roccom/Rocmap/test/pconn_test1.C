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
#include "roccom.h"
#include <iostream>
#include <sstream>

using namespace std;


static int abs_val( int a ) {
	if (a < 0) {return (-a);}
	else {return a;}
}

static void add_ghost_nodes2D( int *pconn , int *new_pconn) {
	int ni = 8;
	int nj = 7;
	int nk = 5;
	int ghost_layers = 2;
	int num_com_panes = pconn[0];

	int indx = 0;

	indx++;

	std::vector< std::vector<int> > real2send(num_com_panes);
	std::vector< std::vector<int> > ghost2recv(num_com_panes);
	std::vector< std::vector<int> > shared_nodes(num_com_panes);
	std::vector< int > pane_info(num_com_panes);

	std::vector< std::vector<int> >::iterator s = shared_nodes.begin();
	std::vector< std::vector<int> >::iterator s_end = shared_nodes.end();

	for (int q=0; q< num_com_panes; q++){
		int id_num = pconn[indx];
		pane_info[q] = id_num;
		indx++;
		int count = pconn[indx];
		indx++;
		int val = indx + count;
		for ( ; indx < val; indx++){
		    (shared_nodes[q]).push_back(pconn[indx]);
		}
	}

	std::vector< std::vector<int> >::iterator p2 = real2send.begin();
	std::vector< std::vector<int> >::iterator g2 = ghost2recv.begin();
	std::vector< std::vector<int> >::iterator s2 = shared_nodes.begin();
	
	for ( ; s2 != s_end; p2++,g2++,s2++){
		int node1 = (*s2)[0];
		int node2 = (*s2)[1];
		int diff = abs_val(node1-node2);
		if ( diff == 1) {
		  if ( (node1 - ni*ghost_layers) < ni ) { //top
			//newnode = node1+ni
			int size = (*s2).size();
			for (int k=0; k < size; k++){
			  for (int m=0; m< ghost_layers; m++){
			    (*p2).push_back( (*s2)[k] + (m+1)*ni);
			    (*g2).push_back( (*s2)[k] - (m+1)*ni);
			  }
			}
		  } else if ( ni*(nj-ghost_layers) - node1 < ni){  //bottom
			//newnode = node1-ni
			int size = (*s2).size();
			for (int k=0; k < size; k++){
			  for (int m=0; m< ghost_layers; m++){
			    (*p2).push_back( (*s2)[k] - (m+1)*ni);
			    (*g2).push_back( (*s2)[k] + (m+1)*ni);
			  }
			}
		} else {
			  //error state
			COM_assertion_msg(false, "Should be top or bottom.");
		}

		} else if ( diff == ni ) {
		  if ( (node1 % ni) == (ni - ghost_layers)){ //right
			//newnode = node1-1
			int size = (*s2).size();
			for (int k=0; k < size; k++){
			  for (int m=0; m< ghost_layers; m++){
			    (*p2).push_back( (*s2)[k] - (m+1));
			    (*g2).push_back( (*s2)[k] + (m+1));
			  }
			}

		  } else if ( (node1 % ni) == (ghost_layers+1) ) { //left
			//newnode = node1+1
			int size = (*s2).size();
			for (int k=0; k < size; k++){
			  for (int m=0; m< ghost_layers; m++){
			    (*p2).push_back( (*s2)[k] + (m+1));
			    (*g2).push_back( (*s2)[k] - (m+1));
			  }
			}

		  } else {
			//error state
			COM_assertion_msg(false, "Should be right or left.");
		  }
		  }

		}

	int size = indx;

	//block 1
	for (int j=0; j<size; j++){
	    new_pconn[j] = pconn[j];
	}

	//block 2
	new_pconn[indx] = num_com_panes;
	indx++;
	for (int k=0; k< num_com_panes; k++){
	    new_pconn[indx] = pane_info[k]; //comm. pane ID
	    indx++;
	    new_pconn[indx] = real2send[k].size(); //num values to follow
	    indx++;
	    for (int m=0; m< real2send[k].size(); m++){
		new_pconn[indx] = real2send[k][m];
		indx++;
	    }
	}

	//block 3
	new_pconn[indx] = num_com_panes;
	indx++;
	for (int k=0; k< num_com_panes; k++){
	    new_pconn[indx] = pane_info[k]; //comm. pane ID
	    indx++;
	    new_pconn[indx] = ghost2recv[k].size(); //num values to follow
	    indx++;
	    for (int m=0; m< ghost2recv[k].size(); m++){
		new_pconn[indx] = ghost2recv[k][m];
		indx++;
	    }
	}
	//block 3
	return;
}

int main(int argc, char *argv[]) {

  int size = 11;

  int myPconn[size];
  myPconn[0] = 2;
  myPconn[1] = 2;
  myPconn[2] = 3;
  myPconn[3] = 36;
  myPconn[4] = 37;
  myPconn[5] = 38;
  myPconn[6] = 3;
  myPconn[7] = 3;
  myPconn[8] = 19;
  myPconn[9] = 27;
  myPconn[10] = 35;

  //int *pconn;
  int new_pconn[45];

  add_ghost_nodes2D( myPconn, new_pconn);
  for (int v=0; v < 45; v++){
	std::cerr << new_pconn[v] << std::endl;
  }

  return 0;
  COM_finalize();
}






