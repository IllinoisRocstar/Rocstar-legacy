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
// $Id: readsdv.C,v 1.5 2008/12/06 08:43:29 mtcampbe Exp $

// This file demonstrates how to read in a sdv file.
#include "roccom.h"
#include <vector>
#include <iostream>

extern "C" void Rocin_load_module(const char *);
extern "C" void Rocin_unload_module(const char *);

struct Subdiv {
  std::vector<int>   subfaces;             // Connectivity of subfaces

  std::vector<int>   subnode_parents;      // Parent face ID
  std::vector<float> subnode_ncs;          // Natural coordinates in parent face
  std::vector<int>   subnode_subIDs;       // Sub-node ID of input nodes
  std::vector<int>   subnode_counterparts; // Sub-node ID in other window

  std::vector<int>   subface_parents;      // Parent face ID
  std::vector<float> subface_nat_coors;    // Natural coors. of subnodes of susface
  std::vector<int>   subface_offsets;      // Smallest sub-face ID in input face
  std::vector<int>   subface_counterparts; // Sub-face ID in other window
};

// A sample program for reading an output file of Rocface for a single pane.
// Arguments: fname: the file name of the HDF file
//            prefix: is the window name (also the base material name) in HDF files
//            pid: the pane ID of the window.
//            sd:  the output structure contained in an Subdiv object.
void read_pane_sp( std::string &fname, 
		   const std::string &prefix, 
		   int pid,
		   Subdiv &sd) {

  // Load Rocin into Roccom
  Rocin_load_module( "SDV_IN");

  // Obtain function handles to Rocin functions
  int hdl_read = COM_get_function_handle( "SDV_IN.read_windows");
  int hdl_obtain = COM_get_function_handle( "SDV_IN.obtain_attribute");

  // Define the base-window and sdv-window names
  std::string bufprefix = "__BUF";
  std::string sdv_material = prefix+"_sdv";
  std::string sdv_wname = bufprefix+sdv_material;

  // Read the pane from the given file. Note that the file contains both
  // the parent and the subdivided windows. Read only the subdivided one.
  MPI_Comm comm_self = MPI_COMM_SELF;
  COM_call_function( hdl_read, fname.c_str(), bufprefix.c_str(), 
		     sdv_material.c_str(), &comm_self);
  int hdl_all = COM_get_attribute_handle( (sdv_wname+".all").c_str());
  COM_call_function( hdl_obtain, &hdl_all, &hdl_all, &pid);

  // Obtain number of sub-nodes, sub-faces, nodes, and faces
  int nsubn, nsubf, nn, nf;
  COM_get_size( (sdv_wname+".sn_parent_fcID").c_str(), pid, &nsubn);
  COM_get_size( (sdv_wname+".:t3").c_str(), pid, &nsubf);

  COM_get_size( (sdv_wname+".sn_subID").c_str(), pid, &nn);
  COM_get_size( (sdv_wname+".sf_offset").c_str(), pid, &nf);

  // Obtain the connectivity
  sd.subfaces.resize( 3*nsubf);
  COM_copy_array( (sdv_wname+".:t3").c_str(), pid, &sd.subfaces[0], 3);
  // NOTE: The last argument (3) indicates that three sub-node IDs are stored
  // consecutively for each sub-face. Use the number one (1) if the first 
  // sub-nodes of all sub-faces are stored together followed by the second 
  // sub-nodes and then third.


  // Obtain subnode_parents, subnode_ncs, and subnode_counterparts
  sd.subnode_parents.resize( nsubn);
  COM_copy_array( (sdv_wname+".sn_parent_fcID").c_str(),
		  pid, &sd.subnode_parents[0]);

  sd.subnode_ncs.resize( 2*nsubn);
  COM_copy_array( (sdv_wname+".sn_parent_ncs").c_str(), pid, &sd.subnode_ncs[0], 2);
  // NOTE: The last argument (2) indicates that the two local coordinates are 
  // stored consecutively. Use the number one (1) if the first local parameter 
  // (xi) of all sub-nodes are stored together followed by the second one (eta).

  sd.subnode_subIDs.resize( nn);
  COM_copy_array( (sdv_wname+".sn_subID").c_str(), pid, &sd.subnode_subIDs[0]);
  
  sd.subnode_counterparts.resize( nsubn);
  COM_copy_array( (sdv_wname+".sn_cntpt_ndID").c_str(),
		  pid, &sd.subnode_counterparts[0]);

  sd.subface_parents.resize( nsubf);
  COM_copy_array( (sdv_wname+".sf_parent").c_str(), pid, &sd.subface_parents[0]);
    
  sd.subface_nat_coors.resize( nsubf*6);
  COM_copy_array( (sdv_wname+".sf_ncs").c_str(), pid, 
		  &sd.subface_nat_coors[0], 6);
  // NOTE: The last argument (6) indicates that the local coordinates are 
  // stored consecutively (xi1, eta1, xi2, eta2, xi3, eta3). Use the number 
  // one (1) if the xi1 for all nodes are stored together and then xi2, etc..

  // Obtain subface_offsets
  sd.subface_offsets.resize( nf);
  COM_copy_array( (sdv_wname+".sf_offset").c_str(), pid, &sd.subface_offsets[0]);

  sd.subface_counterparts.resize( nsubf);
  COM_copy_array( (sdv_wname+".sf_cntpt_fcID").c_str(), pid, 
		  &sd.subface_counterparts[0]);

  // Delete the window created by Rocin.
  COM_delete_window( sdv_wname.c_str());

  // Unload Rocin from Roccom.
  Rocin_unload_module( "SDV_IN");
}

int main(int argc, char *argv[]) {
  COM_init( &argc, &argv);

  if ( argc<3) {
    std::cerr << "Usage: " << argv[0] << " <file-name> <prefix> <pane_id>\n" 
	      << "Example: " << argv[0] << "A_101_sdv.hdf A 101" << std::endl;
    exit(-1);
  }

  std::string fname = argv[1], prefix = argv[2];
  int pid = std::atoi(argv[3]);

  // Reading the given pane from the file into the structure.
  Subdiv sd;
  read_pane_sp( fname, prefix, pid, sd);

  COM_finalize();
}






