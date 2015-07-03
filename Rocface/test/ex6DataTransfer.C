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
// $Id: ex1.C,v 1.20 2008/12/06 08:43:29 mtcampbe Exp $

#include "roccom.h"
#include <iostream>
#include <vector>
#include <cstdio>
#include <cassert>
#include <sstream>
#include <cmath>

COM_EXTERN_MODULE( Rocface);
COM_EXTERN_MODULE( Rocout);

void makeWindow(std::string name){

  COM_new_window(name.c_str());
  std::string solnName = name + ".soln";
  COM_new_attribute(solnName.c_str(), 'n', COM_DOUBLE, 3, "m/s");

  std::string compName = name + ".comp";
  COM_new_attribute(compName.c_str(), 'n', COM_DOUBLE, 3, "m/s");
  
}

void initUnstructuredMesh( std::vector<double> &coors, 
                           std::vector<int> &elmts, 
                           int nrow, int ncol, 
                           int rank, int nproc, int type,
                           int &nnodes,int &nelem) {
  
  // consider the processors as a 2*(nproc/2) grid
  int proc_col=nproc;
  if (nproc%2 ==0) {
    proc_col=nproc/2;
  }
  else {
    proc_col=nproc;
  }

  int row=rank/proc_col, col=rank%proc_col;

  const double width=100., length=100.;

  std::cout << "type = " << type << std::endl;

  if(type == 0){
    nnodes = (nrow*ncol + (nrow-1)*(ncol-1));
    nelem = 4*(nrow-1)*(ncol-1);
    elmts.resize(nelem*3);
  } else if(type == 1){
    nnodes = nrow*ncol;
    nelem = 2*(nrow-1)*(ncol-1);
    elmts.resize(nelem*3);
  } else {
    nnodes = nrow*ncol;
    nelem = (nrow-1)*(ncol-1);
    elmts.resize(nelem*4);
  }
  coors.resize(nnodes*3);

  for (int i=0; i<nrow; ++i) {
    for (int j=0; j<ncol; ++j) {
      coors[3*(i*ncol+j) + 0]=col*length+length/(ncol-1)*j;
      coors[3*(i*ncol+j) + 1]=row*width+width/(nrow-1)*i;
      coors[3*(i*ncol+j) + 2]=0;
    }
  }

  //generating the nodal coordinates
  double xSpacing = 0;
  if(ncol > 1) xSpacing =  width/(ncol-1);
  double ySpacing = 0;
  if(nrow > 1) ySpacing = length/(nrow-1);

  if (type == 0){
     for(int i=0; i < nrow-1; ++i){
        for(int j=0; j < ncol-1; ++j){
           int count = nrow*ncol;
           int elmidx = 4*(i*(ncol-1)+j);

           coors[3*(count+i*(ncol-1)+j) + 0] = xSpacing*j + xSpacing/2 + col*length; 
           coors[3*(count+i*(ncol-1)+j) + 1] = ySpacing*i + ySpacing/2 + row*width; 
           coors[3*(count+i*(ncol-1)+j) + 2] = 0;

           // Nodes per triangle element - 'bottom' triangle
           elmts[3*(elmidx) + 0]=i*ncol+j+1;
           elmts[3*(elmidx) + 1]=i*(ncol-1)+j+count+1;
           elmts[3*(elmidx) + 2]=i*ncol+j+2;

           // Nodes per triangle element - 'top' triangle
           elmts[3*(elmidx+1) + 0]=i*ncol+j+ncol+1;
           elmts[3*(elmidx+1) + 1]=i*ncol+j+ncol+2;
           elmts[3*(elmidx+1) + 2]=i*(ncol-1)+j+count+1;

           // Nodes per triangle element - 'left' triangle
           elmts[3*(elmidx+2) + 0]=i*ncol+j+1;
           elmts[3*(elmidx+2) + 1]=i*ncol+j+ncol+1;
           elmts[3*(elmidx+2) + 2]=i*(ncol-1)+j+count+1;

           // Nodes per triangle element - 'right' triangle
           elmts[3*(elmidx+3) + 0]=i*ncol+j+2;
           elmts[3*(elmidx+3) + 1]=i*(ncol-1)+j+count+1;
           elmts[3*(elmidx+3) + 2]=i*ncol+j+ncol+2;

       }
     }
  }

  if (type == 1){
     for(int i=0; i < nrow-1; ++i){
        for(int j=0; j < ncol-1; ++j){
           int elmidx = 2*(i*(ncol-1)+j);

           // Nodes per triangle element - 'bottom' triangle
           elmts[3*(elmidx) + 0]=i*ncol+j+1;
           elmts[3*(elmidx) + 1]=i*ncol+j+ncol+1;
           elmts[3*(elmidx) + 2]=i*ncol+j+2;

           // Nodes per triangle element - 'top' triangle
           elmts[3*(elmidx+1) + 0]=i*ncol+j+ncol+1;
           elmts[3*(elmidx+1) + 1]=i*ncol+j+ncol+2;
           elmts[3*(elmidx+1) + 2]=i*ncol+j+2;

       }
     }
  }

  if (type == 2){
     for (int i=0; i<nrow-1; ++i) {
       for (int j=0; j<ncol-1; ++j) {
           int elmidx = i*(ncol-1)+j;

           elmts[4*elmidx + 0]=i*ncol+j+1;
           elmts[4*elmidx + 1]=i*ncol+j+ncol+1;
           elmts[4*elmidx + 2]=i*ncol+j+ncol+2;
           elmts[4*elmidx + 3]=i*ncol+j+2;
       }
     }
  }
}

int main(int argc, char *argv[]){
  
  int comm_rank=0;
  int comm_size=1;
  int npanes=4;
  int nwindows=3;

  std::vector<std::string> windowNames(nwindows);
  std::vector< std::vector< std::vector<double> > >  coords(nwindows);
  std::vector< std::vector< std::vector<int> > >     elements(nwindows);
  std::vector< std::vector< std::vector<double> > >  soln(nwindows);
  std::vector< std::vector< std::vector<double> > >  comp(nwindows);

  if ( comm_rank == 0)
    std::cout << "Initializing Roccom and Rocface" << std::endl;
  COM_init( &argc, &argv);

  COM_set_profiling( 1);

  COM_LOAD_MODULE_STATIC_DYNAMIC( Rocface, "RFC");
  int RFC_clear    = COM_get_function_handle("RFC.clear_overlay");
  int RFC_read     = COM_get_function_handle( "RFC.read_overlay");
  int RFC_write    = COM_get_function_handle("RFC.write_overlay");
  int RFC_overlay  = COM_get_function_handle("RFC.overlay");
  int RFC_transfer = COM_get_function_handle( "RFC.least_squares_transfer");
  int RFC_interp   = COM_get_function_handle( "RFC.interpolate");
  int RFC_extrap   = COM_get_function_handle( "RFC.extrapolate");

  



  for(int i=0; i < nwindows; i++)
  {
    std::stringstream ss;
    ss << "Window" << i;
    ss >> windowNames[i];
    if ( comm_rank == 0)
      std::cout << "Creating window " << ss.str() << std::endl;
    makeWindow(windowNames[i]);
  }

  for(int i=0; i < nwindows; i++){
    std::string &windowName(windowNames[i]);

    int nrow, ncol;
    nrow = i+3;
    ncol = i+2;
    int meshType = i%3;
    int nnodes = 0;
    int nelems = 0;

    std::cout << windowName << ":" << std::endl;
    std::cout << nrow << " by " << ncol << std::endl;

    coords[i].resize(npanes);
    elements[i].resize(npanes);
    soln[i].resize(npanes);
    comp[i].resize(npanes);

    for(int j=0; j < npanes; j++){

      int pane_id = j+1;

      initUnstructuredMesh(coords[i][j],elements[i][j],
                           nrow,ncol,j,npanes,meshType,
                           nnodes,nelems);

      // std::cout << "coords[" << i << "][" << j << "].size() = " << coords[i][j].size() 
      //           << std::endl;
      soln[i][j].resize( coords[i][j].size() );
      comp[i][j].resize( coords[i][j].size() );

      for(int k=0; k < soln[i][j].size(); k++){
        soln[i][j][k] = coords[i][j][k];
        comp[i][j][k] = -1.0;
        // std::cout << "soln[" << i << "][" << j << "][" << k 
        //           << "] = " << coords[i][j][k] + 3.0 << std::endl;
      }

      COM_set_size( (windowName+".nc"), pane_id, nnodes);
      COM_set_array( (windowName+".nc"), pane_id, &coords[i][j][0]); 

      std::string connectivityName;
      connectivityName = ((meshType < 2) ? (windowName+".:t3:") : 
                          (windowName+".:q4:"));
      COM_set_size(connectivityName, pane_id, nelems);
      COM_set_array(connectivityName, pane_id, &elements[i][j][0]);

      COM_set_array((windowName+".soln"), pane_id, &soln[i][j][0]); 
      COM_set_array((windowName+".comp"), pane_id, &comp[i][j][0]); 

      COM_window_init_done(windowName.c_str());


      // Checking values
      
      // std::cout << "coords.size() = " << coords.size() << std::endl;
      // std::cout << "coords[i].size() = " << coords[i].size() << std::endl;
      // std::cout << "coords[i][j].size() = " << coords[i][j].size() << std::endl;
      // std::cout << "nnodes = " << nnodes << std::endl;

      // std::cout << "elements.size() = " << elements.size() << std::endl;
      // std::cout << "elements[i].size() = " << elements[i].size() << std::endl;
      // std::cout << "elements[i][j].size() = " << elements[i][j].size() << std::endl;
      // std::cout << "nelems = " << nelems << std::endl;
     
      // std::cout << "pane " << j << ":" << std::endl;
      // std::cout << "coords:" << std::endl;
  //     for(int k=0; k < nnodes; k++){
  //       std::cout << coords[i][j][3*k] << " " << coords[i][j][3*k+1] << " " 
  //                 << coords[i][j][3*k+2] << std::endl;
  //     }
  //     std::cout << "elements:" << std::endl;
  //     if(meshType == 0 || meshType == 1){
  //       for(int k=0; k < nelems; k++){
  //         std::cout << elements[i][j][3*k] << " " << elements[i][j][3*k+1] << " "
  //                   << elements[i][j][3*k+2] << std::endl;
  //       }
  //     }
  //     else if(meshType == 2){
  //       std::cout << "nelems = " << nelems << std::endl;
  //       for(int k=0; k < nelems; k++){
  //         std::cout << elements[i][j][4*k] << " " << elements[i][j][4*k+1] << " "
  //                   << elements[i][j][4*k+2] << " " << elements[i][j][4*k+3]
  //                   << std::endl;
  //       }
  //     }
  //     std::cout << "soln:" << std::endl;
  //     for(int k=0; k < nnodes; k++){
  //       std::cout << soln[i][j][3*k+0] << " " << soln[i][j][3*k+1] << " " 
  //                 << soln[i][j][3*k+2] << std::endl;
  //     }
  //     std::cout << "comp:" << std::endl;
  //     for(int k=0; k < nnodes; k++){
  //       std::cout << comp[i][j][3*k+0] << " " << comp[i][j][3*k+1] << " " 
  //                 << comp[i][j][3*k+2] << std::endl;
  //     }
  // std::cout << "line " << __LINE__ << std::endl;
    }//j Loop over panes 
  }//i Loop over windows

  // std::cout << "line " << __LINE__ << std::endl;

  int tri0_mesh  = COM_get_attribute_handle("Window0.mesh");
  int tri1_mesh  = COM_get_attribute_handle("Window1.mesh");
  int quad_mesh  = COM_get_attribute_handle("Window2.mesh");

  // Perform mesh overlay
  const char *format = "HDF";

  if( comm_size == 1){

    std::cout << "Overlaying meshes..." << std::endl;

    // COM_call_function( RFC_overlay, &tri0_mesh, &tri0_mesh);
    // COM_call_function( RFC_write,   &tri0_mesh, &tri0_mesh, "tri0", "tri_0", format);
    // COM_call_function( RFC_clear,   "Window0", "Window0");
    
    COM_call_function( RFC_overlay, &tri0_mesh, &tri1_mesh);
    COM_call_function( RFC_write,   &tri0_mesh, &tri1_mesh, "tri01", "tri10", format);
    COM_call_function( RFC_clear,   "Window0", "Window1");

    COM_call_function( RFC_overlay, &tri0_mesh, &quad_mesh);
    COM_call_function( RFC_write,   &tri0_mesh, &quad_mesh, "tri02", "quad20", format);
    COM_call_function( RFC_clear,   "Window0", "Window2");

    COM_call_function( RFC_overlay, &tri1_mesh, &quad_mesh);
    COM_call_function( RFC_write,   &tri1_mesh, &quad_mesh, "tri12", "quad21", format);
    COM_call_function( RFC_clear,   "Window1", "Window2");
    
  }

  //COM_call_function( RFC_read, &tri0_mesh, &tri0_mesh, NULL, "tri0", "tri_0", format);
  //COM_call_function( RFC_read, &tri0_mesh, &quad_mesh, NULL, "tri0", "quad", format);
  //COM_call_function( RFC_read, &tri1_mesh, &quad_mesh, NULL, "tri1", "quad", format);

  int tri0_soln = COM_get_attribute_handle( "Window0.soln");
  int tri1_soln = COM_get_attribute_handle( "Window1.soln");
  int quad_soln = COM_get_attribute_handle( "Window2.soln");
  
  int tri0_comp = COM_get_attribute_handle( "Window0.comp");
  int tri1_comp = COM_get_attribute_handle( "Window1.comp");
  int quad_comp = COM_get_attribute_handle( "Window2.comp");
  
  COM_call_function( RFC_read, &tri0_mesh, &tri1_mesh, NULL, "tri01", "tri10", format);

  COM_call_function( RFC_transfer, &tri1_soln, &tri0_comp);
  int check_id = 0;
  std::vector<std::vector< double > >::iterator paneIt = comp[check_id].begin();
  std::vector<std::vector< double > >::iterator paneIt2 = coords[check_id].begin();
  bool pass = true;
  double compTol = 1e-5;
  while(paneIt != comp[check_id].end()){
    std::vector<double> &paneComp(*paneIt++);
    std::vector<double> &paneCoords(*paneIt2++);
    std::vector<double>::iterator pcIt = paneComp.begin();
    std::vector<double>::iterator pcIt2 = paneCoords.begin();
    while(pcIt != paneComp.end()){
      double solnDiff = std::fabs(*pcIt - *pcIt2);
      if(solnDiff > compTol){
        pass = false;
        std::cout << *pcIt << " != " << *pcIt2 
                  << " (" << solnDiff << ")" << std::endl;
      }
      *pcIt++ = -1;
      pcIt2++;
    }
  }
  std::cout << "Nodal transfer from Triangles 1 to Triangles 0 "
            << (pass ? "passed" : "failed") << "." << std::endl;

  COM_call_function( RFC_transfer, &tri0_soln, &tri1_comp);
  check_id = 1;
  paneIt = comp[check_id].begin();
  paneIt2 = coords[check_id].begin();
  pass = true;
  while(paneIt != comp[check_id].end()){
    std::vector<double> &paneComp(*paneIt++);
    std::vector<double> &paneCoords(*paneIt2++);
    std::vector<double>::iterator pcIt = paneComp.begin();
    std::vector<double>::iterator pcIt2 = paneCoords.begin();
    while(pcIt != paneComp.end()){
      double solnDiff = std::fabs(*pcIt - *pcIt2);
      if(solnDiff > compTol){
        pass = false;
        std::cout << *pcIt << " != " << *pcIt2 
                  << " (" << solnDiff << ")" << std::endl;
      }
      *pcIt++ = -1;
      pcIt2++;
    }
  }
  std::cout << "Nodal transfer from Triangles 0 to Triangles 1 "
            << (pass ? "passed" : "failed") << "." << std::endl;

  COM_call_function( RFC_clear,   "Window0", "Window1");  

  COM_call_function( RFC_read, &tri0_mesh, &quad_mesh, NULL, "tri02", "quad20", format);
  COM_call_function( RFC_transfer, &tri0_soln, &quad_comp);
  check_id = 2;
  paneIt = comp[check_id].begin();
  paneIt2 = coords[check_id].begin();
  pass = true;
  while(paneIt != comp[check_id].end()){
    std::vector<double> &paneComp(*paneIt++);
    std::vector<double> &paneCoords(*paneIt2++);
    std::vector<double>::iterator pcIt = paneComp.begin();
    std::vector<double>::iterator pcIt2 = paneCoords.begin();
    while(pcIt != paneComp.end()){
      double solnDiff = std::fabs(*pcIt - *pcIt2);
      if(solnDiff > compTol){
        pass = false;
        std::cout << *pcIt << " != " << *pcIt2 
                  << " (" << solnDiff << ")" << std::endl;
      }
      *pcIt++ = -1;
      pcIt2++;
    }
  }
  std::cout << "Nodal transfer from Triangles 0 to Quads "
            << (pass ? "passed" : "failed") << "." << std::endl;

  COM_call_function( RFC_transfer, &quad_soln, &tri0_comp);
  check_id = 0;
  paneIt = comp[check_id].begin();
  paneIt2 = coords[check_id].begin();
  pass = true;
  while(paneIt != comp[check_id].end()){
    std::vector<double> &paneComp(*paneIt++);
    std::vector<double> &paneCoords(*paneIt2++);
    std::vector<double>::iterator pcIt = paneComp.begin();
    std::vector<double>::iterator pcIt2 = paneCoords.begin();
    while(pcIt != paneComp.end()){
      double solnDiff = std::fabs(*pcIt - *pcIt2);
      if(solnDiff > compTol){
        pass = false;
        std::cout << *pcIt << " != " << *pcIt2 
                  << " (" << solnDiff << ")" << std::endl;
      }
      *pcIt++ = -1;
      pcIt2++;
    }
  }
  std::cout << "Nodal transfer from Quads to Triangles 0 "
            << (pass ? "passed" : "failed") << "." << std::endl;

  COM_call_function( RFC_clear,   "Window0", "Window2");  

  COM_call_function( RFC_read, &tri1_mesh, &quad_mesh, NULL, "tri12", "quad21", format);
  COM_call_function( RFC_transfer, &tri1_soln, &quad_comp);
  check_id = 2;
  paneIt = comp[check_id].begin();
  paneIt2 = coords[check_id].begin();
  pass = true;
  while(paneIt != comp[check_id].end()){
    std::vector<double> &paneComp(*paneIt++);
    std::vector<double> &paneCoords(*paneIt2++);
    std::vector<double>::iterator pcIt = paneComp.begin();
    std::vector<double>::iterator pcIt2 = paneCoords.begin();
    while(pcIt != paneComp.end()){
      double solnDiff = std::fabs(*pcIt - *pcIt2);
      if(solnDiff > compTol){
        pass = false;
        std::cout << *pcIt << " != " << *pcIt2 
                  << " (" << solnDiff << ")" << std::endl;
      }
      *pcIt++ = -1;
      pcIt2++;
    }
  }
  std::cout << "Nodal transfer from Triangles 1 to Quads "
            << (pass ? "passed" : "failed") << "." << std::endl;

  COM_call_function( RFC_transfer, &quad_soln, &tri1_comp);
  check_id = 1;
  paneIt = comp[check_id].begin();
  paneIt2 = coords[check_id].begin();
  pass = true;
  while(paneIt != comp[check_id].end()){
    std::vector<double> &paneComp(*paneIt++);
    std::vector<double> &paneCoords(*paneIt2++);
    std::vector<double>::iterator pcIt = paneComp.begin();
    std::vector<double>::iterator pcIt2 = paneCoords.begin();
    while(pcIt != paneComp.end()){
      double solnDiff = std::fabs(*pcIt - *pcIt2);
      if(solnDiff > compTol){
        pass = false;
        std::cout << *pcIt << " != " << *pcIt2 
                  << " (" << solnDiff << ")" << std::endl;
      }
      *pcIt++ = -1;
      pcIt2++;
    }
  }
  std::cout << "Nodal transfer from Quads to Triangles 1 "
            << (pass ? "passed" : "failed") << "." << std::endl;

  COM_call_function( RFC_clear,   "Window1", "Window2");  

  for(int i=0; i < nwindows; i++){
      COM_delete_window(windowNames[i]);
  }
  COM_finalize();

  return 0;
}
