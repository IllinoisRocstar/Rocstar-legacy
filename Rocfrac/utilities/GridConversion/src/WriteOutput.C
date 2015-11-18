#include <iomanip>
#include "DriverProgram.H"

namespace GridConversion{ namespace DriverProgram{

  int SerialProgram::WriteOutput(){

      std::ostringstream Ostr;
      std::stringstream ss;

      // Open the specified output file for writing
      bool use_outfile = false;
      if(!output_name.empty()){
        use_outfile = true;
        Ouf.open(output_name.c_str());
        if(!Ouf){
          // If the output file failed to open, notify
          // to error stream and return non-zero
          std::ostringstream ErrOstr;
          ErrOstr << "Error: Unable to open output file, " << output_name << ".";
          StdOut(Ostr.str());
          Ostr.str("");
          ErrOut(ErrOstr.str());
          ErrOstr.str("");
          // don't forget to tell the profiler/stacker the
          // function is exiting.
          FunctionExit("Run");
          return(1);
        }
      }

      // Write mesh info to output file or screen
      ss.clear();
      ss.str("");

      //Write Patran packet 25 Intro (unused) 
      ss << "unused line" << std::endl;
      ss << "unused line" << std::endl;
      //Write Patran packet 26 
      ss << "26 1 1 1 " << numNodes << " " << numElems << " 1 1 1" << std::endl;
      ss << "unused line" << std::endl;

      //Write Patran packet 1 node coordinates
      for(int i=0; i < numNodes; i++){
        ss << "1 " << i+1 << std::endl
           << std::setprecision(9) << std::scientific
           << nodes[3*i + 0] << " " 
           << nodes[3*i + 1] << " "
           << nodes[3*i + 2] << std::endl
           << "unused line" << std::endl;
      }
    
      //Write Patran packet 2 element connectivities 
      for(int i=0; i < numElems; i++){
        ss << "2 " << i+1 << " " << elemShape << std::endl
           << numNodesPerElem << " 1 1" << std::endl;
        for(int j=0; j < numNodesPerElem; j++){
          ss << elems[i*numNodesPerElem + j] << " ";
        }
        ss << std::endl;
      }

      //Write Patran packet 8 (all nodes with this boundary type)
      for(int i=0; i < numNodes; i++){
        for(int k=0; k < nodeBCs[i].size(); k++){
          if(nodeBCs[i][k] == 8){
            ss << "8 " << i+1 << " 1 2" << std::endl;
            ss << "0";
            for(int j=0; j < 3; j++)
              ss << int(nodeBCValues[i][k][j]);
            ss << "000" <<  std::endl;
            for(int j=3; j < 6; j++){
              if(nodeBCValues[i][k][j-3] != 0)
                ss << nodeBCValues[i][k][j] << " ";
            }
            ss << std::endl;
          }
        }
      }
   

      //Write Patran packet 6 (all elements with this boundary type)
      //loop over all the elements
      for(int i=0; i < numElems; i++){ 
        //loop over the boundary flags for that element
        for(int j=0; j < elemBCs[i].size(); j++){ 
          if(elemBCs[i][j] == 6){
            ss << "6 " << i+1 << " 1 2 0 0 0 0 0" << std::endl;
            ss << "111100000";
            //loop over the nodes for that element
            for(int k=0; k < numNodesPerElem; k++){
              bool onBC=false;
              int node = elems[i*numNodesPerElem + k]-1;
              //loop over the bc flags for that node
              for(int l=0; l < nodeBCs[node].size(); l++){
                if(nodeBCs[node][l] == 6){
                  //check if the bcs have the same values
                  for(int m=0; m < nodeBCValues[node][l].size(); m++){
                    if(nodeBCValues[node][l][m] != elemBCValues[i][j][m])
                      break;
                    else if(m == nodeBCValues[node][l].size()-1)
                      onBC=true;
                  }
                }
              }//loop over node bcs
              if(onBC)
                ss << "1";
              else
                ss << "0";
            }//loop over nodes for the element
            ss << std::endl;
            for(int m=0; m < elemBCValues[i][j].size(); m++)
              ss << elemBCValues[i][j][m] << " ";
            ss << std::endl;
          }//if it is bc type 6
        }//loop over bc flags for element
      }//loop over all elements
      ss << "99";

      if(use_outfile){
        Ouf << ss.str();
      }  
      else
        StdOut(ss.str());
      // Close output file
      Ouf.close();
 
      StdOut(Ostr.str());
      Ostr.str("");

    return 0;
 
  } //ReadOutput function

}; //DriverProgram namespace
}; //GridConversion namespace
