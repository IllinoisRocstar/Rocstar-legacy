/// 
/// @file
/// @ingroup gridconversion_group
/// @brief Driver program for Grid Conversion.
/// @author Jessica Kress (jkress@illinoisrocstar.com)
/// @date 11/3/2015
/// 

#include "DriverProgram.H"
#include "Mesh.H"

namespace GridConversion {

  namespace DriverProgram {

    int SerialProgram::Run()
    {
      // FunctionEntry("NAME"): Updates the user-defined stack and 
      // the program profiles with timing information. The placement
      // of FunctionEntry and FunctionExit calls is at the developer's
      // discretion.  
      FunctionEntry("Run");

      // ---------- The Program -----------------
      // Drives the grid conversion process

      std::ostringstream Ostr;
      int error=0;        

      // Parse the input file
      error = ReadInput();
      if(error == 1){
        std::ostringstream ErrOstr;
        ErrOstr << "Error reading input file." << std::endl;
        StdOut(Ostr.str());
        Ostr.str("");
        ErrOut(ErrOstr.str());
        ErrOstr.str("");
        exit(1);
      }
  
 
      // Populate nodal coordinates object with nodes vector
      SolverUtils::Mesh::NodalCoordinates utilsNodes(numNodes,&nodes[0]);

      // Populate element connectivty with elems vector
      SolverUtils::Mesh::Connectivity utilsElems;
      utilsElems.AddElements(numElems, numNodesPerElem, elems);

      // Get a map of nodes to elements
      SolverUtils::Mesh::Connectivity nodesToElems;
      utilsElems.Sync();
      utilsElems.Inverse(nodesToElems,numNodes);

      // Populate domain connectivity with domain vectors
      SolverUtils::Mesh::Connectivity utilsDomains;
      for(int i=0; i < domains.size(); i++){
        utilsDomains.AddElement(domains[i]);
      }

      // Get a map of nodes to domains
      SolverUtils::Mesh::Connectivity nodesToDomains;
      utilsDomains.Sync();
      utilsDomains.Inverse(nodesToDomains,numNodes);

      // Print to check the nodes to elements
      if(verblevel > 3){
        Ostr << "Nodes to elements: " << std::endl;
        for(int i=0; i < nodesToElems.size(); i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < nodesToElems[i].size(); j++){
            Ostr << nodesToElems[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print to check the nodes to domains
      if(verblevel > 3){
        Ostr << "Nodes to domains: " << std::endl;
        for(int i=0; i < nodesToDomains.size(); i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < nodesToDomains[i].size(); j++){
            Ostr << nodesToDomains[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print the domain bcs to check
      if(verblevel > 3){
        Ostr << "domain bcs: " << std::endl;
        for(int i=0; i < domainBCs.size(); i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < domainBCs[i].size(); j++){
            Ostr << domainBCs[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print the domain bc values to check
      if(verblevel > 3){
        Ostr << "domain bc values: " << std::endl;
        for(int i=0; i < domainBCValues.size(); i++){
          Ostr << i+1 << ": " << std::endl;
          for(int j=0; j < domainBCValues[i].size(); j++){
            for(int k=0; k < domainBCValues[i][j].size(); k++){
              Ostr << "    " <<  domainBCValues[i][j][k] << " ";
            }
            Ostr << std::endl;
          }
        }
      }


      // Print the edges to check
      if(verblevel > 3){
        Ostr << "edges: " << std::endl;
        for(int i=0; i < edges.size(); i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < edges[i].size(); j++){
            Ostr << edges[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print the edge bcs to check
      if(verblevel > 3){
        Ostr << "edge bcs: " << std::endl;
        for(int i=0; i < edgeBCs.size(); i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < edgeBCs[i].size(); j++){
            Ostr << edgeBCs[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print the edge bc values to check
      if(verblevel > 3){
        Ostr << "edge bc values: " << std::endl;
        for(int i=0; i < edgeBCValues.size(); i++){
          Ostr << i+1 << ": " << std::endl;
          for(int j=0; j < edgeBCValues[i].size(); j++){
            for(int k=0; k < edgeBCValues[i][j].size(); k++){
              Ostr << "    " <<  edgeBCValues[i][j][k] << " ";
            }
            Ostr << std::endl;
          }
        }
      }
 
      // Populate the nodal bc vectors from the domain bc vectors
      nodeBCs.resize(numNodes);
      nodeBCValues.resize(numNodes);
      //loop over all domains
      for(int i=0; i < domains.size(); i++){
        // loop over all nodes in the domain
        for(int j=0; j < domains[i].size(); j++){
          int node;
          node = domains[i][j]-1;
          // nodes will get all bc flags for all the domains
          // they are on
          // loop over all the bc flags for this domain
          for(int k=0; k < domainBCs[i].size(); k++){
            bool newValue = true;
            // loop over all the bc flags for this node
            for(int l=0; l < nodeBCs[node].size(); l++){
              // if the node already has this bc & value skip it
              if(nodeBCs[node][l] == domainBCs[i][k]){
                for(int m=0; m < domainBCValues[i][k].size(); m++){
                  if(domainBCValues[i][k][m] != nodeBCValues[node][l][m])
                    break;
                  else if(m == domainBCValues[i][k].size()-1)
                    newValue = false;
                }
              }
            }// node bc flags
            // if it is a new flag add it and its values to 
            // the node's vectors
            if(newValue){
              nodeBCs[node].push_back(domainBCs[i][k]);
              nodeBCValues[node].push_back(domainBCValues[i][k]); 
            }//it was a new bc flag for the node
          }//domain bc flag loop
        }// loop over nodes in domain
      } //loop over all domains

      // Populate the nodal bc vectors from the edge bc vectors
      //loop over all edges
      for(int i=0; i < edges.size(); i++){
        //loop over all nodes
        for(int j=0; j < nodesToDomains.size(); j++){
          int onEdge=0;
          int node=j;

          //check to see if the node is on the domains of the edge
          for(int k=0; k < nodesToDomains[node].size(); k++){
            if(nodesToDomains[node][k] == edges[i][0] 
              || nodesToDomains[node][k] == edges[i][1])
              onEdge++;
          }

          // If the node is on the edge add the edge bcs to the node's bcs
          if(onEdge == 2){
            bool newValue = true;
            //loop over all the bc flags for this edge
            for(int k=0; k < edgeBCs[i].size(); k++){
              // loop over all the bc flags for this node
              for(int l=0; l < nodeBCs[node].size(); l++){
                // if the node already has this bc & value skip it
                if(nodeBCs[node][l] == edgeBCs[i][k]){
                  for(int m=0; m < edgeBCValues[i][k].size(); m++){
                    if(edgeBCValues[i][k][m] != nodeBCValues[node][l][m])
                      break;
                    else if(m == edgeBCValues[i][k].size()-1)
                      newValue = false;
                  }
                }
              }// node bc flags
              // if it is a new flag add it and its values to 
              // the node's vectors
              if(newValue){
                nodeBCs[node].push_back(edgeBCs[i][k]);
                nodeBCValues[node].push_back(edgeBCValues[i][k]); 
              }//it was a new bc flag for the node
            }//loop over the bcs for the edge 
          } //if the node is on the edge
        } //loop over all the nodes
      } //loop over all edges

      // Print the nodal bcs to check
      if(verblevel > 3){
        Ostr << "nodal bcs: " << std::endl;
        for(int i=0; i < numNodes; i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < nodeBCs[i].size(); j++){
            Ostr << nodeBCs[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print the nodal bc values to check
      if(verblevel > 3){
        Ostr << "nodal bc values: " << std::endl;
        for(int i=0; i < numNodes; i++){
          Ostr << i+1 << ": " << std::endl;
          for(int j=0; j < nodeBCValues[i].size(); j++){
            for(int k=0; k < nodeBCValues[i][j].size(); k++){
              Ostr << "    " << nodeBCValues[i][j][k] << " ";
            }
            Ostr << std::endl;
          }
        }
      }


      // Populate the element bc vectors from the nodal bc vectors
      elemBCs.resize(numNodes);
      elemBCValues.resize(numNodes);
      //loop over all nodes
      for(int i=0; i < numNodes; i++){
        // loop over all elements for the node
        for(int j=0; j < nodesToElems[i].size(); j++){
          int elem;
          elem = nodesToElems[i][j]-1;
          // elems will get all bc flags for all the nodes
          // they contain
          // loop over all the bc flags for this node
          for(int k=0; k < nodeBCs[i].size(); k++){
            bool newValue = true;
            // loop over all the bc flags for this elem
            for(int l=0; l < elemBCs[elem].size(); l++){
              // if the elem already has this bc & value skip it
              if(elemBCs[elem][l] == nodeBCs[i][k]){
                for(int m=0; m < nodeBCValues[i][k].size(); m++){
                  if(nodeBCValues[i][k][m] != elemBCValues[elem][l][m])
                    break;
                  else if(m == nodeBCValues[i][k].size()-1)
                    newValue = false;
                }
              }
            }// elem bc flags
            // if it is a new flag add it and its values to 
            // the elem's vectors
            if(newValue){
              elemBCs[elem].push_back(nodeBCs[i][k]);
              elemBCValues[elem].push_back(nodeBCValues[i][k]); 
            }//it was a new bc flag for the elem
          }//node bc flag loop
        }// loop over elements for the node
      } //loop over all nodes


      // Print the element bcs for a check
      if(verblevel > 3){
        Ostr << "elemBCs:" << std::endl;
        for(int i=0; i < numElems; i++){
          Ostr << i+1 << ": ";
          for(int j=0; j < elemBCs[i].size(); j++){
            Ostr << elemBCs[i][j] << " ";
          }
          Ostr << std::endl;
        }
      }

      // Print the element bc values for a check
      if(verblevel > 3){
        Ostr << "elem BC values:" << std::endl;
        for(int i=0; i < numElems; i++){
          Ostr << i+1 << ": " << std::endl;
          for(int j=0; j < elemBCValues[i].size(); j++){
            for(int k=0; k < elemBCValues[i][j].size(); k++){
              Ostr << "    " <<  elemBCValues[i][j][k] << " ";
            }
            Ostr << std::endl;
          }
        }
      }
      //Write the output file
      error = WriteOutput();
      if(error == 1){
        std::ostringstream ErrOstr;
        ErrOstr << "Error writing output file." << std::endl;
        StdOut(Ostr.str());
        Ostr.str("");
        ErrOut(ErrOstr.str());
        ErrOstr.str("");
        exit(1);
      }
      
      StdOut(Ostr.str());
      //
      // ---------- Program End -----------------
      

      // Update the stacker/profiler that we are exiting 
      // this function.
      FunctionExit("Run");
      // return 0 for success
      return(0);
    };
  };
};
