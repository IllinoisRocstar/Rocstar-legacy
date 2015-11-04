#include "DriverProgram.H"

namespace GridConversion{ namespace DriverProgram{

  int SerialProgram::ReadInput(){

      std::ostringstream Ostr;

      // Open the specified input file for reading
      Inf.open(input_name.c_str());
      if(!Inf){
        // If the input file failed to open, notify to
        // the error stream and return non-zero
        std::ostringstream ErrOstr;
        ErrOstr << "Could not open input file, '" 
             << input_name << "'.\n";
        StdOut(Ostr.str());
        Ostr.str("");
        ErrOut(ErrOstr.str());
        ErrOstr.str("");
        // don't forget to tell the profiler/stacker the
        // function is exiting.
        FunctionExit("Run");
        return(1);
      }

      std::string line;
      double valueD;
      int valueI;
      std::string valueS;
      std::stringstream ss;

      //Read Input file in Como (Patran) format from Gridgen
      std::getline(Inf,line);
      std::getline(Inf,line);
      std::getline(Inf,line);
      ss << line;
      ss >> valueD >> valueD >> valueD >> valueD >> numNodes >> numElems;
      std::getline(Inf,line);

      //Resize nodes vector
      nodes.resize(3*numNodes);

      //Print for check
      if(verblevel > 1){
        Ostr << "Number of nodes = " << numNodes << std::endl;
        Ostr << "Number of elements = " << numElems << std::endl;
      }

      for(int i=0; i < numNodes; i++){
        std::getline(Inf,line);
        std::getline(Inf,line);
        ss.clear();
        ss.str("");
        ss << line;
        ss >> nodes[3*i + 0] >> nodes[3*i + 1] >> nodes[3*i + 2];
        std::getline(Inf,line);
      }

      int shape;
      for(int i=0; i < numElems; i++){
        std::getline(Inf,line);
        ss.clear();
        ss.str("");
        ss << line;
        ss >> valueI >> valueI >> valueI;

        //Check that valueI is a valid element shape
        if(valueI <= 1 || valueI == 6 || valueI > 9){
          std::ostringstream ErrOstr;
          ErrOstr << "element shape value of " << valueI << " is invalid." << std::endl
                    << "Valid shape values:" << std::endl
                    << "  2: bar" << std::endl
                    << "  3: triangle" << std::endl
                    << "  4: quadrilateral" << std::endl
                    << "  5: tetrahedron" << std::endl
                    << "  7: triangular prism" << std::endl
                    << "  8: hexahedron" << std::endl
                    << "  9: pyramid" << std::endl;
          StdOut(Ostr.str());
          Ostr.str("");
          ErrOut(ErrOstr.str());
          ErrOstr.str("");
          return(1);
        }
        
        //Make sure the elements all have the same shape
        //For now we only support meshes with one shape of element
        //(this is what Rocfrac requires as well).
        if(i == 0){
          shape = valueI;
          //Print for check
          if(verblevel > 1)
            Ostr << "Element shape value " << shape << ": " << shapes[shape] << std::endl;
        }
        else{
          if(valueI != shape){
            std::ostringstream ErrOstr;
            ErrOstr << "Meshes must have only one element shape!" << std::endl
                 << "Element " << i << " has shape value " << valueI << " but "
                 << "previous elements have shape value "  << shape << std::endl; 
            StdOut(Ostr.str());
            Ostr.str("");
            ErrOut(ErrOstr.str());
            ErrOstr.str("");
            return(1);
          }
        }

        std::getline(Inf,line);
        ss.clear();
        ss.str("");
        ss << line;
        ss >> valueI;    

        //Check that valueI is a valid number of nodes per element
        if(valueI < 2 || valueI == 7 || valueI == 9 || valueI > 10){
          std::ostringstream ErrOstr;
          ErrOstr << "Error: " << valueI << " number of nodes per element is invalid." << std::endl
                    << "Valid number of nodes:" << std::endl
                    << "  2: bar" << std::endl
                    << "  3: triangle" << std::endl
                    << "  4: quadrilateral or tetrahedron" << std::endl
                    << "  5: pyramid" << std::endl
                    << "  6: triangular prism" << std::endl
                    << "  8: hexahedron" << std::endl
                    << " 10: tehtrahedron (higher order)" << std::endl;
          StdOut(Ostr.str());
          Ostr.str("");
          ErrOut(ErrOstr.str());
          ErrOstr.str("");
          return(1);
        }
 
        //Make sure the elements are all of the same type (number of nodes)
        //For now we only support meshes with one type of element
        //(this is what Rocfrac requires as well).
        if(i == 0){
          numNodesPerElem = valueI;
          //Print for check
          if(verblevel > 1)
            Ostr << "Number of nodes per element " << numNodesPerElem << "." << std::endl;
        }
        else{
          if(valueI != numNodesPerElem){
            std::ostringstream ErrOstr;
            ErrOstr << "Meshes must have elements with the same number of nodes!" << std::endl
                 << "Element " << i << " has " << valueI << " nodes but "
                 << "previous elements have "  << numNodesPerElem << " nodes." <<  std::endl; 
            StdOut(Ostr.str());
            Ostr.str("");
            ErrOut(ErrOstr.str());
            ErrOstr.str("");
            return(1);
          }
        }

        //Resize the elems array
        elems.resize(numElems*numNodesPerElem);

        //Read in the nodes for the element
        std::getline(Inf,line);
        ss.clear();
        ss.str("");
        ss << line;
        for(int j=0; j < numNodesPerElem; j++){
          ss >> elems[i*numNodesPerElem + j];
        }
      }

      //Close input file
      Inf.close();

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
      for(int i=0; i < numNodes; i++){
        ss << nodes[3*i + 0] << " " << nodes[3*i + 1] << " " << nodes[3*i + 2] << std::endl;
      }
     
      for(int i=0; i < numElems; i++){
        for(int j=0; j < numNodesPerElem; j++){
          ss << elems[i*numNodesPerElem + j] << " ";
        }
        ss << std::endl;
      }

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
 
  } //ReadInput function

}; //DriverProgram namespace
}; //GridConversion namespace
