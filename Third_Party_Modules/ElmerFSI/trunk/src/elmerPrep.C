///
/// @file
/// @ingroup solverModule_group
/// @brief Main function to call the Elmer SolverModule
/// @author Masoud Safdari (msafdari@illinoisrocstar.com)
/// @date August 4, 2016
/// 
///
#include "com.h"
#include "com_devel.hpp"
#include <iostream>
#include <cstring>
#include <cstdlib>
#include <stdlib.h>
#include <sstream>
#include "primitive_utilities.H"
#include "SolverAgent.H"
#include "Mesh.H"
#include "InterfaceLayer.H"
#include "SolverModuleDriverParallel.H"
#include "COMM.H"
#include <algorithm>


COM_EXTERN_MODULE( ElmerCSCParallel);
COM_EXTERN_MODULE(SimOut);


namespace COM {
    // typedef for CommunicatorObject
    typedef IRAD::Comm::CommunicatorObject CommType;
    
    void SolverModuleDriver::usage(char *exec){
      std::cout << std::endl 
                << "Usage: " << std::endl << std::endl
                << "  " << exec << "   [-com-mpi]  [-fsi] [-loads] time[0] [time[1]   ...   time[Final]]" 
                << std::endl
                << "   where at least  timeFinal is required. Here is the description of switches :" 
                << std::endl << std::endl
                << " -com-mpi:" << std::endl
                << "   It is not required to use the -com-mpi flag." << std::endl << std::endl
                << " -fsi :" << std::endl
                << "   Should be used for fluid-solid interaction (FSI) problems to register" << std::endl
                << "   mesh and solution for each given timestep. A vtk file will be created" << std::endl
                << "   for each timestep containing FSI surface mesh and solution values." << std::endl 
                << "   It is assumed that all processes share FSI boundaries." << std::endl<<std::endl
                << " -prefix :" << std::endl
                << "   the prefix to be used for SurfX reference" << std::endl;
       std::exit(1);
    }
           
    int SolverModuleDriver::init(int argc, char *argv[]){
      std::cout << "Elmerprep: running initializer ... " << std::endl;
      // processing command-line flags
      std::string arg;    
      double tmpTime;
      changeLoads = false;
      if(argc > 1){
        for(int i=1; i < argc; i++){
            ss.clear();
            ss.str("");
            ss << argv[i];
            if(ss.str() == "-com-mpi")
              continue;
            if(ss.str() == "-fsi"){
              isFSI = true;
              continue;
            }
            if(ss.str() == "-prefix"){
              prefix = argv[++i];
              continue;
            }
            for(int j=0; j<ss.str().size(); j++){
              if(!isdigit(ss.str()[j]) && ss.str()[j] != 'e' 
                 && ss.str()[j] != 'E' && ss.str()[j] != '-'
                 && ss.str()[j] != '.')
                usage(argv[0]);
            }            
            // we dont care about time in Elmerprep
            //ss >> tmpTime;
            //tNext.push_back(tmpTime);
        }
      }      
      else
        usage(argv[0]);
    
      // loading ElmerParallel module
      COM_LOAD_MODULE_STATIC_DYNAMIC(ElmerCSCParallel, "ELMModule");
      
      // loading SimOut on all processes
      COM_LOAD_MODULE_STATIC_DYNAMIC( SimOUT, "OUT");
      
      // check the communicator that the window was loaded on
      MPI_Comm comm_check;
      std::cout << "Checking ELMModule" << std::endl;
      COM_get_communicator("ELMModule", &comm_check);
      if(comm_check == Comm)
        std::cout << "comm_check == Comm!" << std::endl;
      else if(comm_check == MPI_COMM_WORLD)
        std::cout << "comm_check == MPI_COMM_WORLD!" << std::endl;
      else if(comm_check == MPI_COMM_SELF)
        std::cout << "comm_check == MPI_COMM_SELF!" << std::endl;
      else
        std::cout << "comm_check == None!" << std::endl;

      // calling Elmer module initializer     
      // it will load data on the window pane 100 + rank 
      // by default, should be tailored if needed
      if(color == 0){ 
        /// get the handle for the initialize function and call it
        int init_handle = COM_get_function_handle("ELMModule.InitElmerprep");
        bool init_func = (init_handle > 0);
        int verb=3;
        runs = 0;
        if(init_func)
           COM_call_function(init_handle, &runs, &verb);
      }

      // FSI problems will register mesh data, trying to access them  
      if(isFSI){ 
         char getDataItemLoc;
         COM_Type getDataItemType;
         std::string getDataItemUnits;
         // getting node coordinates  
         COM_get_array("srf.nc", myPaneId, &Coord);
         // check for expected number of nodes
         COM_get_size("srf.nc", myPaneId, &nNodes);
         // get connectivity tables for paneIds
         // :q4: is quad element
         std::string stringNames;
         COM_get_connectivities("srf", myPaneId, &nConn, stringNames);
         std::istringstream ConnISS(stringNames);
         std::vector<std::string> connNames;
         for (int i=0; i<nConn; ++i) {
            std::string name;
            ConnISS >> name;
            connNames.push_back(name);
            std::cout << "Rank #" << myRank
                      << ", Elmerprep:init: Connectivity Table # " << i+1 << ": " << name << std::endl;
         }
         // number of nodes per element
         std::string fullConnName("srf."+connNames[0]);
         COM_get_dataitem(fullConnName, &getDataItemLoc, &getDataItemType,
                          &nElemNodes, &getDataItemUnits);
         std::cout << "Rank #" << myRank
                   << ", Elmerprep:init: getDataItemLoc " << getDataItemLoc << std::endl;
         std::cout << "Rank #" << myRank
                   << ", Elmerprep:init: getDataItemType " << getDataItemType << std::endl;
         std::cout << "Rank #" << myRank
                   << ", Elmerprep:init: nElemNodes " << nElemNodes << std::endl;
         std::cout << "Rank #" << myRank
                   << ", Elmerprep:init: getDataItemUnits " << getDataItemUnits << std::endl;
         // get connectivities        
         COM_get_array(fullConnName.c_str(), myPaneId, &Conn);
         COM_get_size(fullConnName, myPaneId, &nElem);
      }
      
      return 0;
    }
    
    int SolverModuleDriver::finalize(){
      if(color == 0){
        /// Get the handle for the finalize function and call it
        int final_handle = COM_get_function_handle("ELMModule.Finalize");
        bool final_func = (final_handle > 0);
        runs = 0;
        if(final_func){
          COM_call_function(final_handle,&runs);
        }
      }
      COM_UNLOAD_MODULE_STATIC_DYNAMIC( ElmerCSCParallel, "ELMModule");
      COM_UNLOAD_MODULE_STATIC_DYNAMIC(SimOUT, "OUT");
      return 0;

    }

    int parallelProgram(int argc, char *argv[]){
         int parErr;
         std::cout <<"Elmeprep:Main: Setting up parallel communicator..." << std::endl;

         // initializing COM 
         COM_init( &argc, &argv);
         MPI_Comm masterComm = COM_get_default_communicator();
         int nproc, rank;
         int color = 0;
         MPI_Comm_size(masterComm, &nproc);
         MPI_Comm_rank(masterComm, &rank);
         std::cout << "Elmerprep:Main:Rank #"
                   << rank
                   << " is ready."
                   << std::endl;

         // instantiating solver driver object
         SolverModuleDriver driverObject;
         driverObject.setComm(masterComm);
         driverObject.setRank(rank);
         driverObject.myPaneId=100+rank;

         // checking streamer process
         if (driverObject.isStreamer())
            std::cout << "Rank #" << rank 
                      << " will continue to stream to output." << std::endl;
         
         // setting color for the driver object
         driverObject.setColor(color);

         // calling initializer, for FSI problems mesh and solution
         // related datastructures will be automatically registered
         driverObject.init(argc, argv);

         
         // get information about what was registered in this window
         int numDataItems=0;
         std::string output;
         COM_get_dataitems("srf", &numDataItems, output);
         std::istringstream Istr(output);
         std::vector<std::string> dataItemNames;  
         for (int i=0; i<numDataItems; ++i) {
           std::string name;
           Istr >> name;
           dataItemNames.push_back(name);
           if (driverObject.isStreamer())
	      std::cout << "Rank #" << rank
			<< ", Elmerprep:main: DataItem # " << i << ": " << name << std::endl;
         }

         // list of panes for this process: each process creates a single pane
         // with paneId = 100 + rank
         int numPanes;
         int* paneList;
         COM_get_panes("srf", &numPanes, &paneList);
         std::cout << "Rank #" << rank
                   << ", Elmerprep:main: Number of Panes " << numPanes << std::endl;
         for (int i=0; i<numPanes; ++i)
           std::cout << "Rank #" << rank
                     << ", Elmerprep:main: Pane ID # " << i+1 << "=" << paneList[i] << std::endl;
       
         /* 
         // see if all processors see eachothers panes
         std::vector<std::vector<int> > paneIds(8);
         COM_get_panes("srf", paneIds[rank], -1);
         std::cout << "Rank #" << rank 
                   << ", Number of panes = "
                   << paneIds[rank].size() << std::endl;
         for (int ii; ii < paneIds[rank].size(); ii++)
             std::cout << "Rank #" << rank
                       << ", I see paneId = " << paneIds[rank][ii]
                       << std::endl;
         */ 

         MPI_Barrier(masterComm);
         // write window and rocin files, only for FSI problems 
         if(driverObject.isFSISim()){
            // writing interface window to HDF
            int OUT_set = COM_get_function_handle( "OUT.set_option");
            int OUT_write = COM_get_function_handle( "OUT.write_dataitem");
            int OUT_rocin_write = COM_get_function_handle("OUT.write_rocin_control_file");
            const char *win_out= "srf";
            std::string win_out_pre(win_out);
            win_out_pre.append(".");
            int OUT_all = COM_get_dataitem_handle((win_out_pre+"all").c_str());
            char tmp[10];
            std::sprintf(tmp,"%4.4d",rank);
            std::string fileOut;
            fileOut = "./Elmer/Rocin/"+ driverObject.prefix  + "." + tmp;
            COM_call_function( OUT_set, "mode", "w");
            COM_call_function( OUT_set, "rankwidth", "0");
            COM_call_function( OUT_set, "format", "HDF4");
            COM_call_function( OUT_write, (fileOut + ".hdf").c_str(), &OUT_all, "solid",
                            "00.000000");

            // write Rocin control file
            // using Rocout
            //fileOut = "Elmer/Rocin/"+ driverObject.prefix  + "." + tmp;
            //COM_call_function( OUT_rocin_write, "srf", fileOut.c_str(), 
            //                   "../Rocin/isolid_in_00.000000.txt");
            // manually
            std::ofstream fout("./Elmer/Rocin/isolid_in_00.000000.txt");
            COM_assertion_msg(fout.is_open(),
                     (std::string("Elmerprep: cannot open control file for interface data")).c_str());
            fout << "@Proc: *" << std::endl;
            fout << "@Files: Elmer/Rocin/" 
                 << driverObject.prefix
                 << ".\%4p.hdf"
                 << std::endl;
            fout << "@Panes: @Cyclic 100";
            fout.close();

            // writing volume window to HDF
            win_out= "vol";
            win_out_pre = win_out;
            win_out_pre.append(".");
            OUT_all = COM_get_dataitem_handle((win_out_pre+"all").c_str());
            std::sprintf(tmp,"%4.4d",rank);
            fileOut = "./Elmer/Rocin/"+ driverObject.prefix  + "_vol." + tmp;
            COM_call_function( OUT_set, "mode", "w");
            COM_call_function( OUT_set, "rankwidth", "0");
            COM_call_function( OUT_set, "format", "HDF4");
            COM_call_function( OUT_write, (fileOut + ".hdf").c_str(), &OUT_all, "solid",
                            "00.000000");
            // write Rocin control file
            //fileOut = "Elmer/Rocin/"+ driverObject.prefix  + "_vol." + tmp;
            //COM_call_function( OUT_rocin_write, "vol", fileOut.c_str(), 
            //                   "../Rocin/solid_in_00.000000.txt");
            // manually
            fout.open("./Elmer/Rocin/solid_in_00.000000.txt");
            COM_assertion_msg(fout.is_open(),
                      (std::string("Elmerprep: cannot open control file for volumetric data")).c_str());
            fout << "@Proc: *" << std::endl;
            fout << "@Files: Elmer/Rocin/" 
                 << driverObject.prefix
                 << "_vol.\%4p.hdf"
                 << std::endl;
            fout << "@Panes: @Cyclic 100";
            fout.close();
         }
         
         // calling finalize
         driverObject.finalize();
  
         // finalize comm
         COM_finalize();

      return 0;
    }
}

int main(int argc, char** argv) {
  //std::cout << "Calling parallelProgram ... " << std::endl;
  return(COM::parallelProgram(argc, argv));
}

