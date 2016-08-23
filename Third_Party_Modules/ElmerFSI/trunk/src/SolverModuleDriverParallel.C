///
/// @file
/// @ingroup solverModule_group
/// @brief Main function to call the Elmer SolverModule
/// @author Masoud Safdari (msafdari@illinoisrocstar.com)
/// @date January 12, 2015
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
                << " -loads :" << std::endl
                << "   Should be used to change loads during simulaiton" << std::endl<< std::endl
                << " time[i], i>0:" << std::endl
                << "   At least one timestep should be given. For steady state problem a timestep" << std::endl
                << "   of 1.0 is enough. Values given should increase monotonically." << std::endl;
       std::exit(1);
    }
           
    int SolverModuleDriver::init(int argc, char *argv[]){
      std::cout << "SoverModuleDriver: running initializer ... " << std::endl;
      // processing command-line flags
      std::string arg;    
      double tmpTime;
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
            if(ss.str() == "-loads"){
              changeLoads = true;
              continue;
            }
            for(int j=0; j<ss.str().size(); j++){
              if(!isdigit(ss.str()[j]) && ss.str()[j] != 'e' 
                 && ss.str()[j] != 'E' && ss.str()[j] != '-'
                 && ss.str()[j] != '.')
                usage(argv[0]);
            }            
            ss >> tmpTime;
            tNext.push_back(tmpTime);
        }
      }      
      else
        usage(argv[0]);
    
      for(int i=1; i < tNext.size(); i++){
        if(tNext[i] <= tNext[i-1]){
          usage(argv[0]);
          exit(1);
        }
      }

      // loading ElmerParallel module
      COM_LOAD_MODULE_STATIC_DYNAMIC(ElmerCSCParallel, "ELMModule");

      // loading SimOut on all processes
      COM_LOAD_MODULE_STATIC_DYNAMIC( SimOUT, "OUT");
  
      // check the communicator that the window was loaded on
      MPI_Comm comm_check;
      outfile << "Checking ELMModule" << std::endl;
      outfile << "comm_check = " << comm_check << std::endl;
      COM_get_communicator("ELMModule", &comm_check);
      if(comm_check == Comm)
        outfile << "comm_check == Comm!" << std::endl;
      else if(comm_check == MPI_COMM_WORLD)
        outfile << "comm_check == MPI_COMM_WORLD!" << std::endl;
      else if(comm_check == MPI_COMM_SELF)
        outfile << "comm_check == MPI_COMM_SELF!" << std::endl;
      else
        outfile << "comm_check == None!" << std::endl;

      // calling module initializer     
      if(color == 0){ 
        /// get the handle for the initialize function and call it
        std::cout << "<<<<<<<<<" << std::endl;
        int init_handle = COM_get_function_handle("ELMModule.InitElmerprep");
        bool init_func = (init_handle > 0);
        int verb=1;
        runs = 0;
        if(init_func)
           COM_call_function(init_handle, &srfWinName, &volWinName, &runs, &verb);
      }

      // FSI problems will register mesh data, accessing them  
      if(isFSI){ 
         char getDataItemLoc;
         COM_Type getDataItemType;
         std::string getDataItemUnits;
         // getting node coordinates  
         COM_get_array("ELMModule.nc", myPaneId, &Coord);
         // check for expected number of nodes
         COM_get_size("ELMModule.nc", myPaneId, &nNodes);
         // get connectivity tables for paneIds
         // :q4: is quad element
         std::string stringNames;
         COM_get_connectivities("ELMModule", myPaneId, &nConn, stringNames);
         std::istringstream ConnISS(stringNames);
         std::vector<std::string> connNames;
         for (int i=0; i<nConn; ++i) {
            std::string name;
            ConnISS >> name;
            connNames.push_back(name);
            std::cout << "Rank #" << myRank
                      << ", ELMModuleDriver:main: Connectivity Table # " << i+1 << ": " << name << std::endl;
         }
         // number of nodes per element
         std::string fullConnName("ELMModule."+connNames[0]);
         COM_get_dataitem(fullConnName, &getDataItemLoc, &getDataItemType,
                          &nElemNodes, &getDataItemUnits);
         std::cout << "Rank #" << myRank
                   << ", ELMModuleDriver:main: getDataItemLoc " << getDataItemLoc << std::endl;
         std::cout << "Rank #" << myRank
                   << ", ELMModuleDriver:main: getDataItemType " << getDataItemType << std::endl;
         std::cout << "Rank #" << myRank
                   << ", ELMModuleDriver:main: nElemNodes " << nElemNodes << std::endl;
         std::cout << "Rank #" << myRank
                   << ", ELMModuleDriver:main: getDataItemUnits " << getDataItemUnits << std::endl;
         // get connectivities        
         COM_get_array(fullConnName.c_str(), myPaneId, &Conn);
         COM_get_size(fullConnName, myPaneId, &nElem);
      }

      return 0;
    }
    
    int SolverModuleDriver::run(){
      //If we want to prescribe loads here's where we do it

      /*
      // set FSI loads for the structures solver
      std::cout << "isFSI =  " << isFSI << std::endl;
      if(isFSI && changeLoads){
        std::cout << "Rank #" << myRank 
                  << ", Applying loads ... " << std::endl;
        COM_get_array("ELMModule.Loads", myPaneId, &Loads);
        if(Loads){
          // Get the FSI load size from the structures solver
          COM_get_size("ELMModule.Loads", myPaneId, &nLoads);
          std::cout << "Load size = " << nLoads << std::endl;
          std::cout << "SoverModuleDriver:run: Checking loads" << std::endl;
          for(int i=0; i < nLoads; i++){
             for(int j=0; j < 3; j++){
                Loads[i*3 + j] = double(i*3 + j);
                std::cout << Loads[i*3+j] << " ";
             }
             std::cout << std::endl;
          }
        }
      }
      std::cout << "Rank #" << myRank 
                << ", Finished applying loads." << std::endl;
      */

      /// Get the handle for the run function and call it
      int run_handle = COM_get_function_handle("ELMModule.Run");
      bool run_func = (run_handle > 0);
      runs = 0;
      if(run_func){
        int timestep = 0;
        for(int i=0; i < tNext.size(); i++){
          // call run function
          std::cout << "SoverModuleDriver:run: Calling run function from driver" << std::endl;
          COM_call_function(run_handle,&runs,&tNext[i]);
          // dump solution to vtk
          vtkDump(tNext[i]);
          // update time 
          timestep++;
        }
      }
      return 0;
    }

    int SolverModuleDriver::updateSolution(){
      // only for FSI problems
      if (!isFSI)
        return(0);
      char getDataItemLoc;
      COM_Type getDataItemType;
      std::string getDataItemUnits;
      // get displacements
      std::string name;
      int myDataItemType;
      name = "ELMModule.Displacements";
      COM_get_dataitem(name, &getDataItemLoc, &getDataItemType,
                       &nDisp, &getDataItemUnits);
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: Displacement Get DataItem" 
                << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemLoc: " << getDataItemLoc << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemType: " << getDataItemType << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: arrayLength: " << nDisp << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemUnits: " << getDataItemUnits << std::endl;
      // translate element (e) to cell (c)
      char myDataItemLoc;
      if (getDataItemLoc == 'e' || getDataItemLoc == 'E') {
          locDisp = 'c';
      } else if (getDataItemLoc == 'n' || getDataItemLoc == 'N') {
          locDisp = 'n';
      } else {
          std::cout << "ELMModuleDriver:main: Unknown Data Item Location" << std::endl;
          exit(1);
      }
      if (getDataItemType == COM_DOUBLE_PRECISION) {
          typeDisp = 8;
      } else {
          std::cout << "ELMModuleDriver:main: Unknown Data Item Type" << std::endl;
          exit(1);
      }
      // acquire dataitem from COM
      COM_get_array("ELMModule.Displacements", myPaneId, &Disp);
      // get loads
      name = "ELMModule.Loads";
      COM_get_dataitem(name, &getDataItemLoc, &getDataItemType,
                       &nLoads, &getDataItemUnits);
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: Load Get DataItem" 
                << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemLoc: " << getDataItemLoc << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemType: " << getDataItemType << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: arrayLength: " << nLoads << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemUnits: " << getDataItemUnits << std::endl;
      // translate element (e) to cell (c)
      if (getDataItemLoc == 'e' || getDataItemLoc == 'E') {
          locLoads = 'c';
      } else if (getDataItemLoc == 'n' || getDataItemLoc == 'N') {
          locLoads = 'n';
      } else {
          std::cout << "ELMModuleDriver:main: Unknown Data Item Location" << std::endl;
          exit(1);
      }
      if (getDataItemType == COM_DOUBLE_PRECISION) {
          typeLoads = 8;
      } else {
          std::cout << "ELMModuleDriver:main: Unknown Data Item Type" << std::endl;
          exit(1);
      }
      // acquire dataitem from COM
      COM_get_array("ELMModule.Loads", myPaneId, &Loads);
      // get pressures
      name = "ELMModule.Pressures";
      COM_get_dataitem(name, &getDataItemLoc, &getDataItemType,
                       &nPress, &getDataItemUnits);
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: Pressures Get DataItem" 
                << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemLoc: " << getDataItemLoc << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemType: " << getDataItemType << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: arrayLength: " << nPress << std::endl;
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: getDataItemUnits: " << getDataItemUnits << std::endl;
      // translate element (e) to cell (c)
      if (getDataItemLoc == 'e' || getDataItemLoc == 'E') {
          locPress = 'c';
      } else {
          std::cout << "ELMModuleDriver:main: Unknown Data Item Location" << std::endl;
          exit(1);
      }
      if (getDataItemType == COM_DOUBLE_PRECISION) {
          typePress = 8;
      } else {
          std::cout << "ELMModuleDriver:main: Unknown Data Item Type" << std::endl;
          exit(1);
      }
      COM_get_array(name.c_str(), myPaneId, &Press);

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

    int SolverModuleDriver::vtkDump(double timeMark){
      // only for FSI problems
      if (!isFSI)
         return(0);
      char getDataItemLoc;
      COM_Type getDataItemType;
      std::string getDataItemUnits;
      // creating solverAgent if not yet
      if (!myAgentIsInit) {
          myAgentIsInit = true;
          // put elements into a vector so we can build the solver agent
          std::vector<unsigned int> connVector;
          for (int i=0; i<nElem; ++i) {
            for (int j=0; j<nElemNodes; ++j) {
              connVector.push_back((Conn[i*nElemNodes+j]));
            }
          }    
          // make a solverAgent to store our data
          myAgent.Solution().Meta().AddField("time", 's', 1, 8, "s");
          myAgent.Solution().Meta().AddField("endTime", 's', 1, 8, "s");
          myAgent.Mesh().nc.init(nNodes, Coord);
          myAgent.Mesh().con.AddElements(nElem, nElemNodes, connVector);
      }
      // updating datastructures
      updateSolution();
      // displacements
      myAgent.Solution().Meta().AddField("Displacement", locDisp, nDisp, 
                                  typeDisp, "");
      // loads
      myAgent.Solution().Meta().AddField("Load", locLoads, nLoads, 
                                           typeLoads, "");
      // get pressures
      myAgent.Solution().Meta().AddField("Pressure", locPress, nPress,
                                         typePress, "");
      // create buffers for the actual data
      myAgent.CreateSoln();
      unsigned int nnodes = myAgent.Mesh().nc.NNodes();
      unsigned int nelem = myAgent.Mesh().con.Nelem();
      // reset the buffers to be our own local buffers
      myAgent.Solution().SetFieldBuffer("Displacement", Disp);
      myAgent.Solution().SetFieldBuffer("Load", Loads);
      myAgent.Solution().SetFieldBuffer("Pressure", Press);
      // write initial mesh data out to a VTK file
      std::ofstream Ouf;
      ss.clear();
      ss.str("");
      ss << timeMark
         << "_proc_"
         << myRank;
      std::string filename;
      filename = "fsi_" + ss.str() +".vtk";
      Ouf.open(filename.c_str());
      if(!Ouf){
            std::cerr << "ELMModuleDriver:main: DumpSolution: Error: Could not open output file, "
                      << filename << "." << std::endl;
            return -1;
      }
      std::cout << "Rank #" << myRank 
                << ", ELMModuleDriver:main: WriteVTKToStream for " << timeMark << std::endl;
      SolverUtils::WriteVTKToStream("ELMModule", myAgent, Ouf);
      Ouf.close();
    }

    int parallelProgram(int argc, char *argv[]){
         int parErr;
         // instantiating IRAD's communicatorObject
         // this constructor calls MPI_init internally and
         // sets up everthing with MPI
         std::cout <<"ElmerModuleDriver:Main: Setting up parallel communicator..." << std::endl;
         CommType communicator(&argc,&argv);
         MPI_Comm masterComm = communicator.GetCommunicator();
         int rank = communicator.Rank();
         int nproc = communicator.Size();
         int color = 0;
         std::cout << "ElmerModuleDriver:Main:Rank #"
                   << rank
                   << ", I see "
                   << nproc
                   << " proccesses."
                   << std::endl;
         // initializing COM 
         COM_init( &argc, &argv);

         // instantiating solver driver object
         SolverModuleDriver driverObject;
         driverObject.setRank(rank);
         driverObject.myPaneId=100+rank;

         // checking streamer process
         if (driverObject.isStreamer())
            std::cout << "Hi, I am a streamer ..." << std::endl;

         // setting up output files for parallel debugging
         std::string outfile_name;
         std::stringstream ss; 
         std::string str_rank;
         ss << rank;
         ss >> str_rank;
         outfile_name = "out" + str_rank + ".dat";
         driverObject.outfile.open(outfile_name.c_str());
         
         // setting color for the driver object
         driverObject.setColor(color);
         driverObject.outfile << "color = " << color << std::endl;
         driverObject.outfile << "rank = " << rank << std::endl;
         CommType newCommunicator;
         communicator.Split(color, rank, newCommunicator);
         driverObject.outfile << "communicator.Size() = " << communicator.Size() << std::endl;
         driverObject.outfile << "newCommunicator.Size() = " << newCommunicator.Size() << std::endl;
         driverObject.setComm(newCommunicator.GetCommunicator());
         MPI_Comm newComm;
         newComm = newCommunicator.GetCommunicator();
         COM_set_default_communicator(newComm);

         // calling initializer, for FSI problems mesh and solution
         // related datastructures will be automatically registered
         driverObject.init(argc, argv);

         // get information about what was registered in this window
         int numDataItems=0;
         std::string output;
         COM_get_dataitems("ELMModule", &numDataItems, output);
         std::istringstream Istr(output);
         std::vector<std::string> dataItemNames;  
         for (int i=0; i<numDataItems; ++i) {
           std::string name;
           Istr >> name;
           dataItemNames.push_back(name);
           std::cout << "Rank #" << rank
                     << ", ElmerModuleDriver:main: DataItem # " << i << ": " << name << std::endl;
         }
         // list of panes for this process: each process creates a single pane
         // with paneId = 100 + rank
         int numPanes;
         int* paneList;
         COM_get_panes("ELMModule", &numPanes, &paneList);
         std::cout << "Rank #" << rank
                   << ", ELMModuleDriver:main: Number of Panes " << numPanes << std::endl;
         for (int i=0; i<numPanes; ++i)
           std::cout << "Rank #" << rank
                     << ", ELMModuleDriver:main: Pane ID # " << i+1 << "=" << paneList[i] << std::endl;
         
         // updating solution
         driverObject.updateSolution();

         // dumping intial conditions to vtk
         //if (driverObject.isFSISim())
         //   driverObject.vtkDump(0); 
         
         // scattering FSI loads to the structures solver processes
         // only for FSI problesm
         if(driverObject.isFSISim() && driverObject.isChangeLoad()){
            std::vector<int> nLoadsVec;
            std::vector<int> nLoadsTotVec;
            std::vector<int> nRecvAllVec;
            nLoadsVec.push_back(driverObject.nNodes*driverObject.nLoads);
            std::cout << "Rank #"<<rank
                      <<", nLoadsVec["<<rank<<"] = "<<nLoadsVec[rank]<<std::endl;
            parErr = newCommunicator.Gatherv(nLoadsVec, nLoadsTotVec, nRecvAllVec);
            if (parErr)
               std::cout << "Error in gathering information from processes." << std::endl;
            if (rank == 0){
               for (int iProc=0; iProc<nLoadsTotVec.size(); iProc++)
                  std::cout << "Rank #"<<rank
                            <<", nLoadsTotVec["<<iProc<<"] = "<<nLoadsTotVec[iProc]<<std::endl;
            }
            // defining new loads
            std::vector<double> loadsVec;
            std::vector<double> loadsTotVec;
            int nLoadsTot = 0;
            if (rank == 0){
              for (int iProc=0; iProc < nLoadsTotVec.size(); iProc++){
                 nLoadsTot += nLoadsTotVec[iProc];
              }
              loadsTotVec.resize(nLoadsTot,0.0);
              for (int iLoad=1; iLoad < nLoadsTot; iLoad+=3)
                  loadsTotVec[iLoad] = 1.0;
              std::cout << "Size of loadsTotVec = " << loadsTotVec.size() << std::endl;
            }
            // parallel scattering
            parErr = newCommunicator.Scatterv(loadsTotVec, nLoadsTotVec, loadsVec);
            if (parErr)
               std::cout << "Error in scattering information to processes." << std::endl;
            std::cout << "Rank #" << rank
                      << ", number of load components I received = " << loadsVec.size()
                      << std::endl;
            // applying loads for each process
            for (int iLoad=0; iLoad<(driverObject.nLoads*driverObject.nNodes); ++iLoad){
               driverObject.Loads[iLoad] = loadsVec[iLoad];
               std::cout << "Rank #" << rank
                         << ", driverObject.Loads ["<<iLoad<<"] = " 
                         << driverObject.Loads[iLoad] << std::endl;
            }
            // setting a barrier here
            std::cout << "Rank #"<<rank<<"...Reaching to barrier"<<std::endl;
            newCommunicator.Barrier();
         }

         // calling run step
         driverObject.run();
         newCommunicator.Barrier();
         
         // gathering all displacements in root
         // only for FSI problesm
         if(driverObject.isFSISim() && driverObject.isChangeLoad()){
            std::vector<double> dispVec;
            std::vector<double> dispTotVec;
            std::vector<int> nSendAllVec;
            for (int iDisp=0; iDisp<(driverObject.nDisp*driverObject.nNodes); ++iDisp){
               dispVec.push_back(driverObject.Disp[iDisp]);
               std::cout << "Rank #" << rank
                         << ", dispVec ["<<iDisp<<"] = " << dispVec[iDisp] << std::endl;
            }
            std::cout << "Rank #" << rank
                      << ", dispVec size = " << dispVec.size() << std::endl;
            // parallel gathering
            parErr =  newCommunicator.Gatherv(dispVec, dispTotVec, nSendAllVec);
            if (parErr)
               std::cout << "Error in gathering information from processes." << std::endl;
            std::cout << "Rank #" << rank
                      << ", size of dispTotVec = " << dispTotVec.size()
                      << std::endl;         
            // setting a barrier here
            std::cout << "Rank #"<<rank<<"...Reaching to barrier"<<std::endl;
            newCommunicator.Barrier();
            // checking gather data vector
            if (rank == 0){
               double maxDisp = 0.0;
               double minDisp = 0.0;
               for (int iDisp=0; iDisp<dispTotVec.size(); iDisp++){
                  maxDisp = std::max(dispTotVec[iDisp], maxDisp);
                  minDisp = std::min(dispTotVec[iDisp], minDisp);
                  std::cout << "Rank #0, dispTotVec [" << iDisp << "] = " 
                            << dispTotVec[iDisp] << std::endl;
               }
               outfile_name = "maxMinDisp.dat";
               std::ofstream testFile;
               testFile.open(outfile_name.c_str());
               testFile << maxDisp << std::endl << minDisp << std::endl;
               testFile.close();
            }
            // writing window to HDF
            int OUT_set = COM_get_function_handle( "OUT.set_option");
            int OUT_write = COM_get_function_handle( "OUT.write_dataitem");
            const char *win_out= "ELMModule";
            std::string win_out_pre(win_out);
            win_out_pre.append(".");
            int OUT_all = COM_get_dataitem_handle((win_out_pre+"all").c_str());
            ss.clear();
            ss.str("");
            ss << "_proc_"
               << rank;
            std::string fileOut;
            fileOut = "ELMModule_window" + ss.str();
            COM_call_function( OUT_set, "format", "HDF4");
            COM_call_function( OUT_write, (fileOut + ".hdf").c_str(), &OUT_all, win_out,
                            "0000");
         }
                  
         // calling finalize
         driverObject.finalize();
  
         // closing log file for the driver object
         driverObject.outfile.close();

         // finalize comm
         COM_finalize();

         // finalizing communicator
         communicator.Finalize();
      return 0;
    }
}

int main(int argc, char** argv) {
  //std::cout << "Calling parallelProgram ... " << std::endl;
  return(COM::parallelProgram(argc, argv));
}

