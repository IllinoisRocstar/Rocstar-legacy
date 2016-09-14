///
/// @file
/// @ingroup impact_group
/// @brief Main function of the driver (in C/C++) for the OpemFoamFSIModule
/// @author Masoud Safdari (msafdari@illinoisrocstar.com)
/// @date  Aug 25, 2015
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
#include "OFModuleDriver.H"
#include "COMM.H" // contains IRAD utilities for MPI
//#include "Pane_communicator.h" // IMPACT's inter-process tools
 
COM_EXTERN_MODULE(OpenFoamFSIPar);
COM_EXTERN_MODULE(SimOut);

namespace COM {
   void Usage(char *exec) {
      std::cout << std::endl << std::endl << "Usage: " << std::endl;
      std::cout << exec << " <flags>" << std::endl
      << "Where flags are :" << std::endl
      << "--parallel" << std::endl
      << "  runs the openFoam in the parallel mode" << std::endl;
   }

  // typedef for CommunicatorObject
  typedef IRAD::Comm::CommunicatorObject CommType;

  // parallel driver for OpenFoam
  int parallelProgram(int argc, char **argv) {

     // instantiating IRAD's communicatorObject
     // this constructor calls MPI_init internally and
     // sets up everthing with MPI
     std::cout <<"OFPrep:Main: Setting up parallel communicator..." << std::endl;
     COM::CommType communicator(&argc,&argv);
     MPI_Comm masterComm = communicator.GetCommunicator();
     int rank = communicator.Rank();
     int nproc = communicator.Size();
     std::cout << "OFPrep:Main:Rank #"
               << rank
               << ", I see "
               << nproc
               << " proccesses."
               << std::endl;

     //int main(int argc, char *argv[])
     COM_init( &argc, &argv);
     
     MPI_Comm newComm = communicator.GetCommunicator();
     
     // flags for operation modes
     // default mode of operation, runs the original OpenFOAM functionality
     bool regression = true;  
     // prescribe a solid surface displacement and run the fluid solver only
     bool prescribedDisplacement = false;  
     // run in parallel mode
     bool runParallel = false;
     // run test cycle
     bool dryRun = false;
   
   
     // we read any driver specific flags passed
     // the strings are then removed from argv and argc is decremented
     // to preserve the input command line for OpenFoam
     std::string arg;
     std::stringstream ss;
     if (argc > 1) {
       for (int i=1; i<argc; ++i) {
	 ss.clear();
	 ss.str("");
	 ss << argv[i];
	 if (ss.str() == "--driverRegression") {
	   regression = true;
	 } else if (ss.str() == "--driverPrescribedDisplacement") {
	   regression = false;
	   prescribedDisplacement = true;
	 } else if (ss.str() == "--parallel") {
	   runParallel = true;
	 } else if (ss.str() == "--dryrun") {
	   dryRun = true;
	 } else {
	   Usage(argv[0]);
	   exit(2);
	 }
       }
     } else {
        Usage(argv[0]);
        exit(2);
     }

     // changing default communicator and testing it
     COM_set_default_communicator(newComm);
     std::cout << "OFPrep:Main:Rank #" << rank
	       << ", Default communicator for COM is now "
	       << newComm
	       << std::endl;
     
     // loading parallel openfoam module 
     // all processes load fluid module
     COM_LOAD_MODULE_STATIC_DYNAMIC( OpenFoamFSIPar, "OFModule");
     
     // loading SimIn on all processes
     COM_LOAD_MODULE_STATIC_DYNAMIC( SimOUT, "OUT");

     // getting number of processes  
     if (rank==0) {
       int* nProcReg;
       COM_get_array("OFModule.nproc", 0, &nProcReg);
       std::cout << "The communicator registered in OFModule uses "
               << *nProcReg << " prcesses" << std::endl;
     }

     // initializing the openfoam 
     // get the handle for the ofprep initialize function and call it
     int init_handle = COM_get_function_handle("OFModule.InitOFPrep");
     if(init_handle <= 0) { // fail
     }
      
     // making a dummy argc/argv for OpenFOAM. 
     // For now, no options passed from the command 
     // line will be used by the driver
     int myArgc = 1;
     char *myArgv[2];
     // passing switch "parallel" triggers openFoam to run in parallel mode
     myArgv[0] = argv[0];
     if (runParallel) {
       myArgc = 2;
       myArgv[1] = const_cast<char *>("-parallel");
     } else {
       myArgv[1] = NULL;
     }
     // setting verbose level
     int verb=3;
     // call the funciton
     COM_call_function(init_handle, &myArgc, &myArgv, &verb);	 
   
     std::cout << "Rank " << rank << " Back to OFPrep." << std::endl;
      
     // getting information about what was registered in this window by another process
     if (rank==0){
	int numDataItems=0;
	std::string output;
	COM_get_dataitems("srf", &numDataItems, output);
	std::istringstream Istr(output);
	std::vector<std::string> dataItemNames;
      
	for (int i=0; i<numDataItems; ++i) {
	  std::string name;
	  Istr >> name;
	  dataItemNames.push_back(name);
	  std::cout << "Rank #" << rank
		    << ", OFPrep:main: DataItem # " << i << ": " << name << std::endl;
	}
     }

     // setting a barrier here
     std::cout << "Rank #"<<rank<<"...Reaching to barrier"<<std::endl;
     communicator.Barrier();

     int OUT_set = COM_get_function_handle( "OUT.set_option");
     int OUT_write = COM_get_function_handle( "OUT.write_dataitem");
     int OUT_rocin_write = COM_get_function_handle("OUT.write_rocin_control_file");

     // writing surface window information to hdf
     const char *win_out= "srf";
     std::string win_out_pre(win_out);
     win_out_pre.append(".");
     int OUT_all = COM_get_dataitem_handle((win_out_pre+"all").c_str());
     char tmp[10];
     std::sprintf(tmp,"%4.4d",rank);
     std::string fileOut;
     fileOut = std::string("../../Rocin/ifluid_") + tmp;
     COM_call_function( OUT_set, "mode", "w");
     COM_call_function( OUT_set, "rankwidth", "0");
     COM_call_function( OUT_set, "format", "HDF4");
     COM_call_function( OUT_write, (fileOut + ".hdf").c_str(), &OUT_all, "ifluid",
	      "00.000000");
     // write Rocin control file
     std::ofstream fout("../../Rocin/ifluid_in_00.000000.txt");
     COM_assertion_msg(fout.is_open(),
       (std::string("OFPrep: cannot open control file for interface data")).c_str());
     fout << "@Proc: *" << std::endl;
     fout << "@Files: OpenFoam/Rocin/"
          << "ifluid_\%4p.hdf"
          << std::endl;
     fout << "@Panes: @Cyclic 200";
     fout.close();
      
         
     // writing volume window information to hdf
     win_out= "vol";
     win_out_pre = win_out;
     win_out_pre.append(".");
     OUT_all = COM_get_dataitem_handle((win_out_pre+"all").c_str());
     std::sprintf(tmp,"%4.4d",rank);
     fileOut = std::string("../../Rocin/fluid_") + tmp;
     COM_call_function( OUT_set, "mode", "w");
     COM_call_function( OUT_set, "rankwidth", "0");
     COM_call_function( OUT_set, "format", "HDF4");
     COM_call_function( OUT_write, (fileOut + ".hdf").c_str(), &OUT_all, "fluid",
	      "00.000000");
     // write Rocin control file
     fout.open("../../Rocin/fluid_in_00.000000.txt");
     COM_assertion_msg(fout.is_open(),
       (std::string("OFPrep: cannot open control file for interface data")).c_str());
     fout << "@Proc: *" << std::endl;
     fout << "@Files: OpenFoam/Rocin/"
          << "fluid_\%4p.hdf"
          << std::endl;
     fout << "@Panes: @Cyclic 200";
     fout.close();

     // Finalizing
     // unloading SimIn on all processes
     COM_UNLOAD_MODULE_STATIC_DYNAMIC( SimOUT, "OUT");
     // unloading openFoam module
     COM_UNLOAD_MODULE_STATIC_DYNAMIC(OpenFoamFSIPar, "OFModule");
     // finalize comm
     COM_finalize();
     // finalizing MPI
     communicator.Finalize();
     return(0);
   }
   
}

int main(int argc, char** argv) {
  //std::cout << "Calling parallelProgram ... " << std::endl;
  return(COM::parallelProgram(argc, argv));
}

