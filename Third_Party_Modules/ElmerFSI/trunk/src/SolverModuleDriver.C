///
/// @file
/// @ingroup solverModule_group
/// @brief Main function to call the Elmer SolverModule
/// @author Jessica Kress (jkress@ilrocstar.com)
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
#include "SolverModuleDriver.H"

COM_EXTERN_MODULE( ElmerCSC);

void SolverModuleDriver::usage(char *exec){
  std::cout << "SolverModuleDriver:usage: Usage: " << std::endl
            << exec << " -com-mpi timeNext timeNext ... timeFinal" << std::endl
            << "where at least -com-mpi and timeFinal is required." << std::endl
            << "NOTES:" << std::endl
            << "*currently it is required to use the -com-mpi flag" << std::endl
            << "*steady state problems will always use a timestep" << std::endl
            << " of 1.0, and should only have a timeFinal" << std::endl
            << "*each time must be greater than the previous time" << std::endl
            << " because the simulation begins at the previous time and runs" << std::endl
            << " to the current time" << std::endl;
   std::exit(1);
}
       
int SolverModuleDriver::init(int argc, char *argv[]){

  COM_init( &argc, &argv);

  std::cout << "SoverModuleDriver:init: After COM_init" << std::endl;

  isNum =  isFSI = changeLoads = false;
  Disp = NULL;
  DispSize = 0;
  Coord = NULL;
  Conn = NULL;
  coord_handle = -1;
  CoordSize=0;
  coordData = false;
  connCorrect = true;
  ConnSize = 0;
  Loads = NULL;
  LoadsSize = 0;
  
  std::string arg;

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
        for(int j=0; j < ss.str().size(); j++){
          if(!isdigit(ss.str()[j]) && ss.str()[j] != 'e' 
             && ss.str()[j] != 'E' && ss.str()[j] != '-'
             && ss.str()[j] != '.')
            usage(argv[0]);
        }
        ss >> var;
        tNext.push_back(var);
    }
  }      
  else
    usage(argv[0]);

  for(int i=1; i < tNext.size(); i++){
    if(tNext[i] <= tNext[i-1])
      usage(argv[0]);
  }

  COM_LOAD_MODULE_STATIC_DYNAMIC( ElmerCSC, "Window1");

  /// Get the handle for the initialize function and call it
  int init_handle = COM_get_function_handle("Window1.Initialize");
  bool init_func = (init_handle > 0);
  int verb=3;
 runs = 0;
  if(init_func){
    COM_call_function(init_handle, &runs, &verb);
  }

  return 0;
}

int SolverModuleDriver::run(){
  if(isFSI){
    //////////////////////////////////////////////////
    // Check coordinate values
    //////////////////////////////////////////////////
    coord_handle = COM_get_dataitem_handle("Window1.nc");
    CoordSize=0;
    coordData = (coord_handle > 0);
    std::cout << "SoverModuleDriver:run: coord_handle = " << coord_handle << std::endl;
    if(coordData){
      COM_get_size("Window1.nc",11,&CoordSize);
      // Get the FSI mesh from the structures solver and print
      // it out to check
      COM_get_array("Window1.nc",11,&Coord);
      std::cout << "SoverModuleDriver:run: Coord: " << std::endl;
      for(int i=0; i < CoordSize; i++){
        for(int j=0; j < 3; j++){
          std::cout << Coord[i*3+j] << " ";
        }
        std::cout << std::endl;
      }
      Mesh().nc.init(CoordSize, Coord);
    }

    //////////////////////////////////////////////////
    // Check connectivity values
    //////////////////////////////////////////////////
    connCorrect = true;
    ConnSize = 0;
    
    // Get the FSI mesh from the structures solver
    COM_get_array("Window1.:b2:",11,&Conn);
    if(Conn){
      // Get the FSI mesh size from the structures solver
      COM_get_size("Window1.:b2:",11,&ConnSize);
      // check the values
      std::cout << "SoverModuleDriver:run: Conn: " << std::endl;
      for(int i=0; i < ConnSize; i++){
        for(int j=0; j < 2; j++){
          std::cout << Conn[i*2+j] << " ";
          elems.push_back(Conn[i*2+j]);
        }
        std::cout << std::endl;
      }
      Mesh().con.AddElements(ConnSize,2,elems);
    }

    if(Conn && coordData){
      std::cout << "SoverModuleDriver:run: WriteMeshToStream: " << std::endl;
      WriteMeshToStream(std::cout);
      std::cout << std::endl;
    }
    // Check displacement data
    COM_get_array("Window1.Displacements",11,&Disp);
    if (Disp){
      // Get the FSI displacement size from the structures solver
      COM_get_size("Window1.Displacements",11,&DispSize);

      std::cout << "SoverModuleDriver:run: Checking displacements" << std::endl;
      for(int i=0; i < DispSize; i++){
         for(int j=0; j < 3; j++){
            std::cout << Disp[i*3+j] << " ";
            DispPass.push_back(Disp[i*3+j]);
         }
         std::cout << std::endl;
      }
    }
  }

  std::cout << "Line = " << __LINE__ << std::endl;

  //If we want to prescribe loads here's where we do it
  Loads = NULL;
  LoadsSize = 0;
  
  // Get the FSI loads from the structures solver
  if(isFSI && changeLoads){
    COM_get_array("Window1.Loads",11,&Loads);
    if(Loads){
      std::cout << "SoverModuleDriver:run: Loads not NULL" << std::endl;
      // Get the FSI load size from the structures solver
      COM_get_size("Window1.Loads",11,&LoadsSize);
      // Check load data

      std::cout << "SoverModuleDriver:run: Checking loads" << std::endl;
      for(int i=0; i < LoadsSize; i++){
         for(int j=0; j < 3; j++){
            Loads[i*3 + j] = double(i*3 + j);
            std::cout << Loads[i*3+j] << " ";
            LoadsPass.push_back(Loads[i*3+j]);
         }
         std::cout << std::endl;
      }
    }
  }
  std::cout << "SoverModuleDriver:run: Line = " << __LINE__ << std::endl;

  //Put the displacements and loads in the Solution object
  //so we can use its utilities and write a vtk file
  if(Disp && isFSI){ 
    Solution().Meta().AddField("displacement",'n',3,8,"m");
    std::cout << "SoverModuleDriver:run: WriteSolnMetaToStream:" << std::endl;
    WriteSolnMetaToStream(std::cout);
    std::cout << std::endl;
  }
  if(Loads && isFSI){
    Solution().Meta().AddField("loads",'n',3,8,"");
    std::cout << "SoverModuleDriver:run: WriteSolnMetaToStream:" << std::endl;
    WriteSolnMetaToStream(std::cout);
    std::cout << std::endl;
  }
  std::cout << "SoverModuleDriver:run: Line = " << __LINE__ << std::endl;
  if((Disp && isFSI) || Loads){
    CreateSoln();
  }
  std::cout << "SoverModuleDriver:run: Line = " << __LINE__ << std::endl;
  if(Disp && isFSI)
    Solution().SetFieldBuffer("displacement",DispPass);
  if(Loads && isFSI)
    Solution().SetFieldBuffer("loads",LoadsPass);

  std::cout << "SoverModuleDriver:run: Line = " << __LINE__ << std::endl;

  //Write vtk file for timestep 0
  if(isFSI && Conn && Coord){
    std::ofstream Ouf;
    std::string filename;
    filename = "fsi0.vtk";
    Ouf.open(filename.c_str());
    if(!Ouf){
        std::cerr << "SoverModuleDriver:run: SolverModuleDriver::DumpSolution:Error: Could not open output file, "
                  << filename << "." << std::endl;
        return -1;
      }   
      std::cout << "SoverModuleDriver:run: WriteVTKToStream time 0" << std::endl;
      SolverUtils::WriteVTKToStream("Window1",*this,Ouf);
      Ouf.close();
  }
 

  /// Get the handle for the run function and call it
  int run_handle = COM_get_function_handle("Window1.Run");
  bool run_func = (run_handle > 0);
  runs = 0;
  if(run_func){
    int timestep = 0;
    for(int i=0; i < tNext.size(); i++){
      //Change the load values as a test
      if(Loads && isFSI){
         std::cout << "SoverModuleDriver:run: Changing load values" << std::endl;
         for(int k=0; k < LoadsSize; k++){
            for(int j=0; j < 3; j++){
               Loads[k*3 + j] = double(k*3 + j) + tNext[i];
               std::cout << Loads[k*3 + j] << " ";
               LoadsPass[k*3 + j] = Loads[k*3 + j];
            }
            std::cout << std::endl;
         }
      }

      std::cout << "SoverModuleDriver:run: Calling run function from driver" << std::endl;
      COM_call_function(run_handle,&runs,&tNext[i]);

      //Update Solution's displacments for writing vtk file
      if(isFSI && Disp){
        std::cout << "SoverModuleDriver:run: Checking displacements" << std::endl;
        for(int k=0; k < DispSize; k++){
           for(int j=0; j < 3; j++){
              std::cout << Disp[k*3+j] << " ";
              DispPass[k*3+j] = Disp[k*3+j];
           }
           std::cout << std::endl;
        }
      }
      timestep++;
      //Write vtk file for timestep
      if(isFSI && Conn && Coord){
        std::ofstream Ouf;
        ss.clear();
        ss.str("");
        ss << timestep;
        std::string filename;
        filename = "fsi" + ss.str() + ".vtk";
        Ouf.open(filename.c_str());
        if(!Ouf){
            std::cerr << "SolverModuleDriver::DumpSolution:Error: Could not open output file, "
                      << filename << "." << std::endl;
            return -1;
          }   
          SolverUtils::WriteVTKToStream("Window1",*this,Ouf);
          Ouf.close();
      }
    }
  }
  return 0;
}

int SolverModuleDriver::finalize(){

  /// Get the handle for the finalize function and call it
  int final_handle = COM_get_function_handle("Window1.Finalize");
  bool final_func = (final_handle > 0);
  runs = 0;
  if(final_func){
    COM_call_function(final_handle,&runs);
  }

  COM_UNLOAD_MODULE_STATIC_DYNAMIC( ElmerCSC, "Window1");

  COM_finalize();
  std::cout << "SolverModuleDriver:finalize: After COM_finalize" << std::endl;

  return 0;

}

int main(int argc, char *argv[]){

  SolverModuleDriver driverObject;

  driverObject.init(argc, argv);
  driverObject.run();
  driverObject.finalize();

  std::ofstream Outfile;
  
  Outfile.open("out.dat");

  Outfile << "howdy!" << std::endl;
  Outfile << "1.2 4.2 5.6 7.8" << std::endl;
  Outfile << "      blargity blarg blarg" << std::endl;

  return 0;
}

