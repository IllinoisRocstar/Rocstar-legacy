/// 
/// @file
/// @ingroup gridconversion_group
/// @brief Driver program for Grid Conversion.
/// @author Jessica Kress (jkress@illinoisrocstar.com)
/// @date 11/3/2015
/// 

#include "DriverProgram.H"


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

      int error=0;        

      //Parse the input file
      error = ReadInput();

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
