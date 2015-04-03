/// 
/// @file
/// @ingroup rocstar_group
/// @brief Main for example parallel program.
/// @author Mike Campbell (mtcampbe@illinois.edu)
/// @date 
/// 
#include "ExampleProgram.H"

typedef Rocstar::ExampleProgram::PEProgramType ProgramType;

int main(int argc,char *argv[])
{
  return(Rocstar::ExampleProgram::Driver<ProgramType>(argc,argv));
}
