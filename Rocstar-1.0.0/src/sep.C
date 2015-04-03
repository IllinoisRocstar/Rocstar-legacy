/// 
/// @file
/// @ingroup rocstar_group
/// @brief Main for example serial program.
/// @author Mike Campbell (mtcampbe@illinois.edu)
/// @date 
/// 
#include "ExampleProgram.H"

typedef Rocstar::ExampleProgram::SEProgramType ProgramType;

int main(int argc,char *argv[])
{
  return(Rocstar::ExampleProgram::Driver<ProgramType>(argc,argv));
}
