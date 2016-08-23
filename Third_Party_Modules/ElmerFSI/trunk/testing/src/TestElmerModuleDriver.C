///
/// @file 
/// @ingroup elmermoduledriver_group
/// @brief Implements a command-line interface for the ElmerModuleDriver tests.
///
/// Note that in this file, the "main" function is separated off from 
/// the program implementation.  This is done to make the documentation
/// usable and should be done for every new program in %ElmerModuleDriver. 
///
#include "ComLine.H"
#include "TestElmerModuleDriver.H"

namespace ElmerModuleDriver{
  
  
  ///
  /// Drives the ElmerModuleDriver::TestObject. 
  /// 
  /// @param argc number of string command line tokens
  /// @param argv string command line tokens
  /// @returns 0 if successful, 1 otherwise
  ///
  /// Drives the ElmerModuleDriver::TestObject, which should encapsulate
  /// all the tests for the ElmerModuleDriver namespace (and thus the project).
  /// 
  /// Command line documentation:
  ///
  ///           elmermoduledriver_test [-h] [-v [level] -o <filename> -l <filename> -n <TestName> ] 
  ///
  ///           -h,--help
  ///              Print out long version of help and exit.
  ///
  ///           -v,--verblevel [level]
  ///              Set the verbosity level. (default = 0)
  ///
  ///           -o,--output <filename>
  ///              Set the output file to <filename>. (default = stdout)
  ///
  ///           -l,--list <filename>
  ///              Set the list file name to <filename>. (no default). The list file should be a text file with one test name per line.
  ///
  ///           -n,--name <TestName>
  ///              Run test by name. (no default)
  ///           
  ///           -s,--source <pathname>
  //               Set the testing source directory. (no default)
  ///
  int Test(int argc,char *argv[])
  {
    int com_initialized = COM_initialized();
    bool com_initialized_pass = (com_initialized <= 0);
  
    COM_init(&argc, &argv);
    if(com_initialized_pass)
      com_initialized = (COM_initialized() > 0);
    else
      std::cout << "COM not initialized!" << std::endl;   
 
    // The default verbosity is 0
    int verblevel = 0;

    // This line creates the ElmerModuleDriver::TestComLine object and passes in 
    // the command line arguments as a (const char **).
    TestComLine comline((const char **)(argv));
    // The call to comline.Initialize() reads the command line arguments
    // from the array passed in the previous line.
    comline.Initialize();
    // The ProcessOptions() call does detailed examination of the command
    // line arguments to check for user errors or other problems. This call
    // will return non-zero if there were errors on the commandline.
    int clerr = comline.ProcessOptions();
    // Check if the user just wanted to get the help and exit
    if(!comline.GetOption("help").empty()){
      // Print out the "long usage" (i.e. help) message to stdout
      std::cout << comline.LongUsage() << std::endl;
      if(verblevel > 2)
        std::cout << "ElmerModuleDriver::Test: Exiting test function (success)" << std::endl;
      return(0);
    }
    if(clerr){
      std::cout << comline.ErrorReport() << std::endl
                << std::endl << comline.ShortUsage() << std::endl;
      if(verblevel > 2)
        std::cout << "ElmerModuleDriver::Test: Exiting test function (fail)" << std::endl;
      return(1);
    }
    // These outstreams allow the output to file to be set up and separated
    // from the stdout.
    std::ofstream Ouf;
    std::ostream *Out = &std::cout;

    // The next few lines populate some strings based on the 
    // users input from the commandline.
    std::string OutFileName(comline.GetOption("output"));
    std::string TestName(comline.GetOption("name"));
    std::string ListName(comline.GetOption("list"));
    std::string sverb(comline.GetOption("verblevel"));
    std::string SourcePath(comline.GetOption("source"));   
 
    // The following block parses and sets the verbosity level
    if(!sverb.empty()){
      verblevel = 1;
      if(sverb != ".true."){
        std::istringstream Istr(sverb);
        Istr >> verblevel;
        if(verblevel < 0)
          verblevel = 1;
      }
    }
    
    // This block sets up the output file if the user specified one
    if(!OutFileName.empty()){
      Ouf.open(OutFileName.c_str());
      if(!Ouf){
        std::cout << "ElmerModuleDriver::Test> Error: Could not open output file, " 
                  <<  OutFileName << " for test output. Exiting (fail)." << std::endl;
        return(1);
      }
      Out = &Ouf;
    }
    
    if(verblevel > 2)
      std::cout << "ElmerModuleDriver::Test: Entering test function" << std::endl;
    
    // Make an instance of the ElmerModuleDriver testing object, ElmerModuleDriver::TestingObject
    ElmerModuleDriver::TestingObject<ElmerModuleDriver::TestResults> test;
    // Make an instance of the ElmerModuleDriver results object, ElmerModuleDriver::TestResults
    ElmerModuleDriver::TestResults results;

    //Set the source directory for the testing object if it was input
    if(!SourcePath.empty()){
      test.SetSourceDirPath(SourcePath);
    }
    
    // If the user specified a name, then run only the named test
    if(!TestName.empty()){
      // This call runs a test by name
      test.RunTest(TestName,results);
    }
    // Otherwise, if the user specified a list, then read the list and
    // run the listed tests.
    else if(!ListName.empty()){
      std::ifstream ListInf;
      ListInf.open(ListName.c_str());
      if(!ListInf){
        std::cout << "ElmerModuleDriver::Test> Error: Could not open list of tests in file " 
                  << ListName << ". Exiting (fail)." << std::endl;
        return(1);
      }
      std::string testname;
      while(std::getline(ListInf,testname))
        test.RunTest(testname,results);
      ListInf.close();
    }
    else {
      // This call runs all the tests for the ElmerModuleDriver namespace.
      test.Process(results);
    }

    COM_finalize();
    if(com_initialized_pass)
    com_initialized_pass = (COM_initialized() <= 0);

    *Out << results << std::endl;
    
    if(Ouf)
      Ouf.close();

    if(verblevel > 2)
      *Out << "ElmerModuleDriver::Test: Exiting test function (success)" << std::endl;
    
    return(0);
  }
};

int main(int argc,char *argv[])
{
  return(ElmerModuleDriver::Test(argc,argv));
}
