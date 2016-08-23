///
/// @file
/// @ingroup elmermoduledriver_group
/// @brief Testing object for ElmerModuleDriver.
///
/// This file contains the implementation of the
/// overarching testing object for the project.
/// Every project should have one of these as
/// it allows external entities unified methodology
/// for access project or package-specific tests.
///
#ifndef __ELMERMODULEDRIVER_TEST_H__
#define __ELMERMODULEDRIVER_TEST_H__
#ifdef _ELMERMODULEDRIVER_PARALLEL_
#include "COMM.H"
#endif
#include "Testing.H"
#include "ExampleHeader.H"
#include "Profiler.H"
#include <vector>
#include <cmath>
#include <iomanip>
#include "com.h"
#include "com_devel.hpp"
#include "UnixUtils.H"

COM_EXTERN_MODULE( ElmerCSC );
COM_EXTERN_MODULE( ElmerCSCParallel );

namespace ElmerModuleDriver {

  ///
  /// Namespace for storing simple test utility fixtures
  ///
  namespace TestFixture {
    ///
    /// Simple test function fixture returns \f$2x\f$.
    /// 
    /// F1 is a 1st order function in x that has unity integral over [0,1].
    ///
    double F1(double x) { return (2.0*x); };
    ///
    /// Simple test function fixture returns \f$3x^2\f$.
    ///
    /// F2 is quadratic function in x and integrates to unity over [0,1].
    ///
    double F2(double x) { return (3.0*x*x); };
  };
  ///
  /// Project-specific test results type.
  ///
  /// Test results types can be custom implemented
  /// by the project developer.  The requirement
  /// is that they produce the proper test results
  /// output when used with outstreams.
  ///
  typedef IRAD::Util::TestResults TestResults;

  /// 
  /// Project-specific testing object.
  ///
  /// The project-specific testing object encapsulates
  /// all tests for the project. This is the interface
  /// to all testing available for the given project.
  /// It implements the interface given by 
  /// IRAD::Util::TestingObject.
  ///
  /// Every project should define a "TestingObject"
  /// which implements (or interfaces) all of the tests
  /// for the project.
  /// If necessary (e.g. for parallel tests through
  /// batch systems), this is the object responsible
  /// for spawning off children processes to conduct
  /// parallel tests and for gathering the results
  /// of those tests.
  ///
  template<typename ResultsType>
  class TestingObject : public IRAD::Util::TestingObject<ResultsType>
  {
    typedef IRAD::Util::TestingObject<ResultsType> TestingObjectBaseType;
  protected:
    // Example test fixtures for ElmerModuleDriver testing.
    //
    // In a real project, there would be many test fixtures
    // in the TestingObject.
    ///
    /// A sample string test fixture. 
    ///
    std::string ExampleTestFixture;
    ///
    /// A set of values for testing quadrature methods.
    /// 
    std::vector<int> N;
    ///
    /// The string for the testing source directory.
    ///
    std::string SourceDirPath;
  public:
    /// 
    /// Default constructor.
    ///
    TestingObject() : TestingObjectBaseType() {};
    
    ///
    /// Sets the string value of the testing source directory.
    /// 
    void SetSourceDirPath(std::string input){
      SourceDirPath = input;
    }


    ///
    /// Tears down the testing fixtures if needed.
    ///
    virtual void Epilogue() {};

    ///
    /// Sets up the data fixtures for the tests.
    /// 
    /// This function gives the object a chance to 
    /// allocate and set up any "fixtures" before the
    /// tests are run.
    ///
    virtual void  Prologue(){
      ExampleTestFixture.assign("ExampleTestData");
      for(int i = 10;i < 10000000;i*=10) N.push_back(i);
    }
    ///
    /// Simple test function fixture returns \f$2x\f$.
    /// 
    /// F1 is a 1st order function in x that has unity integral over [0,1].
    ///
    double F1(double x) { return (2.0*x); };
    ///
    /// Simple test function fixture returns \f$3x^2\f$.
    ///
    /// F2 is quadratic function in x and integrates to unity over [0,1].
    ///
    double F2(double x) { return (3.0*x*x); };
    ///
    /// Test for ElmerModuleDriver::ExampleFunction
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    /// This function implements a simple test of the 
    /// function ElmerModuleDriver::ExampleFunction - which is designed to
    /// simply return a copy of the input string.
    ///
    ///
    ///************************************************************************
    /// List of Serial tests
    ///************************************************************************
    ///
    virtual void Test__ExampleFunction(ResultsType &result) {
      // This is an actual test of the function called 
      // ExampleFunction.   The name Test__XXXXX will
      // eventually help automated utilities with 
      // running tests by name.
      std::string ExampleResult(ExampleFunction(ExampleTestFixture));      
      result.UpdateResult("ExampleFunction:Works",
                          ExampleResult == ExampleTestFixture);
      result.UpdateResult("ExampleFunction:Fails",
                          ExampleResult != ExampleTestFixture);
    }
    ///
    /// Test for ElmerModuleDriver::TrapezoidQuadrature
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    /// This function implements a simple test of the 
    /// function ElmerModuleDriver::TrapezoidQuadrature - which is designed to
    /// integrate a function, f, over an n-way partitioned interval 
    /// \f$[x_0,x_n]\f$ . 
    ///
    /// The test first makes sure that the method integrates a linear 
    /// function exactly, and then that the error term has the proper
    /// dependence on the spacings.
    /// 
    virtual void Test__TrapezoidQuadrature(ResultsType &result) {
      std::ostringstream Ostr;
      std::vector<double> E;
      size_t n = 2*N.size();
      bool runs = true;
      for(std::vector<int>::iterator i = N.begin();i != N.end();i++){
        double Ii = 0.0;
        try {
          Ii = ElmerModuleDriver::TrapezoidQuadrature(TestFixture::F1,0,1,*i);
        } catch (...) {
          runs = false;
        }
        E.push_back(std::fabs(Ii-1.0));
      }
      bool order2 = (E[0] < 1e-14);
      for(std::vector<int>::iterator i = N.begin();i != N.end();i++){
        double Ii = 0.0;
        try {
          Ii = ElmerModuleDriver::TrapezoidQuadrature(TestFixture::F2,0,1,*i);
        } catch (...) {
          runs = false;
        }
        E.push_back(std::fabs(Ii-1.0));
      }
      result.UpdateResult("TrapezoidQuadrature:Runs",runs);
      result.UpdateResult("TrapezoidQuadrature:Accurate",E[n-1] < 1e-12);
      for(int i = N.size();i < n - 1;i++){
        double e = E[i+1]/E[i];
        double n1 = static_cast<double>(N[i-N.size()])/static_cast<double>(N[(i-N.size())+1]);
        double p = std::log(e)/std::log(n1);
        p -= 2;
        p = std::abs(p);
        if(p > 2e-2){
          order2 = false;
        }
      }
      result.UpdateResult("TrapezoidQuadrature:Order2",order2);
    }

    ///
    /// Test for ElmerModuleDriver::MidPointQuadrature
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    /// This function implements a simple test of the 
    /// function ElmerModuleDriver::MidPointQuadrature - which is designed to
    /// integrate a function, f, over an interval \f$[x_0,x_n]\f$. 
    ///
    /// The test first makes sure that the method integrates a linear 
    /// function exactly, and then that the error term has the proper
    /// dependence on the spacings.
    /// 
    virtual void Test__MidPointQuadrature(ResultsType &result) {
      std::ostringstream Ostr;
      std::vector<double> Ibar;
      std::vector<double> E;
      size_t n = 2*N.size();
      bool runs = true;
      bool order2 = true;
      for(std::vector<int>::iterator i = N.begin();i != N.end();i++){
        double Ii = 0.0;
        try {
          Ii = ElmerModuleDriver::MidPointQuadrature(TestFixture::F1,0,1,*i);
        } catch (...) {
          runs = false;
        }
        Ibar.push_back(Ii);
        E.push_back(std::fabs(Ii-1.0));
      }
      order2 = (E[0] < 1e-14);
      for(std::vector<int>::iterator i = N.begin();i != N.end();i++){
        double Ii = 0.0;
        try {
          Ii = ElmerModuleDriver::MidPointQuadrature(TestFixture::F2,0,1,*i);
        } catch (...) {
          runs = false;
        }
        Ibar.push_back(Ii);
        E.push_back(std::fabs(Ii-1.0));
      }
      result.UpdateResult("MidPointQuadrature:Runs",runs);
      result.UpdateResult("MidPointQuadrature:Accurate",E[n-1] < 1e-12);
      for(int i = N.size();i < n - 1;i++){
        double e = E[i+1]/E[i];
        double n1 = static_cast<double>(N[i-N.size()])/static_cast<double>(N[(i-N.size())+1]);
        double p = std::log(e)/std::log(n1);
        p -= 2;
        p = std::abs(p);
        if(p > 1e-2){
          order2 = false;
        }
      }
      result.UpdateResult("MidPointQuadrature:Order2",order2);
    }
  

    /// Function to compare two files
    /// 
    /// @param std::istream &stream1 File 1 to compare
    /// @param std::istream &stream2 File 2 to compare
    /// @param std::stringstream *ss Optional argument which returns
    /// the lines that differ
    //
    // Example use:
    // std::ifstream Inf1;
    // std::ifstream Inf2;
    // Inf1.open("file1");
    // Inf2.open("file2");
    //
    // Just find out if they differ....
    // if(streamdiff(Inf1,Inf2))
    //   std::cout << "Files differ." << std::endl;
    // 
    // Report the differences....
    // (reset the files)
    // streamdiff(Inf1,Inf2,&std::cout);
    //
    int streamdiff(std::istream &stream1,std::istream &stream2,std::stringstream *ss = NULL){
      int retval = 0;
      std::string line1,line2;
      while(!stream1.eof()){
        std::getline(stream1,line1);
        if(!stream2.eof())
          std::getline(stream2,line2);
        if(line1 != line2){
          if(ss){
            *ss << "1> " << line1 << std::endl
                           << "2> " << line2 << std::endl;
            retval = 1;
          }
          else
            return(1);
        }
      }
      if(stream1.eof() != stream2.eof()){
        return(1);
      }
      return(0);
    }
 
    /// Function to copy input data from source testing directory
    /// for use in unit tests.
    /// 
    /// @param suffix std::string for the input data location (it is
    /// appended to the source directory inputted on the command line)
    /// @param TestDir std::string path for directory to run the test in
    virtual int GetInputData(std::string suffix,std::string TestDir){

      std::cout << "In GetInputData" << std::endl;
      std::string DirName;
      std::string OrgDir;

      //Save original working directory for later
      OrgDir = IRAD::Sys::CWD();

      //Create full path for test input data directory
      DirName = SourceDirPath + suffix;

      //Check if data directory exists
      if(!IRAD::Sys::FILEEXISTS(DirName)){
        std::cout << "GetInputData Error: Could not find data directory, "
                  << DirName << ". Exiting (fail)." << std::endl;
        return -1;
      }

      std::ifstream Inf;
      std::ofstream Ouf;
      std::string InFileName;
      std::string OutFileName;
      int IntDir;

      //Create TestDir directory in current directory (if it doesn't
      //already exist) to run our tests in.
      if(!IRAD::Sys::FILEEXISTS(TestDir)){
        IntDir = IRAD::Sys::CreateDirectory(TestDir);
        if(IntDir == -1){
          std::cout << "GetInputData Error: Could not make directory, "
                    << TestDir << ". Exiting (fail)." << std::endl;
          return -1;
        }
      }

      //Open the input test data directory
      IRAD::Sys::Directory SourceDir(DirName);
     
      //Change directories to TestDir directory for running 
      IntDir = IRAD::Sys::ChDir(TestDir);
      if(IntDir == -1){
        std::cout << "GetInputData Error: Could not change directories to "
                  << TestDir << ". Exiting (fail)." << std::endl;
        return -1;
      }

      //Copy input data to TestDir directory for running 
      for(std::vector<std::string>::iterator it = SourceDir.begin();
          it != SourceDir.end(); ++it){ 
        InFileName = DirName + "/" + *it;
        OutFileName = IRAD::Sys::CWD() + "/" + *it;
        // copy source to distnation recursively
        if(IRAD::Sys::ISLINK(InFileName))
          // preserve links, don't traverse
          int IntDir = IRAD::Sys::SymLink(InFileName, OutFileName);
        else if(IRAD::Sys::ISDIR(InFileName)) {
          // recursively copy directories
          int IntDir = GetInputData(InFileName, OutFileName);
        } else {
          // copy files, travserse into directories recursively
          Inf.open(InFileName.c_str());
          Ouf.open(OutFileName.c_str());
          Ouf << Inf.rdbuf();
          Ouf.close();
          Inf.close();
        }

      }    

      //Change directories to back to original directory 
      IntDir = IRAD::Sys::ChDir(OrgDir);
      if(IntDir == -1){
        std::cout << "GetInputData Error: Could not change directories to "
                  << OrgDir << ". Exiting (fail)." << std::endl;
        return -1;
      }

      return 0;
    }
 

    ///*****************************************************************************
    /// Function to test the run function of Initialize,TimeStepper, and Finalize
    /// functionalities of SolverModule. Also, this function tests the registration
    /// and values of mesh and displacment data.
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    ///*****************************************************************************
    virtual void Test__ElmerUnitTests(ResultsType &result){
      std::cout << "Running Test__ElmerUnitTests" << std::endl; 
 
      int IntDir;
      std::string OrgDir, TestDir;
      bool ItAllWorks = true;

      // Arrays that old data for check *******************************************
      double CoordCheck[15][3] =
          { {2.2999999999999998, 0.19296300000000000, 0.0000000000000000},
            {2.2999999999999998, 0.00000000000000000, 0.0000000000000000},
            {2.2999999999999998, 0.39523399999999997, 0.0000000000000000},
            {2.2999999999999998, 0.60476600000000003, 0.0000000000000000},
            {2.2999999999999998, 0.80703700000000000, 0.0000000000000000},
            {2.2999999999999998, 1.00000000000000000, 0.0000000000000000},
            {2.3585799999999999, 1.14142000000000010, 0.0000000000000000},
            {2.5000000000000000, 1.20000000000000000, 0.0000000000000000},
            {2.6414200000000001, 1.14142000000000010, 0.0000000000000000},
            {2.7000000000000002, 1.00000000000000000, 0.0000000000000000},
            {2.7000000000000002, 0.80703700000000000, 0.0000000000000000},
            {2.7000000000000002, 0.60476600000000003, 0.0000000000000000},
            {2.7000000000000002, 0.39523399999999997, 0.0000000000000000},
            {2.7000000000000002, 0.19296300000000000, 0.0000000000000000},
            {2.7000000000000002, 0.00000000000000000, 0.0000000000000000} };
      int ConnCheck[14][2] = 
           { {1,2}, {3,1}, {4,3}, {5,4}, {6,5}, {7,6}, {8,7}, {9,8}, {10,9},
           {11,10}, {12,11}, {13,12}, {14,13}, {15,14} }; 

      // Getting Input data********************************************************
      //***************************************************************************
      
      //Call GetInputData to copy input test data to temp directory for running tests.
      std::string suffix = "/share/Testing/test_data/FSIobstacle";

      //Make path for directory to run the test in.
      TestDir = IRAD::Sys::CWD() + "/UnitTestData";
      IntDir = GetInputData(suffix, TestDir);

      //Check if GetInputData exited correctly.
      if(IntDir != 0){
        std::cout << "ElmerUnitTests Error: GetInputData call failed."
                  << std::endl;
        result.UpdateResult("ElmerUnitTests:Run", false);
        return;
      }

      //Check if TestDir directory (from GetInputData) exists in current directory
      if(!IRAD::Sys::FILEEXISTS(TestDir)){
        std::cout << "ElmerUnitTests Error: Could not find directory, "
                  << TestDir << ", for running tests." << std::endl;
        result.UpdateResult("ElmerUnitTests:Run", false);
        return;
      }

      //Save original directory name for later
      OrgDir = IRAD::Sys::CWD();

      //Change directories to TestDir directory for running 
      IntDir = IRAD::Sys::ChDir(TestDir);
      if(IntDir == -1){
        std::cout << "ElmerUnitTests Error: Could not change directories to "
                  << TestDir << "." << std::endl;
        result.UpdateResult("ElmerUnitTests:Run", false);
        return;
      }

      //load Elmer module**********************************************************
      //***************************************************************************
      COM_LOAD_MODULE_STATIC_DYNAMIC( ElmerCSC, "Window1");
   
      bool load_unload = true;

      //Get Winodw handle
      int h=COM_get_window_handle("Window1");
      if(h <= 0){
        std::cout << " After loading, COM_get_window_handle(\"Window1\") returns "
                  << h << std::endl;
        load_unload=false;
        std::cout << "ElmerUnitTests Error: Could not get window handle."
                  << "." << std::endl;
        result.UpdateResult("LoadSolverModule:Works",false);
        result.UpdateResult("ElmerUnitTests:Run", false);
        return;
      }
      else
        result.UpdateResult("LoadSolverModule:Works",true);

      ////////////////////////////////////////////////////
      // Get Initialize function handle and call Function
      ////////////////////////////////////////////////////
      int init_handle = COM_get_function_handle("Window1.Initialize");
      bool init_func = (init_handle > 0);
      int init_runs = 0;
      int verb=3;
      std::cout << "init = " << init_handle << std::endl;
      if(init_handle){
        //Call the function
        COM_call_function(init_handle, &init_runs, &verb);
      }
      else{
        std::cout << "InitializeRuns Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("Initialize:Runs", false);
        return;
      }

      //Check if function changed value of runs properly
      if(init_runs == 1){
        result.UpdateResult("Initialize:Runs", true);
      }
      else{
        result.UpdateResult("Initialize:Runs", false);
        ItAllWorks = false;
      }

      //////////////////////////////////////////////////
      // Check coordinate values
      //////////////////////////////////////////////////
      int coord_handle = COM_get_dataitem_handle("Window1.nc");
      int CoordSize=0;
      bool coordData = (coord_handle > 0);
      bool coordCorrect = true;
      std::cout << "coord_handle = " << coord_handle << std::endl;
      if(coordData){
        COM_get_size("Window1.nc",11,&CoordSize);
        if(CoordSize != 15){
          std::cout << "ElmerUnitTests Error:"
                    << " Coord array is incorrect size!" << std::endl;
          ItAllWorks = false;
          coordCorrect = false;
        }
        else{
          // Get the FSI mesh from the structures solver and print
          // it out to check
          double* Coord;
          COM_get_array("Window1.nc",11,&Coord);
          std::cout << "Coord:" << std::endl;
          for(int i=0; i < CoordSize; i++){
            for(int j=0; j < 3; j++){
              std::cout << Coord[i*3+j] << " ";
              if(fabs(CoordCheck[i][j] - Coord[i*3+j]) > 1.0e-12){
                ItAllWorks = false;
                coordCorrect = false;
              }
            }
            std::cout << std::endl;
          }
        }
        result.UpdateResult("CoordinateData:Correct", coordCorrect);
        result.UpdateResult("CoordinateData:Registered", true);
      }
      else{
        result.UpdateResult("CoordinateData:Registered", false);
        result.UpdateResult("CoordinateData:Correct", false);
        ItAllWorks = false;
      }

      //////////////////////////////////////////////////
      // Check connectivity values
      //////////////////////////////////////////////////
      bool connCorrect = true;
      int ConnSize = 0;
      int* Conn = NULL;

      // Get the FSI mesh from the structures solver
      COM_get_array("Window1.:b2:",11,&Conn);
      if(Conn){
        // Get the FSI mesh size from the structures solver
        COM_get_size("Window1.:b2:",11,&ConnSize);
        if(ConnSize != 14){
          std::cout << "ElmerUnitTests Error:"
                    << " Conn array is incorrect size!" << std::endl;
          ItAllWorks = false;
          connCorrect = false;
        }
        else{
          // check the values
          std::cout << "Conn:" << std::endl;
          for(int i=0; i < ConnSize; i++){
            for(int j=0; j < 2; j++){
              std::cout << Conn[i*2+j] << " ";
              if( (ConnCheck[i][j] - Conn[i*2+j]) != 0){
                ItAllWorks = false;
                connCorrect = false;
              }
            }
            std::cout << std::endl;
          }
        }
        result.UpdateResult("ConnectivityData:Correct", connCorrect);
        result.UpdateResult("ConnectivityData:Registered", true);
      }
      else{
        result.UpdateResult("ConnectivityData:Registered", false);
        result.UpdateResult("ConnectivityData:Correct", false);
        ItAllWorks = false;
      }

      //////////////////////////////////////////////////
      // Check load registration & initialization
      //////////////////////////////////////////////////
      double* Loads = NULL;
      int LoadsSize = 0;
      bool loadsRegistered = true, loadsInitialized = true;
      // Get the FSI loads from the structures solver
      COM_get_array("Window1.Loads",11,&Loads);
      if(Loads){
        std::cout << "Loads not NULL" << std::endl;
        // Get the FSI load size from the structures solver
        COM_get_size("Window1.Loads",11,&LoadsSize);
        if(LoadsSize != 15){
          std::cout << "ElmerUnitTests Error:"
                    << " Loads array is incorrect size!" << std::endl;
          ItAllWorks = false;
          loadsInitialized = false;
        }
        else{
          // check the values
          for(int i=0; i < LoadsSize; i++){
            for(int j=0; j < 3; j++){
              if(fabs(Loads[i*3 + j]) > 1.0e-12){
                loadsInitialized = false;
                ItAllWorks = false;
              }
            }
          }
        }
      }
      else{
        std::cout << "Loads are NULL" << std::endl;
        loadsRegistered = false;
        loadsInitialized = false;
        ItAllWorks = false;
      }
      result.UpdateResult("LoadData:Registered", loadsRegistered); 
      result.UpdateResult("LoadData:Initialized", loadsInitialized); 

      ////////////////////////////////////////////////////
      // Get TimeStepper function handle and call Function
      ////////////////////////////////////////////////////

      // Arrays that hold disp data for check *********************************
      double DispTime1[15][3] =
          {{  7.1605980866005031E-002,   3.5788052113870331E-002,   0.0000000000000000},     
           { -6.8871825230548747E-026,  -6.7515261027908644E-026,   0.0000000000000000},     
           {  0.16559117138148402    ,   6.1512134471618775E-002,   0.0000000000000000},     
           {  0.27105716643234351    ,   7.0647177803958988E-002,   0.0000000000000000},     
           {  0.37408733220172607    ,   6.6788253467455649E-002,   0.0000000000000000},     
           {  0.46975382688278072    ,   5.5232228766620040E-002,   0.0000000000000000},     
           {  0.53059533135003267    ,   1.7568173579321143E-002,   0.0000000000000000},     
           {  0.54088718131665003    ,  -5.0723278288525193E-002,   0.0000000000000000},     
           {  0.49733519851449115    ,  -0.10431856777439819    ,   0.0000000000000000},     
           {  0.42342243323024942    ,  -0.11544236710979815    ,   0.0000000000000000},     
           {  0.32858350010626158    ,  -9.6940873722311166E-002,   0.0000000000000000},     
           {  0.22913389910789400    ,  -7.6890455177259009E-002,   0.0000000000000000},     
           {  0.13256597264490513    ,  -5.4789959945688596E-002,   0.0000000000000000},     
           {  5.4547795662606154E-002,  -2.9570884780950012E-002,   0.0000000000000000},     
           { -6.8871825230548747E-026,  -6.7515261027908632E-026,   0.0000000000000000}};     
      double DispTime2[15][3] =
           {{  7.8382763777475994E-002,    3.7159701956680058E-002,   0.0000000000000000}, 
            { -1.3908554346559520E-053,   -1.3634598387444821E-053,   0.0000000000000000}, 
            {  0.17746729339780631    ,    6.3782065644847119E-002,   0.0000000000000000}, 
            {  0.28589059231716707    ,    7.3724507560836025E-002,   0.0000000000000000}, 
            {  0.39007099539893136    ,    7.0647599706400666E-002,   0.0000000000000000}, 
            {  0.48577572075292708    ,    5.9851545424915178E-002,   0.0000000000000000},  
            {  0.54619785916039254    ,    2.2562624600359866E-002,   0.0000000000000000}, 
            {  0.55515630051911513    ,   -4.5683794688744256E-002,   0.0000000000000000}, 
            {  0.51164185034902454    ,   -9.8434990773815095E-002,   0.0000000000000000}, 
            {  0.43876588136755668    ,   -0.10966508014917155    ,   0.0000000000000000}, 
            {  0.34337311330396592    ,   -9.2057033659364212E-002,   0.0000000000000000}, 
            {  0.24185886373003795    ,   -7.3235312951635573E-002,   0.0000000000000000}, 
            {  0.14168683127415216    ,   -5.2598213602584751E-002,   0.0000000000000000}, 
            {  5.9082767678702033E-002,   -2.8938645702423191E-002,   0.0000000000000000}, 
            { -1.3908554346559520E-053,   -1.3634598387444801E-053,   0.0000000000000000}}; 
      //*********************************************************************
      int runs_handle = COM_get_function_handle("Window1.Run");
      bool runs_func = (runs_handle > 0);
      bool DispCorrect = true;
      int runs_runs = 0;
      double time1 =  8.0;
      double time2 = 100.0;
      double* Disp = NULL;
      int DispSize = 0;
      COM_get_array("Window1.Displacements",11,&Disp);
      std::cout << "runs = " << runs_handle << std::endl;
      if(runs_handle){
        //Call the run function for Time1
        if(Loads){
           std::cout << "Changing load values" << std::endl;
           for(int i=0; i < LoadsSize; i++){
              for(int j=0; j < 3; j++){
                 Loads[i*3 + j] = double(i*3 + j);
                 std::cout << Loads[i*3 + j] << " ";
              }
              std::cout << std::endl;
           }
        }
        COM_call_function(runs_handle, &runs_runs, &time1);
      }
      else{
        std::cout << "TimeStepperRuns Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("TimeStepper:Runs", false);
        return;
      }

      //Check if function changed value of runs properly
      if(runs_runs > 0){
        result.UpdateResult("TimeStepper:Runs", true);
      }
      else{
        result.UpdateResult("TimeStepper:Runs", false);
        ItAllWorks = false;
      }

      // Check reigstration of displacement data
      if (Disp){
         result.UpdateResult("DisplacementData:Registered", true);
         // Get the FSI displacement size from the structures solver
         COM_get_size("Window1.Displacements",11,&DispSize);
         if(DispSize != 15){
           std::cout << "ElmerUnitTests Error:"
                     << " Disp array is incorrect size!" << std::endl;
           ItAllWorks = false;
           DispCorrect = false;
         }
      }
      else{
        result.UpdateResult("DisplacementData:Registered", false);
        DispCorrect = false;
        ItAllWorks = false;
      }
      
      // Check values of displacements after Run func @ Time1
      if (Disp && runs_handle && DispCorrect){
        std::cout << "Checking Displacements Time 1 (" << time1 << ")" << std::endl;
        for(int i=0; i < DispSize; i++){
           for(int j=0; j < 3; j++){
              std::cout << Disp[i*3+j] << " ";
              if(fabs(DispTime1[i][j] - Disp[i*3+j]) > 1.0e-12){
                ItAllWorks = false;
                DispCorrect = false;
              }
           }
           std::cout << std::endl;
        }
        //Change the load values again for testing
        if(Loads){
           std::cout << "Changing load values" << std::endl;
           for(int i=0; i < LoadsSize; i++){
              for(int j=0; j < 3; j++){
                 Loads[i*3 + j] = double(i*3 + j) + 1.0;
                 std::cout << Loads[i*3 + j] << " ";
              }
              std::cout << std::endl;
           }
        }
        //Call the run function for Time2
        int runs_runs2 = 0;
        COM_call_function(runs_handle, &runs_runs2, &time2);
        // Check values of displacements after Run func @ Time1
        std::cout << "Checking Displacements Time 2 (" << time2 << ")" << std::endl;
        for(int i=0; i < DispSize; i++){
           for(int j=0; j < 3; j++){
              std::cout << Disp[i*3+j] << " ";
              if(fabs(DispTime2[i][j] - Disp[i*3+j]) > 1.0e-12){
                ItAllWorks = false;
                DispCorrect = false;
              }
           }
           std::cout << std::endl;
        }
        //Check if function changed value of runs properly in Loads function
        if(runs_runs > 1 && runs_runs2 > 1){
          result.UpdateResult("TimeLoads:Runs", true);
        }
        else{
          std::cout << "runs_runs = " << runs_runs << std::endl;
          std::cout << "runs_runs2 = " << runs_runs2 << std::endl;
          result.UpdateResult("TimeLoads:Runs", false);
          ItAllWorks = false;
        }
      }
      result.UpdateResult("DisplacementData:Correct", DispCorrect);


      ////////////////////////////////////////////////////
      // Get Finalize function handle and call Function
      ////////////////////////////////////////////////////
      int final_handle = COM_get_function_handle("Window1.Finalize");
      bool final_func = (final_handle > 0);
      int final_runs = 0;
      std::cout << "final = " << final_handle << std::endl;
      if(final_handle){
        //Call the function
        COM_call_function(final_handle, &final_runs);
      }
      else{
        std::cout << "FinalizeRuns Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("Finalize:Runs", false);
        return;
      }

      //Check if function changed value of runs properly
      if(final_runs == 1){
        result.UpdateResult("Finalize:Runs", true);
      }
      else{
        result.UpdateResult("Finalize:Runs", false);
        ItAllWorks = false;
      }
      ////////////////////////////////////////////////////

      //Unload Elmer module********************************************************
      //***************************************************************************
      COM_UNLOAD_MODULE_STATIC_DYNAMIC( ElmerCSC, "Window1");

      //Make sure module unloaded
      h=COM_get_window_handle("Window1");
      if(h > 0){
        std::cout << "After unloading, COM_get_window_handle(\"Window1\") returns "
                  << h << std::endl;
        load_unload=false;
        std::cout << "ElmerUnitTests Error: Did not properly unload module."
                  << "." << std::endl;
        result.UpdateResult("ElmerUnitTests:Run", false);
        result.UpdateResult("UnloadSolverModule:Works",false);
        return;
      }
      else
        result.UpdateResult("UnloadSolverModule:Works",true);

      //Change directories out of TestDir directory back to original directory
      IntDir = IRAD::Sys::ChDir(OrgDir);
      if(IntDir == -1){
        std::cout << "ElmerUnitTests Error: Could not change directories to "
                  << OrgDir << "." << std::endl;
        result.UpdateResult("ElmerUnitTests:Run", false);
        return;
      }

      result.UpdateResult("ElmerUnitTests:Run", ItAllWorks);

    }


    ///*****************************************************************************
    ///*****************************************************************************

    ///*****************************************************************************
    /// Function to test our load function on a structures only problem.
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    ///*****************************************************************************
    virtual void Test__ElasticBeam3D_LoadFunction(ResultsType &result){
      std::cout << "Running Test__ElasticBeam3D_LoadFunction" << std::endl; 
 
      int IntDir;
      std::string OrgDir, TestDir;
      bool ItAllWorks = true;

      // Getting Input data********************************************************
      //***************************************************************************
      
      //Call GetInputData to copy input test data to temp directory for running tests.
      std::string suffix = "/share/Testing/test_data/ElasticBeam3D_LoadFunction";

      //Make path for directory to run the test in.
      TestDir = IRAD::Sys::CWD() + "/ElasticBeam3D_TestData";
      IntDir = GetInputData(suffix, TestDir);

      //Check if GetInputData exited correctly.
      if(IntDir != 0){
        std::cout << "ElasticBeam3D_LoadFunction Error: GetInputData call failed."
                  << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      //Check if TestDir directory (from GetInputData) exists in current directory
      if(!IRAD::Sys::FILEEXISTS(TestDir)){
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not find directory, "
                  << TestDir << ", for running tests." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      //Save original directory name for later
      OrgDir = IRAD::Sys::CWD();

      //Change directories to TestDir directory for running 
      IntDir = IRAD::Sys::ChDir(TestDir);
      if(IntDir == -1){
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not change directories to "
                  << TestDir << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      //load Elmer module**********************************************************
      //***************************************************************************
      COM_LOAD_MODULE_STATIC_DYNAMIC( ElmerCSC, "Window1");
   
      bool load_unload = true;

      //Get Winodw handle
      int h=COM_get_window_handle("Window1");
      if(h <= 0){
        std::cout << " After loading, COM_get_window_handle(\"Window1\") returns "
                  << h << std::endl;
        load_unload=false;
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not get window handle."
                  << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      ////////////////////////////////////////////////////
      // Get Initialize function handle and call Function
      ////////////////////////////////////////////////////
      int init_handle = COM_get_function_handle("Window1.Initialize");
      bool init_func = (init_handle > 0);
      int init_runs = 0;
      int verb=3;
      std::cout << "init = " << init_handle << std::endl;
      if(init_handle){
        //Call the function
        COM_call_function(init_handle, &init_runs, &verb);
      }
      else{
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      //////////////////////////////////////////////////
      // Check load registration & initialization
      //////////////////////////////////////////////////
      double* Loads = NULL;
      int LoadsSize = 0;
      bool loadsRegistered = true, loadsInitialized = true;
      // Get the FSI mesh from the structures solver
      COM_get_array("Window1.Loads",11,&Loads);
      if(Loads){
        COM_get_size("Window1.Loads",11,&LoadsSize);
        std::cout << "Loads not NULL" << std::endl;
        std::cout << "LoadsSize = " << LoadsSize << std::endl;
        // check the values
        for(int i=0; i < LoadsSize; i++){
          for(int j=0; j < 3; j++){
            if(fabs(Loads[i*3 + j]) > 1.0e-12){
               std::cout << "Loads were not intialized to 0.0!" << std::endl;
              result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
              return;
            }
          }
        }
      }
      else{
        std::cout << "Loads are NULL" << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      ////////////////////////////////////////////////////
      // Get TimeStepper function handle and call Function
      ////////////////////////////////////////////////////

      int runs_handle = COM_get_function_handle("Window1.Run");
      bool runs_func = (runs_handle > 0);
      bool DispCorrect = true;
      int runs_runs = 0;
      double time1 =  1.0;
      double* Disp = NULL;
      std::cout << "runs = " << runs_handle << std::endl;
      if(runs_handle){
        //Call the run function for Time1
        if(Loads){
           std::cout << "Changing load values" << std::endl;
           for(int i=0; i < LoadsSize; i++){
              Loads[i*3 + 1] = -4.0e5;
              for(int j=0; j < 3; j++){
                 std::cout << Loads[i*3 + j] << " ";
              }
              std::cout << std::endl;
           }
        }
        COM_call_function(runs_handle, &runs_runs, &time1);
      }
      else{
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      ////////////////////////////////////////////////////
      // Get Finalize function handle and call Function
      ////////////////////////////////////////////////////
      int final_handle = COM_get_function_handle("Window1.Finalize");
      bool final_func = (final_handle > 0);
      int final_runs = 0;
      std::cout << "final = " << final_handle << std::endl;
      if(final_handle){
        //Call the function
        COM_call_function(final_handle, &final_runs);
      }
      else{
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      ////////////////////////////////////////////////////
     
      ////////////////////////////////////////////////////
      // Compare the output file with an archived output file
      ////////////////////////////////////////////////////
      std::ifstream Inf, InfComp;
      std::stringstream ss;
      std::string word, line;

      Inf.open("case.ep");
      InfComp.open("case.ep_check");

      std::getline(Inf,line);
      std::getline(Inf,line);
      std::getline(InfComp,line);
      std::getline(InfComp,line);

      bool LoadWorks = true; 
      int diffReturn = -1; 
 
      diffReturn = streamdiff(Inf, InfComp, &ss);

      if(diffReturn){
        std::cout << "ElasticBeam3D_LoadFunction Error: case.ep "
                  << "case.ep_check differ!" << std::endl;
        std::cout << ss.str() << std::endl;
        if(ss.str() != " ")
          LoadWorks = false;
      }
  
      std::cout << "LoadWorks = " << LoadWorks << std::endl; 
      result.UpdateResult("ElasticBeam3D_LoadFunction:Works",LoadWorks);

      //Unload Elmer module********************************************************
      //***************************************************************************
      COM_UNLOAD_MODULE_STATIC_DYNAMIC( ElmerCSC , "Window1");

      //Make sure module unloaded
      h=COM_get_window_handle("Window1");
      if(h > 0){
        std::cout << "After unloading, COM_get_window_handle(\"Window1\") returns "
                  << h << std::endl;
        load_unload=false;
        std::cout << "ElasticBeam3D_LoadFunction Error: Did not properly unload module."
                  << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      //Change directories out of TestDir directory back to original directory
      IntDir = IRAD::Sys::ChDir(OrgDir);
      if(IntDir == -1){
        std::cout << "ElasticBeam3D_LoadFunction Error: Could not change directories to "
                  << OrgDir << "." << std::endl;
        result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", false);
        return;
      }

      result.UpdateResult("ElasticBeam3D_LoadFunction:Runs", true);

    }
    ///*****************************************************************************
    ///*****************************************************************************

    ///************************************************************************
    /// Functions to call Serial tests
    ///************************************************************************
    ///
    /// Runs all tests implemented by the ElmerModuleDriver::TestingObject.
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    virtual void Process(ResultsType &result){
      Prologue();
      Test__ExampleFunction(result);
      Test__TrapezoidQuadrature(result);
      Test__MidPointQuadrature(result);
      Test__ElmerUnitTests(result);
      Epilogue();
    }
    
    ///
    /// Runs a test specified by name
    ///
    /// @param name String name of the test to run.
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    virtual void RunTest(const std::string &name,ResultsType &result)
    {
      Prologue();
      if(name == "ExampleFunction")
        Test__ExampleFunction(result);
      else if(name == "TrapezoidQuadrature")
        Test__TrapezoidQuadrature(result);
      else if(name == "MidPointQuadrature")
        Test__MidPointQuadrature(result);
      else if(name == "ElmerUnitTests")
        Test__ElmerUnitTests(result);
      else if(name == "ElasticBeam3D_LoadFunction")
        Test__ElasticBeam3D_LoadFunction(result);
      Epilogue();
    }

    ///
    /// Runs a list of tests specified by name
    /// 
    /// @param test_names list of string test names
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    virtual void ProcessTests(std::list<std::string> &test_names,ResultsType &result){
      Prologue();
      std::list<std::string>::iterator tni = test_names.begin();
      while(tni != test_names.end())
        RunTest(*tni++,result);
      Epilogue();
    }


  };

    ///************************************************************************
    /// List of Parallel tests
    ///************************************************************************
    ///
#ifdef _ELMERMODULEDRIVER_PARALLEL_
  /// 
  /// Project-specific parallel testing object.
  ///
  /// The project-specific parallel testing object encapsulates
  /// all parallel tests for the project. This is the interface
  /// to all "in-core" parallel testing available for the given project.
  /// It implements the interface given by 
  /// ElmerModuleDriver::TestingObject and has an 
  /// IRAD::Comm::CommunicatorObject for parallel capabilities.
  ///
  template<typename CommType,typename ResultsType>
  class ParallelTestingObject : TestingObject<ResultsType>
  {
  protected:
    // Example test fixtures for parallel ElmerModuleDriver testing.
    //
    /// 
    /// The CommunicatorObject for the parallel testing object.
    ///
    CommType _communicator;
    ///
    /// A set of values for testing quadrature methods.
    /// 
    std::vector<int> N;
    ///
    /// The string for the testing source directory.
    ///
    std::string SourceDirPath;
  public:
    ///
    /// Constructor with incoming communicator.
    ///
    ParallelTestingObject(CommType &incomm) :
      TestingObject<ResultsType>(), _communicator(incomm) {};
    CommType &GetCommunicator() { return(_communicator); };

    ///
    /// Tears down the testing fixtures if needed.
    ///
    virtual void Epilogue() {};

    ///
    /// Sets the string value of the testing source directory.
    /// 
    void SetSourceDirPath(std::string input){
      SourceDirPath = input;
    }

    ///
    /// Sets up the data fixtures for the tests.
    /// 
    /// This function gives the object a chance to 
    /// allocate and set up any "fixtures" before the
    /// tests are run.
    ///
    virtual void  Prologue(){
      for(int i = _communicator.Size();i <= 1000000;i*=2) N.push_back(i);
    };

    /// Function to copy input data from source testing directory
    /// for use in unit tests.
    /// 
    /// @param suffix std::string for the input data location (it is
    /// appended to the source directory inputted on the command line)
    /// @param TestDir std::string path for directory to run the test in
    virtual int GetInputData(std::string sourceDirName, std::string TestDir){
      std::string OrgDir;
      //Save original working directory for later
      OrgDir = IRAD::Sys::CWD();
      //Check if data directory exists
      if(!IRAD::Sys::FILEEXISTS(sourceDirName)){
        std::cout << "GetInputData Error: Could not find data directory, "
                  << sourceDirName << ". Exiting (fail)." << std::endl;
        return -1;
      }
      std::ifstream Inf;
      std::ofstream Ouf;
      std::string InFileName;
      std::string OutFileName;
      int IntDir;
      //Create TestDir directory in current directory (if it doesn't
      //already exist) to run our tests in.
      if(!IRAD::Sys::FILEEXISTS(TestDir)){
        IntDir = IRAD::Sys::CreateDirectory(TestDir);
        if(IntDir == -1){
          std::cout << "GetInputData Error: Could not make directory, "
                    << TestDir << ". Exiting (fail)." << std::endl;
          return -1;
        }
      }
      //Open the input test data directory
      IRAD::Sys::Directory SourceDir(sourceDirName);
      //Change directories to TestDir directory for running 
      IntDir = IRAD::Sys::ChDir(TestDir);
      if(IntDir == -1){
        std::cout << "GetInputData Error: Could not change directories to "
                  << TestDir << ". Exiting (fail)." << std::endl;
        return -1;
      }
      //Copy input data to TestDir directory for running 
      for(std::vector<std::string>::iterator it = SourceDir.begin();
          it != SourceDir.end(); ++it){ 
        InFileName = sourceDirName + "/" + *it;
        OutFileName = IRAD::Sys::CWD() + "/" + *it;
        //std::cout << "InFileName = " << InFileName 
        //          << ", OutFileName = " << OutFileName 
        //          << std::endl;
        // copy source to distnation recursively
        if(IRAD::Sys::ISLINK(InFileName))
          // preserve links, don't traverse
          int IntDir = IRAD::Sys::SymLink(InFileName, OutFileName);
        else if(IRAD::Sys::ISDIR(InFileName)) {
          // recursively copy directories
          int IntDir = GetInputData(InFileName, OutFileName);
        } else {
          // copy files, travserse into directories recursively
          Inf.open(InFileName.c_str());
          Ouf.open(OutFileName.c_str());
          Ouf << Inf.rdbuf();
          Ouf.close();
          Inf.close();
        }

      }    
      //Change directories to back to original directory 
      IntDir = IRAD::Sys::ChDir(OrgDir);
      if(IntDir == -1){
        std::cout << "GetInputData Error: Could not change directories to "
                  << OrgDir << ". Exiting (fail)." << std::endl;
        return -1;
      }
      return 0;
    }

    ///
    /// Parallel test for ElmerModuleDriver::TrapezoidQuadrature
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    /// This function implements a simple parallel test of the 
    /// function ElmerModuleDriver::TrapezoidQuadrature - which is designed to
    /// integrate a function, f, over an n-way partitioned interval 
    /// \f$[x_0,x_n]\f$ in parallel. 
    ///
    /// The test first makes sure that the method integrates a linear 
    /// function exactly, and then that the error term has the proper
    /// dependence on the spacings. Then the algorithm scaling is checked
    /// by running on subsets of the total number of processors and 
    /// checking time-to-solution.
    /// 
    virtual void Test__ParallelTrapezoidQuadrature(ResultsType &result) {
      std::ostringstream Ostr;
      int fixed_n = 1000000;
      int rank  = _communicator.Rank();
      int nproc = _communicator.Size(); 
      double Ii = 0.0;
      bool runs = true;
      bool accurate = true;
      bool order2 = true;
      bool scales = true;
      std::vector<double> E;
      std::vector<double> times;
      for(int i = 1;i <= nproc;i*=nproc){
        CommType subcomm;
        int color = (rank < i);
        _communicator.Split(color,rank,subcomm);
        int nproc_color = subcomm.Size();
        double time0 = IRAD::Profiler::Time();
        if(color){
          Ii = 0.0;
          for(int j = 0;j < 200;j++){
            if(runs){
              try {
                Ii += ElmerModuleDriver::TrapezoidQuadrature(TestFixture::F1,0,1,fixed_n);
              } catch (...) {
                subcomm.SetExit(1);
              }
              if(subcomm.Check())
                runs = false;
            }
          }
          if(runs){
            double Itot = 0.0;
            subcomm.AllReduce(Ii, Itot,IRAD::Comm::DTDOUBLE, IRAD::Comm::SUMOP);
            double error = 0.0;
            error = std::abs(Itot - (200*nproc_color));
            accurate = (error < 1e-14);
            order2 = accurate;
          }
        }
        times.push_back(IRAD::Profiler::Time() - time0);
        if(nproc == 1)
          break;
      }
      if(!rank){
        double dt = *times.rbegin() - *times.begin();
        double percent_change = dt/(*times.begin());
        scales = (percent_change < (nproc*.005));
      }
      result.UpdateResult("ParallelTrapezoidQuadrature:Runs",runs);
      result.UpdateResult("ParallelTrapezoidQuadrature:Accurate",accurate);
      if(nproc > 1) // only report scaling results when procs are more than 1.
        result.UpdateResult("ParallelTrapezoidQuadrature:WeakScaling",scales);
      times.resize(0);
      if(nproc > 1){ // If running on more than one proc, then do strong scaling test.
        for(int i = 1;i <= nproc;i*=nproc){
          CommType subcomm;
          int color = (rank < i);
          _communicator.Split(color,rank,subcomm);
          int nproc_color = subcomm.Size();
          double time0 = IRAD::Profiler::Time();
          if(color){
            int npart = *N.rbegin()/nproc_color;
            Ii = 0.0;
            for(int j = 0;j < 200;j++){
              if(runs){
                try {
                  Ii += ElmerModuleDriver::TrapezoidQuadrature(TestFixture::F1,0,1,npart);
                } catch (...) {
                  subcomm.SetExit(1);
                }
                if(subcomm.Check())
                  runs = false;
              }
            }
            if(runs){
              double Itot = 0.0;
              subcomm.AllReduce(Ii, Itot,IRAD::Comm::DTDOUBLE, IRAD::Comm::SUMOP);
              double error = 0.0;
              error = std::abs(Itot - (200*nproc_color));
              accurate = (error < 1e-14);
              order2 = accurate;
            }
          }
          times.push_back(IRAD::Profiler::Time() - time0);
        }
        if(!rank){
          double n_t = (*times.begin())/(*times.rbegin());
          double n_p = n_t - nproc;
          n_p = std::abs(n_p)/nproc;
          scales = (n_p < 2e-1);
        }
        result.UpdateResult("ParallelTrapezoidQuadrateure:StrongScaling",scales);
      }
      for(int i = nproc;i <= 1000000;i*=10){
        if(runs){
          Ii = 0.0;
          int n = i/nproc;
          try {
            Ii = ElmerModuleDriver::MidPointQuadrature(TestFixture::F2,0,1,n);
          } catch (...) {
            _communicator.SetExit(1);
          }
          if(_communicator.Check())
            runs = false;
          if(runs){
            double Itot = 0.0;
            _communicator.AllReduce(Ii, Itot,IRAD::Comm::DTDOUBLE, IRAD::Comm::SUMOP);
            double error = 0.0;
            error = std::abs(Itot - nproc);
            E.push_back(error);
          }
        }
      }
      int esize = E.size();
      for(int i = 0;i < esize-1;i++){
        double e = E[i+1]/E[i];
        double n1 = .1;
        double p = std::log(e)/std::log(n1);
        p -= 2;
        p = std::abs(p);
        if(p > 1e-1){
          order2 = false;
        }
      }
      result.UpdateResult("ParallelTrapezoidQuadrature:Order2",order2);
    }
    
    
    ///
    /// Parallel test for ElmerModuleDriver::MidPointQuadrature
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    /// This function implements a simple parallel test of the 
    /// function ElmerModuleDriver::MidPointQuadrature - which is designed to
    /// integrate a function, f, over an n-way partitioned interval 
    /// \f$[x_0,x_n]\f$ in parallel. 
    ///
    /// The test first makes sure that the method integrates a linear 
    /// function exactly, and then that the error term has the proper
    /// dependence on the spacings.  Then the parallel scaling of the
    /// algorithm is checked by running on subsets of the total number
    /// of processors and checking time-to-solution.
    /// 
    virtual void Test__ParallelMidPointQuadrature(ResultsType &result) {
      std::ostringstream Ostr;
      int fixed_n = 1000000;
      int rank  = _communicator.Rank();
      int nproc = _communicator.Size(); 
      double Ii = 0.0;
      bool runs = true;
      bool accurate = true;
      bool order2 = true;
      bool scales = true;
      std::vector<double> E;
      std::vector<double> times;
      for(int i = 1;i <= nproc;i*=nproc){
        CommType subcomm;
        int color = (rank < i);
        _communicator.Split(color,rank,subcomm);
        int nproc_color = subcomm.Size();
        double time0 = IRAD::Profiler::Time();
        if(color){
          Ii = 0.0;
          for(int j = 0;j < 200;j++){
            if(runs){
              try {
                Ii += ElmerModuleDriver::MidPointQuadrature(TestFixture::F1,0,1,fixed_n);
              } catch (...) {
                subcomm.SetExit(1);
              }
              if(subcomm.Check())
                runs = false;
            }
          }
          if(runs){
            double Itot = 0.0;
            subcomm.AllReduce(Ii, Itot,IRAD::Comm::DTDOUBLE, IRAD::Comm::SUMOP);
            double error = 0.0;
            error = std::abs(Itot - (200*nproc_color));
            accurate = (error < 1e-14);
            order2 = accurate;
          }
        }
        times.push_back(IRAD::Profiler::Time() - time0);
        if(nproc == 1)
          break;
      }
      if(!rank){
        double dt = *times.rbegin() - *times.begin();
        double percent_change = dt/(*times.begin());
        scales = (percent_change < (nproc*.005));
        // std::cout << "Weak scaling timings:" << std::endl; 
        // std::vector<double>::iterator ti = weak_times.begin();
        // while(ti != weak_times.end()){
        //   std::cout << ti-weak_times.begin()+1 << "   " 
        //             << *ti << std::endl;
        //   ti++;
        // }
      }
      result.UpdateResult("ParallelMidPointQuadrature:Runs",runs);
      result.UpdateResult("ParallelMidPointQuadrature:Accurate",accurate);
      if(nproc > 1) // only report scaling results for nproc > 1
        result.UpdateResult("ParallelMidPointQuadrature:WeakScaling",scales);
      times.resize(0);
      if(nproc > 1){ // only do scaling test if nproc > 1
        for(int i = 1;i <= nproc;i*=nproc){
          CommType subcomm;
          int color = (rank < i);
          _communicator.Split(color,rank,subcomm);
          int nproc_color = subcomm.Size();
          double time0 = IRAD::Profiler::Time();
          if(color){
            int npart = *N.rbegin()/nproc_color;
            Ii = 0.0;
            for(int j = 0;j < 200;j++){
              if(runs){
                try {
                  Ii += ElmerModuleDriver::MidPointQuadrature(TestFixture::F1,0,1,npart);
                } catch (...) {
                  subcomm.SetExit(1);
                }
                if(subcomm.Check())
                  runs = false;
              }
            }
            if(runs){
              double Itot = 0.0;
              subcomm.AllReduce(Ii, Itot,IRAD::Comm::DTDOUBLE, IRAD::Comm::SUMOP);
              double error = 0.0;
              error = std::abs(Itot - (200*nproc_color));
              accurate = (error < 1e-14);
              order2 = accurate;
            }
          }
          times.push_back(IRAD::Profiler::Time() - time0);
        }
        if(!rank){
          double n_t = (*times.begin())/(*times.rbegin());
          double n_p = n_t - nproc;
          n_p = std::abs(n_p)/nproc;
          scales = (n_p < 2e-1); 
       }
        result.UpdateResult("ParallelMidPointQuadrateure:StrongScaling",scales);
      }
      for(int i = nproc;i <= 1000000;i*=10){
        if(runs){
          Ii = 0.0;
          int n = i/nproc;
          try {
            Ii = ElmerModuleDriver::MidPointQuadrature(TestFixture::F2,0,1,n);
          } catch (...) {
            _communicator.SetExit(1);
          }
          if(_communicator.Check())
            runs = false;
          if(runs){
            double Itot = 0.0;
            _communicator.AllReduce(Ii, Itot,IRAD::Comm::DTDOUBLE, IRAD::Comm::SUMOP);
            double error = 0.0;
            error = std::abs(Itot - nproc);
            E.push_back(error);
          }
        }
      }
      int esize = E.size();
      for(int i = 0;i < esize-1;i++){
        double e = E[i+1]/E[i];
        double n1 = .1;
        double p = std::log(e)/std::log(n1);
        p -= 2;
        p = std::abs(p);
        if(p > 1e-1){
          order2 = false;
        }
      }
      result.UpdateResult("ParallelMidPointQuadrature:Order2",order2);
    }

    ///*****************************************************************************
    /// Function to test if parallel communicator for Elmer module is set properly
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    ///*****************************************************************************
    virtual void Test__ParCommunicatorPassToModule(ResultsType &result){
      std::cout << "Running Test__ParCommunicatorPassToModule" << std::endl;
      bool commSets = false;
      bool commSize = false;
      bool commPass = false;
      int rank = (GetCommunicator()).Rank();
      int nProc = (GetCommunicator()).Size();
      MPI_Comm comm_check, comm_orig;
      // check communicator
      comm_orig = (GetCommunicator()).GetCommunicator();
      // changing default communicator for COM
      COM_set_default_communicator((GetCommunicator()).GetCommunicator());
      // loading parallel Elmer module   
      COM_LOAD_MODULE_STATIC_DYNAMIC( ElmerCSCParallel, "ElmerModule");
      // cheking default communicator
      COM_get_communicator("ElmerModule", &comm_check);
      if (comm_check == comm_orig)
         commSets = true;
      else
         std::cout << "Rank #" << rank << ", "
                   << "Default communicator for window is not set properly. It supposed to be "
                   << comm_orig << ", but it is set to " << comm_check << std::endl; 
      // cheking the number of processors
      int* nProcWindow;
      COM_get_array("ElmerModule.nProc", 0, &nProcWindow);
      if (nProc == *nProcWindow)
         commSize = true;
      else
         std::cout << "Rank #" << rank << ", "
                   << "Size of the communicator for window is not correct. It supposed to be "
                   << nProc << ", but it is set to " << *nProcWindow << std::endl; 
      // cheking communicator
      if (commSize && commSets)
        commPass = true; 
      // unloading the module
      COM_UNLOAD_MODULE_STATIC_DYNAMIC( ElmerCSCParallel, "ElmerModule");
      // setting test flag
      result.UpdateResult("ParCommunicatorSetToModule:Works", commSets);
      result.UpdateResult("ParCommunicatorSizeToModule:Works", commSets);
      result.UpdateResult("ParCommunicatorPassToModule:Works", commPass);
    }
 

    ///************************************************************************
    /// Parallel Elmer module unit tests.
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    ///************************************************************************
    virtual void Test__ParElmerUnitTests(ResultsType &result){
      std::cout << "Running Test__ParElmerUnitTests" << std::endl;

      int IntDir;
      std::string OrgDir, TestDir;
      int ItAllWorks = 1;
      int nproc = _communicator.Size();
      int rank = _communicator.Rank();

      // Arrays that old data for check *******************************************
      /*
      int ConnCheck[14][2] = 
           { {1,2}, {3,1}, {4,3}, {5,4}, {6,5}, {7,6}, {8,7}, {9,8}, {10,9},
           {11,10}, {12,11}, {13,12}, {14,13}, {15,14} }; 
      */

      /////////////////////////////////////////
      // Getting Input data
      /////////////////////////////////////////
      
      //Call GetInputData to copy input test data to temp directory for running tests.
      std::string sourceDirName = SourceDirPath + "/share/Testing/test_data/ParElasticBeam3D_LoadFunction";
      //Make path for directory to run the test in.
      TestDir = IRAD::Sys::CWD() + "/ParUnitTestData";
      if (rank == 0){
        IntDir = GetInputData(sourceDirName, TestDir);
        //Check if GetInputData exited correctly.
        if(IntDir != 0){
          std::cout << "ParElmerUnitTests Error: GetInputData call failed."
                    << std::endl;
          result.UpdateResult("ElmerUnitTests:Run", false);
        }
      }
      _communicator.Barrier();
      //Check if TestDir directory (from GetInputData) exists in current directory
      if(!IRAD::Sys::FILEEXISTS(TestDir)){
        std::cout << "ParElmerUnitTests Error: Could not find directory, "
                  << TestDir << ", for running tests." << std::endl;
        result.UpdateResult("ParElmerUnitTests:Run", false);
        return;
      }
      //Save original directory name for later
      OrgDir = IRAD::Sys::CWD();
      //Change directories to TestDir directory for running 
      IntDir = IRAD::Sys::ChDir(TestDir);
      if(IntDir == -1){
        std::cout << "ElmerUnitTests Error: Could not change directories to "
                  << TestDir << "." << std::endl;
        result.UpdateResult("ElmerUnitTests:Run", false);
        return;
      }

      ////////////////////////////////////////////////////
      //load Elmer module
      ////////////////////////////////////////////////////
      COM_LOAD_MODULE_STATIC_DYNAMIC( ElmerCSCParallel, "Window1");
      bool load_unload = true;

      //Get Winodw handle
      int h = COM_get_window_handle("Window1");
      if(h <= 0){
        std::cout << " After loading, COM_get_window_handle(\"Window1\") returns "
                  << h << std::endl;
        load_unload=false;
        std::cout << "ElmerUnitTests Error: Could not get window handle."
                  << "." << std::endl;
        result.UpdateResult("ParLoadSolverModule:Works",false);
        result.UpdateResult("ParElmerUnitTests:Run", false);
        return;
      }
      else
        result.UpdateResult("ParLoadSolverModule:Works",true);
      
      
      ////////////////////////////////////////////////////
      // Get Initialize function handle and call Function
      ////////////////////////////////////////////////////
      int init_handle = COM_get_function_handle("Window1.Initialize");
      bool init_func = (init_handle > 0);
      int init_runs = 0;
      int verb=3;
      if(init_handle){
        //Call the function
        COM_call_function(init_handle, &init_runs, &verb);
      }
      else{
        std::cout << "InitializeRuns Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("ParInitialize:Runs", false);
        return;
      }
      //Check if function changed value of runs properly
      if(init_runs == 1){
        result.UpdateResult("ParInitialize:Runs", true);
      }
      else{
        result.UpdateResult("ParInitialize:Runs", false);
        ItAllWorks = 0;
      }
      
      //////////////////////////////////////////////////
      // Check coordinate values
      //////////////////////////////////////////////////
      double CoordCheck0[306][3] = {
              {0.013875704358900, 0.050000000000000, 0.000000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.000000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.010000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.010000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.000000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.010000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.000000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.010000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.000000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.010000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.000000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.010000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.020000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.020000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.020000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.020000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.020000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.020000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.030000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.030000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.030000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.030000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.030000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.030000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.040000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.040000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.040000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.040000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.040000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.040000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.050000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.050000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.050000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.050000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.050000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.050000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.060000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.060000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.060000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.060000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.060000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.060000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.070000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.070000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.070000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.070000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.070000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.070000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.080000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.080000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.080000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.080000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.080000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.080000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.090000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.090000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.090000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.090000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.090000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.090000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.100000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.100000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.100000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.100000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.100000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.100000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.110000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.110000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.110000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.110000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.110000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.110000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.120000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.120000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.120000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.120000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.120000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.120000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.130000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.130000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.130000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.130000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.130000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.130000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.140000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.140000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.140000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.140000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.140000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.140000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.150000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.150000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.150000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.150000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.150000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.150000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.160000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.160000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.160000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.160000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.160000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.160000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.170000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.170000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.170000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.170000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.170000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.170000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.180000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.180000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.180000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.180000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.180000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.180000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.190000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.190000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.190000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.190000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.190000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.190000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.200000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.200000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.200000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.200000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.200000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.200000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.210000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.210000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.210000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.210000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.210000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.210000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.220000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.220000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.220000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.220000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.220000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.220000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.230000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.230000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.230000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.230000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.230000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.230000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.240000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.240000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.240000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.240000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.240000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.240000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.250000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.250000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.250000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.250000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.250000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.250000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.260000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.260000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.260000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.260000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.260000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.260000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.270000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.270000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.270000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.270000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.270000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.270000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.280000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.280000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.280000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.280000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.280000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.280000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.290000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.290000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.290000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.290000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.290000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.290000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.300000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.300000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.300000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.300000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.300000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.300000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.310000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.310000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.310000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.310000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.310000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.310000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.320000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.320000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.320000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.320000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.320000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.320000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.330000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.330000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.330000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.330000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.330000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.330000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.340000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.340000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.340000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.340000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.340000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.340000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.350000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.350000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.350000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.350000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.350000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.350000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.360000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.360000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.360000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.360000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.360000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.360000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.370000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.370000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.370000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.370000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.370000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.370000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.380000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.380000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.380000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.380000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.380000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.380000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.390000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.390000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.390000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.390000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.390000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.390000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.400000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.400000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.400000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.400000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.400000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.400000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.410000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.410000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.410000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.410000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.410000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.410000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.420000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.420000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.420000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.420000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.420000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.420000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.430000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.430000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.430000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.430000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.430000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.430000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.440000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.440000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.440000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.440000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.440000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.440000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.450000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.450000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.450000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.450000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.450000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.450000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.460000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.460000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.460000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.460000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.460000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.460000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.470000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.470000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.470000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.470000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.470000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.470000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.480000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.480000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.480000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.480000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.480000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.480000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.490000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.490000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.490000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.490000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.490000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.490000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.500000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.500000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.500000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.500000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.500000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.500000000000000} }; 

      double CoordCheck1[306][3] = {
              {0.013875704358900, 0.050000000000000, 0.500000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.500000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.510000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.510000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.500000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.510000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.500000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.510000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.500000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.510000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.500000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.510000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.520000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.520000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.520000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.520000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.520000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.520000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.530000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.530000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.530000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.530000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.530000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.530000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.540000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.540000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.540000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.540000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.540000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.540000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.550000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.550000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.550000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.550000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.550000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.550000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.560000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.560000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.560000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.560000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.560000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.560000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.570000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.570000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.570000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.570000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.570000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.570000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.580000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.580000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.580000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.580000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.580000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.580000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.590000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.590000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.590000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.590000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.590000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.590000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.600000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.600000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.600000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.600000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.600000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.600000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.610000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.610000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.610000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.610000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.610000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.610000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.620000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.620000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.620000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.620000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.620000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.620000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.630000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.630000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.630000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.630000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.630000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.630000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.640000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.640000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.640000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.640000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.640000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.640000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.650000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.650000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.650000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.650000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.650000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.650000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.660000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.660000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.660000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.660000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.660000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.660000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.670000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.670000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.670000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.670000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.670000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.670000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.680000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.680000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.680000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.680000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.680000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.680000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.690000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.690000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.690000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.690000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.690000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.690000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.700000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.700000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.700000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.700000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.700000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.700000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.710000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.710000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.710000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.710000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.710000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.710000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.720000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.720000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.720000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.720000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.720000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.720000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.730000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.730000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.730000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.730000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.730000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.730000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.740000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.740000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.740000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.740000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.740000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.740000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.750000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.750000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.750000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.750000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.750000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.750000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.760000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.760000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.760000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.760000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.760000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.760000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.770000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.770000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.770000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.770000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.770000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.770000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.780000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.780000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.780000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.780000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.780000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.780000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.790000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.790000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.790000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.790000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.790000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.790000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.800000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.800000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.800000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.800000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.800000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.800000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.810000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.810000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.810000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.810000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.810000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.810000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.820000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.820000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.820000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.820000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.820000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.820000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.830000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.830000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.830000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.830000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.830000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.830000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.840000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.840000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.840000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.840000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.840000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.840000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.850000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.850000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.850000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.850000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.850000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.850000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.860000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.860000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.860000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.860000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.860000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.860000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.870000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.870000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.870000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.870000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.870000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.870000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.880000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.880000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.880000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.880000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.880000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.880000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.890000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.890000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.890000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.890000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.890000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.890000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.900000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.900000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.900000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.900000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.900000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.900000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.910000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.910000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.910000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.910000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.910000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.910000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.920000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.920000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.920000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.920000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.920000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.920000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.930000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.930000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.930000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.930000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.930000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.930000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.940000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.940000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.940000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.940000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.940000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.940000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.950000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.950000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.950000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.950000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.950000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.950000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.960000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.960000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.960000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.960000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.960000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.960000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.970000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.970000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.970000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.970000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.970000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.970000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.980000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.980000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.980000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.980000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.980000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.980000000000000}, 
              {0.000000000000000, 0.050000000000000, 0.990000000000000}, 
              {0.013875704358900, 0.050000000000000, 0.990000000000000}, 
              {0.033498913650700, 0.050000000000000, 0.990000000000000}, 
              {0.066501086349300, 0.050000000000000, 0.990000000000000}, 
              {0.086124295641100, 0.050000000000000, 0.990000000000000}, 
              {0.100000000000000, 0.050000000000000, 0.990000000000000}, 
              {0.000000000000000, 0.050000000000000, 1.000000000000000}, 
              {0.013875704358900, 0.050000000000000, 1.000000000000000}, 
              {0.033498913650700, 0.050000000000000, 1.000000000000000}, 
              {0.066501086349300, 0.050000000000000, 1.000000000000000}, 
              {0.086124295641100, 0.050000000000000, 1.000000000000000}, 
              {0.100000000000000, 0.050000000000000, 1.000000000000000} };
      int coord_handle = COM_get_dataitem_handle("Window1.nc");
      int CoordSize=0;
      bool coordData = (coord_handle > 0);
      bool coordCorrect = true;
      if(coordData){
        COM_get_size("Window1.nc",100+rank,&CoordSize);
        std::cout << "Rank #" << rank 
                  << ", coord size = " << CoordSize 
                  << std::endl;
        if(CoordSize != 306){
          std::cout << "ParElmerUnitTests Error:"
                    << " Coord array is incorrect size!" << std::endl;
          ItAllWorks = 0;
          coordCorrect = false;
        }
        else{
          // Get the FSI mesh from the structures solver and print
          // it out to check
         
          //std::ofstream Ouf;
          //std::stringstream ss; 
          //std::string str_rank;
          //ss << rank;
          //ss >> str_rank;
          //std::string outFileName = "coords_" + str_rank + ".dat";
          //Ouf.open(outFileName.c_str());
          double* Coord;
          COM_get_array("Window1.nc",100+rank,&Coord);
          std::cout << "Checking registered coordinates:" << std::endl;
          for(int i=0; i < CoordSize; i++){
            //std::cout << "Rank #" << rank << " Point #" << i 
            //          << " --- ";
            //Ouf << "{" ;
            for(int j=0; j < 3; j++){
               //std::cout << std::fixed << std::setprecision(15) << Coord[i*3+j] << " ";
               //Ouf << std::fixed << std::setprecision(15) << Coord[i*3+j];
               //if (j<2)
               //  Ouf << ", ";
               //else
               //  Ouf << "}, ";
              if (rank == 0) {
                 if(fabs(CoordCheck0[i][j] - Coord[i*3+j]) > 1.0e-12){
                   std::cout << "Wrong coordinate for Rank #" << rank 
                             << " Point #" << i 
                             << " Component #" << j
                             << " check = " << CoordCheck0[i][j]
                             << " actual = " << Coord[i*3+j]
                             << std::endl;
                   ItAllWorks = 0;
                   coordCorrect = false;
                   break;
                 }
              } else if (rank == 1) {
                 if(fabs(CoordCheck1[i][j] - Coord[i*3+j]) > 1.0e-12){
                   std::cout << "Wrong coordinate for Rank #" << rank 
                             << " Point #" << i 
                             << " Component #" << j
                             << " check = " << CoordCheck1[i][j]
                             << " actual = " << Coord[i*3+j]
                             << std::endl;
                   ItAllWorks = 0;
                   coordCorrect = false;
                   break;
                 }
              }
            }
            //std::cout << std::endl;
            //Ouf << std::endl;
          }
          //Ouf.close();
        }
        result.UpdateResult("ParCoordinateData:Correct", coordCorrect);
        result.UpdateResult("ParCoordinateData:Registered", true);
      }
      else{
        result.UpdateResult("ParCoordinateData:Registered", false);
        result.UpdateResult("ParCoordinateData:Correct", false);
        ItAllWorks = 0;
      }

      //////////////////////////////////////////////////
      // Check connectivity values
      //////////////////////////////////////////////////
      int connCheck[250][4] = {
           {1, 2, 3, 4},
           {5, 1, 4, 6},
           {7, 5, 6, 8},
           {9, 7, 8, 10},
           {11, 9, 10, 12},
           {4, 3, 13, 14},
           {6, 4, 14, 15},
           {8, 6, 15, 16},
           {10, 8, 16, 17},
           {12, 10, 17, 18},
           {14, 13, 19, 20},
           {15, 14, 20, 21},
           {16, 15, 21, 22},
           {17, 16, 22, 23},
           {18, 17, 23, 24},
           {20, 19, 25, 26},
           {21, 20, 26, 27},
           {22, 21, 27, 28},
           {23, 22, 28, 29},
           {24, 23, 29, 30},
           {26, 25, 31, 32},
           {27, 26, 32, 33},
           {28, 27, 33, 34},
           {29, 28, 34, 35},
           {30, 29, 35, 36},
           {32, 31, 37, 38},
           {33, 32, 38, 39},
           {34, 33, 39, 40},
           {35, 34, 40, 41},
           {36, 35, 41, 42},
           {38, 37, 43, 44},
           {39, 38, 44, 45},
           {40, 39, 45, 46},
           {41, 40, 46, 47},
           {42, 41, 47, 48},
           {44, 43, 49, 50},
           {45, 44, 50, 51},
           {46, 45, 51, 52},
           {47, 46, 52, 53},
           {48, 47, 53, 54},
           {50, 49, 55, 56},
           {51, 50, 56, 57},
           {52, 51, 57, 58},
           {53, 52, 58, 59},
           {54, 53, 59, 60},
           {56, 55, 61, 62},
           {57, 56, 62, 63},
           {58, 57, 63, 64},
           {59, 58, 64, 65},
           {60, 59, 65, 66},
           {62, 61, 67, 68},
           {63, 62, 68, 69},
           {64, 63, 69, 70},
           {65, 64, 70, 71},
           {66, 65, 71, 72},
           {68, 67, 73, 74},
           {69, 68, 74, 75},
           {70, 69, 75, 76},
           {71, 70, 76, 77},
           {72, 71, 77, 78},
           {74, 73, 79, 80},
           {75, 74, 80, 81},
           {76, 75, 81, 82},
           {77, 76, 82, 83},
           {78, 77, 83, 84},
           {80, 79, 85, 86},
           {81, 80, 86, 87},
           {82, 81, 87, 88},
           {83, 82, 88, 89},
           {84, 83, 89, 90},
           {86, 85, 91, 92},
           {87, 86, 92, 93},
           {88, 87, 93, 94},
           {89, 88, 94, 95},
           {90, 89, 95, 96},
           {92, 91, 97, 98},
           {93, 92, 98, 99},
           {94, 93, 99, 100},
           {95, 94, 100, 101},
           {96, 95, 101, 102},
           {98, 97, 103, 104},
           {99, 98, 104, 105},
           {100, 99, 105, 106},
           {101, 100, 106, 107},
           {102, 101, 107, 108},
           {104, 103, 109, 110},
           {105, 104, 110, 111},
           {106, 105, 111, 112},
           {107, 106, 112, 113},
           {108, 107, 113, 114},
           {110, 109, 115, 116},
           {111, 110, 116, 117},
           {112, 111, 117, 118},
           {113, 112, 118, 119},
           {114, 113, 119, 120},
           {116, 115, 121, 122},
           {117, 116, 122, 123},
           {118, 117, 123, 124},
           {119, 118, 124, 125},
           {120, 119, 125, 126},
           {122, 121, 127, 128},
           {123, 122, 128, 129},
           {124, 123, 129, 130},
           {125, 124, 130, 131},
           {126, 125, 131, 132},
           {128, 127, 133, 134},
           {129, 128, 134, 135},
           {130, 129, 135, 136},
           {131, 130, 136, 137},
           {132, 131, 137, 138},
           {134, 133, 139, 140},
           {135, 134, 140, 141},
           {136, 135, 141, 142},
           {137, 136, 142, 143},
           {138, 137, 143, 144},
           {140, 139, 145, 146},
           {141, 140, 146, 147},
           {142, 141, 147, 148},
           {143, 142, 148, 149},
           {144, 143, 149, 150},
           {146, 145, 151, 152},
           {147, 146, 152, 153},
           {148, 147, 153, 154},
           {149, 148, 154, 155},
           {150, 149, 155, 156},
           {152, 151, 157, 158},
           {153, 152, 158, 159},
           {154, 153, 159, 160},
           {155, 154, 160, 161},
           {156, 155, 161, 162},
           {158, 157, 163, 164},
           {159, 158, 164, 165},
           {160, 159, 165, 166},
           {161, 160, 166, 167},
           {162, 161, 167, 168},
           {164, 163, 169, 170},
           {165, 164, 170, 171},
           {166, 165, 171, 172},
           {167, 166, 172, 173},
           {168, 167, 173, 174},
           {170, 169, 175, 176},
           {171, 170, 176, 177},
           {172, 171, 177, 178},
           {173, 172, 178, 179},
           {174, 173, 179, 180},
           {176, 175, 181, 182},
           {177, 176, 182, 183},
           {178, 177, 183, 184},
           {179, 178, 184, 185},
           {180, 179, 185, 186},
           {182, 181, 187, 188},
           {183, 182, 188, 189},
           {184, 183, 189, 190},
           {185, 184, 190, 191},
           {186, 185, 191, 192},
           {188, 187, 193, 194},
           {189, 188, 194, 195},
           {190, 189, 195, 196},
           {191, 190, 196, 197},
           {192, 191, 197, 198},
           {194, 193, 199, 200},
           {195, 194, 200, 201},
           {196, 195, 201, 202},
           {197, 196, 202, 203},
           {198, 197, 203, 204},
           {200, 199, 205, 206},
           {201, 200, 206, 207},
           {202, 201, 207, 208},
           {203, 202, 208, 209},
           {204, 203, 209, 210},
           {206, 205, 211, 212},
           {207, 206, 212, 213},
           {208, 207, 213, 214},
           {209, 208, 214, 215},
           {210, 209, 215, 216},
           {212, 211, 217, 218},
           {213, 212, 218, 219},
           {214, 213, 219, 220},
           {215, 214, 220, 221},
           {216, 215, 221, 222},
           {218, 217, 223, 224},
           {219, 218, 224, 225},
           {220, 219, 225, 226},
           {221, 220, 226, 227},
           {222, 221, 227, 228},
           {224, 223, 229, 230},
           {225, 224, 230, 231},
           {226, 225, 231, 232},
           {227, 226, 232, 233},
           {228, 227, 233, 234},
           {230, 229, 235, 236},
           {231, 230, 236, 237},
           {232, 231, 237, 238},
           {233, 232, 238, 239},
           {234, 233, 239, 240},
           {236, 235, 241, 242},
           {237, 236, 242, 243},
           {238, 237, 243, 244},
           {239, 238, 244, 245},
           {240, 239, 245, 246},
           {242, 241, 247, 248},
           {243, 242, 248, 249},
           {244, 243, 249, 250},
           {245, 244, 250, 251},
           {246, 245, 251, 252},
           {248, 247, 253, 254},
           {249, 248, 254, 255},
           {250, 249, 255, 256},
           {251, 250, 256, 257},
           {252, 251, 257, 258},
           {254, 253, 259, 260},
           {255, 254, 260, 261},
           {256, 255, 261, 262},
           {257, 256, 262, 263},
           {258, 257, 263, 264},
           {260, 259, 265, 266},
           {261, 260, 266, 267},
           {262, 261, 267, 268},
           {263, 262, 268, 269},
           {264, 263, 269, 270},
           {266, 265, 271, 272},
           {267, 266, 272, 273},
           {268, 267, 273, 274},
           {269, 268, 274, 275},
           {270, 269, 275, 276},
           {272, 271, 277, 278},
           {273, 272, 278, 279},
           {274, 273, 279, 280},
           {275, 274, 280, 281},
           {276, 275, 281, 282},
           {278, 277, 283, 284},
           {279, 278, 284, 285},
           {280, 279, 285, 286},
           {281, 280, 286, 287},
           {282, 281, 287, 288},
           {284, 283, 289, 290},
           {285, 284, 290, 291},
           {286, 285, 291, 292},
           {287, 286, 292, 293},
           {288, 287, 293, 294},
           {290, 289, 295, 296},
           {291, 290, 296, 297},
           {292, 291, 297, 298},
           {293, 292, 298, 299},
           {294, 293, 299, 300},
           {296, 295, 301, 302},
           {297, 296, 302, 303},
           {298, 297, 303, 304},
           {299, 298, 304, 305},
           {300, 299, 305, 306} };
      bool connCorrect = true;
      int ConnSize = 0;
      int* Conn = NULL;

      // Get the FSI mesh from the structures solver
      COM_get_array("Window1.:q4:", 100+rank, &Conn);
      if(Conn){
        // Get the FSI mesh size from the structures solver
        COM_get_size("Window1.:q4:", 100+rank, &ConnSize);
        std::cout << "Rank #" << rank
                  << ", ConnSize = " << ConnSize << std::endl;
        if(ConnSize != 250){
          std::cout << "ParElmerUnitTests Error:"
                    << " Conn array is incorrect size!" << std::endl;
          ItAllWorks = 0;
          connCorrect = false;
        }
        else{
          //std::ofstream Ouf;
          //std::stringstream ss; 
          //std::string str_rank;
          //ss << rank;
          //ss >> str_rank;
          //std::string outFileName = "conn_" + str_rank + ".dat";
          //Ouf.open(outFileName.c_str());
          // check the values
          std::cout << "Element connectivity check:" << std::endl;
          for(int i=0; i < ConnSize; i++){
            //std::cout << "Rank #" << rank << ", Element #" << i
            //          << " --- ";
            //Ouf << "{";
            for(int j=0; j < 4; j++){
              //std::cout << Conn[i*4+j] << " ";
              //Ouf << Conn[i*4+j]; 
              //if (j<3)
              //  Ouf << ", ";
              //else
              //  Ouf << "},";
              if( (connCheck[i][j] - Conn[i*4+j]) != 0){
                ItAllWorks = 0;
                connCorrect = false;
                std::cout << "Rank #" << rank
                          << ", Connectivity error for element #"
                          << i << std::endl;
                break;
              }
            }
            //std::cout << std::endl;
            //Ouf << std::endl;
          }
          //Ouf.close();
        }
        result.UpdateResult("ParConnectivityData:Correct", connCorrect);
        result.UpdateResult("ParConnectivityData:Registered", true);
      }
      else{
        result.UpdateResult("ParConnectivityData:Registered", false);
        result.UpdateResult("ParConnectivityData:Correct", false);
        ItAllWorks = 0;
      }

      //////////////////////////////////////////////////
      // Check load registration & initialization
      //////////////////////////////////////////////////
      double* Loads = NULL;
      int LoadsSize = 0;
      int loadsRegistered = 1;
      int loadsInitialized = 1;
      // Get the FSI loads from the structures solver
      COM_get_array("Window1.Loads",100+rank,&Loads);
      if(Loads){
        // Get the FSI load size from the structures solver
        COM_get_size("Window1.Loads",100+rank,&LoadsSize);
        if(LoadsSize != 306){
          std::cout << "ParElmerUnitTests Error:"
                    << " Loads array is incorrect size!" << std::endl;
          ItAllWorks = 0;
          loadsInitialized = 0;
        } else {
          // check the values
          for(int i=0; i < LoadsSize; i++){
            for(int j=0; j < 3; j++){
              if(fabs(Loads[i*3 + j]) > 1.0e-12){
                loadsInitialized = 0;
                ItAllWorks = 0;
              }
            }
          }
        }
      }
      else{
        std::cout << "Loads are NULL" << std::endl;
        loadsRegistered = 0;
        loadsInitialized = 0;
        ItAllWorks = 0;
      }
      int globalLoadsReg = 0;
      _communicator.Reduce(loadsRegistered, globalLoadsReg, IRAD::Comm::DTINT, IRAD::Comm::PRODOP, 0);
      _communicator.BroadCast(globalLoadsReg, 0);
      _communicator.Barrier();
      result.UpdateResult("ParLoadData:Registered", globalLoadsReg); 
      int globalLoadsInit = 0;
      _communicator.Reduce(loadsInitialized, globalLoadsInit, IRAD::Comm::DTINT, IRAD::Comm::PRODOP, 0);
      _communicator.BroadCast(globalLoadsInit, 0);
      result.UpdateResult("ParLoadData:Initialized", globalLoadsInit); 

      
      ////////////////////////////////////////////////////
      // Get TimeStepper function handle and call Function
      ////////////////////////////////////////////////////
      // Arrays that hold disp data for check *********************************
      double dispCheck0[306][3] = {
                {8.89791e-29, 5.79913e-29, 7.46431e-29},
                {1.28823e-28, 8.9512e-29, 1.11686e-28},
                {-9.12997e-10, 2.61913e-10, -9.67976e-10},
                {-4.88536e-10, 5.71438e-10, -8.47714e-10},
                {7.9991e-29, 4.67047e-29, 5.88094e-29},
                {-1.79416e-10, 7.91904e-10, -9.64027e-10},
                {7.9991e-29, 4.85741e-29, 5.88094e-29},
                {1.79416e-10, 7.91904e-10, -9.64027e-10},
                {8.89791e-29, 5.79913e-29, 7.26265e-29},
                {4.88536e-10, 5.71438e-10, -8.47714e-10},
                {1.28823e-28, 8.9512e-29, 1.11686e-28},
                {9.12997e-10, 2.61913e-10, -9.67976e-10},
                {-1.30674e-09, 5.53139e-10, -2.00149e-09},
                {-8.29688e-10, 1.12277e-09, -1.88002e-09},
                {-3.09652e-10, 1.48967e-09, -1.97899e-09},
                {3.09652e-10, 1.48967e-09, -1.97899e-09},
                {8.29688e-10, 1.12277e-09, -1.88002e-09},
                {1.30674e-09, 5.53139e-10, -2.00149e-09},
                {-1.53588e-09, 1.4023e-09, -3.03066e-09},
                {-1.02256e-09, 2.09415e-09, -2.94963e-09},
                {-4.06122e-10, 2.58669e-09, -3.01403e-09},
                {4.06122e-10, 2.58669e-09, -3.01403e-09},
                {1.02256e-09, 2.09415e-09, -2.94963e-09},
                {1.53588e-09, 1.4023e-09, -3.03066e-09},
                {-1.65194e-09, 2.74524e-09, -4.06398e-09},
                {-1.1312e-09, 3.50194e-09, -4.01856e-09},
                {-4.64648e-10, 4.07848e-09, -4.05812e-09},
                {4.64648e-10, 4.07848e-09, -4.05812e-09},
                {1.1312e-09, 3.50194e-09, -4.01856e-09},
                {1.65194e-09, 2.74524e-09, -4.06398e-09},
                {-1.7077e-09, 4.55432e-09, -5.08993e-09},
                {-1.18805e-09, 5.34468e-09, -5.06867e-09},
                {-4.97736e-10, 5.97431e-09, -5.09189e-09},
                {4.97736e-10, 5.97431e-09, -5.09189e-09},
                {1.18805e-09, 5.34468e-09, -5.06867e-09},
                {1.7077e-09, 4.55432e-09, -5.08993e-09},
                {-1.72838e-09, 6.80444e-09, -6.10479e-09},
                {-1.21386e-09, 7.61027e-09, -6.09826e-09},
                {-5.14516e-10, 8.27047e-09, -6.11064e-09},
                {5.14516e-10, 8.27047e-09, -6.11064e-09},
                {1.21386e-09, 7.61027e-09, -6.09826e-09},
                {1.72838e-09, 6.80444e-09, -6.10479e-09},
                {-1.72783e-09, 9.47846e-09, -7.10481e-09},
                {-1.22065e-09, 1.02882e-08, -7.10703e-09},
                {-5.21091e-10, 1.09635e-08, -7.11194e-09},
                {5.21091e-10, 1.09635e-08, -7.11194e-09},
                {1.22065e-09, 1.02882e-08, -7.10703e-09},
                {1.72783e-09, 9.47846e-09, -7.10481e-09},
                {-1.71411e-09, 1.25622e-08, -8.08747e-09},
                {-1.21557e-09, 1.33683e-08, -8.09489e-09},
                {-5.21259e-10, 1.40483e-08, -8.09459e-09},
                {5.21259e-10, 1.40483e-08, -8.09459e-09},
                {1.21557e-09, 1.33683e-08, -8.09489e-09},
                {1.71411e-09, 1.25622e-08, -8.08747e-09},
                {-1.69212e-09, 1.60435e-08, -9.05132e-09},
                {-1.20299e-09, 1.68411e-08, -9.06183e-09},
                {-5.17366e-10, 1.7519e-08, -9.05791e-09},
                {5.17366e-10, 1.7519e-08, -9.05791e-09},
                {1.20299e-09, 1.68411e-08, -9.06183e-09},
                {1.69212e-09, 1.60435e-08, -9.05132e-09},
                {-1.66494e-09, 1.99114e-08, -9.99557e-09},
                {-1.18565e-09, 2.06974e-08, -1.00079e-08},
                {-5.10887e-10, 2.13687e-08, -1.00015e-08},
                {5.10887e-10, 2.13687e-08, -1.00015e-08},
                {1.18565e-09, 2.06974e-08, -1.00079e-08},
                {1.66494e-09, 1.99114e-08, -9.99557e-09},
                {-1.63455e-09, 2.41559e-08, -1.09199e-08},
                {-1.16531e-09, 2.49284e-08, -1.09333e-08},
                {-5.02763e-10, 2.55902e-08, -1.09253e-08},
                {5.02763e-10, 2.55902e-08, -1.09253e-08},
                {1.16531e-09, 2.49284e-08, -1.09333e-08},
                {1.63455e-09, 2.41559e-08, -1.09199e-08},
                {-1.6022e-09, 2.87677e-08, -1.18241e-08},
                {-1.14311e-09, 2.95254e-08, -1.18382e-08},
                {-4.93606e-10, 3.01759e-08, -1.1829e-08},
                {4.93606e-10, 3.01759e-08, -1.1829e-08},
                {1.14311e-09, 2.95254e-08, -1.18382e-08},
                {1.6022e-09, 2.87677e-08, -1.18241e-08},
                {-1.56872e-09, 3.37379e-08, -1.27083e-08},
                {-1.11979e-09, 3.44802e-08, -1.27228e-08},
                {-4.83813e-10, 3.51182e-08, -1.27129e-08},
                {4.83813e-10, 3.51182e-08, -1.27129e-08},
                {1.11979e-09, 3.44802e-08, -1.27228e-08},
                {1.56872e-09, 3.37379e-08, -1.27083e-08},
                {-1.53464e-09, 3.90581e-08, -1.35726e-08},
                {-1.09584e-09, 3.97844e-08, -1.35872e-08},
                {-4.73647e-10, 4.04093e-08, -1.3577e-08},
                {4.73647e-10, 4.04093e-08, -1.3577e-08},
                {1.09584e-09, 3.97844e-08, -1.35872e-08},
                {1.53464e-09, 3.90581e-08, -1.35726e-08},
                {-1.50031e-09, 4.47198e-08, -1.44172e-08},
                {-1.07157e-09, 4.543e-08, -1.44318e-08},
                {-4.63281e-10, 4.60415e-08, -1.44213e-08},
                {4.63281e-10, 4.60415e-08, -1.44213e-08},
                {1.07157e-09, 4.543e-08, -1.44318e-08},
                {1.50031e-09, 4.47198e-08, -1.44172e-08},
                {-1.46597e-09, 5.0715e-08, -1.52422e-08},
                {-1.04721e-09, 5.14091e-08, -1.52568e-08},
                {-4.52828e-10, 5.20069e-08, -1.52462e-08},
                {4.52828e-10, 5.20069e-08, -1.52462e-08},
                {1.04721e-09, 5.14091e-08, -1.52568e-08},
                {1.46597e-09, 5.0715e-08, -1.52422e-08},
                {-1.43176e-09, 5.70357e-08, -1.60478e-08},
                {-1.02288e-09, 5.77137e-08, -1.60623e-08},
                {-4.42362e-10, 5.82978e-08, -1.60517e-08},
                {4.42362e-10, 5.82978e-08, -1.60517e-08},
                {1.02288e-09, 5.77137e-08, -1.60623e-08},
                {1.43176e-09, 5.70357e-08, -1.60478e-08},
                {-1.39779e-09, 6.36743e-08, -1.68343e-08},
                {-9.98685e-10, 6.43362e-08, -1.68487e-08},
                {-4.31933e-10, 6.49065e-08, -1.68381e-08},
                {4.31933e-10, 6.49065e-08, -1.68381e-08},
                {9.98685e-10, 6.43362e-08, -1.68487e-08},
                {1.39779e-09, 6.36743e-08, -1.68343e-08},
                {-1.3641e-09, 7.06229e-08, -1.76019e-08},
                {-9.74672e-10, 7.12689e-08, -1.76161e-08},
                {-4.21572e-10, 7.18256e-08, -1.76056e-08},
                {4.21572e-10, 7.18256e-08, -1.76056e-08},
                {9.74672e-10, 7.12689e-08, -1.76161e-08},
                {1.3641e-09, 7.06229e-08, -1.76019e-08},
                {-1.33076e-09, 7.7874e-08, -1.83507e-08},
                {-9.50883e-10, 7.85043e-08, -1.83649e-08},
                {-4.113e-10, 7.90475e-08, -1.83544e-08},
                {4.113e-10, 7.90475e-08, -1.83544e-08},
                {9.50883e-10, 7.85043e-08, -1.83649e-08},
                {1.33076e-09, 7.7874e-08, -1.83507e-08},
                {-1.29778e-09, 8.54202e-08, -1.90811e-08},
                {-9.27345e-10, 8.60348e-08, -1.90951e-08},
                {-4.0113e-10, 8.65646e-08, -1.90848e-08},
                {4.0113e-10, 8.65646e-08, -1.90848e-08},
                {9.27345e-10, 8.60348e-08, -1.90951e-08},
                {1.29778e-09, 8.54202e-08, -1.90811e-08},
                {-1.26519e-09, 9.32541e-08, -1.97933e-08},
                {-9.04073e-10, 9.38533e-08, -1.98071e-08},
                {-3.91073e-10, 9.43699e-08, -1.97969e-08},
                {3.91073e-10, 9.43699e-08, -1.97969e-08},
                {9.04073e-10, 9.38533e-08, -1.98071e-08},
                {1.26519e-09, 9.32541e-08, -1.97933e-08},
                {-1.23299e-09, 1.01368e-07, -2.04874e-08},
                {-8.81079e-10, 1.01952e-07, -2.05011e-08},
                {-3.81134e-10, 1.02456e-07, -2.0491e-08},
                {3.81134e-10, 1.02456e-07, -2.0491e-08},
                {8.81079e-10, 1.01952e-07, -2.05011e-08},
                {1.23299e-09, 1.01368e-07, -2.04874e-08},
                {-1.20119e-09, 1.09756e-07, -2.11638e-08},
                {-8.58368e-10, 1.10325e-07, -2.11772e-08},
                {-3.71315e-10, 1.10815e-07, -2.11673e-08},
                {3.71315e-10, 1.10815e-07, -2.11673e-08},
                {8.58368e-10, 1.10325e-07, -2.11772e-08},
                {1.20119e-09, 1.09756e-07, -2.11638e-08},
                {-1.16979e-09, 1.1841e-07, -2.18226e-08},
                {-8.35946e-10, 1.18964e-07, -2.18359e-08},
                {-3.61621e-10, 1.19442e-07, -2.1826e-08},
                {3.61621e-10, 1.19442e-07, -2.1826e-08},
                {8.35946e-10, 1.18964e-07, -2.18359e-08},
                {1.16979e-09, 1.1841e-07, -2.18226e-08},
                {-1.13881e-09, 1.27323e-07, -2.24641e-08},
                {-8.13816e-10, 1.27862e-07, -2.24772e-08},
                {-3.52052e-10, 1.28327e-07, -2.24675e-08},
                {3.52052e-10, 1.28327e-07, -2.24675e-08},
                {8.13816e-10, 1.27862e-07, -2.24772e-08},
                {1.13881e-09, 1.27323e-07, -2.24641e-08},
                {-1.10824e-09, 1.36488e-07, -2.30885e-08},
                {-7.91979e-10, 1.37013e-07, -2.31014e-08},
                {-3.4261e-10, 1.37466e-07, -2.30918e-08},
                {3.4261e-10, 1.37466e-07, -2.30918e-08},
                {7.91979e-10, 1.37013e-07, -2.31014e-08},
                {1.10824e-09, 1.36488e-07, -2.30885e-08},
                {-1.07808e-09, 1.45899e-07, -2.3696e-08},
                {-7.70437e-10, 1.4641e-07, -2.37087e-08},
                {-3.33295e-10, 1.4685e-07, -2.36993e-08},
                {3.33295e-10, 1.4685e-07, -2.36993e-08},
                {7.70437e-10, 1.4641e-07, -2.37087e-08},
                {1.07808e-09, 1.45899e-07, -2.3696e-08},
                {-1.04834e-09, 1.55549e-07, -2.42869e-08},
                {-7.49191e-10, 1.56045e-07, -2.42995e-08},
                {-3.24108e-10, 1.56474e-07, -2.42901e-08},
                {3.24108e-10, 1.56474e-07, -2.42901e-08},
                {7.49191e-10, 1.56045e-07, -2.42995e-08},
                {1.04834e-09, 1.55549e-07, -2.42869e-08},
                {-1.01901e-09, 1.65431e-07, -2.48614e-08},
                {-7.28241e-10, 1.65913e-07, -2.48738e-08},
                {-3.15048e-10, 1.6633e-07, -2.48646e-08},
                {3.15048e-10, 1.6633e-07, -2.48646e-08},
                {7.28241e-10, 1.65913e-07, -2.48738e-08},
                {1.01901e-09, 1.65431e-07, -2.48614e-08},
                {-9.90099e-10, 1.75538e-07, -2.54197e-08},
                {-7.07588e-10, 1.76007e-07, -2.54319e-08},
                {-3.06117e-10, 1.76412e-07, -2.54228e-08},
                {3.06117e-10, 1.76412e-07, -2.54228e-08},
                {7.07588e-10, 1.76007e-07, -2.54319e-08},
                {9.90099e-10, 1.75538e-07, -2.54197e-08},
                {-9.61602e-10, 1.85865e-07, -2.59621e-08},
                {-6.87231e-10, 1.86321e-07, -2.59741e-08},
                {-2.97314e-10, 1.86714e-07, -2.59652e-08},
                {2.97314e-10, 1.86714e-07, -2.59652e-08},
                {6.87231e-10, 1.86321e-07, -2.59741e-08},
                {9.61602e-10, 1.85865e-07, -2.59621e-08},
                {-9.33521e-10, 1.96405e-07, -2.64887e-08},
                {-6.67171e-10, 1.96847e-07, -2.65006e-08},
                {-2.8864e-10, 1.97229e-07, -2.64918e-08},
                {2.8864e-10, 1.97229e-07, -2.64918e-08},
                {6.67171e-10, 1.96847e-07, -2.65006e-08},
                {9.33521e-10, 1.96405e-07, -2.64887e-08},
                {-9.05856e-10, 2.07152e-07, -2.69999e-08},
                {-6.47409e-10, 2.07581e-07, -2.70116e-08},
                {-2.80093e-10, 2.07951e-07, -2.70029e-08},
                {2.80093e-10, 2.07951e-07, -2.70029e-08},
                {6.47409e-10, 2.07581e-07, -2.70116e-08},
                {9.05856e-10, 2.07152e-07, -2.69999e-08},
                {-8.78606e-10, 2.18099e-07, -2.74959e-08},
                {-6.27943e-10, 2.18515e-07, -2.75074e-08},
                {-2.71676e-10, 2.18874e-07, -2.74989e-08},
                {2.71676e-10, 2.18874e-07, -2.74989e-08},
                {6.27943e-10, 2.18515e-07, -2.75074e-08},
                {8.78606e-10, 2.18099e-07, -2.74959e-08},
                {-8.51773e-10, 2.29241e-07, -2.79768e-08},
                {-6.08774e-10, 2.29644e-07, -2.79882e-08},
                {-2.63387e-10, 2.29993e-07, -2.79797e-08},
                {2.63387e-10, 2.29993e-07, -2.79797e-08},
                {6.08774e-10, 2.29644e-07, -2.79882e-08},
                {8.51773e-10, 2.29241e-07, -2.79768e-08},
                {-8.25355e-10, 2.40571e-07, -2.8443e-08},
                {-5.89903e-10, 2.40962e-07, -2.84541e-08},
                {-2.55226e-10, 2.413e-07, -2.84458e-08},
                {2.55226e-10, 2.413e-07, -2.84458e-08},
                {5.89903e-10, 2.40962e-07, -2.84541e-08},
                {8.25355e-10, 2.40571e-07, -2.8443e-08},
                {-7.99353e-10, 2.52084e-07, -2.88946e-08},
                {-5.71329e-10, 2.52463e-07, -2.89056e-08},
                {-2.47194e-10, 2.5279e-07, -2.88974e-08},
                {2.47194e-10, 2.5279e-07, -2.88974e-08},
                {5.71329e-10, 2.52463e-07, -2.89056e-08},
                {7.99353e-10, 2.52084e-07, -2.88946e-08},
                {-7.73768e-10, 2.63774e-07, -2.93319e-08},
                {-5.53052e-10, 2.64141e-07, -2.93427e-08},
                {-2.3929e-10, 2.64457e-07, -2.93346e-08},
                {2.3929e-10, 2.64457e-07, -2.93346e-08},
                {5.53052e-10, 2.64141e-07, -2.93427e-08},
                {7.73768e-10, 2.63774e-07, -2.93319e-08},
                {-7.48598e-10, 2.75635e-07, -2.9755e-08},
                {-5.35072e-10, 2.7599e-07, -2.97657e-08},
                {-2.31515e-10, 2.76296e-07, -2.97578e-08},
                {2.31515e-10, 2.76296e-07, -2.97578e-08},
                {5.35072e-10, 2.7599e-07, -2.97657e-08},
                {7.48598e-10, 2.75635e-07, -2.9755e-08},
                {-7.23845e-10, 2.87662e-07, -3.01644e-08},
                {-5.17389e-10, 2.88005e-07, -3.01748e-08},
                {-2.23868e-10, 2.88301e-07, -3.01671e-08},
                {2.23868e-10, 2.88301e-07, -3.01671e-08},
                {5.17389e-10, 2.88005e-07, -3.01748e-08},
                {7.23845e-10, 2.87662e-07, -3.01644e-08},
                {-6.99507e-10, 2.99849e-07, -3.05601e-08},
                {-5.00003e-10, 3.00181e-07, -3.05703e-08},
                {-2.1635e-10, 3.00467e-07, -3.05627e-08},
                {2.1635e-10, 3.00467e-07, -3.05627e-08},
                {5.00003e-10, 3.00181e-07, -3.05703e-08},
                {6.99507e-10, 2.99849e-07, -3.05601e-08},
                {-6.75585e-10, 3.12191e-07, -3.09424e-08},
                {-4.82915e-10, 3.12511e-07, -3.09525e-08},
                {-2.0896e-10, 3.12788e-07, -3.0945e-08},
                {2.0896e-10, 3.12788e-07, -3.0945e-08},
                {4.82915e-10, 3.12511e-07, -3.09525e-08},
                {6.75585e-10, 3.12191e-07, -3.09424e-08},
                {-6.5208e-10, 3.24682e-07, -3.13115e-08},
                {-4.66124e-10, 3.24991e-07, -3.13214e-08},
                {-2.01699e-10, 3.25258e-07, -3.1314e-08},
                {2.01699e-10, 3.25258e-07, -3.1314e-08},
                {4.66124e-10, 3.24991e-07, -3.13214e-08},
                {6.5208e-10, 3.24682e-07, -3.13115e-08},
                {-6.2899e-10, 3.37318e-07, -3.16677e-08},
                {-4.4963e-10, 3.37616e-07, -3.16774e-08},
                {-1.94566e-10, 3.37873e-07, -3.16702e-08},
                {1.94566e-10, 3.37873e-07, -3.16702e-08},
                {4.4963e-10, 3.37616e-07, -3.16774e-08},
                {6.2899e-10, 3.37318e-07, -3.16677e-08},
                {-6.06317e-10, 3.50092e-07, -3.20112e-08},
                {-4.33433e-10, 3.5038e-07, -3.20207e-08},
                {-1.87562e-10, 3.50628e-07, -3.20136e-08},
                {1.87562e-10, 3.50628e-07, -3.20136e-08},
                {4.33433e-10, 3.5038e-07, -3.20207e-08},
                {6.06317e-10, 3.50092e-07, -3.20112e-08},
                {-5.8406e-10, 3.63001e-07, -3.23422e-08},
                {-4.17534e-10, 3.63278e-07, -3.23516e-08},
                {-1.80687e-10, 3.63517e-07, -3.23446e-08},
                {1.80687e-10, 3.63517e-07, -3.23446e-08},
                {4.17534e-10, 3.63278e-07, -3.23516e-08},
                {5.8406e-10, 3.63001e-07, -3.23422e-08},
                {-5.62218e-10, 3.76038e-07, -3.2661e-08},
                {-4.01931e-10, 3.76305e-07, -3.26702e-08},
                {-1.7394e-10, 3.76535e-07, -3.26633e-08},
                {1.7394e-10, 3.76535e-07, -3.26633e-08},
                {4.01931e-10, 3.76305e-07, -3.26702e-08},
                {5.62218e-10, 3.76038e-07, -3.2661e-08},
                {-5.40793e-10, 3.892e-07, -3.29677e-08},
                {-3.86626e-10, 3.89457e-07, -3.29768e-08},
                {-1.67321e-10, 3.89678e-07, -3.297e-08},
                {1.67321e-10, 3.89678e-07, -3.297e-08},
                {3.86626e-10, 3.89457e-07, -3.29768e-08},
                {5.40793e-10, 3.892e-07, -3.29677e-08},
                {-5.19783e-10, 4.02482e-07, -3.32627e-08},
                {-3.71618e-10, 4.02728e-07, -3.32716e-08},
                {-1.60831e-10, 4.02941e-07, -3.3265e-08},
                {1.60831e-10, 4.02941e-07, -3.3265e-08},
                {3.71618e-10, 4.02728e-07, -3.32716e-08},
                {5.19783e-10, 4.02482e-07, -3.32627e-08} };

      double dispCheck1[306][3] = {
                {-3.71618e-10, 4.02728e-07, -3.32716e-08},
                {-5.19783e-10, 4.02482e-07, -3.32627e-08},
                {-4.9919e-10, 4.15878e-07, -3.35461e-08},
                {-3.56907e-10, 4.16115e-07, -3.35548e-08},
                {-1.60831e-10, 4.02941e-07, -3.3265e-08},
                {-1.5447e-10, 4.16319e-07, -3.35484e-08},
                {1.60831e-10, 4.02941e-07, -3.3265e-08},
                {1.5447e-10, 4.16319e-07, -3.35484e-08},
                {3.71618e-10, 4.02728e-07, -3.32716e-08},
                {3.56907e-10, 4.16115e-07, -3.35548e-08},
                {5.19783e-10, 4.02482e-07, -3.32627e-08},
                {4.9919e-10, 4.15878e-07, -3.35461e-08},
                {-4.79013e-10, 4.29385e-07, -3.38182e-08},
                {-3.42493e-10, 4.29612e-07, -3.38267e-08},
                {-1.48236e-10, 4.29808e-07, -3.38204e-08},
                {1.48237e-10, 4.29808e-07, -3.38204e-08},
                {3.42493e-10, 4.29612e-07, -3.38267e-08},
                {4.79013e-10, 4.29385e-07, -3.38182e-08},
                {-4.59251e-10, 4.42997e-07, -3.40793e-08},
                {-3.28377e-10, 4.43215e-07, -3.40876e-08},
                {-1.42132e-10, 4.43403e-07, -3.40814e-08},
                {1.42132e-10, 4.43403e-07, -3.40814e-08},
                {3.28377e-10, 4.43215e-07, -3.40876e-08},
                {4.59251e-10, 4.42997e-07, -3.40793e-08},
                {-4.39906e-10, 4.56711e-07, -3.43294e-08},
                {-3.14557e-10, 4.56919e-07, -3.43376e-08},
                {-1.36156e-10, 4.571e-07, -3.43315e-08},
                {1.36156e-10, 4.571e-07, -3.43315e-08},
                {3.14558e-10, 4.56919e-07, -3.43376e-08},
                {4.39906e-10, 4.56711e-07, -3.43294e-08},
                {-4.20977e-10, 4.70522e-07, -3.4569e-08},
                {-3.01035e-10, 4.70721e-07, -3.45769e-08},
                {-1.30309e-10, 4.70894e-07, -3.4571e-08},
                {1.30309e-10, 4.70894e-07, -3.4571e-08},
                {3.01035e-10, 4.70721e-07, -3.45769e-08},
                {4.20977e-10, 4.70522e-07, -3.4569e-08},
                {-4.02464e-10, 4.84425e-07, -3.47981e-08},
                {-2.8781e-10, 4.84616e-07, -3.48059e-08},
                {-1.2459e-10, 4.84781e-07, -3.48001e-08},
                {1.2459e-10, 4.84781e-07, -3.48001e-08},
                {2.8781e-10, 4.84616e-07, -3.48059e-08},
                {4.02464e-10, 4.84425e-07, -3.47981e-08},
                {-3.84367e-10, 4.98418e-07, -3.50171e-08},
                {-2.74883e-10, 4.986e-07, -3.50247e-08},
                {-1.18999e-10, 4.98758e-07, -3.5019e-08},
                {1.18999e-10, 4.98758e-07, -3.5019e-08},
                {2.74883e-10, 4.986e-07, -3.50247e-08},
                {3.84367e-10, 4.98418e-07, -3.50171e-08},
                {-3.66685e-10, 5.12495e-07, -3.52261e-08},
                {-2.62252e-10, 5.12669e-07, -3.52336e-08},
                {-1.13537e-10, 5.12819e-07, -3.5228e-08},
                {1.13537e-10, 5.12819e-07, -3.5228e-08},
                {2.62252e-10, 5.12669e-07, -3.52336e-08},
                {3.66686e-10, 5.12495e-07, -3.52261e-08},
                {-3.4942e-10, 5.26653e-07, -3.54255e-08},
                {-2.49919e-10, 5.26819e-07, -3.54327e-08},
                {-1.08204e-10, 5.26962e-07, -3.54273e-08},
                {1.08204e-10, 5.26962e-07, -3.54273e-08},
                {2.49919e-10, 5.26819e-07, -3.54327e-08},
                {3.4942e-10, 5.26653e-07, -3.54255e-08},
                {-3.32571e-10, 5.40888e-07, -3.56153e-08},
                {-2.37883e-10, 5.41046e-07, -3.56224e-08},
                {-1.02999e-10, 5.41183e-07, -3.56172e-08},
                {1.02999e-10, 5.41183e-07, -3.56172e-08},
                {2.37883e-10, 5.41046e-07, -3.56224e-08},
                {3.32571e-10, 5.40888e-07, -3.56153e-08},
                {-3.16138e-10, 5.55196e-07, -3.5796e-08},
                {-2.26144e-10, 5.55347e-07, -3.58029e-08},
                {-9.79225e-11, 5.55476e-07, -3.57978e-08},
                {9.79225e-11, 5.55476e-07, -3.57978e-08},
                {2.26144e-10, 5.55347e-07, -3.58029e-08},
                {3.16138e-10, 5.55196e-07, -3.5796e-08},
                {-3.00121e-10, 5.69574e-07, -3.59676e-08},
                {-2.14702e-10, 5.69717e-07, -3.59743e-08},
                {-9.29746e-11, 5.6984e-07, -3.59693e-08},
                {9.29747e-11, 5.6984e-07, -3.59693e-08},
                {2.14702e-10, 5.69717e-07, -3.59743e-08},
                {3.00121e-10, 5.69574e-07, -3.59676e-08},
                {-2.8452e-10, 5.84018e-07, -3.61304e-08},
                {-2.03557e-10, 5.84154e-07, -3.6137e-08},
                {-8.81553e-11, 5.8427e-07, -3.61321e-08},
                {8.81553e-11, 5.8427e-07, -3.61321e-08},
                {2.03557e-10, 5.84154e-07, -3.6137e-08},
                {2.8452e-10, 5.84018e-07, -3.61304e-08},
                {-2.69335e-10, 5.98525e-07, -3.62847e-08},
                {-1.9271e-10, 5.98653e-07, -3.62911e-08},
                {-8.34645e-11, 5.98764e-07, -3.62864e-08},
                {8.34645e-11, 5.98764e-07, -3.62864e-08},
                {1.9271e-10, 5.98653e-07, -3.62911e-08},
                {2.69335e-10, 5.98525e-07, -3.62847e-08},
                {-2.54566e-10, 6.13091e-07, -3.64307e-08},
                {-1.8216e-10, 6.13212e-07, -3.64369e-08},
                {-7.89022e-11, 6.13316e-07, -3.64323e-08},
                {7.89022e-11, 6.13316e-07, -3.64323e-08},
                {1.8216e-10, 6.13212e-07, -3.64369e-08},
                {2.54566e-10, 6.13091e-07, -3.64307e-08},
                {-2.40213e-10, 6.27712e-07, -3.65686e-08},
                {-1.71907e-10, 6.27827e-07, -3.65747e-08},
                {-7.44684e-11, 6.27925e-07, -3.65702e-08},
                {7.44684e-11, 6.27925e-07, -3.65702e-08},
                {1.71907e-10, 6.27827e-07, -3.65747e-08},
                {2.40213e-10, 6.27712e-07, -3.65686e-08},
                {-2.26276e-10, 6.42387e-07, -3.66987e-08},
                {-1.61951e-10, 6.42495e-07, -3.67045e-08},
                {-7.01631e-11, 6.42588e-07, -3.67002e-08},
                {7.01631e-11, 6.42588e-07, -3.67002e-08},
                {1.61951e-10, 6.42495e-07, -3.67045e-08},
                {2.26276e-10, 6.42387e-07, -3.66987e-08},
                {-2.12756e-10, 6.57111e-07, -3.68211e-08},
                {-1.52292e-10, 6.57212e-07, -3.68268e-08},
                {-6.59863e-11, 6.573e-07, -3.68226e-08},
                {6.59863e-11, 6.573e-07, -3.68226e-08},
                {1.52292e-10, 6.57212e-07, -3.68268e-08},
                {2.12756e-10, 6.57111e-07, -3.68211e-08},
                {-1.99651e-10, 6.71882e-07, -3.69361e-08},
                {-1.42931e-10, 6.71977e-07, -3.69416e-08},
                {-6.19381e-11, 6.72059e-07, -3.69375e-08},
                {6.19381e-11, 6.72059e-07, -3.69375e-08},
                {1.42931e-10, 6.71977e-07, -3.69416e-08},
                {1.99651e-10, 6.71882e-07, -3.69361e-08},
                {-1.86962e-10, 6.86696e-07, -3.7044e-08},
                {-1.33867e-10, 6.86785e-07, -3.70493e-08},
                {-5.80183e-11, 6.86862e-07, -3.70454e-08},
                {5.80183e-11, 6.86862e-07, -3.70454e-08},
                {1.33867e-10, 6.86785e-07, -3.70493e-08},
                {1.86962e-10, 6.86696e-07, -3.7044e-08},
                {-1.74689e-10, 7.01552e-07, -3.71449e-08},
                {-1.25099e-10, 7.01635e-07, -3.71501e-08},
                {-5.42271e-11, 7.01707e-07, -3.71462e-08},
                {5.42271e-11, 7.01707e-07, -3.71462e-08},
                {1.25099e-10, 7.01635e-07, -3.71501e-08},
                {1.74689e-10, 7.01552e-07, -3.71449e-08},
                {-1.62832e-10, 7.16445e-07, -3.72392e-08},
                {-1.1663e-10, 7.16523e-07, -3.72441e-08},
                {-5.05644e-11, 7.1659e-07, -3.72404e-08},
                {5.05644e-11, 7.1659e-07, -3.72404e-08},
                {1.1663e-10, 7.16523e-07, -3.72441e-08},
                {1.62832e-10, 7.16445e-07, -3.72392e-08},
                {-1.51392e-10, 7.31374e-07, -3.73269e-08},
                {-1.08457e-10, 7.31446e-07, -3.73317e-08},
                {-4.70302e-11, 7.31509e-07, -3.73282e-08},
                {4.70302e-11, 7.31509e-07, -3.73282e-08},
                {1.08457e-10, 7.31446e-07, -3.73317e-08},
                {1.51392e-10, 7.31374e-07, -3.73269e-08},
                {-1.40367e-10, 7.46336e-07, -3.74085e-08},
                {-1.00581e-10, 7.46403e-07, -3.74131e-08},
                {-4.36245e-11, 7.46462e-07, -3.74097e-08},
                {4.36246e-11, 7.46462e-07, -3.74097e-08},
                {1.00581e-10, 7.46403e-07, -3.74131e-08},
                {1.40367e-10, 7.46336e-07, -3.74085e-08},
                {-1.29758e-10, 7.61329e-07, -3.7484e-08},
                {-9.3003e-11, 7.61391e-07, -3.74884e-08},
                {-4.03474e-11, 7.61445e-07, -3.74851e-08},
                {4.03474e-11, 7.61445e-07, -3.74851e-08},
                {9.3003e-11, 7.61391e-07, -3.74884e-08},
                {1.29758e-10, 7.61329e-07, -3.7484e-08},
                {-1.19565e-10, 7.7635e-07, -3.75537e-08},
                {-8.57219e-11, 7.76407e-07, -3.7558e-08},
                {-3.71987e-11, 7.76457e-07, -3.75548e-08},
                {3.71988e-11, 7.76457e-07, -3.75548e-08},
                {8.57219e-11, 7.76407e-07, -3.7558e-08},
                {1.19566e-10, 7.7635e-07, -3.75537e-08},
                {-1.09789e-10, 7.91397e-07, -3.7618e-08},
                {-7.87379e-11, 7.91449e-07, -3.7622e-08},
                {-3.41786e-11, 7.91495e-07, -3.7619e-08},
                {3.41786e-11, 7.91495e-07, -3.7619e-08},
                {7.8738e-11, 7.91449e-07, -3.7622e-08},
                {1.09789e-10, 7.91397e-07, -3.7618e-08},
                {-1.00428e-10, 8.06468e-07, -3.76769e-08},
                {-7.20512e-11, 8.06516e-07, -3.76807e-08},
                {-3.1287e-11, 8.06557e-07, -3.76779e-08},
                {3.1287e-11, 8.06557e-07, -3.76779e-08},
                {7.20512e-11, 8.06516e-07, -3.76807e-08},
                {1.00428e-10, 8.06468e-07, -3.76769e-08},
                {-9.14837e-11, 8.2156e-07, -3.77307e-08},
                {-6.56616e-11, 8.21604e-07, -3.77344e-08},
                {-2.85239e-11, 8.21642e-07, -3.77316e-08},
                {2.85239e-11, 8.21642e-07, -3.77316e-08},
                {6.56616e-11, 8.21604e-07, -3.77344e-08},
                {9.14837e-11, 8.2156e-07, -3.77307e-08},
                {-8.29551e-11, 8.36672e-07, -3.77796e-08},
                {-5.95692e-11, 8.36712e-07, -3.77832e-08},
                {-2.58893e-11, 8.36746e-07, -3.77805e-08},
                {2.58894e-11, 8.36746e-07, -3.77805e-08},
                {5.95693e-11, 8.36712e-07, -3.77832e-08},
                {8.29552e-11, 8.36672e-07, -3.77796e-08},
                {-7.48426e-11, 8.51802e-07, -3.7824e-08},
                {-5.37741e-11, 8.51838e-07, -3.78273e-08},
                {-2.33833e-11, 8.51869e-07, -3.78248e-08},
                {2.33833e-11, 8.51869e-07, -3.78248e-08},
                {5.37741e-11, 8.51838e-07, -3.78273e-08},
                {7.48426e-11, 8.51802e-07, -3.7824e-08},
                {-6.71461e-11, 8.66948e-07, -3.78639e-08},
                {-4.82761e-11, 8.6698e-07, -3.78671e-08},
                {-2.10057e-11, 8.67008e-07, -3.78647e-08},
                {2.10058e-11, 8.67008e-07, -3.78647e-08},
                {4.82761e-11, 8.6698e-07, -3.78671e-08},
                {6.71462e-11, 8.66948e-07, -3.78639e-08},
                {-5.98657e-11, 8.82108e-07, -3.78997e-08},
                {-4.30753e-11, 8.82137e-07, -3.79027e-08},
                {-1.87567e-11, 8.82162e-07, -3.79005e-08},
                {1.87568e-11, 8.82162e-07, -3.79005e-08},
                {4.30753e-11, 8.82137e-07, -3.79027e-08},
                {5.98657e-11, 8.82108e-07, -3.78997e-08},
                {-5.30012e-11, 8.97281e-07, -3.79316e-08},
                {-3.81717e-11, 8.97306e-07, -3.79344e-08},
                {-1.66362e-11, 8.97329e-07, -3.79323e-08},
                {1.66363e-11, 8.97329e-07, -3.79323e-08},
                {3.81717e-11, 8.97306e-07, -3.79344e-08},
                {5.30013e-11, 8.97281e-07, -3.79316e-08},
                {-4.65529e-11, 9.12465e-07, -3.79598e-08},
                {-3.35653e-11, 9.12487e-07, -3.79624e-08},
                {-1.46442e-11, 9.12507e-07, -3.79604e-08},
                {1.46443e-11, 9.12507e-07, -3.79604e-08},
                {3.35653e-11, 9.12487e-07, -3.79624e-08},
                {4.65529e-11, 9.12465e-07, -3.79598e-08},
                {-4.05205e-11, 9.27658e-07, -3.79845e-08},
                {-2.92561e-11, 9.27678e-07, -3.79869e-08},
                {-1.27808e-11, 9.27696e-07, -3.79851e-08},
                {1.27808e-11, 9.27696e-07, -3.79851e-08},
                {2.92561e-11, 9.27678e-07, -3.79869e-08},
                {4.05206e-11, 9.27658e-07, -3.79845e-08},
                {-3.49043e-11, 9.42861e-07, -3.80059e-08},
                {-2.52441e-11, 9.42878e-07, -3.80082e-08},
                {-1.10459e-11, 9.42893e-07, -3.80065e-08},
                {1.10459e-11, 9.42893e-07, -3.80065e-08},
                {2.52441e-11, 9.42878e-07, -3.80082e-08},
                {3.49043e-11, 9.42861e-07, -3.80059e-08},
                {-2.97041e-11, 9.5807e-07, -3.80244e-08},
                {-2.15294e-11, 9.58084e-07, -3.80265e-08},
                {-9.43949e-12, 9.58098e-07, -3.8025e-08},
                {9.43952e-12, 9.58098e-07, -3.8025e-08},
                {2.15294e-11, 9.58084e-07, -3.80265e-08},
                {2.97041e-11, 9.5807e-07, -3.80244e-08},
                {-2.492e-11, 9.73285e-07, -3.80401e-08},
                {-1.81119e-11, 9.73297e-07, -3.80421e-08},
                {-7.96169e-12, 9.73309e-07, -3.80406e-08},
                {7.96172e-12, 9.73309e-07, -3.80406e-08},
                {1.8112e-11, 9.73297e-07, -3.80421e-08},
                {2.492e-11, 9.73285e-07, -3.80401e-08},
                {-2.05521e-11, 9.88505e-07, -3.80533e-08},
                {-1.49918e-11, 9.88516e-07, -3.8055e-08},
                {-6.61251e-12, 9.88525e-07, -3.80537e-08},
                {6.61254e-12, 9.88525e-07, -3.80537e-08},
                {1.49919e-11, 9.88516e-07, -3.8055e-08},
                {2.05521e-11, 9.88505e-07, -3.80533e-08},
                {-1.66005e-11, 1.00373e-06, -3.80641e-08},
                {-1.21692e-11, 1.00374e-06, -3.80657e-08},
                {-5.39202e-12, 1.00375e-06, -3.80645e-08},
                {5.39205e-12, 1.00375e-06, -3.80645e-08},
                {1.21693e-11, 1.00374e-06, -3.80657e-08},
                {1.66006e-11, 1.00373e-06, -3.80641e-08},
                {-1.30655e-11, 1.01896e-06, -3.80729e-08},
                {-9.64438e-12, 1.01896e-06, -3.80743e-08},
                {-4.30039e-12, 1.01897e-06, -3.80732e-08},
                {4.30042e-12, 1.01897e-06, -3.80732e-08},
                {9.64441e-12, 1.01896e-06, -3.80743e-08},
                {1.30655e-11, 1.01896e-06, -3.80729e-08},
                {-9.94721e-12, 1.03419e-06, -3.80798e-08},
                {-7.41765e-12, 1.03419e-06, -3.8081e-08},
                {-3.33787e-12, 1.0342e-06, -3.80801e-08},
                {3.3379e-12, 1.0342e-06, -3.80801e-08},
                {7.41768e-12, 1.03419e-06, -3.8081e-08},
                {9.94724e-12, 1.03419e-06, -3.80798e-08},
                {-7.24624e-12, 1.04942e-06, -3.80851e-08},
                {-5.48971e-12, 1.04942e-06, -3.80862e-08},
                {-2.50485e-12, 1.04943e-06, -3.80854e-08},
                {2.50488e-12, 1.04943e-06, -3.80854e-08},
                {5.48975e-12, 1.04942e-06, -3.80862e-08},
                {7.24627e-12, 1.04942e-06, -3.80851e-08},
                {-4.96318e-12, 1.06465e-06, -3.8089e-08},
                {-3.8615e-12, 1.06465e-06, -3.80899e-08},
                {-1.80185e-12, 1.06466e-06, -3.80893e-08},
                {1.80188e-12, 1.06466e-06, -3.80893e-08},
                {3.86153e-12, 1.06465e-06, -3.80899e-08},
                {4.96321e-12, 1.06465e-06, -3.8089e-08},
                {-3.09882e-12, 1.07988e-06, -3.80918e-08},
                {-2.53409e-12, 1.07988e-06, -3.80925e-08},
                {-1.22916e-12, 1.07989e-06, -3.8092e-08},
                {1.2292e-12, 1.07989e-06, -3.8092e-08},
                {2.53413e-12, 1.07988e-06, -3.80925e-08},
                {3.09886e-12, 1.07988e-06, -3.80918e-08},
                {-1.65404e-12, 1.09511e-06, -3.80936e-08},
                {-1.50768e-12, 1.09512e-06, -3.80941e-08},
                {-7.85584e-13, 1.09512e-06, -3.80938e-08},
                {7.85617e-13, 1.09512e-06, -3.80938e-08},
                {1.50772e-12, 1.09512e-06, -3.80941e-08},
                {1.65407e-12, 1.09511e-06, -3.80936e-08},
                {-6.29428e-13, 1.11035e-06, -3.80946e-08},
                {-7.76853e-13, 1.11035e-06, -3.80951e-08},
                {-4.65364e-13, 1.11035e-06, -3.80949e-08},
                {4.65397e-13, 1.11035e-06, -3.80949e-08},
                {7.76886e-13, 1.11035e-06, -3.80951e-08},
                {6.29461e-13, 1.11035e-06, -3.80946e-08},
                {-3.35459e-15, 1.12558e-06, -3.80952e-08},
                {-3.14262e-13, 1.12558e-06, -3.80956e-08},
                {-2.57658e-13, 1.12558e-06, -3.80957e-08},
                {2.57692e-13, 1.12558e-06, -3.80957e-08},
                {3.14296e-13, 1.12558e-06, -3.80956e-08},
                {3.38831e-15, 1.12558e-06, -3.80952e-08},
                {3.77179e-13, 1.14081e-06, -3.80957e-08},
                {-6.03705e-14, 1.14081e-06, -3.80961e-08},
                {-1.69333e-13, 1.14081e-06, -3.80962e-08},
                {1.69367e-13, 1.14081e-06, -3.80962e-08},
                {6.04047e-14, 1.14081e-06, -3.80961e-08},
                {-3.77144e-13, 1.14081e-06, -3.80957e-08} };
      //*********************************************************************
      int runs_handle = COM_get_function_handle("Window1.Run");
      bool runs_func = (runs_handle > 0);
      bool DispCorrect = true;
      int runs_runs = 0;
      double time1 =  1.0;
      double* Disp = NULL;
      int DispSize = 0;
      COM_get_array("Window1.Displacements", 100+rank, &Disp);
      std::cout << "runs = " << runs_handle << std::endl;
      if(runs_handle){
        //Call the run function for Time1
        if(Loads){
           std::cout << "Changing load values" << std::endl;
           for(int i=0; i < LoadsSize; i++){
              std::cout << "Rank #" << rank
                        << ", Point #" << i
                        << " ---- ";
              for(int j=1; j < 2; j++){
                 Loads[i*3 + j] = 1.0;//double(i*3 + j) + 300*3*rank;
                 std::cout << Loads[i*3 + j] << " ";
              }
              std::cout << std::endl;
           }
        }
        COM_call_function(runs_handle, &runs_runs, &time1);
      } else {
        std::cout << "ParTimeStepperRuns Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("ParTimeStepper:Runs", false);
        return;
      }

      //Check if function changed value of runs properly
      if(runs_runs > 0){
        result.UpdateResult("ParTimeStepper:Runs", true);
      } else {
        result.UpdateResult("ParTimeStepper:Runs", false);
        ItAllWorks = 0;
      }

      // Check reigstration of displacement data
      if (Disp){
         result.UpdateResult("ParDisplacementData:Registered", true);
         // Get the FSI displacement size from the structures solver
         COM_get_size("Window1.Displacements", 100+rank, &DispSize);
         if(DispSize != 306){
           std::cout << "ParElmerUnitTests Error:"
                     << " Disp array is incorrect size!" << std::endl;
           ItAllWorks = 0;
           DispCorrect = false;
         }
      } else {
        result.UpdateResult("ParDisplacementData:Registered", false);
        DispCorrect = false;
        ItAllWorks = 0;
      }
      
      // Check values of displacements after Run func @ Time1
      if (Disp && runs_handle && DispCorrect){
        //std::ofstream Ouf;
        //std::stringstream ss; 
        //std::string str_rank;
        //ss << rank;
        //ss >> str_rank;
        //std::string outFileName = "disp_" + str_rank + ".dat";
        //Ouf.open(outFileName.c_str());
        std::cout << "Checking Displacements for time = " << time1 << std::endl;
        for(int i=0; i < DispSize; i++){
           //std::cout << "Rank #" << rank << ", Point #" << i
           //          << " --- ";
           //Ouf << "{";
           for(int j=0; j < 3; j++){
              //std::cout << Disp[i*3+j] << " ";            
              //Ouf << Disp[i*3+j];
              //if (j < 2)
              //  Ouf << ", ";
              //else
              //  Ouf << "},";
              if (rank==0) {
                if(fabs(dispCheck0[i][j] - Disp[i*3+j]) > 1.0e-10){
                  std::cout << "Rank #" << rank
                            << ", Wrong displacements for point #" << i
                            << " component #" << j
                            << ", check = " << dispCheck0[i][j]
                            << " actual = " << Disp[i*3+j]
                            << " diff = " << fabs(dispCheck0[i][j] - Disp[i*3+j]) 
                            << std::endl;
                  ItAllWorks = 0;
                  DispCorrect = false;
                  break;
                }
              } else if (rank==1) {
                if(fabs(dispCheck1[i][j] - Disp[i*3+j]) > 1.0e-10){
                  std::cout << "Rank #" << rank
                            << ", Wrong displacements for point #" << i
                            << " component #" << j
                            << ", check = " << dispCheck1[i][j]
                            << " actual = " << Disp[i*3+j]
                            << " diff = " << fabs(dispCheck1[i][j] - Disp[i*3+j]) 
                            << std::endl;
                  ItAllWorks = 0;
                  DispCorrect = false;
                  break;
                }
              }
           }
           //std::cout << std::endl;
           //Ouf << std::endl;
        }
        //Ouf.close();
      }
      // gathering each process results to root
      bool globalDispCorrect = 0;
      _communicator.Reduce(DispCorrect, globalDispCorrect, IRAD::Comm::DTINT, IRAD::Comm::PRODOP, 0);
      _communicator.BroadCast(globalDispCorrect, 0);
      result.UpdateResult("ParDisplacementData:Correct", globalDispCorrect);

      
      ////////////////////////////////////////////////////
      // Get Finalize function handle and call Function
      ////////////////////////////////////////////////////
      int final_handle = COM_get_function_handle("Window1.Finalize");
      bool final_func = (final_handle > 0);
      int final_runs = 0;
      if(final_handle){
        //Call the function
        COM_call_function(final_handle, &final_runs);
      }
      else{
        std::cout << "ParFinalizeRuns Error: Could not get function handle."
                  << "." << std::endl;
        result.UpdateResult("ParFinalize:Runs", false);
        return;
      }

      //Check if function changed value of runs properly
      if(final_runs == 1){
        result.UpdateResult("ParFinalize:Runs", true);
      }
      else{
        result.UpdateResult("ParFinalize:Runs", false);
        ItAllWorks = 0;
      }

      ////////////////////////////////////////////////////
      //Unload Elmer module
      ////////////////////////////////////////////////////
      COM_UNLOAD_MODULE_STATIC_DYNAMIC( ElmerCSCParallel, "Window1");

      //Make sure module unloaded
      h=COM_get_window_handle("Window1");
      if(h > 0){
        std::cout << "After unloading, COM_get_window_handle(\"Window1\") returns "
                  << h << std::endl;
        load_unload=false;
        std::cout << "ParElmerUnitTests Error: Did not properly unload module."
                  << "." << std::endl;
        result.UpdateResult("ParElmerUnitTests:Run", false);
        result.UpdateResult("ParUnloadSolverModule:Works",false);
        return;
      }
      else
        result.UpdateResult("ParUnloadSolverModule:Works",true);

      //Change directories out of TestDir directory back to original directory
      IntDir = IRAD::Sys::ChDir(OrgDir);
      if(IntDir == -1){
        std::cout << "ParElmerUnitTests Error: Could not change directories to "
                  << OrgDir << "." << std::endl;
        result.UpdateResult("ParElmerUnitTests:Run", false);
        return;
      }

      // gathering each process results to root
      int globalItAllWorks = 0;
      _communicator.Reduce(ItAllWorks, globalItAllWorks, IRAD::Comm::DTINT, IRAD::Comm::PRODOP, 0);
      _communicator.BroadCast(globalItAllWorks, 0);
      std::cout << "Rank #" << rank
                << ", local ItAllWorks = " << ItAllWorks
                << " globalItAllWorks = " << globalItAllWorks
                << std::endl;
      result.UpdateResult("ParElmerUnitTests:Run", globalItAllWorks);
    } 
       

    ///************************************************************************
    /// Functions to call Parallel tests
    ///************************************************************************
    ///
    /// Runs all tests implemented by the ElmerModuleDriver::TestingObject.
    ///
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    virtual void Process(ResultsType &result){
      Prologue();
      Test__ParallelTrapezoidQuadrature(result);
      Test__ParallelMidPointQuadrature(result);
      Test__ParCommunicatorPassToModule(result);
      Test__ParElmerUnitTests(result);
      Epilogue();
    }
    
    ///
    /// Runs a test specified by name
    ///
    /// @param name String name of the test to run.
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    virtual void RunTest(const std::string &name,ResultsType &result)
    {
      if(name == "ParallelTrapezoidQuadrature")
        Test__ParallelTrapezoidQuadrature(result);
      if(name == "ParallelMidPointQuadrature")
        Test__ParallelMidPointQuadrature(result);
      if(name == "ParCommunicatorPassToModule")
        Test__ParCommunicatorPassToModule(result);
      if(name == "ParElmerUnitTest")
        Test__ParElmerUnitTests(result);
    }

    ///
    /// Runs a list of tests specified by name
    /// 
    /// @param test_names list of string test names
    /// @param result ElmerModuleDriver::TestResults object to store test results.
    ///
    virtual void ProcessTests(std::list<std::string> &test_names,ResultsType &result){
      Prologue();
      std::list<std::string>::iterator tni = test_names.begin();
      while(tni != test_names.end())
        RunTest(*tni++,result);
      Epilogue();
    }


  };
#endif
};
#endif
