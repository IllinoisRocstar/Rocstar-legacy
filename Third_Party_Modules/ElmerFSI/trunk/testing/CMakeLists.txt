cmake_minimum_required(VERSION 2.8)
project(ElmerModuleDriver)

# Compiled objects output paths
set (EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single directory for all executables.")
set (LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib CACHE PATH "Single directory for all libraries and archives.")
mark_as_advanced (LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
# Set up RPATH
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

#Project-specific constructs
INCLUDE_DIRECTORIES(include)

# 
# ========= USE IN EXISTING PROJECT ============
#
# In order to use this IllinoisRocstar Project Template
# in an existing project COPY this file from this point
# on and paste it into your project's CMakeLists.txt 
# file *before* your existing INSTALL directives.
#
INCLUDE(CTest)
IF(BUILD_TESTING)
  SET(BUILDNAME "${BUILDNAME}" CACHE STRING "Name of build on the dashboard")
  MARK_AS_ADVANCED(BUILDNAME)
ENDIF(BUILD_TESTING)

find_package(MPI)
IF(MPI_CXX_COMPILER)
  set (CMAKE_CXX_COMPILER ${MPI_CXX_COMPILER})
ENDIF()

# Information for ElmerModuleDriver CMake
FIND_LIBRARY(IMPACT_LIBF SITCOMF)
FIND_LIBRARY(IMPACT_LIB SITCOM)
FIND_LIBRARY(IRAD_LIB IRAD)
FIND_FILE(IMPACT_HDR comf90.h)
FIND_FILE(IRAD_HDR COMM.H)
GET_FILENAME_COMPONENT(IMPACT_LIBPATH ${IMPACT_LIB} PATH)
GET_FILENAME_COMPONENT(IMPACT_INCPATH ${IMPACT_HDR} PATH)
GET_FILENAME_COMPONENT(IRAD_INCPATH ${IRAD_HDR} PATH)
include_directories(${IMPACT_INCPATH} ${IRAD_INCPATH})

message("SOLVER_LIB = ${SOLVER_LIB}")
message("IMPACT_LIB = ${IMPACT_LIB}")

# Project-specific constructs
ADD_EXECUTABLE(elmermoduledriver_test src/TestElmerModuleDriver.C)
TARGET_LINK_LIBRARIES(elmermoduledriver_test ElmerModuleDriver ${IMPACT_LIB} ${IMPACT_LIBF} ${IRAD_LIB})

IF(MPI_CXX_COMPILER)
  ADD_DEFINITIONS(-D_ELMERMODULEDRIVER_PARALLEL_)
  ADD_EXECUTABLE(elmermoduledriver_parallel_test src/ParallelTestElmerModuleDriver.C)
  TARGET_LINK_LIBRARIES(elmermoduledriver_parallel_test ElmerModuleDriver ${IMPACT_LIB} ${IMPACT_LIBF} ${IRAD_LIB})
ENDIF()

# The 3 tests below actually run all the tests
ADD_TEST(RunElmerModuleDriverUnitTests ${EXECUTABLE_OUTPUT_PATH}/elmermoduledriver_test -s ${PROJECT_SOURCE_DIR} -com-mpi -o elmermoduledriver_testresults.txt)
ADD_TEST(RunElasticBeam3DUnitTests ${EXECUTABLE_OUTPUT_PATH}/elmermoduledriver_test -s ${PROJECT_SOURCE_DIR} -com-mpi -n ElasticBeam3D_LoadFunction -o elmermoduledriver_testresults_2.txt)
ADD_TEST(RunTestScripts ${RUNTEST} -l ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/tests.list -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -v 2 -o elmermoduledriver_testresults.txt)
#ADD_TEST(FSIobstacleRegressionTest:Runs ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/FSIobstacle.csh -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -v 2 -o elmermoduledriver_testresults.txt)
ADD_TEST(FSIobstacleRegressionTest:Runs ${RUNTEST} -f
${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/regression.csh -s
${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -a ${IRAD_EXEPATH} -v 2 -o
elmermoduledriver_testresults.txt -a ${IRAD_EXEPATH})
ADD_TEST(VortexRegressionTest:Runs ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/Vortex.csh -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -v 2 -o elmermoduledriver_testresults.txt)
ADD_TEST(ElasticBeam3DRegressionTest:Runs ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/ElasticBeam3D.csh -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -v 2 -o elmermoduledriver_testresults.txt)
ADD_TEST(HronTurekVerficationTest:Runs ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/HronTurekBeam.csh -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -a ${IRAD_EXEPATH} -v 2 -o elmermoduledriver_testresults.txt)
ADD_TEST(RunPlatformSpecificTests ${RUNTEST} -p ${PROJECT_SOURCE_DIR}/share/Platforms/platforms -o elmermoduledriver_testresults.txt)
ADD_TEST(RunSpecificTestScript    ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Platforms/test_stub.csh -o elmermoduledriver_testresults.txt)

# Simple test of return code of a program
message("EXECUTABLE_OUTPUT_PATH = ${EXECUTABLE_OUTPUT_PATH}")
message("PROJECT_SOURCE_DIR = ${PROJECT_SOURCE_DIR}")
message("CMAKE_BINARY_DIR = ${CMAKE_BINARY_DIR}")

ADD_TEST(ExampleProgram:Runs ${EXECUTABLE_OUTPUT_PATH}/sep cmake_install.cmake)

# The remaining "tests" just use the "testresults" utility to check the results of the tests run above
ADD_TEST(TestStubWorks ${TESTRESULT} TestStubWorks elmermoduledriver_testresults.txt)
ADD_TEST(ExampleProgram:Works ${TESTRESULT} ExampleProgram:Works elmermoduledriver_testresults.txt)
ADD_TEST(ExampleFunction:Works ${TESTRESULT} ExampleFunction:Works elmermoduledriver_testresults.txt)
ADD_TEST(TrapezoidQuadrature:Runs ${TESTRESULT} TrapezoidQuadrature:Runs elmermoduledriver_testresults.txt)
ADD_TEST(TrapezoidQuadrature:Accurate ${TESTRESULT} TrapezoidQuadrature:Accurate elmermoduledriver_testresults.txt)
ADD_TEST(TrapezoidQuadrature:Order ${TESTRESULT} TrapezoidQuadrature:Order2 elmermoduledriver_testresults.txt)
ADD_TEST(MidPointQuadrature:Runs ${TESTRESULT} MidPointQuadrature:Runs elmermoduledriver_testresults.txt)
ADD_TEST(MidPointQuadrature:Accurate ${TESTRESULT} MidPointQuadrature:Accurate elmermoduledriver_testresults.txt)
ADD_TEST(MidPointQuadrature:Order ${TESTRESULT} MidPointQuadrature:Order2 elmermoduledriver_testresults.txt)
ADD_TEST(LoadSolverModule:Works ${TESTRESULT} LoadSolverModule:Works elmermoduledriver_testresults.txt)
ADD_TEST(FSIregressionTest:Works ${TESTRESULT} FSIregressionTest:Works elmermoduledriver_testresults.txt)
ADD_TEST(VortexRegressionTest:Works ${TESTRESULT} VortexRegressionTest:Works elmermoduledriver_testresults.txt)
ADD_TEST(ElasticBeam3DRegressionTest:Works ${TESTRESULT} ElasticBeam3DRegressionTest:Works elmermoduledriver_testresults.txt)
ADD_TEST(HronTurekBeam:Works ${TESTRESULT} HronTurekBeam:Works elmermoduledriver_testresults.txt)
ADD_TEST(Initialize:Runs ${TESTRESULT} Initialize:Runs elmermoduledriver_testresults.txt)
#ADD_TEST(ModuleFunctions:Runs ${TESTRESULT} ModuleFunctions:Runs elmermoduledriver_testresults.txt)
ADD_TEST(CoordinateData:Registered ${TESTRESULT} CoordinateData:Registered elmermoduledriver_testresults.txt)
ADD_TEST(CoordinateData:Correct ${TESTRESULT} CoordinateData:Correct elmermoduledriver_testresults.txt)
ADD_TEST(ConnectivityData:Registered ${TESTRESULT} ConnectivityData:Registered elmermoduledriver_testresults.txt)
ADD_TEST(ConnectivityData:Correct ${TESTRESULT} ConnectivityData:Correct elmermoduledriver_testresults.txt)
ADD_TEST(TimeStepper:Runs ${TESTRESULT} TimeStepper:Runs elmermoduledriver_testresults.txt)
ADD_TEST(DisplacementData:Registered ${TESTRESULT} DisplacementData:Registered elmermoduledriver_testresults.txt)
ADD_TEST(DisplacementData:Correct ${TESTRESULT} DisplacementData:Correct elmermoduledriver_testresults.txt)
ADD_TEST(LoadData:Registered ${TESTRESULT} LoadData:Registered elmermoduledriver_testresults.txt)
ADD_TEST(LoadData:Initialized ${TESTRESULT} LoadData:Initialized elmermoduledriver_testresults.txt)
ADD_TEST(TimeLoads:Runs ${TESTRESULT} TimeLoads:Runs elmermoduledriver_testresults.txt)
ADD_TEST(Finalize:Runs ${TESTRESULT} Finalize:Runs elmermoduledriver_testresults.txt)
ADD_TEST(UnloadSolverModule:Works ${TESTRESULT} UnloadSolverModule:Works elmermoduledriver_testresults.txt)
ADD_TEST(ElmerUnitTests:Run ${TESTRESULT} ElmerUnitTests:Run elmermoduledriver_testresults.txt)
ADD_TEST(ElasticBeam3D_LoadFunction:Runs ${TESTRESULT} ElasticBeam3D_LoadFunction:Runs elmermoduledriver_testresults_2.txt)
ADD_TEST(ElasticBeam3D_LoadFunction:Works ${TESTRESULT} ElasticBeam3D_LoadFunction:Works elmermoduledriver_testresults_2.txt)

IF(MPI_CXX_COMPILER)
 ADD_TEST(RunParallelPlatformTests ${RUNTEST} -p ${PROJECT_SOURCE_DIR}/share/Platforms/parallel_platforms -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -o elmermoduledriver_testresults.txt)
  # The remainder use "testresults" utility to check the results of the parallel tests run above
  ADD_TEST(ParallelExample:Runs ${TESTRESULT} PEPI:Runs elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelExample:Works ${TESTRESULT} PEPI:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParLoadedElasticBeam:Runs ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/LoadedElasticBeamParallel.csh -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -a ${IRAD_EXEPATH} -v 2 -o elmermoduledriver_testresults.txt)
  ADD_TEST(ParElasticBeam3D_LoadFunction:Runs ${RUNTEST} -f ${PROJECT_SOURCE_DIR}/share/Testing/test_scripts/ParElasticBeam_LoadFunction.csh -s ${PROJECT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -a ${IRAD_EXEPATH} -v 2 -o elmermoduledriver_testresults.txt)
  ADD_TEST(ParLoadSolverModule:Works ${TESTRESULT} ParLoadSolverModule:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParCoordinateData:Registered ${TESTRESULT} ParCoordinateData:Registered elmermoduledriver_testresults.txt)
  ADD_TEST(ParCoordinateData:Correct ${TESTRESULT} ParCoordinateData:Correct elmermoduledriver_testresults.txt)
  ADD_TEST(ParConnectivityData:Registered ${TESTRESULT} ParConnectivityData:Registered elmermoduledriver_testresults.txt)
  ADD_TEST(ParConnectivityData:Correct ${TESTRESULT} ParConnectivityData:Correct elmermoduledriver_testresults.txt)
  ADD_TEST(ParLoadData:Registered ${TESTRESULT} ParLoadData:Registered elmermoduledriver_testresults.txt)
  ADD_TEST(ParLoadData:Initialized ${TESTRESULT} ParLoadData:Initialized elmermoduledriver_testresults.txt)
  ADD_TEST(ParDisplacementData:Registered ${TESTRESULT} ParDisplacementData:Registered elmermoduledriver_testresults.txt)
  ADD_TEST(ParDisplacementData:Correct ${TESTRESULT} ParDisplacementData:Correct elmermoduledriver_testresults.txt)
  ADD_TEST(ParTimeStepper:Runs ${TESTRESULT} ParTimeStepper:Runs elmermoduledriver_testresults.txt)
  ADD_TEST(ParFinalize:Runs ${TESTRESULT} ParFinalize:Runs elmermoduledriver_testresults.txt)
  ADD_TEST(ParUnloadSolverModule:Works ${TESTRESULT} ParUnloadSolverModule:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParElmerUnitTests:Run ${TESTRESULT} ParElmerUnitTests:Run elmermoduledriver_testresults.txt)
  ADD_TEST(ParCommunicatorSetToModule:Works ${TESTRESULT} ParCommunicatorSetToModule:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParCommunicatorSizeToModule:Works ${TESTRESULT} ParCommunicatorSizeToModule:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParCommunicatorPassToModule:Works ${TESTRESULT} ParCommunicatorPassToModule:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParLoadedElasticBeam:Works ${TESTRESULT} ParLoadedElasticBeam:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParElasticBeam3D_LoadFunction:Works ${TESTRESULT} ParElasticBeam3D_LoadFunction:Works elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelTrapezoidQuadrature:Runs ${TESTRESULT} ParallelTrapezoidQuadrature:Runs elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelTrapezoidQuadrature:Accurate ${TESTRESULT} ParallelTrapezoidQuadrature:Accurate elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelTrapezoidQuadrature:Order ${TESTRESULT} ParallelTrapezoidQuadrature:Order2 elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelTrapezoidQuadrature:WeakScaling ${TESTRESULT} ParallelTrapezoidQuadrature:WeakScaling elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelTrapezoidQuadrature:StrongScaling ${TESTRESULT} ParallelTrapezoidQuadrature:StrongScaling elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelMidPointQuadrature:Runs ${TESTRESULT} ParallelMidPointQuadrature:Runs elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelMidPointQuadrature:Accurate ${TESTRESULT} ParallelMidPointQuadrature:Accurate elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelMidPointQuadrature:Order ${TESTRESULT} ParallelMidPointQuadrature:Order2 elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelMidPointQuadrature:WeakScaling ${TESTRESULT} ParallelMidPointQuadrature:WeakScaling elmermoduledriver_testresults.txt)
  ADD_TEST(ParallelMidPointQuadrature:StrongScaling ${TESTRESULT} ParallelMidPointQuadrature:StrongScaling elmermoduledriver_testresults.txt)
ENDIF()

INSTALL(TARGETS elmermoduledriver_test RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
IF(MPI_CXX_COMPILER)
  INSTALL(TARGETS elmermoduledriver_parallel_test RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
ENDIF()
# ========= USE IN EXISTING PROJECT ==============
