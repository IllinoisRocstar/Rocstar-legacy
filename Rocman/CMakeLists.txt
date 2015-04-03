cmake_minimum_required (VERSION 2.8)
project(Rocman)

load_cache(../)
IF(NOT ENABLE_MPI)
  add_definitions( -DDUMMY_MPI )
ELSE()
  FIND_PACKAGE(MPI REQUIRED)
  add_definitions( -DMPICH_IGNORE_CXX_SEEK )
#  IF(MPI_CXX_COMPILER)
#    set (CMAKE_CXX_COMPILER ${MPI_CXX_COMPILER})
#  ENDIF(MPI_CXX_COMPILER)
ENDIF()

#INCLUDE(CTest)
find_path(ROCCOM_INC roccom.h HINTS ../Roccom/include) 
find_path(ROCIN_INC HDF4.h HINTS ../Roccom/Rocin/include) 
set (LIB_SRCS src/rocstar_driver.C src/Action.C src/Scheduler.C src/Coupling.C src/Agent.C src/SolidAgent.C 
              src/BurnAgent.C src/basic_actions.C src/transfer_actions.C src/Interpolate.C src/builtin_couplings.C
              src/derived_couplings.C src/Surfdiver.C src/FluidAgent.C)
set (ALL_SRCS ${LIB_SRCS})

set_source_files_properties(${ALL_SRCS} PROPERTIES COMPILE_FLAGS "-fPIC" )
#set(TEST_SRCS src/MANTest.C)

# rpath settings
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

include_directories(include ${ROCCOM_INC} ${ROCIN_INC})


IF(NOT BUILD_STATIC)
  add_library(Rocman SHARED ${LIB_SRCS})
ELSE()
  add_library(Rocman STATIC ${LIB_SRCS})
  add_definitions( -DSTATIC_LINK )
ENDIF()
target_link_libraries(Rocman Roccom)

#add_executable(testx ${TEST_SRCS})
#target_link_libraries(testx MAN ${APPUTIL_LIB})

#ADD_TEST(RunAllTests ${EXECUTABLE_OUTPUT_PATH}/testx testresults.txt)
#ADD_TEST(TestObject:ValidHandle ${TEST_RESULTS} TestObject:ValidHandle testresults.txt)
#ADD_TEST(TestObject:Exists ${TEST_RESULTS} TestObject:Exists testresults.txt)

INSTALL(TARGETS Rocman RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
