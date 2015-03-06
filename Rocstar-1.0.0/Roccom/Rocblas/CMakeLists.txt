cmake_minimum_required (VERSION 2.8)
project(Rocblas)
#ENABLE_LANGUAGE( CXX Fortran )
#include(FortranCInterface)
#FortranCInterface_HEADER(${PROJECT_SOURCE_DIR}/include/FC.h MACRO_NAMESPACE "FC_")
#FortranCInterface_VERIFY(CXX QUIET)
#set (BUILD_STATIC FALSE CACHE BOOL "Build static COM library")
#set (ENABLE_MPI TRUE CACHE BOOL "Build with MPI Support")
#set (EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single directory for all executables.")
#set (LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib CACHE PATH "Single directory for all libraries and archives.")
#mark_as_advanced (LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
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

find_library(DL_LIB dl)

set (BLASLIB_SRCS src/Rocblas.C src/axpy.C src/dots.C src/op2args.C src/op3args.C)
set (ALL_BLAS_SRCS "${BLASLIB_SRCS}")
set (TEST_SRCS test/blastest.C)
set_source_files_properties(${ALL_BLAS_SRCS} PROPERTIES COMPILE_FLAGS "-fPIC" )
#set(TEST_SRCS src/COMTest.C)

# rpath settings
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

include_directories(include)

IF(NOT BUILD_STATIC)
  add_library(Rocblas SHARED ${BLASLIB_SRCS})
ELSE()
  add_library(Rocblas STATIC ${LIB_SRCS})
  add_definitions( -DSTATIC_LINK )
ENDIF()
target_link_libraries(Rocblas Roccom)

add_executable(blastest ${TEST_SRCS})
target_link_libraries(blastest Rocblas)

#ADD_TEST(RunAllTests ${EXECUTABLE_OUTPUT_PATH}/testx testresults.txt)
#ADD_TEST(TestObject:ValidHandle ${TEST_RESULTS} TestObject:ValidHandle testresults.txt)
#ADD_TEST(TestObject:Exists ${TEST_RESULTS} TestObject:Exists testresults.txt)

INSTALL(TARGETS Rocblas RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
