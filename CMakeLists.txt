cmake_minimum_required (VERSION 2.8)
project(Rocstar Fortran C CXX)
set (BUILD_STATIC FALSE CACHE BOOL "Build static libraries")
set (ENABLE_MPI TRUE CACHE BOOL "Build with MPI Support")

set (EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single directory for all executables.")
set (LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib CACHE PATH "Single directory for all libraries and archives.")
mark_as_advanced (LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
IF(NOT ENABLE_MPI)
  add_definitions( -DDUMMY_MPI )
ELSE()
  FIND_PACKAGE(MPI REQUIRED)
  add_definitions( -DMPICH_IGNORE_CXX_SEEK )
  INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
ENDIF()
if (NOT CMAKE_BUILD_TYPE)
  message(STATUS "No build type selected, default to Release")
  set(CMAKE_BUILD_TYPE "Release")
endif()

# 
# Add testing support from IRAD
#
INCLUDE(CTest)
IF(BUILD_TESTING)
  SET(BUILDNAME "${BUILDNAME}" CACHE STRING "Name of build on the dashboard")
  MARK_AS_ADVANCED(BUILDNAME)
ENDIF(BUILD_TESTING)

IF(EXISTS ${CMAKE_SOURCE_DIR}/IRAD)
  add_subdirectory(IRAD)
  SET(IRAD_INCPATH ${CMAKE_SOURCE_DIR}/IRAD/include)
  SET(RUNTEST ${EXECUTABLE_OUTPUT_PATH}/runtest)
  SET(TESTRESULT ${EXECUTABLE_OUTPUT_PATH}/testresults)
  SET(IRAD_LIB IRAD)
ELSE()
  FIND_LIBRARY(IRAD_LIB IRAD)
  FIND_FILE(IRAD_HDR COMM.H)
  FIND_PROGRAM(RUNTEST runtest)
  FIND_PROGRAM(TESTRESULT testresults)
  GET_FILENAME_COMPONENT(IRAD_LIBPATH ${IRAD_LIB} PATH)
  GET_FILENAME_COMPONENT(IRAD_INCPATH ${IRAD_HDR} PATH)
  GET_FILENAME_COMPONENT(IRAD_EXEPATH ${RUNTEST} PATH)
ENDIF()
include_directories(${IRAD_INCPATH})

# add some stubs for testing. this crap all gets removed
# when we add the real tests for Rocstar

#ADD_LIBRARY(Rocstar SHARED src/ExampleSourceFile.C)
#TARGET_LINK_LIBRARIES(Rocstar ${IRAD_LIB})
#ADD_EXECUTABLE(rocstar_test src/TestRocstar.C)
#TARGET_LINK_LIBRARIES(rocstar_test Rocstar)
#ADD_EXECUTABLE(sep src/ExampleSerialProgram.C src/sep.C)
#TARGET_LINK_LIBRARIES(sep Rocstar)
#ADD_EXECUTABLE(make_project src/MakeProject.C)
#TARGET_LINK_LIBRARIES(make_project Rocstar)

IF(MPI_CXX_COMPILER)
  ADD_DEFINITIONS(-D_ROCSTAR_PARALLEL_)
#  ADD_EXECUTABLE(pepi src/ExampleParallelProgram.C src/pepi.C)
#  TARGET_LINK_LIBRARIES(pepi Rocstar)
#  ADD_EXECUTABLE(rocstar_parallel_test src/ParallelTestRocstar.C)
#  TARGET_LINK_LIBRARIES(rocstar_parallel_test Rocstar)
ENDIF()
#
# original rocstar stuff below here
#

add_subdirectory(Roccom)
add_subdirectory(Rocface)
add_subdirectory(Rocprop)
add_subdirectory(Rocmop)
add_subdirectory(Rocon)
add_subdirectory(Rocman)
add_subdirectory(RocfluidMP)
add_subdirectory(Rocfrac)
add_subdirectory(Rocburn)
add_subdirectory(Rocprep)
add_subdirectory(testing)

include_directories(./Roccom/include ./Rocman/include)
add_executable(rocstar src/rocstar.C)
set_source_files_properties(src/rocstar.C PROPERTIES COMPILE_FLAGS "${MPI_CXX_COMPILE_FLAGS}")
set_target_properties(rocstar PROPERTIES LINK_FLAGS "${MPI_CXX_LINK_FLAGS}")
target_link_libraries(rocstar Rocman Roccomf Roccom ${IRAD_LIB} ${MPI_CXX_LIBRARIES})

INSTALL(TARGETS rocstar RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)

# Add target to build the documentation
option(BUILD_DOCUMENTATION "Create and install the HTML based API
documentation (requires Doxygen)" ON)
IF(BUILD_DOCUMENTATION)

  FIND_PACKAGE(Doxygen)
  IF(NOT DOXYGEN_FOUND)
    MESSAGE(FATAL_ERROR "Doxygen is needed to build the documentation.")
  ENDIF()
  
#  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/doc DESTINATION
#  ${CMAKE_CURRENT_BINARY_DIR})
  
  
  SET( doxyfile_in          ${CMAKE_CURRENT_SOURCE_DIR}/Docs/Rocstar.cfg.in )
  SET( doxyfile             ${CMAKE_CURRENT_BINARY_DIR}/Rocstar.cfg        )
  SET( documentation        ${CMAKE_CURRENT_BINARY_DIR}/Docs                 )

  CONFIGURE_FILE( ${doxyfile_in} ${doxyfile} @ONLY )

  ADD_CUSTOM_COMMAND( OUTPUT ${documentation}
                      COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
                      MAIN_DEPENDENCY ${doxyfile} ${doxyfile_in}
                      COMMENT "Generating documentation")

  ADD_CUSTOM_TARGET( Docs DEPENDS ${documentation} )

  INSTALL( DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Docs DESTINATION
share/Rocstar )

  #FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/Docs/RocstarQuickStart.pdf DESTINATION
#${CMAKE_CURRENT_BINARY_DIR}/Docs/)
  #FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/Docs/legacy DESTINATION
#${CMAKE_CURRENT_BINARY_DIR}/Docs/)

ENDIF()

INCLUDE(CPack)




