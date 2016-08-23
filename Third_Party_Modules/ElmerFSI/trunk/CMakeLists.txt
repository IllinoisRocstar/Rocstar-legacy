cmake_minimum_required(VERSION 2.8)
project(ElmerPrep)
load_cache(../../../)

# Compiled objects output paths
# Set up RPATH
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

INCLUDE(CTest)
IF(BUILD_TESTING)
  SET(BUILDNAME "${BUILDNAME}" CACHE STRING "Name of build on the dashboard")
  MARK_AS_ADVANCED(BUILDNAME)
ENDIF(BUILD_TESTING)

# finding IRAD
FIND_LIBRARY(IRAD_LIB IRAD)
FIND_FILE(IRAD_HDR COMM.H HINTS ${CMAKE_SOURCE_DIR}/IMPACT/IRAD/include)
FIND_PROGRAM(RUNTEST runtest)
FIND_PROGRAM(TESTRESULT testresults)
GET_FILENAME_COMPONENT(IRAD_LIBPATH ${IRAD_LIB} PATH)
GET_FILENAME_COMPONENT(IRAD_INCPATH ${IRAD_HDR} PATH)
GET_FILENAME_COMPONENT(IRAD_EXEPATH ${RUNTEST} PATH)
include_directories(${IRAD_INCPATH} include )

# finding IMPACT
FIND_FILE(IMPACT_HDR comf90.h HINTS ${CMAKE_SOURCE_DIR}/IMPACT/COM/include)
GET_FILENAME_COMPONENT(IMPACT_INCPATH ${IMPACT_HDR} PATH)
include_directories(${IMPACT_INCPATH} include )

find_package(MPI)
IF(MPI_CXX_COMPILER)
  set (CMAKE_CXX_COMPILER ${MPI_CXX_COMPILER})
ENDIF()

# finding SolverUtils
FIND_FILE(SolverUtils_HDR InterfaceLayer.H HINTS ${CMAKE_SOURCE_DIR}/IMPACT/SolverUtils/include)
GET_FILENAME_COMPONENT(SolverUtils_INCPATH ${SolverUtils_HDR} PATH)
include_directories(${SolverUtils_INCPATH} ${CMAKE_SOURCE_DIR}/IMPACT/SolverUtils/include include)

# only parallel ElmerPrep will be created
#add_executable(SolverModuleDriver src/SolverModuleDriver.C)
#target_link_libraries(SolverModuleDriver ${SOLVERUTILS} ${IRAD_LIB} ${IMPACT_FLIB} ${IMPACT_LIB})
IF(MPI_CXX_COMPILER)
  ADD_DEFINITIONS(-D_ELMERMODULEDRIVER_PARALLEL_)
  add_executable(elmerprep src/elmerPrep.C)
  #target_link_libraries(elmerprep ${SolverUtils_LIB} ${IRAD_LIB} ${IMPACT_FLIB} ${IMPACT_LIB})
  target_link_libraries(elmerprep SolverUtils ${IRAD_LIB} ${IMPACT_FLIB} ${IMPACT_LIB})
ENDIF()

# Add target to build the documentation
option(BUILD_DOCUMENTATION "Create and install the HTML based API documentation (requires Doxygen)" ON)
IF(BUILD_DOCUMENTATION)
  FIND_PACKAGE(Doxygen)
  IF(NOT DOXYGEN_FOUND)
    MESSAGE(FATAL_ERROR "Doxygen is needed to build the documentation.")
  ENDIF()
#  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/doc DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  SET( doxyfile_in          ${CMAKE_CURRENT_SOURCE_DIR}/doc/elmerPrep.cfg.in )
  SET( doxyfile             ${CMAKE_CURRENT_BINARY_DIR}/elmerPrep.cfg        )
  SET( documentation        ${CMAKE_CURRENT_BINARY_DIR}/docs                 )
  CONFIGURE_FILE( ${doxyfile_in} ${doxyfile} @ONLY )
  ADD_CUSTOM_COMMAND( OUTPUT ${documentation}
                      COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
                      MAIN_DEPENDENCY ${doxyfile} ${doxyfile_in}
                      DEPENDS elmerprep ${doxy_extra_files}
                      COMMENT "Generating documentation")
  ADD_CUSTOM_TARGET( docs DEPENDS ${documentation} )
  INSTALL( DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/docs DESTINATION share/ElmerModuleDriver )
ENDIF()

IF(MPI_CXX_COMPILER)
  INSTALL(TARGETS elmerprep RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
ENDIF()

# Avoid testing for now
#ADD_SUBDIRECTORY (testing)
