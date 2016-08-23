SET(prefix "${CMAKE_INSTALL_PREFIX}")
load_cache(../)
FILE(GLOB SRC_FILES *.src)
#FOREACH(FNAME ${SRC_FILES})
#GET_FILENAME_COMPONENT(BASENAME ${FNAME} NAME_WE)
#ADD_CUSTOM_COMMAND(
#  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.F90
#  COMMAND ${CMAKE_COMMAND} -E copy ${FNAME}
#    ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.F90
#  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.src
#  )
#ENDFOREACH()
INCLUDE_DIRECTORIES("${CMAKE_BINARY_DIR}/elmerfem/fem/src")
INCLUDE_DIRECTORIES("${CMAKE_SOURCE_DIR}/elmerfem/hutiter/src")
INCLUDE_DIRECTORIES("${CMAKE_BINARY_DIR}/elmerfem/fem/src/binio")

FIND_LIBRARY(IMPACT_LIB SITCOM)
FIND_LIBRARY(IMPACT_FLIB SITCOMF)
FIND_FILE(IMPACT_HDR com.h)
GET_FILENAME_COMPONENT(IMPACT_LIBPATH ${IMPACT_LIB} PATH)
GET_FILENAME_COMPONENT(IMPACT_INCPATH ${IMPACT_HDR} PATH)

include_directories(${IMPACT_INCPATH})
add_definitions(-DUSE_ISO_C_BINDINGS)

set (BUILD_STATIC FALSE CACHE BOOL "Build static COM library")

set (SolverMod_SRCS Finalize.F90    GeneralModule.F90      TimeStepper.F90  Initialize.F90 TimeModule.F90  ElmerCSC.F90 LoadFunctionLibrary.F90 ModTestObject.F90)
set (SolverMod_Par_SRCS Finalize.F90    GeneralModule.F90      TimeStepper.F90  InitializeParallel.F90 TimeModule.F90  ElmerCSCParallel.F90 LoadFunctionLibrary.F90 ModTestObject.F90)
set (Library_SRCS Finalize.F90    GeneralModule.F90      TimeStepper.F90  Initialize.F90 TimeModule.F90 LoadFunctionLibrary.F90 ModTestObject.F90)

IF(NOT BUILD_STATIC)
  add_library(ElmerCSC SHARED ${SolverMod_SRCS})
  add_library(LoadFunctionLibrary SHARED LoadFunctionLibrary.F90 ${Library_SRCS})
ELSE()
  add_library(ElmerCSC STATIC ${SolverMod_SRCS})
  add_library(LoadFunctionLibrary STATIC LoadFunctionLibrary.F90 ${Library_SRCS})
  add_definitions( -DSTATIC_LINK )
ENDIF()
SET_TARGET_PROPERTIES(ElmerCSC PROPERTIES INSTALL_RPATH ${ELMERSOLVER_RPATH_STRING})
SET_TARGET_PROPERTIES(LoadFunctionLibrary PROPERTIES PREFIX "")
SET_TARGET_PROPERTIES(LoadFunctionLibrary PROPERTIES SUFFIX "")
SET_TARGET_PROPERTIES(LoadFunctionLibrary PROPERTIES INSTALL_RPATH ${ELMERSOLVER_RPATH_STRING})
target_link_libraries(ElmerCSC elmersolver ${IMPACT_FLIB} ${IMPACT_LIB})
target_link_libraries(LoadFunctionLibrary elmersolver)

INSTALL(TARGETS ElmerCSC LoadFunctionLibrary LIBRARY DESTINATION "lib") 

IF(MPI_CXX_COMPILER)
  ADD_DEFINITIONS(-D_ELMERCSC_PARALLEL_)
  IF(NOT BUILD_STATIC)
    add_library(ElmerCSCParallel SHARED ${SolverMod_Par_SRCS})
  ELSE()
    add_library(ElmerCSCParallel STATIC ${SolverMod_Par_SRCS})
    add_definitions( -DSTATIC_LINK )
  ENDIF()
  SET_TARGET_PROPERTIES(ElmerCSCParallel PROPERTIES INSTALL_RPATH ${ELMERSOLVER_RPATH_STRING})
  target_link_libraries(ElmerCSCParallel elmersolver ${IMPACT_FLIB} ${IMPACT_LIB})
  
  INSTALL(TARGETS ElmerCSCParallel LIBRARY DESTINATION "lib") 
ENDIF()
