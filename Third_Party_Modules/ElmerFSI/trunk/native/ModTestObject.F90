MODULE TESTOBJECT

  USE ModelDescription

  TYPE t_global
     ! Solver information
     CHARACTER(80) :: window_name
     CHARACTER(80) :: other_window_name
     CHARACTER(80) :: c_window_name
     INTEGER :: other_window_handle
     INTEGER :: c_window_handle
     TYPE(Model_t), POINTER :: MyModel
     INTEGER :: ElmerComm
     INTEGER :: nProc
     INTEGER :: procId
     ! ElmerFoamFSI API
     DOUBLE PRECISION, POINTER :: NodeDisplacements(:), Coords(:)
     DOUBLE PRECISION, POINTER :: PreviousNodeDisplacements(:)
     DOUBLE PRECISION, POINTER :: NodeLoads(:), PreviousLoads(:,:)
     DOUBLE PRECISION, POINTER :: FacePressures(:), FaceLoads(:)
     DOUBLE PRECISION, POINTER :: NodePressures(:)
     ! Rocstar API 
     DOUBLE PRECISION, POINTER :: u(:), vs(:), uhat(:), bv(:) ! Nodal values
     DOUBLE PRECISION, POINTER :: ts_alp(:), bf2c(:)    ! Elemental values
     ! MISC 
     INTEGER :: FSIbcId, nElem, nNodes, nConn, SolverId
     INTEGER, POINTER :: Conn(:), verbosity(:)
     CHARACTER(80) :: MeshType
     INTEGER, POINTER :: MyToElmerNodes(:)
     ! Volumetric informaiton
     INTEGER :: nElemBlk, nNodesBlk, nConnBlk
     DOUBLE PRECISION, POINTER :: CoordsBlk(:)
     CHARACTER(80) :: MeshTypeBlk
     INTEGER, POINTER :: ConnBlk(:) 
     INTEGER, POINTER :: MyToElmerNodesBlk(:)
  END TYPE t_global

  CONTAINS
    SUBROUTINE ASSOCIATE_POINTER( attr, ptr)
      TYPE(t_global), POINTER :: attr, ptr
      ptr => attr
    END SUBROUTINE ASSOCIATE_POINTER

END MODULE TESTOBJECT
