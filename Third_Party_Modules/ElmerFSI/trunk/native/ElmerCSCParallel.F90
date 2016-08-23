!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! * 
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation; either
! * version 2.1 of the License, or (at your option) any later version.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! * 
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library (in file ../LGPL-2.1); if not, write 
! * to the Free Software Foundation, Inc., 51 Franklin Street, 
! * Fifth Floor, Boston, MA  02110-1301  USA
! *
! *****************************************************************************/

!> \ingroup ElmerLib

!------------------------------------------------------------------------------
!> A caller for the Elmer main program.
!------------------------------------------------------------------------------

!> \ingroup ElmerLib

!------------------------------------------------------------------------------
!> A caller for the Elmer main program.
!------------------------------------------------------------------------------


#include "../config.h"

! ******************************************************************************

SUBROUTINE InitElmerprep(global, runs, verbIn)
   USE TESTOBJECT
   USE Types
   USE GeneralUtils
   USE ParallelUtils
   USE TimeModule

   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs, counter, nindex, verbIn
   INTEGER :: IERROR

   REAL(KIND=dp) :: CT, RT
!   INTEGER, PARAMETER :: Init=0
   CHARACTER(LEN=MAX_NAME_LEN) :: DateStr, toutput
   INTEGER, POINTER :: TempNodeList1(:), TempNodeListBlk(:)
   LOGICAL :: NodePresent
   INTEGER :: LastFSIElement
   INTEGER :: paneId
!   INTEGER, POINTER :: Conn2(:)
#if defined(MINGW32)
   CALL SET_STDIO_BUFS()
#endif

   WRITE(6,*) 'ElmerCSCParallel: InitElmerprep starting ...'

   !Allocate space for storing the verbosity
   ALLOCATE(global%verbosity(1))
   global%verbosity = verbIn
   WRITE(*,*) 'ElmerCSCParallel: Verbosity = ', global%verbosity

   ! setting up Elmer core
   CALL envir( 'ELMERSOLVER_OUTPUT_TOTAL'//CHAR(0), toutput, tlen )
   Silent = toutput(1:1)=='0' .OR. toutput(1:5)=='false'
   CT = CPUtime()
   RT = RealTime()
   IF ( .NOT. Silent ) THEN
     DateStr = FormatDate()
     WRITE( *,'(A,A)' ) "ELMER SOLVER (v " // VERSION // ") STARTED AT: ", TRIM(DateStr)
     CALL FLUSH(6)
   END IF

   ! calling elmer initializer to read input files and setup the problem
   IF( MyVerbosity > 3) WRITE( *, * ) 'ElmerCSCParallel: Calling ElmerInitialize ... '
   CALL ElmerInitialize(runs, global%ElmerComm)
   global%MyModel => CurrentModel

   !Find out which BC tag is the FSI BC
   ! TODO: Currently only 1 FSI BCS can be registered
   global%FSIbcId = -1
   DO t=1,CurrentModel % NumberOfBCs
      IsFSI = ListGetLogical(CurrentModel % BCs(t) % Values,&
             'FSI BC', GotIt )
      IF ( IsFSI ) THEN
         IF (MyVerbosity > 3) WRITE(*,*) 'ElmerCSCParallel: BC', t, 'is FSI'
         global%FSIbcId = t
      END IF
   END DO
   IF (global%FSIbcId == -1) THEN
      WRITE(*,*) 'ElmerCSCParallel: No FSI BC was found!'
      RETURN
   END IF
     
   !Determine the number of elements & nodes
   MyMesh => CurrentModel % Meshes
   global%nNodes = 0
   global%nElem = 0
   global%nNodesBlk = 0
   global%nElemBlk = MyMesh % NumberOfBulkElements

   !Loop over the boundary elements
   WRITE (*,*) "ElmerCSCParallel: Number of Boundary elements = ", MyMesh % NumberOfBoundaryElements
   !Allocate a temporay array to avoid repeating nodes 
   t = MyMesh % NumberOfBulkElements
   !WRITE (*,*) "Regular Element DIM = ", MyMesh % Elements(t) % TYPE % DIMENSION     
   MyCurrentElement => MyMesh % Elements(t+1)
   t = MyCurrentElement % TYPE % DIMENSION
   counter = (MyMesh % NumberOfBoundaryElements)*(t + 1)
   ALLOCATE(TempNodeList1(counter)) 
   TempNodeList1 = -1
   counter = 0
   DO t = MyMesh % NumberOfBulkElements+1, &
          MyMesh % NumberOfBulkElements + &
          MyMesh % NumberOfBoundaryElements
      MyCurrentElement => MyMesh % Elements(t)
      bc_id = GetBCId(MyCurrentElement)
      !WRITE (*,*) "Boundary Element DIM = ", MyCurrentElement % TYPE % DIMENSION     
      IF ( bc_id == global%FSIbcId ) THEN
         LastFSIElement = t
         IF( MyVerbosity > 3) THEN 
           WRITE(*,*) 'ElmerCSCParallel: FSI Element = ',t,'  on bc_id = ',bc_id
         END IF
         global%nElem = global%nElem + 1
         DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
            MyNodeIndexes => MyCurrentElement % NodeIndexes
            IF( MyVerbosity > 3) THEN 
              WRITE(*,*) 'ElmerCSCParallel: Node index = ', MyNodeIndexes(nt)
            END IF
            NodePresent = .FALSE.
            DO nk=1,counter
              IF (TempNodeList1(nk) == MyNodeIndexes(nt)) THEN
                 NodePresent = .TRUE.
                 EXIT
              END IF
            END DO
            IF ( NodePresent .eqv. .FALSE. ) THEN
               counter = counter + 1
               IF( MyVerbosity > 4) THEN 
                 WRITE(*,*) 'counter = ', counter
               END IF
               TempNodeList1(counter) = MyNodeIndexes(nt)
            END IF
         END DO
      END IF
   END DO
   IF (counter > 0) THEN
      WRITE (*,*) "ElmerCSCParallel: FSI Nodes identified "
   END IF
   CALL MPI_BARRIER(global%ElmerComm, IERROR)
   IF( MyVerbosity > 5) THEN 
     WRITE(*,*) 'ElmerCSCParallel: Number of FSI nodes =', SIZE(TempNodeList1)
     DO t = 1,SIZE(TempNodeList1)
       WRITE(*,*) TempNodeList1(t)
     END DO
   END IF 
   global%nNodes = counter 
   IF( MyVerbosity > 4) THEN 
     WRITE(*,*) 'ElmerCSCParallel number of FSI elements = ',global%nElem
     WRITE(*,*) 'ElmerCSCParallel number of FSI nodes = ',global%nNodes
   END IF

   !Loop over the nodes
   WRITE (*,*) "ElmerCSCParallel: Number of bulk elements = ", MyMesh % NumberOfBulkElements
   WRITE (*,*) "ElmerCSCParallel: Number of nodes = ", global%MyModel%Nodes%NumberOfNodes
   t = global%MyModel%Nodes%NumberOfNodes
   global%nNodesBlk = t
   ALLOCATE(global%MyToElmerNodesBlk(t))
   ALLOCATE(global%CoordsBlk(3*t))
   global%MyToElmerNodesBlk = -1
   global%CoordsBlk = 0.0
   DO t = 1, global%MyModel%Nodes%NumberOfNodes 
      global%MyToElmerNodesBlk(t) = t
      global%CoordsBlk((t-1)*3 + 1) = global % MyModel % Nodes % x(t)
      global%CoordsBlk((t-1)*3 + 2) = global % MyModel % Nodes % y(t)
      global%CoordsBlk((t-1)*3 + 3) = global % MyModel % Nodes % z(t)
   END DO
   IF( MyVerbosity > 4) THEN 
     WRITE(*,*) 'ElmerCSCParallel: number of bulk elements = ',global%nElemBlk
     WRITE(*,*) 'ElmerCSCParallel: number of nodes = ',global%nNodesBlk
   END IF

   !Allocate space for storing the node data
   !This location will be registered with IMPACT
   ALLOCATE(global%MyToElmerNodes(global%nNodes))
   ALLOCATE(CurrentModel%ElmerToMyNodes(CurrentModel % NumberOfNodes))
   global%MyToElmerNodes = -1
   CurrentModel%ElmerToMyNodes = -1

   !Allocate the mesh arrays now that their sizes are known
   !TODO: Assuming the last bulk element is the representative of the all
   !TODO: Only one type of element at the time is allowed for the bulk
   t = MyMesh % NumberOfBulkElements
   MyCurrentElement => MyMesh % Elements(t)
   IF (MyCurrentElement % TYPE % DIMENSION == 3) THEN 
      IF (MyCurrentElement % TYPE % NumberOfNodes == 8) THEN
         global%MeshTypeBlk = ':H8'
         global%nConnBlk = 8
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 6) THEN
         global%MeshTypeBlk = ':P6'
         global%nConnBlk = 6
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 5) THEN
         global%MeshTypeBlk = ':P5'
         global%nConnBlk = 5
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 4) THEN
         global%MeshTypeBlk = ':T4'
         global%nConnBlk = 4
      ELSE
         CALL Fatal( ' ', '3D elements must have at least 4 nodes!' )
      END IF
   ELSE
      CALL Fatal( ' ', 'FSI elements cannot have dimension greater than 2!' )
   END IF
   IF( MyVerbosity > 3) THEN 
     WRITE(*,*) 'ElmerCSCParallel: Bulk elements are of type = ',global%MeshTypeBlk
     WRITE(*,*) 'ElmerCSCParallel: with connectivity =',global%nConnBlk
   END IF

   !Populate the 3D mesh arrays
   !Loop over the 3D elements
   ALLOCATE(global%ConnBlk(global%nConnBlk*global%nElemBlk))
   counter = 0
   DO t = 1, MyMesh % NumberOfBulkElements
      MyCurrentElement => MyMesh % Elements(t)
      !For debugging - you can remove later
      CALL GetElementNodes(ElementNodes,MyCurrentElement)
      !WRITE(*,*) 'Elmer elements:'    
      !DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
      !  WRITE(*,*) nt,':',ElementNodes % x(nt),' ',ElementNodes % y(nt),' ',&
      !                    ElementNodes % z(nt)
      !END DO
      !end of debugging
      counter = counter + 1
      IF( MyVerbosity > 3) THEN 
         WRITE(*,*) 'element = ',t
      END IF
      DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
         MyNodeIndexes => MyCurrentElement % NodeIndexes
         IF( MyVerbosity > 3) THEN 
           WRITE(*,*) 'Node index = ', MyNodeIndexes(nt)
           WRITE(*,*) ElementNodes % x(nt), ElementNodes % y(nt), &
                      ElementNodes % z(nt)
         END IF
         global%ConnBlk((counter-1)*global%nConnBlk + nt) = MyNodeIndexes(nt)
      END DO
   END DO
   
   !Allocate the interface mesh arrays now that their sizes are known
   MyCurrentElement => MyMesh % Elements(LastFSIElement)
   IF (MyCurrentElement % TYPE % DIMENSION == 1) THEN 
      IF (MyCurrentElement % TYPE % NumberOfNodes == 2) THEN
         global%MeshType = ':b2:'
         global%nConn = 2
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 3) THEN
         global%MeshType = ':b3:'
         global%nConn = 3
      ELSE
         CALL Fatal( ' ', 'FSI elements of 1 dimension must have 2 or 3 nodes!' )
      END IF
   ELSE IF (MyCurrentElement % TYPE % DIMENSION == 2) THEN
      IF( MyVerbosity > 3) THEN
         WRITE(*,*) 'FSI BC element NumberOfNodes =',&
                    MyCurrentElement % TYPE % NumberOfNodes 
      END IF
      IF (MyCurrentElement % TYPE % NumberOfNodes == 3) THEN
         global%MeshType = ':t3:'
         global%nConn = 3
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 6) THEN
         global%MeshType = ':t6:'
         global%nConn = 6
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 4) THEN
         global%MeshType = ':q4:'
         global%nConn = 4
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 8) THEN
         global%MeshType = ':q8:'
         global%nConn = 8
      ELSE
         CALL Fatal( ' ', 'FSI elements of 2 dimensions must have 3 or 6 node&
                   triangles or 4 or 8 node quadrilaterals!' )
      END IF
   ELSE
      CALL Fatal( ' ', 'FSI elements cannot have dimension greater than 2!' )
   END IF

   IF( MyVerbosity > 3) THEN 
     WRITE(*,*) 'ElmerCSCParallel: FSI elements are of type = ',global%MeshType
     WRITE(*,*) 'ElmerCSCParallel: with connectivity =',global%nConn
   END IF
   ALLOCATE(global%Conn(global%nConn*global%nElem))
   ALLOCATE(global%Coords(3*global%nNodes))

   !Populate the interface mesh arrays
   !Loop over the boundary elements
   counter = 0
   DO t = MyMesh % NumberOfBulkElements+1, &
          MyMesh % NumberOfBulkElements + &
          MyMesh % NumberOfBoundaryElements

      MyCurrentElement => MyMesh % Elements(t)
      bc_id = GetBCId(MyCurrentElement)

      !For debugging - you can remove later
      CALL GetElementNodes(ElementNodes,MyCurrentElement)
      !WRITE(*,*) 'Elmer elements:'    
      !DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
      !  WRITE(*,*) nt,':',ElementNodes % x(nt),' ',ElementNodes % y(nt),' ',&
      !                    ElementNodes % z(nt)
      !END DO
      !end of debugging

      IF ( bc_id == global%FSIbcId ) THEN
         counter = counter + 1
         IF( MyVerbosity > 3) THEN 
           WRITE(*,*) 'element = ',t
         END IF
         CALL GetElementNodes(ElementNodes,MyCurrentElement)

         DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
            MyNodeIndexes => MyCurrentElement % NodeIndexes
            
            IF( MyVerbosity > 3) THEN 
              WRITE(*,*) 'Node index = ', MyNodeIndexes(nt)
              WRITE(*,*) ElementNodes % x(nt), ElementNodes % y(nt), &
                         ElementNodes % z(nt)
            END IF

            DO nk=1,global%nNodes
               IF (TempNodeList1(nk) == MyNodeIndexes(nt)) THEN
                  nindex = nk 
                  EXIT
               END IF
            END DO

            !Conn2((counter-1)*2+nt) = 10
            global%MyToElmerNodes(nindex) = MyNodeIndexes(nt)
            CurrentModel%ElmerToMyNodes(MyNodeIndexes(nt)) = nindex 
            global%Conn(global%nConn*(counter-1) + nt) = nindex
            global%Coords(3*(nindex-1) + 1) = ElementNodes % x(nt)
            global%Coords(3*(nindex-1) + 2) = ElementNodes % y(nt)
            global%Coords(3*(nindex-1) + 3) = ElementNodes % z(nt)
         END DO
      END IF
   END DO

   IF( MyVerbosity >= 5) THEN 
     WRITE(6,*) 'ElmerCSCParallel: global%MyToElmerNodes = '
     DO i=1,global%nNodes
        WRITE(6,*) i,' = ',global%MyToElmerNodes(i)
     END DO
     WRITE(6,*) 'ElmerCSCParallel: global%MyToElmerNodesBlk = '
     DO i=1,global%nNodesBlk
        WRITE(6,*) i,' = ',global%MyToElmerNodesBlk(i)
     END DO
     WRITE(6,*) 'ElmerCSCParallel: CurrentModel%ElmerToMyNodes = '
     DO i=1,CurrentModel % NumberOfNodes
        WRITE(6,*) i,' = ',CurrentModel%ElmerToMyNodes(i)
     END DO

     WRITE(6,*)'-------------------------------------------'
     WRITE(*,*)'global%Conn = '
     WRITE(6,*)'-------------------------------------------'
     DO t = 1, global%nElem
        WRITE(*,*) t,':',global%Conn(global%nConn*(t-1) + 1),&
                         global%Conn(global%nConn*(t-1) + 2)
     END DO

     WRITE(6,*)'-------------------------------------------'
     WRITE(*,*)'global%ConnBlk = '
     WRITE(6,*)'-------------------------------------------'
     DO t = 1, global%nElemBlk
        WRITE(*,*) t,':',global%ConnBlk(global%nConnBlk*(t-1) + 1),&
                         global%ConnBlk(global%nConnBlk*(t-1) + 2)
     END DO

     WRITE(6,*)
     WRITE(6,*)'-------------------------------------------'
     WRITE(*,*) 'global%Coords = '
     WRITE(6,*)'-------------------------------------------'
     DO t = 1, global%nNodes
        WRITE(*,*) t,':',global%Coords(3*(t-1)+1),global%Coords(3*(t-1)+2),&
                   global%Coords(3*(t-1)+3)
     END DO
   END IF
   WRITE(6,*)

   !Register the mesh data on IMPACT 
   IF( MyVerbosity > 3) THEN 
      WRITE(*,*) 'ElmerCSCParallel: Interface data will be register in ', &
              'srf.'//TRIM(global%MeshType)
      WRITE(*,*) 'ElmerCSCParallel: Volume data will be register in ', &
              'srf.'//TRIM(global%MeshTypeBlk)
   END IF
   !Register data to pane starting from 100
   paneId = 100 + global % procId

   !Surface window registration
   CALL COM_NEW_WINDOW('srf', MPI_COMM_NULL)
   CALL COM_SET_SIZE('srf.nc',paneId,global%nNodes)
   CALL COM_SET_ARRAY('srf.nc',paneId,global%Coords,3)
   CALL COM_SET_SIZE('srf.'//TRIM(global%MeshType),paneId,&
                     global%nElem)
   CALL COM_SET_ARRAY('srf.'//TRIM(global%MeshType),paneId,&
                      global%Conn, global%nConn) 
   CALL COM_WINDOW_INIT_DONE('srf', 1)


   !Surface window registration
   CALL COM_NEW_WINDOW('vol', MPI_COMM_NULL)
   CALL COM_SET_SIZE('vol.nc',paneId,global%nNodesBlk)
   CALL COM_SET_ARRAY('vol.nc',paneId,global%CoordsBlk,3)
   CALL COM_SET_SIZE('vol'//'.'//TRIM(global%MeshTypeBlk),paneId,&
                     global%nElemBlk)
   CALL COM_SET_ARRAY('vol'//'.'//TRIM(global%MeshTypeBlk),paneId,&
                      global%ConnBlk, global%nConnBlk) 
   CALL COM_WINDOW_INIT_DONE('vol', 1)

   WRITE(6,*) 'ElmerCSCParallel:InitElmerprep finished.'
END SUBROUTINE InitElmerPrep


! Old API
!SUBROUTINE Initializer(global, runs, verbIn)
! New API
SUBROUTINE Initializer(global, InitialTime, MPI_COMM_ROCSTAR, MAN_init, surfIn, volIn, obtain_attr)

! global : is the global object
! InitialTime : a double representing initial time
! MPI_COMM_ROCSTAR : MPI communicator of the Rocstar
! MAN_init : Rocman's initializer 
! surfIn : the name of the surface (interface) window loaded by Rocstar
! volIn : the name of the volumer window loaded by Rocstar
! obtain_attr : the handle of the obtain attribute provided by SimIN

   USE TESTOBJECT
   USE Types
   USE GeneralUtils
   USE ParallelUtils
   USE TimeModule

   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER                 :: runs, counter, nindex, verbIn
   REAL*8, INTENT(IN)      :: InitialTime
   INTEGER, INTENT(IN)     :: MPI_COMM_ROCSTAR
   INTEGER, INTENT(IN)     :: MAN_init, obtain_Attr
   CHARACTER(*), INTENT(IN):: surfIn, volIn
   
   ! default windows to load stuff for Rocstar   
   CHARACTER(*), PARAMETER :: surfWin = "sElmer"
   CHARACTER(*), PARAMETER :: volWin = "vElmer" 

   ! Additional information
   INTEGER                 :: procId, numProc, IERROR
   LOGICAL                 :: stream
   

   REAL(KIND=dp) :: CT, RT
!   INTEGER, PARAMETER :: Init=0
   CHARACTER(LEN=MAX_NAME_LEN) :: DateStr, toutput
   INTEGER, POINTER :: TempNodeList(:)
   LOGICAL :: NodePresent
   INTEGER :: LastFSIElement
   INTEGER :: paneId
!   INTEGER, POINTER :: Conn2(:)
#if defined(MINGW32)
   CALL SET_STDIO_BUFS()
#endif

   ! old API initialization to default
   runs   = 1;
   verbIn = 0;

   ! initializations
   procId = global%procId
   numProc  = global%nProc
   CALL Info( 'ElmerCSCParallel:Initializer', ' Starting.... ')

   !Allocate space for storing the verbosity
   ALLOCATE(global%verbosity(1))
   global%verbosity = verbIn
   IF (stream) CALL Info('ElmerCSCParallel:Initializer',' verbosity = '//TRIM(i2s(verbIn)))

   ! Elmer initialization step
   CALL envir( 'ELMERSOLVER_OUTPUT_TOTAL'//CHAR(0), toutput, tlen )
   Silent = toutput(1:1)=='0' .OR. toutput(1:5)=='false'
   CT = CPUtime()
   RT = RealTime()
   IF ( .NOT. Silent ) THEN
     DateStr = FormatDate()
     IF (stream) WRITE( *,'(A,A)' ) "ELMER SOLVER (v " // VERSION // ") STARTED AT: ", TRIM(DateStr)
     CALL FLUSH(6)
   END IF
   
   ! Calling Elmer intializer
   IF( MyVerbosity > 3) THEN
       IF (stream) WRITE(*, *) 'ElmerCSCParallel:Initializer: Calling ElmerInitialize ... '
   END IF
   CALL ElmerInitialize(runs, global%ElmerComm)

   global%MyModel => CurrentModel

   !Assigning stuff & printing out to check
   global%SolverId =  -1
   DO i=1, CurrentModel % NumberOfSolvers
      Solver => global % MyModel % Solvers(i)
      SolverParams => GetSolverParams(Solver)
      Equation = GetString(SolverParams, 'Equation', GotIt)
      CALL Info('ElmerCSCParalle:Initialize',' Type of Equation = '//Equation)
      IF( Equation .eq. 'nonlinear elasticity' ) THEN
         global%SolverId =  i
         EXIT
      ELSE IF( Equation .eq. 'linear elasticity' ) THEN
         global%SolverId =  i
         EXIT
      END IF
   END DO

   !Find out which BC tag is the FSI BC
   global%FSIbcId = -1
   DO t=1,CurrentModel % NumberOfBCs
      IsFSI = ListGetLogical(CurrentModel % BCs(t) % Values,&
             'FSI BC', GotIt )
      IF ( IsFSI ) THEN
         IF (MyVerbosity > 3) WRITE(*,*) 'BC', t, 'is FSI'
         global%FSIbcId = t
      END IF
   END DO

   IF (global%FSIbcId == -1) THEN
      WRITE(*,*) 'Not registering data no FSI BC!'
      RETURN
   END IF
     
   !Determine the number of elements & nodes on the
   !FSI BC so we can allocate the arrays
   MyMesh => CurrentModel % Meshes
   global%nNodes = 0
   global%nElem = 0

   !Allocate a temporay array to avoid repeating nodes 
   WRITE (*,*) 'Rank '//i2s(global%procId)//' Number of Bulk elemenets = ', MyMesh % NumberOfBulkElements
   WRITE (*,*) 'Rank '//i2s(global%procId)//' Number of Boundary elemenets = ', MyMesh % NumberOfBoundaryElements
 
   t = MyMesh % NumberOfBulkElements
   !WRITE (*,*) "Regular Element DIM = ", MyMesh % Elements(t) % TYPE % DIMENSION     
   MyCurrentElement => MyMesh % Elements(t+1)
   t = MyCurrentElement % TYPE % DIMENSION
   counter = (MyMesh % NumberOfBoundaryElements)*(t + 1)
   ALLOCATE(TempNodeList(counter)) 
   TempNodeList = -1

   !Loop over the boundary elements
   counter = 0
   DO t = MyMesh % NumberOfBulkElements+1, &
          MyMesh % NumberOfBulkElements + &
          MyMesh % NumberOfBoundaryElements

      MyCurrentElement => MyMesh % Elements(t)
      bc_id = GetBCId(MyCurrentElement)
      !WRITE (*,*) "Boundary Element DIM = ", MyCurrentElement % TYPE % DIMENSION     
 
      IF ( bc_id == global%FSIbcId ) THEN
         LastFSIElement = t
         IF( MyVerbosity > 3) THEN 
           WRITE(*,*) 'Initializer element = ',t,' bc_id = ',bc_id
         END IF
         global%nElem = global%nElem + 1
         DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
            MyNodeIndexes => MyCurrentElement % NodeIndexes
            IF( MyVerbosity > 3) THEN 
              WRITE(*,*) 'Node index = ', MyNodeIndexes(nt)
            END IF
            NodePresent = .FALSE.
            DO nk=1,counter
              IF (TempNodeList(nk) == MyNodeIndexes(nt)) THEN
                 NodePresent = .TRUE.
                 EXIT
              END IF
            END DO
            IF ( NodePresent .eqv. .FALSE. ) THEN
               counter = counter + 1
               IF( MyVerbosity > 4) THEN 
                 WRITE(*,*) 'counter = ', counter
               END IF
               TempNodeList(counter) = MyNodeIndexes(nt)
            END IF
         END DO
      END IF
   END DO

   IF( MyVerbosity > 5) THEN 
     WRITE(*,*) 'SIZE(TempNodeList) =', SIZE(TempNodeList)
     DO t = 1,SIZE(TempNodeList)
       WRITE(*,*) TempNodeList(t)
     END DO
   END IF 
  
   global%nNodes = counter 

   IF( MyVerbosity > 4) THEN 
     WRITE(*,*) 'global%nElem = ',global%nElem
     WRITE(*,*) 'global%nNodes = ',global%nNodes
   END IF
   
   !IMPACT data allocation and initialization

   !ElmerFoamFSI API
   !Allocate space for storing the NodeDisplacements
   !This location will be registered with IMPACT
   IF( MyVerbosity > 3) WRITE(*,*) 'global%nNodes =', global%nNodes
   ALLOCATE(global%NodeDisplacements(3*global%nNodes))
   global%NodeDisplacements = 0.0
   ALLOCATE(global%PreviousNodeDisplacements(3*global%nNodes))
   global%PreviousNodeDisplacements = 0.d0
   ALLOCATE(global%MyToElmerNodes(global%nNodes))
   IF( MyVerbosity > 3) THEN 
     WRITE(*,*) 'SIZE(NodeDisplacements)=',SIZE(global%NodeDisplacements)
   END IF
   ALLOCATE(CurrentModel%ElmerToMyNodes(CurrentModel % NumberOfNodes))
   global%MyToElmerNodes = -1
   CurrentModel%ElmerToMyNodes = -1

   !Allocate space for storing the FaceLoads
   !This location will be registered with IMPACT
   ALLOCATE(global%FaceLoads(3*global%nElem))
   global%FaceLoads = 0.0
   !Allocate space for storing the FacePressures
   !This location will be registered with IMPACT
   ALLOCATE(global%FacePressures(global%nElem))
   global%FacePressures = 0.0
   !Allocate space for storing the NodePressures
   !This location will be registered with IMPACT
   ALLOCATE(global%NodePressures(global%nNodes))
   global%NodePressures = 0
   !Allocate space for storing the NodeLoads
   !This location will be registered with IMPACT
   ALLOCATE(global%NodeLoads(3*global%nNodes))
   global%NodeLoads = 0.0
   !Allocate space for storing the previous loads
   !so linear interpolation can be done
   ALLOCATE(global%PreviousLoads(3,global%nNodes))
   global%PreviousLoads = 0.0
   !Allocate the NodeLoads that gets passed to the user defined function
   ALLOCATE(CurrentModel%NodeLoadsPass(3,global%nNodes))
   IF( MyVerbosity > 3) WRITE(*,*) 'SIZE(NodeLoads)=',SIZE(global%NodeLoads)
   !Initialize the loads to 0.0
   DO t = 1, global%nNodes
     DO j =1,3
!       CurrentModel%NodeLoadsPass(j,t) = (t-1)*3.0 + j*1.0
        CurrentModel%NodeLoadsPass(j,t) = 0.0
        global%PreviousLoads(j,t) = 0.0
     END DO
   END DO
   
   ! ROCSTAR API
   ALLOCATE(global%u(3*global%nNodes))
   global%u = 0.d0
   ALLOCATE(global%uhat(3*global%nNodes))
   global%uhat = 0.d0
   ALLOCATE(global%vs(3*global%nNodes))
   global%vs = 0.d0
   ALLOCATE(global%bv(global%nNodes))
   global%bv = 0.d0
   ALLOCATE(global%ts_alp(3*global%nElem))
   global%ts_alp = 0.d0
   ALLOCATE(global%bf2c(global%nElem))
   global%bf2c = 0.d0


   !Allocate the mesh arrays now that their sizes are known
   MyCurrentElement => MyMesh % Elements(LastFSIElement)
   IF (MyCurrentElement % TYPE % DIMENSION == 1) THEN 
      IF (MyCurrentElement % TYPE % NumberOfNodes == 2) THEN
         global%MeshType = ':b2:'
         global%nConn = 2
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 3) THEN
         global%MeshType = ':b3:'
         global%nConn = 3
      ELSE
         CALL Fatal( ' ', 'FSI elements of 1 dimension must have 2 or 3 nodes!' )
      END IF
   ELSE IF (MyCurrentElement % TYPE % DIMENSION == 2) THEN
      IF( MyVerbosity > 3) THEN
         WRITE(*,*) 'FSI BC element NumberOfNodes =',&
                    MyCurrentElement % TYPE % NumberOfNodes 
      END IF
      IF (MyCurrentElement % TYPE % NumberOfNodes == 3) THEN
         global%MeshType = ':t3:'
         global%nConn = 3
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 6) THEN
         global%MeshType = ':t6:'
         global%nConn = 6
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 4) THEN
         global%MeshType = ':q4:'
         global%nConn = 4
      ELSE IF (MyCurrentElement % TYPE % NumberOfNodes == 8) THEN
         global%MeshType = ':q8:'
         global%nConn = 8
      ELSE
         CALL Fatal( ' ', 'FSI elements of 2 dimensions must have 3 or 6 node&
                   triangles or 4 or 8 node quadrilaterals!' )
      END IF
   ELSE
      CALL Fatal( ' ', 'FSI elements cannot have dimension greater than 2!' )
   END IF

   IF( MyVerbosity > 3) THEN 
     WRITE(*,*) 'global%MeshType = ',global%MeshType
     WRITE(*,*) 'global%nConn =',global%nConn
   END IF
   ALLOCATE(global%Conn(global%nConn*global%nElem))
   ALLOCATE(global%Coords(3*global%nNodes))

   !Populate the mesh arrays
   !Loop over the boundary elements
   counter = 0
   DO t = MyMesh % NumberOfBulkElements+1, &
          MyMesh % NumberOfBulkElements + &
          MyMesh % NumberOfBoundaryElements

      !WRITE(*,*) "ElmerCSCParallel element = ", t

      MyCurrentElement => MyMesh % Elements(t)
      bc_id = GetBCId(MyCurrentElement)

      !For debugging - you can remove later
      CALL GetElementNodes(ElementNodes,MyCurrentElement)
      !WRITE(*,*) 'Elmer elements:'    
      !DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
      !  WRITE(*,*) nt,':',ElementNodes % x(nt),' ',ElementNodes % y(nt),' ',&
      !                    ElementNodes % z(nt)
      !END DO
      !end of debugging

      IF ( bc_id == global%FSIbcId ) THEN
         counter = counter + 1
         IF( MyVerbosity > 3) THEN 
           WRITE(*,*) 'element = ',t
         END IF
         CALL GetElementNodes(ElementNodes,MyCurrentElement)

         global%FacePressures(counter) = 0.0
         global%FaceLoads(3*(counter-1) + 1) = 0.0
         global%FaceLoads(3*(counter-1) + 1) = 0.0
         global%FaceLoads(3*(counter-1) + 1) = 0.0

         DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
            MyNodeIndexes => MyCurrentElement % NodeIndexes
            
            IF( MyVerbosity > 3) THEN 
              WRITE(*,*) 'Node index = ', MyNodeIndexes(nt)
              WRITE(*,*) ElementNodes % x(nt), ElementNodes % y(nt), &
                         ElementNodes % z(nt)
            END IF

            DO nk=1,global%nNodes
               IF (TempNodeList(nk) == MyNodeIndexes(nt)) THEN
                  nindex = nk 
                  EXIT
               END IF
            END DO

            !Conn2((counter-1)*2+nt) = 10
            global%MyToElmerNodes(nindex) = MyNodeIndexes(nt)
            CurrentModel%ElmerToMyNodes(MyNodeIndexes(nt)) = nindex 
            global%Conn(global%nConn*(counter-1) + nt) = nindex
            global%Coords(3*(nindex-1) + 1) = ElementNodes % x(nt)
            global%Coords(3*(nindex-1) + 2) = ElementNodes % y(nt)
            global%Coords(3*(nindex-1) + 3) = ElementNodes % z(nt)
            global%NodeLoads(3*(nindex-1) + 1) = 0.0
            global%NodeLoads(3*(nindex-1) + 2) = 0.0
            global%NodeLoads(3*(nindex-1) + 3) = 0.0
            global%NodePressures(nindex) = 0.0
         END DO
      END IF
   END DO

   IF( MyVerbosity >= 5) THEN 
     WRITE(6,*) 'global%MyToElmerNodes = '
     DO i=1,global%nNodes
        WRITE(6,*) i,' = ',global%MyToElmerNodes(i)
     END DO
     WRITE(6,*) 'CurrentModel%ElmerToMyNodes = '
     DO i=1,CurrentModel % NumberOfNodes
        WRITE(6,*) i,' = ',CurrentModel%ElmerToMyNodes(i)
     END DO

     WRITE(6,*)'-------------------------------------------'
     WRITE(*,*) 'global%Conn = '
     WRITE(6,*)'-------------------------------------------'
     DO t = 1, global%nElem
        WRITE(*,*) t,':',global%Conn(global%nConn*(t-1) + 1),&
                         global%Conn(global%nConn*(t-1) + 2)
     END DO

     WRITE(6,*)

     WRITE(6,*)'-------------------------------------------'
     WRITE(*,*) 'global%Coords = '
     WRITE(6,*)'-------------------------------------------'
     DO t = 1, global%nNodes
        WRITE(*,*) t,':',global%Coords(3*(t-1)+1),global%Coords(3*(t-1)+2),&
                   global%Coords(3*(t-1)+3)
     END DO
   END IF

   WRITE(6,*)

   !Register the mesh 
   IF( MyVerbosity > 3) THEN 
      WRITE(*,*) 'window.mesh = ', &
              TRIM(global%window_name)//'.'//TRIM(global%MeshType)
   END IF
   
   !holding for all processes to finish
   CALL MPI_BARRIER(global%ElmerComm, IERROR);

   ! setting up volume window
   ! TODO: This window will be empty for now, but in near future
   ! TODO: it should load rocstar restar information to Elmer
   CALL COM_new_window(TRIM(volWin))
   CALL COM_WINDOW_INIT_DONE(TRIM(volWin), 1)

   !Register data to pane starting from 100
   paneId = 100 + global % procId

   !Setting up surface window
   CALL COM_new_window(TRIM(surfWin))
   CALL COM_SET_SIZE(TRIM(surfWin)//'.nc',paneId,global%nNodes)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.nc',paneId,global%Coords,3)
   CALL COM_SET_SIZE(TRIM(surfWin)//'.'//TRIM(global%MeshType),paneId,&
                     global%nElem)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.'//TRIM(global%MeshType),paneId,&
                      global%Conn, global%nConn) 

   !CALL COM_SET_SIZE('Window1.:b2:real', 11,global%nElem)
   !CALL COM_RESIZE_ARRAY('Window1.:b2:real',11)
   !CALL COM_RESIZE_ARRAY(TRIM(global%window_name)//'.'//TRIM(global%MeshType),11)
   !CALL COM_SET_ARRAY('Window1.:b2:real',11,Conn,2)

   !Set the displacments array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.Displacements', 'n', COM_DOUBLE_PRECISION, 3, 'm')
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.Displacements',paneId,&
                      global%NodeDisplacements,3)
   !Masoud
   ! n: means node quantity, 3: number of columns, 11: window number (currently
   ! default used everywhere), 'm': is units of the quanitity
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.PreviousDisplacements', 'n', COM_DOUBLE_PRECISION, 3, 'm')
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.PreviousDisplacements',paneId,&
                      global%PreviousNodeDisplacements,3)
   !Masoud End

   !Set the loads array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.Loads', 'n', COM_DOUBLE_PRECISION, 3, '')
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.Loads',paneId,&
                      global%NodeLoads,3)

   !Set the loads array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.NodePressures',&
                         'n', COM_DOUBLE_PRECISION, 1, '')
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.NodePressures',paneId,&
                      global%NodePressures,1)

   !Set the pressures array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.Pressures', 'e',&
                         COM_DOUBLE_PRECISION, 1, '')
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.Pressures',paneId,&
                      global%FacePressures,1)

   !Set the face loads array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.FaceLoads', 'e',&
                         COM_DOUBLE_PRECISION, 3, '')
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.FaceLoads',paneId,&
                      global%FaceLoads,3)

   ! Registering additional information needed for Rocstar
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.rhos', 'w', COM_DOUBLE_PRECISION, 1, 'kg/m^3')
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.u', 'n',  COM_DOUBLE_PRECISION, 3, 'm')
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.vs', 'n', COM_DOUBLE_PRECISION, 3, 'm/s')
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.uhat', 'n', COM_DOUBLE_PRECISION, 3, 'm')

   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.ts_alp', 'e', COM_DOUBLE_PRECISION, 3, 'Pa')
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.bv', 'n', COM_INTEGER, 1, '')
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.bf2c', 'e', COM_INTEGER, 1, '')
   CALL COM_NEW_DATAITEM(TRIM(surfWin)//'.bcflag', 'p', COM_INTEGER, 1, '')

   ! Setting up some of the information
   CALL COM_SET_SIZE(TRIM(surfWin)//'.bcflag', paneId, 1)
   CALL COM_RESIZE_ARRAY(TRIM(surfWin)//'.bcflag', paneId)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.u', paneId, global%u, 3)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.uhat', paneId, global%uhat, 3)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.vs', paneId, global%vs, 3)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.bv', paneId, global%bv, 1)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.ts_alp', paneId, global%ts_alp, 3)
   CALL COM_SET_ARRAY(TRIM(surfWin)//'.bf2c', paneId, global%bf2c, 1)
   
   ! to create mapping
   CALL COM_WINDOW_INIT_DONE(TRIM(surfWin), 1)


   ! invoking Rocman's initializer to build buffer windows
   CALL COM_call_function( MAN_init, 2, surfWin, volWin)

   PreviousTime = 0.0

   IF( MyVerbosity > 4) THEN 
     WRITE(6,*)'-------------------------------------------'
     WRITE(*,*) 'global%NodeDisplacements = '
     WRITE(6,*)'-------------------------------------------'
     DO i = 1,global%nNodes
        DO j=1,3
           global%NodeDisplacements(3*(i-1) + j) = 0.0
        ENDDO
        WRITE(6,*) (global%NodeDisplacements(3*(i-1) + j),j=1,3)
     ENDDO

     WRITE(6,*)
   END IF


   CALL Info('ElmerCSCParallel:Initializer',' Done.....')

   NULLIFY(TempNodeList)
 
END SUBROUTINE Initializer




! *****************************************************************************
! ElmerFoamFSI interface
!SUBROUTINE RUN(global, runs, tFinal)
! Rocstar interface
SUBROUTINE RUN(global, currentTime, currentTimeStep, MAN_update_inbuff)
  
   USE TESTOBJECT
   USE GeneralModule
   USE TimeModule

   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs = 1
   DOUBLE PRECISION :: tFinal
   DOUBLE PRECISION :: standardTimestep
   DOUBLE PRECISION :: var1, var2, var3
   DOUBLE PRECISION :: deltaTime
   ! Rocstar input variables
   DOUBLE PRECISION, INTENT(IN) :: currentTime, currentTimeStep
   INTEGER, INTENT(IN) :: MAN_update_inbuff
   ! MISC
   INTEGER :: IERR
   DOUBLE PRECISION :: alpha

   !INTEGER, PARAMETER :: Initialize=0
   !INTEGER :: TimeIntervals, CoupledMinIter, CoupledMaxIter, Timestep
   !INTEGER, POINTER, SAVE :: OutputIntervals(:)
   !LOGICAL :: LastSaved, Transient, Scanning

   INTERFACE
     SUBROUTINE UpdateDisplacements(global,runs,tFinal)
       USE TESTOBJECT
       USE GeneralModule
       USE TimeModule

       TYPE(t_global), POINTER :: global
       INTEGER :: runs
       DOUBLE PRECISION :: tFinal

     END SUBROUTINE UpdateDisplacements
   END INTERFACE

   ! Rocstar's needed final time
   tFinal = currentTime + currentTimeStep

   ! Informing user
   IF (global%procId .EQ. 0 ) THEN 
      CALL Info('ElmerCSCParallel:Run', ' Stepping in time ... ')
      CALL Info('ElmerCSCParallel:Run', ' Rocstar input :')
      WRITE (*,*) " Rocstar's currentTime         = ", currentTime
      WRITE (*,*) " Rocstar's currentTimeStep     = ", currentTimeStep
      WRITE (*,*) " ElmerModule's currentTime     = ", sTime(1)
      WRITE (*,*) " ElmerModule's currentTimeStep = ", TimestepSizes(1,1) 
      WRITE (*,*) "   + + +           ---> tFinal = ", tFinal
   END IF
   CALL MPI_BARRIER(global%ElmerComm, IERR)

   !Update input buffer for tractions
   !Compute alpha for Rocfrac call
   ! sTime : Current Elmer time
   ! currentTime: Rocstar's current Time
   ! CurrentTimeStep : Rocstar's current timestep
   !alpha = 0.d0
   alpha =  (sTime(1) - currentTime)/currentTimeStep
   CALL COM_call_function( MAN_update_inbuff, 1, alpha)

   ! check rocstar input
   IF (MyVerbosity > 3) THEN
      CALL Info('ElmerCSCParallel:Run','ts_alp = ')
      DO t = 1, global%nElem
        WRITE(*,*) 'Rank '//i2s(global%procID)//'Boundary Element '//i2s(t)&
                   //'Traction = ', global%ts_alp(3*(t-1)+1),  global%ts_alp(3*(t-1)+2),&
                                    global%ts_alp(3*(t-1)+3)
      END DO
   END IF
   CALL MPI_BARRIER(global%ElmerComm, IERR)

   !calculate time stepping for Elmer
   CurrentModel => global%MyModel   
   FinalTime = tFinal  
   IF (PreviousTime >= FinalTime) THEN
     CALL Fatal( 'ElmerCSCParallel:Run ','can not back up in time, sorry!')
   END IF

   standardTimestep = TimestepSizes(1,1)
   deltaTime = tFinal - sTime(1)
   Timesteps(1) = FLOOR(deltaTime/standardTimestep)
 
   IF (MyVerbosity > 3) THEN
     WRITE(*,*) 'tFinal = ', tFinal 
     WRITE(*,*) 'sTime(1) = ', sTime(1)
     WRITE(*,*) 'standardTimestep = ', standardTimestep
   END IF
   var3 = deltaTime - INT(deltaTime/standardTimestep)*standardTimestep

   IF ( abs(var3) < 1.0d-12 ) THEN
      TimeIntervals = 1
      IF (MyVerbosity > 3) WRITE(*,*)  'standardTimestep evenly divides time'
   ELSE
      TimestepSizes(2,1) = deltaTime - Timesteps(1)*standardTimestep
      TimeIntervals = 2
      Timesteps(2) = 1
      IF (OutputIntervals(1) == 1) THEN
        OutputIntervals(2) = 1
      END IF
      IF (MyVerbosity > 3) THEN
        WRITE(*,*) 'Remainder for time'
        WRITE(*,*) 'TimestepSizes(2,1) = ', TimestepSizes(2,1)
      END IF
   END IF
   
   IF (MyVerbosity > 3) THEN
     WRITE(*,*) 'In Run function'
     WRITE(*,*) 'tFinal = ', tFinal 
     WRITE(*,*) 'sTime(1) = ', sTime(1)
     WRITE(*,*) 'Timesteps(1) = ', Timesteps(1)
     WRITE(*,*) 'standardTimestep = ', standardTimestep
     !Print out the NodePressures as a check
     WRITE(*,*) 'NodeLoads = '
     DO t = 1, 3*global%nElem
       WRITE(*,*) global%ts_alp(t)
     END DO

   END IF

!------------------------------------------------------------------------------
!      Here we actually start the simulation ....
!      First go trough timeintervals
!------------------------------------------------------------------------------
   !WRITE(*,*) 'global%nNodes = ',global%nNodes
   IF (MyVerbosity > 3) WRITE(*,*) 'Calling TimeStepper' 
      CALL TimeStepper(global,runs)
   IF (MyVerbosity > 3) WRITE(*,*) 'Done with TimeStepper' 

   IF (global%FSIbcId /= -1) THEN
      CALL Info('ElmerCSCParallel:Run','Updating Displacements')
      CALL UpdateDisplacements(global,runs, tFinal)
   END IF

   !Setting PreviousLoads values to NodeLoads now that
   !Run is finished
   DO t = 1, global%nNodes
     DO j =1,3
       global%PreviousLoads(j,t) = global%NodeLoads(3*(t-1) + j)
     END DO
   END DO

   !Setting PreviousTime to FinalTime now that
   !Run is finished
   PreviousTime = FinalTime
   IF (runs == 1 .AND. CurrentModel%GetTestLoads .eqv. .TRUE. & 
      .AND. CurrentModel%UDFUsed .eqv. .TRUE.) THEN
     runs = 2
   END IF
   CALL Info('ElmerCSCParallel:Run',' Finished time stepping')

END SUBROUTINE RUN

!**************************************************************************

SUBROUTINE UpdateDisplacements(global,runs, tFinal)
   USE TESTOBJECT
   USE GeneralModule
   USE TimeModule

   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs
   DOUBLE PRECISION :: tFinal
   INTEGER :: nCount, counter, nCountMax
   INTEGER :: IERR

   CALL Info('ElmerCSCParallel:UpdateDisplacements',&
              'Reporting displacements to Rocstar')
   CALL MPI_BARRIER(global%ElmerComm, IERR)   

   !access the displacements here
   Solver => CurrentModel % Solvers(global%SolverId)

   !IF (TRIM(Equation) == "nonlinear elasticity") THEN
   StressSol => Solver % Variable
   Displacement   => StressSol % Values
   MyPerm => StressSol % Perm

   IF( MyVerbosity > 3) THEN
     WRITE(*,*) 'The SIZE(Displacement) = ', SIZE(Displacement)
     WRITE(*,*) 'After SolverActivate Call, Displacement(1) = ',&
              Displacement(1)
     DO t=1,SIZE(Displacement)
        WRITE(*,*) 'Displacement',t,'=',Displacement(t)
     END DO
   END IF


   !Jess adding stuff to try accessing wetted surface
   ElementCount = GetNOFActive()
   BoundaryElementCount = GetNOFBoundaryElements()

   MyMesh => CurrentModel % Meshes
         
   !Write out nodes, coordinates, and connectivities
   !Store at registered address
   nCount = 0
   nCountMax = 0
   counter = 0
   IF( MyVerbosity > 3) THEN 
     WRITE(*,*) 'Number of bulk elements = ', MyMesh % NumberOfBulkElements
     WRITE(*,*) 'Number of boundary elements = ', MyMesh % NumberOfBoundaryElements
   END IF
   DO t = MyMesh % NumberOfBulkElements+1, &
          MyMesh % NumberOfBulkElements + &
          MyMesh % NumberOfBoundaryElements
      MyCurrentElement => MyMesh % Elements(t)
      bc_id = GetBCId(MyCurrentElement)
      IF ( bc_id == global%FSIbcId ) THEN
         counter = counter+1
         IF( MyVerbosity > 3) THEN
           WRITE(*,*) '*************************************'
           WRITE(*,*) 'element ',counter,'on FSI boundary'
           WRITE(*,*) 'Update t = ',t,' bc_id = ',bc_id
           WRITE(*,*)
           WRITE(*,*) 'Element no. of nodes:', &
                      MyCurrentElement % TYPE % NumberOfNodes
         END IF

         CALL GetElementNodes(ElementNodes,MyCurrentElement)

         DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
            MyNodeIndexes => MyCurrentElement % NodeIndexes
              
            IF( MyVerbosity > 3) THEN
              WRITE(*,*) '*********************'
              WRITE(*,*) 'Node index = ', MyNodeIndexes(nt)
              WRITE(*,*) ElementNodes % x(nt), ElementNodes % y(nt), &
                         ElementNodes % z(nt)
              WRITE(*,*) 'MyPerm = ', MyPerm(MyNodeIndexes(nt))
            END IF

            IF ( MyPerm(MyNodeIndexes(nt)) > 0 ) THEN
               nk = (StressSol % DOFs)*(MyPerm(MyNodeIndexes(nt)) - 1)
               IF( MyVerbosity > 3) WRITE(*,*) 'DOFs = ', StressSol % DOFs

               nCount = CurrentModel%ElmerToMyNodes(MyNodeIndexes(nt))

               IF ( nCount > nCountMax ) THEN
                  IF( MyVerbosity > 3) THEN
                  WRITE(6,*) 'Updating NodeDisplacement(',nCount,')'
                 END IF
 
                 IF ( StressSol % DOFs == 1 ) THEN
                     IF( MyVerbosity > 3) WRITE(*,*) Displacement(nk+1), 0.0, 0.0
                     global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)
                     global%NodeDisplacements(3*(nCount-1) + 2) = 0.0d0
                     global%NodeDisplacements(3*(nCount-1) + 3) = 0.0d0
                 ELSE IF ( StressSol % DOFs == 2 ) THEN
                     IF( MyVerbosity > 3) WRITE(*,*) Displacement(nk+1), Displacement(nk+2)
                     global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)
                     global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)
                     global%NodeDisplacements(3*(nCount-1) + 3) = 0.0d0
                 ELSE IF ( StressSol % DOFs == 3 ) THEN
                     ! ElmerFoamFSI
                     !Calculating displacement differences to pass to fluid solver
                     !global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)&
                     !- global%PreviousNodeDisplacements(3*(nCount-1) + 1)
                     !global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)&
                     !- global%PreviousNodeDisplacements(3*(nCount-1) + 2)
                     !global%NodeDisplacements(3*(nCount-1) + 3) = Displacement(nk+3)&
                     !- global%PreviousNodeDisplacements(3*(nCount-1) + 3)
                     ! Rocstar
                     ! displacements
                     global%u(3*(nCount-1) + 1) = Displacement(nk+1)
                     global%u(3*(nCount-1) + 2) = Displacement(nk+2)
                     global%u(3*(nCount-1) + 3) = Displacement(nk+3)
                     ! velocities
                     global%vs(3*(nCount-1) + 1) = (Displacement(nk+1)&
                                                   - global%PreviousNodeDisplacements(3*(nCount-1) + 1))&
                                                   /TimestepSizes(1,1)
                     global%vs(3*(nCount-1) + 2) = (Displacement(nk+2)&
                                                   - global%PreviousNodeDisplacements(3*(nCount-1) + 2))&
                                                   /TimestepSizes(1,1)
                     global%vs(3*(nCount-1) + 3) = (Displacement(nk+3)&
                                                   - global%PreviousNodeDisplacements(3*(nCount-1) + 3))&
                                                   /TimestepSizes(1,1)
                     !Updating previous displacements to the current values for the
                     !next timestep
                     global%PreviousNodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1) 
                     global%PreviousNodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)
                     global%PreviousNodeDisplacements(3*(nCount-1) + 3) = Displacement(nk+3)
                     !Priniting increament displacement values for the user
                     !WRITE(*,*) ' increament disps= ', global%NodeDisplacements(3*(nCount-1) + 1),&
                     !global%NodeDisplacements(3*(nCount-1) + 2), &
                     !global%NodeDisplacements(3*(nCount-1) + 3)
                     !WRITE(*,*) '----------------------'
                     ! Masoud : End
                 ELSE
                     WRITE(*,*) 'StressSol % DOFs = ', StressSol % DOFs
                     WRITE(*,*) 'DOFs are assumed to be <= 3'
                     CALL Fatal( ' ', 'StressSol DOFs are greater than 3 ' )
                 END IF
                 nCountMax = MAX(nCount, nCountMax)
               END IF

            END IF

         END DO                
      END IF
   END DO

   IF( MyVerbosity > 50) THEN
     WRITE(6,*) 'tFinal = ', tFinal
     WRITE(6,*) 'NodeDisplacements'
     DO i = 1,global%nNodes
        WRITE(6,*) (global%NodeDisplacements(3*(i-1) + j),j=1,3)
     ENDDO
     WRITE(6,*) 'Exiting UpdateDisplacements'
   END IF

END SUBROUTINE UpdateDisplacements

! ********************************************************************
! ElmerFoamFSI API
!SUBROUTINE Finalize(global, runs)
! Rocstar API
SUBROUTINE Finalize(global)
   USE TESTOBJECT
   USE Types
   USE GeneralUtils
   USE ParallelUtils
   USE GeneralModule, only : FirstTime
 
   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs = 1

   !INTEGER, PARAMETER :: Initialize=0
   INTEGER :: tlen
   LOGICAL :: Silent
   CHARACTER(LEN=MAX_NAME_LEN) :: DateStr, toutput

   CurrentModel => global%MyModel

   CALL Info('ElmerCSCParallel:Finalize','Finishing simulation...')

   CALL ElmerFinalize(runs)

   CALL envir( 'ELMERSOLVER_OUTPUT_TOTAL'//CHAR(0), toutput, tlen )
   Silent = toutput(1:1)=='0' .OR. toutput(1:5)=='false'

   IF ( .NOT. Silent ) THEN
      IF ( ParEnv % myPE == 0 ) THEN
         !WRITE( *,'(a,F12.2,F12.2)' ) 'SOLVER TOTAL TIME(CPU,REAL): ', &
         !           CPUTime()-CT, RealTime()-RT
         DateStr = FormatDate()
         WRITE( *,'(A,A)' ) 'ELMER SOLVER FINISHED AT: ', TRIM(DateStr)
      END IF
   END IF

END SUBROUTINE Finalize

! ********************************************************************

SUBROUTINE ElmerCSCParallel_LOAD_MODULE(name)

  USE TESTOBJECT

  IMPLICIT NONE

  INCLUDE "comf90.h"

  INTERFACE
    SUBROUTINE COM_set_pointer(attr,ptr,asso)
      USE TESTOBJECT
      CHARACTER(*), INTENT(IN) :: attr
      TYPE(t_global), POINTER  :: ptr
      EXTERNAL asso
    END SUBROUTINE COM_set_pointer

    SUBROUTINE InitElmerprep(global, runs, verbIn)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs, verbIn
    END SUBROUTINE InitElmerprep

    SUBROUTINE Initializer(global, runs, verbIn)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs, verbIn
    END SUBROUTINE Initializer
    
    ! ElmerFoamFSI API
    !SUBROUTINE RUN(global, runs, tFinal)
    !  USE TESTOBJECT
    !  TYPE(t_global), POINTER :: global
    !  INTEGER :: runs
    !  DOUBLE PRECISION, INTENT(IN) :: tFinal
    !END SUBROUTINE RUN

    SUBROUTINE RUN(global, currentTime, currentTimeStep, MAN_update_inbuff)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      DOUBLE PRECISION, INTENT(IN) :: currentTime
      DOUBLE PRECISION, INTENT(IN) :: currentTimeStep
      INTEGER, INTENT(IN) :: MAN_update_inbuff
    END SUBROUTINE RUN

    SUBROUTINE UpdateDisplacements(global, runs, tFinal)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs
      DOUBLE PRECISION :: tFinal
    END SUBROUTINE UpdateDisplacements

    !ElmerFoamFSI interface
    !SUBROUTINE Finalize(global, runs)
    !  USE TESTOBJECT
    !  TYPE(t_global), POINTER :: global
    !  INTEGER :: runs
    !END SUBROUTINE Finalize

    !Rocstar interface
    SUBROUTINE Finalize(global)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
    END SUBROUTINE Finalize
  END INTERFACE

  CHARACTER(*),intent(in) :: name
  INTEGER :: com_types(7)
  TYPE(t_global), POINTER :: glb
  INTEGER :: iError
  INTEGER, target :: nProc, procId

  WRITE(*,'(A)') "ElmerCSCParallel: Loading on "//TRIM(name)
  

  ALLOCATE(glb)
  glb%window_name = TRIM(name)
  glb%other_window_handle = -1
  glb%c_window_handle = -1
  CALL COM_NEW_WINDOW(TRIM(name), MPI_COMM_NULL)

  glb%ElmerComm = COM_GET_DEFAULT_COMMUNICATOR()
  CALL MPI_COMM_SIZE(glb%ElmerComm, nProc, iError)
  glb%nProc = nProc
  CALL MPI_COMM_RANK(glb%ElmerComm, procId, iError)
  glb%procId = procId

  CALL COM_new_dataitem(TRIM(name)//'.global','w',COM_F90POINTER,1,'')
  CALL COM_allocate_array(TRIM(name)//'.global')
  CALL COM_NEW_DATAITEM(TRIM(name)//'.nProc','w',COM_INTEGER,1,'')
  CALL COM_set_size(TRIM(name)//'.nProc',0,1)
  CALL COM_SET_ARRAY(name//'.nProc', 0, glb%nProc)

  com_types(1) = COM_F90POINTER
  com_types(2) = COM_INTEGER
  com_types(3) = COM_INTEGER
  CALL COM_set_member_function(TRIM(name)//'.InitElmerprep',InitElmerprep, &
                               TRIM(name)//'.global','bbi',com_types)

  com_types(1) = COM_F90POINTER
  com_types(2) = COM_DOUBLE_PRECISION
  com_types(3) = COM_MPI_COMM
  com_types(4) = COM_INTEGER
  com_types(5) = COM_STRING
  com_types(6) = COM_STRING
  com_types(7) = COM_INTEGER
  CALL COM_set_member_function(TRIM(name)//'.initialize',Initializer, &
                               TRIM(name)//'.global','biiiiii',com_types)

  com_types(3) = COM_DOUBLE_PRECISION
 
  CALL COM_set_member_function(TRIM(name)//'.update_solution',RUN, &
                               TRIM(name)//'.global','biii',com_types)

  CALL COM_set_member_function(TRIM(name)//'.finalize',Finalize, &
                               TRIM(name)//'.global','b',com_types)

  CALL COM_WINDOW_INIT_DONE(name)

  CALL COM_set_pointer(name//'.global',glb,associate_pointer )

END SUBROUTINE ElmerCSCParallel_LOAD_MODULE


SUBROUTINE ElmerCSCParallel_UNLOAD_MODULE(name)
  USE TESTOBJECT
  IMPLICIT NONE
  INCLUDE "comf90.h"
  INTERFACE 
    SUBROUTINE COM_get_pointer(attr,ptr,asso)
      USE TESTOBJECT
      CHARACTER(*), INTENT(IN) :: attr
      TYPE(t_global), POINTER :: ptr
      EXTERNAL asso
    END SUBROUTINE COM_get_pointer
  END INTERFACE
  character(*),intent(in) :: name
  TYPE(t_global), POINTER :: glb
  INTEGER :: window_handle,other_window_handle,c_window_handle, owlen

  WRITE(*,'(A)') "Unloading ElmerCSCParallel: "//TRIM(name)
  NULLIFY(glb)
  window_handle = COM_GET_WINDOW_HANDLE(TRIM(name))
  if(window_handle .gt. 0) then
     CALL COM_get_pointer(TRIM(name)//'.global',glb,associate_pointer)
     IF(ASSOCIATED(glb).eqv..true.) THEN       
        WRITE(*,'(A)') 'Fortran module '//TRIM(glb%window_name)//' unloading name '//TRIM(name)
        if(glb%other_window_handle .gt. 0) then
           WRITE(*,*) 'Fortran module '//TRIM(glb%window_name)//&
                ' unloading external Fortran module '//TRIM(glb%other_window_name)//'.'
           other_window_handle = COM_GET_WINDOW_HANDLE(TRIM(glb%other_window_name))
           IF(other_window_handle .gt. 0) THEN
              CALL COM_UNLOAD_MODULE("ElmerCSCParallel",TRIM(glb%other_window_name))
           ENDIF
        endif
        if(glb%c_window_handle .gt. 0) then
           WRITE(*,*) 'Fortran module '//TRIM(glb%window_name)//&
                ' unloading external C module '//TRIM(glb%c_window_name)//'.'
           c_window_handle = COM_GET_WINDOW_HANDLE(TRIM(glb%c_window_name))
           IF(c_window_handle .gt. 0) THEN
              CALL COM_UNLOAD_MODULE("ElmerCSCParallel",TRIM(glb%c_window_name))
           ENDIF
        endif
!        DEALLOCATE(glb)
     ENDIF
     CALL COM_DELETE_WINDOW(TRIM(name))
  endif
END SUBROUTINE ElmerCSCParallel_UNLOAD_MODULE

! ******************************************************************************

!> \}
