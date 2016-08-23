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

SUBROUTINE Initializer(global, runs, verbIn)
   USE TESTOBJECT
   USE Types
   USE GeneralUtils
   USE ParallelUtils
   USE TimeModule

   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs, counter, nindex, verbIn

   REAL(KIND=dp) :: CT, RT
!   INTEGER, PARAMETER :: Init=0
   CHARACTER(LEN=MAX_NAME_LEN) :: DateStr, toutput
   INTEGER, POINTER :: TempNodeList(:)
   LOGICAL :: NodePresent
   INTEGER :: LastFSIElement
!   INTEGER, POINTER :: Conn2(:)
#if defined(MINGW32)
   CALL SET_STDIO_BUFS()
#endif

   WRITE(6,*) 'ElmerCSC:Initializer: Starting ...'

   !Allocate space for storing the verbosity
   ALLOCATE(global%verbosity(1))
   !Initialize verbosity to 1
   global%verbosity = verbIn

   WRITE(*,*) 'verbosity = ', global%verbosity

   CALL envir( 'ELMERSOLVER_OUTPUT_TOTAL'//CHAR(0), toutput, tlen )
   Silent = toutput(1:1)=='0' .OR. toutput(1:5)=='false'

   CT = CPUtime()
   RT = RealTime()

   IF ( .NOT. Silent ) THEN
     DateStr = FormatDate()
     WRITE( *,'(A,A)' ) "ELMER SOLVER (v " // VERSION // ") STARTED AT: ", TRIM(DateStr)
     CALL FLUSH(6)
   END IF

   IF( MyVerbosity > 3) WRITE( *, * ) 'Calling ElmerInitialize'

   CALL ElmerInitialize(runs)

   global%MyModel => CurrentModel

   !Assigning stuff & printing out to check
   global%SolverId =  -1
   DO i=1, CurrentModel % NumberOfSolvers
      Solver => global % MyModel % Solvers(i)
      SolverParams => GetSolverParams(Solver)
      Equation = GetString(SolverParams, 'Equation', GotIt)
      WRITE(*,*) 'Equation = ',Equation
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
   t = MyMesh % NumberOfBulkElements
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

   !Allocate space for storing the NodeDisplacements
   !This location will be registered with IMPACT
   IF( MyVerbosity > 3) WRITE(*,*) 'global%nNodes =', global%nNodes
   ALLOCATE(global%NodeDisplacements(3*global%nNodes))
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
   !Allocate space for storing the FacePressures
   !This location will be registered with IMPACT
   ALLOCATE(global%FacePressures(global%nElem))
   !Allocate space for storing the NodePressures
   !This location will be registered with IMPACT
   ALLOCATE(global%NodePressures(global%nNodes))
   !Allocate space for storing the NodeLoads
   !This location will be registered with IMPACT
   ALLOCATE(global%NodeLoads(3*global%nNodes))
   !Allocate space for storing the previous loads
   !so linear interpolation can be done
   ALLOCATE(global%PreviousLoads(3,global%nNodes))
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

      !WRITE(*,*) "ElmerCSC element = ", t

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
            !Masoud: Marking LoadLessNodes
            IF (ABS(ElementNodes % x(nt) - 0.6) < 10.0*EPSILON(1.0)) THEN
               !global%IsLoadFreeNode(nindex-1) = .TRUE.
            END IF
            !Masoud End
         END DO
      END IF
   END DO

   !Masoud : printing IsLoadFreeNode
   !WRITE(*,*) 'global%IsLoadFreeNode = ', global%IsLoadFreeNode
   !Masoud End

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
   CALL COM_SET_SIZE(TRIM(global%window_name)//'.nc',11,global%nNodes)
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.nc',11,global%Coords,3)
   CALL COM_SET_SIZE(TRIM(global%window_name)//'.'//TRIM(global%MeshType),11,&
                     global%nElem)
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.'//TRIM(global%MeshType),11,&
                      global%Conn, global%nConn) 

   !CALL COM_SET_SIZE('Window1.:b2:real', 11,global%nElem)
   !CALL COM_RESIZE_ARRAY('Window1.:b2:real',11)
   !CALL COM_RESIZE_ARRAY(TRIM(global%window_name)//'.'//TRIM(global%MeshType),11)
   !CALL COM_SET_ARRAY('Window1.:b2:real',11,Conn,2)

   !Set the displacments array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.Displacements', 'n', COM_DOUBLE_PRECISION, 3, 'm')
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.Displacements',11,&
                      global%NodeDisplacements,3)
   !Masoud
   ! n: means node quantity, 3: number of columns, 11: window number (currently
   ! default used everywhere), 'm': is units of the quanitity
   CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.PreviousDisplacements', 'n', COM_DOUBLE_PRECISION, 3, 'm')
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.PreviousDisplacements',11,&
                      global%PreviousNodeDisplacements,3)
   !Masoud End

   !Set the loads array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.Loads', 'n', COM_DOUBLE_PRECISION, 3, '')
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.Loads',11,&
                      global%NodeLoads,3)

   !Set the loads array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.NodePressures',&
                         'n', COM_DOUBLE_PRECISION, 1, '')
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.NodePressures',11,&
                      global%NodePressures,1)

   !Set the pressures array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.Pressures', 'e',&
                         COM_DOUBLE_PRECISION, 1, '')
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.Pressures',11,&
                      global%FacePressures,1)

   !Set the face loads array with COM now that the mesh is registered
   CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.FaceLoads', 'e',&
                         COM_DOUBLE_PRECISION, 3, '')
   CALL COM_SET_ARRAY(TRIM(global%window_name)//'.FaceLoads',11,&
                      global%FaceLoads,3)

   !Set the verbosity with COM 
   !CALL COM_NEW_DATAITEM(TRIM(global%window_name)//'.verbosity',&
   !                      'w', COM_INTEGER, 1, '')
   !CALL COM_SET_ARRAY(TRIM(global%window_name)//'.verbosity',11,&
   !                   global%verbosity,1)

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


   WRITE(6,*) 'ElmerCSC:Initializer: Done.....'

   NULLIFY(TempNodeList)
 
END SUBROUTINE Initializer

! *****************************************************************************

SUBROUTINE RUN(global, runs, tFinal)
  
   USE TESTOBJECT
   USE GeneralModule
   USE TimeModule

   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs
   DOUBLE PRECISION, INTENT(IN) :: tFinal
   DOUBLE PRECISION :: standardTimestep
   DOUBLE PRECISION :: var1, var2, var3
   DOUBLE PRECISION :: deltaTime
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

   CurrentModel => global%MyModel   

   FinalTime = tFinal  

   IF (PreviousTime >= FinalTime) THEN
     CALL Fatal( ' ', 'Next time step passed to RUN is greater than &
                  previous time step!')
   END IF

   IF (MyVerbosity > 3) WRITE(*,*) 'In RUN function'
     
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
     !Printing out the Pressures as a check
     WRITE(*,*) 'Pressures '
     DO t = 1, global%nElem
       WRITE(*,*) global%FacePressures(t)
     END DO
     !Printing out the FaceLoads as a check
     WRITE(*,*) 'FaceLoads '
     DO t = 1, global%nElem
       WRITE(*,*) global%FaceLoads(3*(t-1) + 1), global%FaceLoads(3*(t-1) + 2),&
                  global%FaceLoads(3*(t-1) + 3)
     END DO
     !Print out the NodePressures as a check
     WRITE(*,*) 'NodePressures '
     DO t = 1, global%nNodes
       WRITE(*,*) global%NodePressures(t)
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
      IF (MyVerbosity > 3) WRITE(*,*) 'Calling UpdateDisplacements'
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

   WRITE(*,*) 'ElmerCSC:Run: runs = ',runs
   WRITE(*,*) 'ElmerCSC:Run: CurrentModel%GetTestLoads = ',&
              CurrentModel%GetTestLoads
   WRITE(*,*) 'ElmerCSC:Run: CurrentModel%UDFUsed = ',&
              CurrentModel%UDFUsed
   IF (runs == 1 .AND. CurrentModel%GetTestLoads .eqv. .TRUE. & 
      .AND. CurrentModel%UDFUsed .eqv. .TRUE.) THEN
     runs = 2
   END IF

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

   WRITE(*,*) 'ElmerCSC:UpdateDisplacements:',&
              ' Reporting displacements to the fluid solver'

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
               
               ! Masoud : Avoding displacement update duplicates and
               !          implementing a correct displacement update

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
                     ! Original
                     !global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)
                     !global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)
                     !global%NodeDisplacements(3*(nCount-1) + 3) = Displacement(nk+3)
                     ! Original: End

                     ! Masoud
                     !Printing old and new displacements for the user
                     !WRITE(*,*) '----------------------'
                     !WRITE(*,*) ' new disps= ', Displacement(nk+1),&
                     !           Displacement(nk+2), Displacement(nk+3)
                     !WRITE(*,*) ' old disps= ',&
                     !global%PreviousNodeDisplacements(3*(nCount-1) + 1),&
                     !global%PreviousNodeDisplacements(3*(nCount-1) + 2), &
                     !global%PreviousNodeDisplacements(3*(nCount-1) + 3)
                     !Calculating displacement differences to pass to fluid solver
                     global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)&
                     - global%PreviousNodeDisplacements(3*(nCount-1) + 1)
                     global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)&
                     - global%PreviousNodeDisplacements(3*(nCount-1) + 2)
                     global%NodeDisplacements(3*(nCount-1) + 3) = Displacement(nk+3)&
                     - global%PreviousNodeDisplacements(3*(nCount-1) + 3)
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
               ! Masoud:  End

               ! Original: uses a funny update order with dubplicates
               !!IF( MyVerbosity > 3) THEN
               !  WRITE(6,*) 'Updating NodeDisplacement(',nCount,')'
               !!END IF

               !IF ( StressSol % DOFs == 1 ) THEN
               !   IF( MyVerbosity > 3) WRITE(*,*) Displacement(nk+1), 0.0, 0.0
               !   global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)
               !   global%NodeDisplacements(3*(nCount-1) + 2) = 0.0d0
               !   global%NodeDisplacements(3*(nCount-1) + 3) = 0.0d0
               !ELSE IF ( StressSol % DOFs == 2 ) THEN
               !   IF( MyVerbosity > 3) WRITE(*,*) Displacement(nk+1), Displacement(nk+2)
               !   global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)
               !   global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)
               !   global%NodeDisplacements(3*(nCount-1) + 3) = 0.0d0
               !ELSE IF ( StressSol % DOFs == 3 ) THEN
               !   ! Masoud
               !   !WRITE(*,*) '----------------------'
               !   WRITE(*,*) ' new = ', Displacement(nk+1),&
               !              Displacement(nk+2), Displacement(nk+3)
               !   WRITE(*,*) ' old = ', global%NodeDisplacements(3*(nCount-1) + 1),&
               !   global%NodeDisplacements(3*(nCount-1) + 2), &
               !   global%NodeDisplacements(3*(nCount-1) + 3)
               !   ! Masoud : end 

               !   ! Original
               !   global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1)
               !   global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2)
               !   global%NodeDisplacements(3*(nCount-1) + 3) = Displacement(nk+3)
               !   ! Original: End

               !   !WRITE(*,*) '----------------------'
               !   !global%NodeDisplacements(3*(nCount-1) + 1) = Displacement(nk+1) - global%NodeDisplacements(3*(nCount-1) + 1)
               !   !global%NodeDisplacements(3*(nCount-1) + 2) = Displacement(nk+2) - global%NodeDisplacements(3*(nCount-1) + 2)
               !   !global%NodeDisplacements(3*(nCount-1) + 3) = Displacement(nk+3) - global%NodeDisplacements(3*(nCount-1) + 3)
               !   ! Masoud : End
               !ELSE
               !   WRITE(*,*) 'StressSol % DOFs = ', StressSol % DOFs
               !   WRITE(*,*) 'DOFs are assumed to be <= 3'
               !   CALL Fatal( ' ', 'StressSol DOFs are greater than 3 ' )
               !END IF
               ! Original : End
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

SUBROUTINE Finalize(global, runs)
   USE TESTOBJECT
   USE Types
   USE GeneralUtils
   USE ParallelUtils
   USE GeneralModule, only : FirstTime
 
   IMPLICIT NONE

   INCLUDE 'comf90.h'

   TYPE(t_global), POINTER :: global
   INTEGER :: runs

   !INTEGER, PARAMETER :: Initialize=0
   INTEGER :: tlen
   LOGICAL :: Silent
   CHARACTER(LEN=MAX_NAME_LEN) :: DateStr, toutput

   CurrentModel => global%MyModel

   WRITE(*,*) 'ElmerCSC:Finalize: Finishing simulation'

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

   WRITE(6,*) 'ElmerCSC:Finalize: End Finalize. FirstTime = ',FirstTime 

END SUBROUTINE Finalize

! ********************************************************************

SUBROUTINE ElmerCSC_LOAD_MODULE(name)

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

    SUBROUTINE Initializer(global, runs, verbIn)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs, verbIn
    END SUBROUTINE Initializer

    SUBROUTINE RUN(global, runs, tFinal)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs
      DOUBLE PRECISION, INTENT(IN) :: tFinal
    END SUBROUTINE RUN

    SUBROUTINE UpdateDisplacements(global, runs, tFinal)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs
      DOUBLE PRECISION :: tFinal
    END SUBROUTINE UpdateDisplacements

    SUBROUTINE Finalize(global, runs)
      USE TESTOBJECT
      TYPE(t_global), POINTER :: global
      INTEGER :: runs
    END SUBROUTINE Finalize

  END INTERFACE

  CHARACTER(*),intent(in) :: name
  INTEGER :: com_types(7)
  TYPE(t_global), POINTER :: glb
  

  WRITE(*,'(A)') "Loading ElmerCSC: "//TRIM(name)
  

  ALLOCATE(glb)
  glb%window_name = TRIM(name)
  glb%other_window_handle = -1
  glb%c_window_handle = -1
  CALL COM_NEW_WINDOW(TRIM(name))

  CALL COM_new_dataitem(TRIM(name)//'.global','w',COM_F90POINTER,1,'')
  CALL COM_allocate_array(TRIM(name)//'.global')

  com_types(1) = COM_F90POINTER
  com_types(2) = COM_INTEGER
  com_types(3) = COM_INTEGER
  
  CALL COM_set_member_function(TRIM(name)//'.Initialize',Initializer, &
                               TRIM(name)//'.global','bbi',com_types)

  com_types(3) = COM_DOUBLE_PRECISION
 
  CALL COM_set_member_function(TRIM(name)//'.Run',RUN, &
                               TRIM(name)//'.global','bbi',com_types)

  CALL COM_set_member_function(TRIM(name)//'.Finalize',Finalize, &
                               TRIM(name)//'.global','bb',com_types)

  CALL COM_WINDOW_INIT_DONE(name)

  CALL COM_set_pointer(name//'.global',glb,associate_pointer )

END SUBROUTINE ElmerCSC_LOAD_MODULE


SUBROUTINE ElmerCSC_UNLOAD_MODULE(name)
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

  WRITE(*,'(A)') "Unloading ElmerCSC: "//TRIM(name)
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
              CALL COM_UNLOAD_MODULE("ElmerCSC",TRIM(glb%other_window_name))
           ENDIF
        endif
        if(glb%c_window_handle .gt. 0) then
           WRITE(*,*) 'Fortran module '//TRIM(glb%window_name)//&
                ' unloading external C module '//TRIM(glb%c_window_name)//'.'
           c_window_handle = COM_GET_WINDOW_HANDLE(TRIM(glb%c_window_name))
           IF(c_window_handle .gt. 0) THEN
              CALL COM_UNLOAD_MODULE("ElmerCSC",TRIM(glb%c_window_name))
           ENDIF
        endif
!        DEALLOCATE(glb)
     ENDIF
     CALL COM_DELETE_WINDOW(TRIM(name))
  endif
END SUBROUTINE ElmerCSC_UNLOAD_MODULE

! ******************************************************************************

!> \}
