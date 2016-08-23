!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! * 
! *  This library is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU Lesser General Public
! *  License as published by the Free Software Foundation; either
! *  version 2.1 of the License, or (at your option) any later version.
! *
! *  This library is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! *  Lesser General Public License for more details.
! * 
! *  You should have received a copy of the GNU Lesser General Public
! *  License along with this library (in file ../LGPL-2.1); if not, write 
! *  to the Free Software Foundation, Inc., 51 Franklin Street, 
! *  Fifth Floor, Boston, MA  02110-1301  USA
! *
! *****************************************************************************/
!
!/******************************************************************************
! *
! *  ELMER/FEM Solver main program
! *
! ******************************************************************************
! *
! *  Authors: Juha Ruokolainen
! *  Email:   Juha.Ruokolainen@csc.fi
! *  Web:     http://www.csc.fi/elmer
! *  Address: CSC - IT Center for Science Ltd.
! *           Keilaranta 14
! *           02101 Espoo, Finland 
! *
! *  Original Date: 02 Jun 1997
! *
! *****************************************************************************/

!> \defgroup Solvers Dynamically linked solvers

!> \defgroup UDF Dynamically linked functions

!> \defgroup Programs Utility programs

!> \degroup ElmerLib Elmer library routines

!> \ingroup ElmerLib
!> \{

!------------------------------------------------------------------------------
!> The main program for Elmer. Solves the equations as defined by the input files.
!------------------------------------------------------------------------------
   SUBROUTINE ElmerInitialize(runs)
!------------------------------------------------------------------------------
     USE TESTOBJECT
     USE GeneralModule
!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------
     
!      INTEGER :: Initialize

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------

!     INTEGER :: i,j,k,n,l,t,k1,k2,iter,Ndeg,istat,nproc,tlen,nthreads
     CHARACTER(LEN=MAX_STRING_LEN) :: CoordTransform
!     CHARACTER(LEN=MAX_STRING_LEN) :: threads
!
!     REAL(KIND=dp) :: s,dt,dtfunc
!     REAL(KIND=dP), POINTER :: WorkA(:,:,:) => NULL()
!     REAL(KIND=dp), POINTER, SAVE :: sTime(:), sStep(:), sInterval(:), sSize(:), &
!           steadyIt(:),nonlinIt(:),sPrevSizes(:,:),sPeriodic(:)
!
!     TYPE(Element_t),POINTER :: CurrentElement
!
!     LOGICAL :: GotIt,Transient,Scanning,LastSaved
!
!     INTEGER :: TimeIntervals,interval,timestep, &
!       TotalTimesteps,SavedSteps,CoupledMaxIter,CoupledMinIter
!
!     INTEGER, POINTER, SAVE :: Timesteps(:),OutputIntervals(:), ActiveSolvers(:)
!     REAL(KIND=dp), POINTER, SAVE :: TimestepSizes(:,:)
!
!     INTEGER(KIND=AddrInt) :: ControlProcedure
!
!     LOGICAL :: InitDirichlet, ExecThis
!
!     TYPE(ElementType_t),POINTER :: elmt
!
!     TYPE(ParEnv_t), POINTER :: ParallelEnv
!
!     CHARACTER(LEN=MAX_NAME_LEN) :: ModelName, eq, ExecCommand
!     CHARACTER(LEN=MAX_STRING_LEN) :: OutputFile, PostFile, RestartFile, &
!                OutputName=' ',PostName=' ', When, OptionString
!
!     TYPE(Variable_t), POINTER :: Var
!     TYPE(Mesh_t), POINTER :: Mesh
!     TYPE(Solver_t), POINTER :: Solver
!
!     REAL(KIND=dp) :: RealTime,CPUTime
!
!     LOGICAL :: FirstLoad = .TRUE., FirstTime=.TRUE., Found
!     LOGICAL :: Silent, Version, GotModelName
!
     INTEGER :: iargc
!
!     INTEGER :: ExtrudeLevels
!     TYPE(Mesh_t), POINTER :: ExtrudedMesh
!
!     INTEGER :: omp_get_max_threads
!
!#ifdef HAVE_TRILINOS
!INTERFACE
!      SUBROUTINE TrilinosCleanup() BIND(C,name='TrilinosCleanup')
!      IMPLICIT NONE
!      END SUBROUTINE TrilinosCleanup
!END INTERFACE
!#endif

     INTEGER :: runs


     FirstLoad = .TRUE.
     FirstTime=.TRUE.
     Initialize = 0

     ! Start the watches, store later
     !--------------------------------
     RT0 = RealTime()
     CT0 = CPUTime()


     ! If parallel execution requested, initialize parallel environment:
     !------------------------------------------------------------------
     ParallelEnv => ParallelInit()
     OutputPE = ParEnv % MyPE

!tt = realtime()
     IF ( FirstTime ) THEN
       !
       ! Print banner to output:
#include "../config.h"
       ! -----------------------
       NoArgs = IARGC()
       ! Info Level is always true until the model has been read!
       ! This makes it possible to cast something 
       Silent = .FALSE.
       Version = .FALSE.
       IF( NoArgs > 0 ) THEN 
         DO i = 1, NoArgs 
           CALL getarg( i,OptionString )
           Silent = Silent .OR. &
               ( OptionString=='-s' .OR. OptionString=='--silent' ) 
           Version = Version .OR. &
               ( OptionString=='-v' .OR. OptionString == '--version' )
         END DO
       END IF

       ! Set number of OpenMP threads
       nthreads = 1
       !$ nthreads = omp_get_max_threads()
       IF (nthreads > 1) THEN
         ! Check if OMP_NUM_THREADS environment variable is set
         CALL envir( 'OMP_NUM_THREADS'//CHAR(0), threads, tlen )
         IF (tlen==0) THEN
           CALL Warn('MAIN','OMP_NUM_THREADS not set. Using only 1 thread.')
           nthreads = 1
           ! Set number of threads to 1
           !$ CALL omp_set_num_threads(nthreads)
#ifdef HAVE_MKL
           CALL mkl_set_num_threads(nthreads)
#endif
         END IF
       END IF


       IF( .NOT. Silent ) THEN
         CALL Info( 'MAIN', ' ')
         CALL Info( 'MAIN', '=============================================================')
         CALL Info( 'MAIN', 'MyElmerSolver finite element software, Welcome!                ')
         CALL Info( 'MAIN', 'This program is free software licensed under (L)GPL          ')
         CALL Info( 'MAIN', 'Copyright 1st April 1995 - , CSC - IT Center for Science Ltd.')
         CALL Info( 'MAIN', 'Webpage http://www.csc.fi/elmer, Email elmeradm@csc.fi       ')
         CALL Info( 'MAIN', 'Library version: ' // VERSION &
#ifdef REVISION
             // ' (Rev: ' // REVISION // ')' )
#else
         )
#endif
         IF ( ParEnv % PEs > 1 ) &
             CALL Info( 'MAIN', ' Running in parallel using ' // &
             TRIM(i2s(ParEnv % PEs)) // ' tasks.')

         ! Print out number of threads in use
         IF ( nthreads > 1 ) &
             CALL Info('MAIN', ' Running in parallel with ' // &
                       TRIM(i2s(nthreads)) // ' threads per task.')

#ifdef HAVE_HYPRE
         CALL Info( 'MAIN', ' HYPRE library linked in.')
#endif
#ifdef HAVE_TRILINOS
         CALL Info( 'MAIN', ' Trilinos library linked in.')
#endif
#ifdef HAVE_MUMPS
         CALL Info( 'MAIN', ' MUMPS library linked in.')
#endif
#ifdef HAVE_CHOLMOD
         CALL Info( 'MAIN', ' CHOLMOD library linked in.')
#endif
#ifdef HAVE_SUPERLU
         CALL Info( 'MAIN', ' SUPERLU library linked in.')
#endif
#ifdef HAVE_PARDISO
         CALL Info( 'MAIN', ' PARDISO library linked in.')
#endif
#ifdef HAVE_MKL
         CALL Info( 'MAIN', ' Intel MKL linked in.' )
#endif
         CALL Info( 'MAIN', '=============================================================')
       END IF

       IF( Version ) RETURN
       
       CALL InitializeElementDescriptions
       
       WRITE(*,*) "Initialize: Setting FirstTime to FALSE"
       FirstTime = .FALSE.
     END IF

     ! Read input file name either as an argument, or from the default file:
     !----------------------------------------------------------------------
     GotModelName = .FALSE.
     IF ( ParEnv % PEs <= 1 .AND. NoArgs > 0 ) THEN
       CALL getarg( 1,ModelName )
       IF( ModelName(1:1) /= '-') THEN 
         GotModelName = .TRUE.
         IF ( NoArgs > 1 ) CALL getarg( 2,eq )
       END IF
     END IF
         
     IF( .NOT. GotModelName ) THEN
       OPEN( 1, File='ELMERSOLVER_STARTINFO', STATUS='OLD', ERR=10 )
       READ(1,'(a)') ModelName
       CLOSE(1)
     END IF

!------------------------------------------------------------------------------
!    Read element definition file, and initialize element types
!------------------------------------------------------------------------------
     IF ( Initialize==1 ) THEN
       CALL FreeModel(CurrentModel)
       FirstLoad=.TRUE.
     END IF


!------------------------------------------------------------------------------
!    Read Model and mesh from Elmer mesh data base
!------------------------------------------------------------------------------
!     DO WHILE( .TRUE. )

       IF ( initialize==2 ) GOTO 1

       IF( MyVerbosity > 3) WRITE(*,*) 'FirstLoad = ', FirstLoad

       IF ( FirstLoad ) THEN
         IF( .NOT. Silent ) THEN
           CALL Info( 'MAIN', ' ')
           CALL Info( 'MAIN', ' ')
           CALL Info( 'MAIN', '-------------------------------------')
           CALL Info( 'MAIN', 'Reading Model: '//TRIM( ModelName) )
         END IF

         INQUIRE(Unit=InFileUnit, Opened=GotIt)
         IF ( gotIt ) CLOSE(inFileUnit)
     
         IF( MyVerbosity > 3) WRITE(*,*) 'ModelName = ', ModelName
    
         OPEN( Unit=InFileUnit, Action='Read',File=ModelName,Status='OLD',ERR=20 )
         IF( MyVerbosity > 3) WRITE(*,*) 'Before LoadModel call'
         CurrentModel => LoadModel(ModelName,.FALSE.,ParEnv % PEs,ParEnv % MyPE )
         IF( MyVerbosity > 3) WRITE(*,*) 'After LoadModel call'

         ! Optionally perform simple extrusion to increase the dimension of the mesh
         !----------------------------------------------------------------------------------
         ExtrudeLevels=GetInteger(CurrentModel % Simulation,'Extruded Mesh Levels',Found)
         IF(ExtrudeLevels>1) THEN
           ExtrudedMesh => MeshExtrude(CurrentModel % Meshes, ExtrudeLevels-2)
           DO i=1,CurrentModel % NumberOfSolvers
             IF(ASSOCIATED(CurrentModel % Solvers(i) % Mesh,CurrentModel % Meshes)) &
               CurrentModel % Solvers(i) % Mesh => ExtrudedMesh 
           END DO
           ExtrudedMesh % Next => CurrentModel % Meshes % Next
           CurrentModel % Meshes => ExtrudedMesh

           ! If periodic BC given, compute boundary mesh projector:
           ! ------------------------------------------------------
           DO i = 1,CurrentModel % NumberOfBCs
             IF(ASSOCIATED(CurrentModel % Bcs(i) % PMatrix)) &
               CALL FreeMatrix( CurrentModel % BCs(i) % PMatrix )
             CurrentModel % BCs(i) % PMatrix => NULL()
             k = ListGetInteger( CurrentModel % BCs(i) % Values, 'Periodic BC', GotIt )
             IF( GotIt ) THEN
               CurrentModel % BCs(i) % PMatrix =>  PeriodicProjector( CurrentModel, ExtrudedMesh, i, k )
             END IF
           END DO
         END IF
         
         ! If requested perform coordinate transformation directly after is has been obtained.
         ! Don't maintain the original mesh. 
         !----------------------------------------------------------------------------------
         CoordTransform = ListGetString(CurrentModel % Simulation,'Coordinate Transformation',GotIt)
         IF( GotIt ) THEN
           ! Masoud
           WRITE(*,*) 'Initialize: Coordinate Transformation is activated for Elmer.\n'
           ! Masoud : End
           CALL CoordinateTransformation( CurrentModel % Meshes, CoordTransform, &
               CurrentModel % Simulation, .TRUE. )
         END IF
         ! IF( ListCheckPresent(CurrentModel % Simulation,'Coordinate Transformation') ) THEN
         !   CALL CoordinateTransformation( CurrentModel % Meshes, .TRUE. )
         ! END IF

         IF(.NOT. Silent ) THEN
            CALL Info( 'MAIN', '-------------------------------------')
          END IF 
       ELSE
         IF ( Initialize==3 ) THEN
           INQUIRE(Unit=InFileUnit, Opened=GotIt)
           IF ( gotIt ) CLOSE(inFileUnit)
           OPEN( Unit=InFileUnit, Action='Read', & 
                    File=ModelName,Status='OLD',ERR=20 )
         END IF

         IF (MyVerbosity > 3) THEN
            WRITE(6,*) 'Initialize: CurrentModel % NumberOfNodes&
            = ', CurrentModel % NumberOfNodes
         END IF

!         IF ( .NOT.ReloadInputFile(CurrentModel) ) EXIT

         Mesh => CurrentModel % Meshes
         DO WHILE( ASSOCIATED(Mesh) )
           Mesh % SavesDone = 0
           Mesh => Mesh % Next
         END DO
       END IF

1      CONTINUE

       CALL ListAddLogical( CurrentModel % Simulation, &
             'Initialization Phase', .TRUE. )

       ! Save the start time to the list so that it may be retrieved when necessary
       ! This could perhaps also be a global variable etc, but this will do for now.
       !-------------------------------------------------------------------------
       IF( ListGetLogical( CurrentModel % Simulation,'Simulation Timing',GotIt) ) THEN
         CALL ListAddConstReal( CurrentModel % Simulation,'cputime0',ct0 )
         CALL ListAddConstReal( CurrentModel % Simulation,'realtime0',rt0 )
       END IF
#if 0
!------------------------------------------------------------------------------
!      Check for transient case
!------------------------------------------------------------------------------
       eq = ListGetString( CurrentModel % Simulation, 'Simulation Type', GotIt )
       Scanning  = eq == 'scanning'
       Transient = eq == 'transient'


!------------------------------------------------------------------------------
!      To more conveniently support the use of VTK based visualization there 
!      is a hack that recognizes VTU suffix and creates a instance of output solver.
!      Note that this is really quite a dirty hack, and is not a good example.
!-----------------------------------------------------------------------------
       CALL AddVtuOutputSolverHack()

!------------------------------------------------------------------------------
!      Figure out what (flow,heat,stress,...) should be computed, and get
!      memory for the dofs
!------------------------------------------------------------------------------
       CALL AddSolvers()

!------------------------------------------------------------------------------
!      Time integration and/or steady state steps
!------------------------------------------------------------------------------
       IF ( Transient .OR. Scanning ) THEN
         Timesteps => ListGetIntegerArray( CurrentModel % Simulation, &
                       'Timestep Intervals', GotIt )

         IF ( .NOT.GotIt ) THEN
           CALL Fatal( ' ', 'Keyword > Timestep Intervals < MUST be ' //  &
                   'defined for transient and scanning simulations' )
         END IF 
         TimestepSizes => ListGetConstRealArray( CurrentModel % Simulation, &
                               'Timestep Sizes', GotIt )

         IF ( .NOT.GotIt ) THEN
           IF( Scanning .OR. ListCheckPresent( CurrentModel % Simulation,'Timestep Size') ) THEN
             ALLOCATE(TimestepSizes(SIZE(Timesteps),1))
             TimestepSizes = 1.0_dp
           ELSE
             CALL Fatal( ' ', 'Keyword [Timestep Sizes] MUST be ' //  &
                 'defined for time dependent simulations' )
             STOP
           END IF
         END IF 

         TimeIntervals = SIZE(Timesteps)

         CoupledMaxIter = ListGetInteger( CurrentModel % Simulation, &
               'Steady State Max Iterations', GotIt, minv=1 )
         IF ( .NOT. GotIt ) CoupledMaxIter = 1
!------------------------------------------------------------------------------
       ELSE
!------------------------------------------------------------------------------
!        Steady state
!------------------------------------------------------------------------------
         ALLOCATE(Timesteps(1))

         Timesteps(1) = ListGetInteger( CurrentModel % Simulation, &
               'Steady State Max Iterations', GotIt,minv=1 )
         IF ( .NOT. GotIt ) Timesteps(1)=1
  
         ALLOCATE(TimestepSizes(1,1))
         TimestepSizes(1,1) = 1.0D0

         CoupledMaxIter = 1
         TimeIntervals  = 1
       END IF
       WRITE (*,*) " ***TimestepSizes = ", TimestepSizes(1,1)

       IF ( FirstLoad ) &
         ALLOCATE( sTime(1), sStep(1), sInterval(1), sSize(1), &
             steadyIt(1), nonLinit(1), sPrevSizes(1,5), sPeriodic(1) )

       dt   = 0._dp

       sTime = 0._dp
       sStep = 0
       sPeriodic = 0._dp

       sSize = dt
       sPrevSizes = 0_dp

       sInterval = 0._dp

       steadyIt = 0
       nonlinIt = 0

       CoupledMinIter = ListGetInteger( CurrentModel % Simulation, &
                  'Steady State Min Iterations', GotIt )

!------------------------------------------------------------------------------
!      Add coordinates and simulation time to list of variables so that
!      coordinate dependent parameter computing routines can ask for
!      them...
!------------------------------------------------------------------------------
       IF ( FirstLoad ) CALL AddMeshCoordinatesAndTime

!------------------------------------------------------------------------------
!      Get Output File Options
!------------------------------------------------------------------------------

       OutputIntervals => ListGetIntegerArray( CurrentModel % Simulation, &
                       'Output Intervals', GotIt )
       IF ( .NOT. GotIt ) THEN
         ALLOCATE( OutputIntervals(SIZE(TimeSteps)) )
         OutputIntervals = 1
       END IF


       ! Initial Conditions:
       ! -------------------
       IF ( FirstLoad ) CALL SetInitialConditions


       ! Compute the total number of steps that will be saved to the files
       ! Particularly look if the last step will be saved, or if it has
       ! to be saved separately.
       !------------------------------------------------------------------
       TotalTimesteps = 0
       LastSaved = .TRUE.
       DO interval=1,TimeIntervals
         DO timestep = 1,Timesteps(interval)
           IF( OutputIntervals(Interval) == 0 ) CYCLE
           LastSaved = .FALSE.
           IF ( MOD(Timestep-1, OutputIntervals(Interval))==0 ) THEN
              LastSaved = .TRUE.
              TotalTimesteps = TotalTimesteps + 1
           END IF
         END DO
       END DO

       DO i=1,CurrentModel % NumberOfSolvers
          Solver => CurrentModel % Solvers(i)
          IF(.NOT.ASSOCIATED(Solver % Variable)) CYCLE
          IF(.NOT.ASSOCIATED(Solver % Variable % Values)) CYCLE
          When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
          IF ( GotIt ) THEN
             IF ( When == 'after simulation' .OR. When == 'after all' ) THEN
                LastSaved = .FALSE.
             END IF
          ELSE
           IF ( Solver % SolverExecWhen == SOLVER_EXEC_AFTER_ALL ) THEN
              LastSaved = .FALSE.
           END IF
          END IF
       END DO

       IF ( .NOT.LastSaved ) TotalTimesteps = TotalTimesteps + 1
       IF( TotalTimesteps == 0 ) TotalTimesteps = 1

       CALL ListAddLogical( CurrentModel % Simulation,  &
            'Initialization Phase', .FALSE. )

       FirstLoad = .FALSE.
       IF (MyVerbosity > 3) THEN
         WRITE(6,*) 'MyElmerSolver 493 Initialize = ', Initialize     
       END IF
!       IF ( Initialize == 1 ) EXIT
#endif

!     END DO

     IF (MyVerbosity > 3) THEN
       WRITE(6,*) 'MyRun: CurrentModel % NumberOfNodes = ', &
                  CurrentModel % NumberOfNodes
     END IF

!------------------------------------------------------------------------------
!      Check for transient case
!------------------------------------------------------------------------------
       eq = ListGetString( CurrentModel % Simulation, 'Simulation Type', GotIt )
       Scanning  = eq == 'scanning'
       Transient = eq == 'transient'


!------------------------------------------------------------------------------
!      To more conveniently support the use of VTK based visualization there 
!      is a hack that recognizes VTU suffix and creates a instance of output solver.
!      Note that this is really quite a dirty hack, and is not a good example.
!-----------------------------------------------------------------------------
       IF (MyVerbosity > 3) THEN
         write(6,*) 'calling AddVtuOutputSolverHack' 
       END IF
       CALL AddVtuOutputSolverHack()
       IF (MyVerbosity > 3) THEN
         write(6,*) 'leaving AddVtuOutputSolverHack'
       END IF

!------------------------------------------------------------------------------
!      Figure out what (flow,heat,stress,...) should be computed, and get
!      memory for the dofs
!------------------------------------------------------------------------------
       IF (MyVerbosity > 3) THEN
         write(6,*) 'calling AddSolvers'
       END IF
       CALL AddSolvers()
       IF (MyVerbosity > 3) THEN
         write(6,*) 'leaving AddSolvers'
       END IF

!------------------------------------------------------------------------------
!      Time integration and/or steady state steps
!------------------------------------------------------------------------------
       IF ( Transient .OR. Scanning ) THEN
         Timesteps => ListGetIntegerArray( CurrentModel % Simulation, &
                       'Timestep Intervals', GotIt )

         IF ( .NOT.GotIt ) THEN
           CALL Fatal( ' ', 'Keyword > Timestep Intervals < MUST be ' //  &
                   'defined for transient and scanning simulations' )
         END IF 
         TimestepSizes => ListGetConstRealArray( CurrentModel % Simulation, &
                               'Timestep Sizes', GotIt )

         IF ( .NOT.GotIt ) THEN
           IF( Scanning .OR. ListCheckPresent( CurrentModel % Simulation,'Timestep Size') ) THEN
             ALLOCATE(TimestepSizes(SIZE(Timesteps),1))
             TimestepSizes = 1.0_dp
           ELSE
             CALL Fatal( ' ', 'Keyword [Timestep Sizes] MUST be ' //  &
                 'defined for time dependent simulations' )
             STOP
           END IF
         END IF 

         TimeIntervals = SIZE(Timesteps)

         CoupledMaxIter = ListGetInteger( CurrentModel % Simulation, &
               'Steady State Max Iterations', GotIt, minv=1 )
         IF ( .NOT. GotIt ) CoupledMaxIter = 1
!------------------------------------------------------------------------------
       ELSE
!------------------------------------------------------------------------------
!        Steady state
!------------------------------------------------------------------------------
         ALLOCATE(Timesteps(1))

         Timesteps(1) = ListGetInteger( CurrentModel % Simulation, &
               'Steady State Max Iterations', GotIt,minv=1 )
         IF ( .NOT. GotIt ) Timesteps(1)=1
  
         ALLOCATE(TimestepSizes(1,1))
         TimestepSizes(1,1) = 1.0D0

         CoupledMaxIter = 1
         TimeIntervals  = 1
       END IF

       IF ( FirstLoad ) &
         ALLOCATE( sTime(1), sStep(1), sInterval(1), sSize(1), &
             steadyIt(1), nonLinit(1), sPrevSizes(1,5), sPeriodic(1) )

       dt   = 0._dp

       sTime = 0._dp
       sStep = 0
       sPeriodic = 0._dp

       sSize = dt
       sPrevSizes = 0_dp

       sInterval = 0._dp

       steadyIt = 0
       nonlinIt = 0

       CoupledMinIter = ListGetInteger( CurrentModel % Simulation, &
                  'Steady State Min Iterations', GotIt )

!------------------------------------------------------------------------------
!      Add coordinates and simulation time to list of variables so that
!      coordinate dependent parameter computing routines can ask for
!      them...
!------------------------------------------------------------------------------
       IF ( FirstLoad ) CALL AddMeshCoordinatesAndTime

!------------------------------------------------------------------------------
!      Get Output File Options
!------------------------------------------------------------------------------

       OutputIntervals => ListGetIntegerArray( CurrentModel % Simulation, &
                       'Output Intervals', GotIt )

       IF ( .NOT. GotIt ) THEN
         ALLOCATE( OutputIntervals(SIZE(TimeSteps)) )
         OutputIntervals = 1
       END IF

       IF (MyVerbosity > 3) THEN
         write(6,*) 'MyRunI SIZE(OutputIntervals) =', SIZE(OutputIntervals)
         write(6,*) 'MyRunI OutputIntervals(1) = ',OutputIntervals(1)
       END IF      

       ! Initial Conditions:
       ! -------------------
       IF ( FirstLoad ) CALL SetInitialConditions


       ! Compute the total number of steps that will be saved to the files
       ! Particularly look if the last step will be saved, or if it has
       ! to be saved separately.
       !------------------------------------------------------------------
       TotalTimesteps = 0
       LastSaved = .TRUE.
       DO interval=1,TimeIntervals
         DO timestep = 1,Timesteps(interval)
           IF( OutputIntervals(Interval) == 0 ) CYCLE
           LastSaved = .FALSE.
           IF ( MOD(Timestep-1, OutputIntervals(Interval))==0 ) THEN
              LastSaved = .TRUE.
              TotalTimesteps = TotalTimesteps + 1
           END IF
         END DO
       END DO

       DO i=1,CurrentModel % NumberOfSolvers
          Solver => CurrentModel % Solvers(i)
          IF(.NOT.ASSOCIATED(Solver % Variable)) CYCLE
          IF(.NOT.ASSOCIATED(Solver % Variable % Values)) CYCLE
          When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
          IF ( GotIt ) THEN
             IF ( When == 'after simulation' .OR. When == 'after all' ) THEN
                LastSaved = .FALSE.
             END IF
          ELSE
           IF ( Solver % SolverExecWhen == SOLVER_EXEC_AFTER_ALL ) THEN
              LastSaved = .FALSE.
           END IF
          END IF
       END DO

       IF ( .NOT.LastSaved ) TotalTimesteps = TotalTimesteps + 1
       IF( TotalTimesteps == 0 ) TotalTimesteps = 1

       CALL ListAddLogical( CurrentModel % Simulation,  &
            'Initialization Phase', .FALSE. )

       FirstLoad = .FALSE.
       IF (MyVerbosity > 3) THEN
         WRITE(6,*) 'MyElmerSolver 493 Initialize = ', Initialize     
         write(6,*) 'TimeIntervals from inside RunI = ', TimeIntervals
         WRITE(*,*) 'NumberOfSolvers = ', CurrentModel % NumberOfSolvers    
       END IF

!       IF ( Initialize == 1 ) EXIT
 
     DO i=1,CurrentModel % NumberOfSolvers
        Solver => CurrentModel % Solvers(i)
        IF (MyVerbosity > 3) THEN
          WRITE(*,*) 'Solver % PROCEDURE = ', Solver % PROCEDURE
        END IF
        IF ( Solver % PROCEDURE==0 ) CYCLE
        IF ( Solver % SolverExecWhen == SOLVER_EXEC_AHEAD_ALL ) THEN
           IF (MyVerbosity > 3) THEN
             WRITE(*,*) 'Calling SolverActivate for Solver', i
           END IF
           CALL SolverActivate( CurrentModel,Solver,dt,Transient )
        END IF
     END DO

     runs = runs + 1

     RETURN

10   CONTINUE
     CALL Fatal( 'MyElmerSolver', 'Unable to find ELMERSOLVER_STARTINFO, can not execute.' )

20   CONTINUE
     CALL Fatal( 'MyElmerSolver', 'Unable to find input file [' // &
              TRIM(Modelname) // '], can not execute.' )


!------------------------------------------------------------------------------
  END SUBROUTINE ElmerInitialize
!------------------------------------------------------------------------------

!> \} ElmerLib
