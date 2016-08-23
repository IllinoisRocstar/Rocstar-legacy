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
   MODULE GeneralModule
!------------------------------------------------------------------------------

     USE MainUtils

!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------

     INTEGER :: Initialize

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------

     INTEGER :: i,j,k,n,l,t,k1,k2,iter,Ndeg,istat,nproc,tlen,nthreads
     CHARACTER(LEN=MAX_STRING_LEN) :: threads

     REAL(KIND=dp) :: s,dt,dtfunc
     REAL(KIND=dP), POINTER :: WorkA(:,:,:) => NULL()
     REAL(KIND=dp), POINTER, SAVE :: sTime(:), sStep(:), sInterval(:), sSize(:), &
           steadyIt(:),nonlinIt(:),sPrevSizes(:,:),sPeriodic(:)

     TYPE(Element_t),POINTER :: CurrentElement

     LOGICAL :: GotIt,Transient,Scanning,LastSaved

     INTEGER :: TimeIntervals,interval,timestep, &
       TotalTimesteps,SavedSteps,CoupledMaxIter,CoupledMinIter

     INTEGER, POINTER, SAVE :: Timesteps(:),OutputIntervals(:), ActiveSolvers(:)
     REAL(KIND=dp), POINTER, SAVE :: TimestepSizes(:,:)

     INTEGER(KIND=AddrInt) :: ControlProcedure

     LOGICAL :: InitDirichlet, ExecThis

     TYPE(ElementType_t),POINTER :: elmt

     TYPE(ParEnv_t), POINTER :: ParallelEnv

     CHARACTER(LEN=MAX_NAME_LEN) :: ModelName, eq, ExecCommand
     CHARACTER(LEN=MAX_STRING_LEN) :: OutputFile, PostFile, RestartFile, &
                OutputName=' ',PostName=' ', When, OptionString

     TYPE(Variable_t), POINTER :: Var
     TYPE(Mesh_t), POINTER :: Mesh
     TYPE(Solver_t), POINTER :: Solver

     REAL(KIND=dp) :: CT0,RT0,tt

     LOGICAL :: FirstLoad = .TRUE., FirstTime=.TRUE., Found
     LOGICAL :: Silent, Version, GotModelName

     INTEGER :: NoArgs

     INTEGER :: ExtrudeLevels
     TYPE(Mesh_t), POINTER :: ExtrudedMesh

     INTEGER :: omp_get_max_threads
     INTEGER :: MyVerbosity = 1


#ifdef HAVE_TRILINOS
INTERFACE
      SUBROUTINE TrilinosCleanup() BIND(C,name='TrilinosCleanup')
      IMPLICIT NONE
      END SUBROUTINE TrilinosCleanup
END INTERFACE
#endif


   CONTAINS 
     

     ! This is a dirty hack that adds an instance of ResultOutputSolver to the list of Solvers.
     ! The idea is that it is much easier for the end user to take into use the vtu output this way.
     ! The solver itself has limited set of parameters needed and is therefore approapriate for this
     ! kind of hack. It can of course be also added as a regular solver also.
     !----------------------------------------------------------------------------------------------
     SUBROUTINE AddVtuOutputSolverHack()     
       TYPE(Solver_t), POINTER :: ABC(:), PSolver
       CHARACTER(LEN=MAX_NAME_LEN) :: str
       INTEGER :: i,j,j2,j3,k,n
       TYPE(ValueList_t), POINTER :: Params
       LOGICAL :: gotIt, VtuFormat

       str = ListGetString( CurrentModel % Simulation,'Post File',GotIt) 
       IF(.NOT. GotIt) RETURN
       k = INDEX( str,'.vtu' )
       VtuFormat = ( k /= 0 ) 

       IF(.NOT. VtuFormat ) RETURN
       
       CALL Info('AddVtuOutputSolverHack','Adding ResultOutputSolver to write VTU output in file: '&
           //TRIM(str(1:k-1)))
     
       CALL ListRemove( CurrentModel % Simulation,'Post File')
       n = CurrentModel % NumberOfSolvers+1
       ALLOCATE( ABC(n) )
       DO i=1,n-1
         ! Def_Dofs is the only allocatable structure within Solver_t:
         IF( ALLOCATED( CurrentModel % Solvers(i) % Def_Dofs ) ) THEN
           j = SIZE(CurrentModel % Solvers(i) % Def_Dofs,1)
           j2 = SIZE(CurrentModel % Solvers(i) % Def_Dofs,2)
           j3 = SIZE(CurrentModel % Solvers(i) % Def_Dofs,3)
           ALLOCATE( ABC(i) % Def_Dofs(j,j2,j3) )
         END IF

         ! Copy the content of the Solver structure
         ABC(i) = CurrentModel % Solvers(i)

         ! Nullify the old structure since otherwise bad things may happen at deallocation
         NULLIFY( CurrentModel % Solvers(i) % ActiveElements )
         NULLIFY( CurrentModel % Solvers(i) % Values )
         NULLIFY( CurrentModel % Solvers(i) % Mesh )
         NULLIFY( CurrentModel % Solvers(i) % BlockMatrix )
         NULLIFY( CurrentModel % Solvers(i) % Matrix )
         NULLIFY( CurrentModel % Solvers(i) % Variable )
       END DO

       ! Deallocate the old structure and set the pointer to the new one
       DEALLOCATE( CurrentModel % Solvers )
       CurrentModel % Solvers => ABC
       CurrentModel % NumberOfSolvers = n

       ! Now create the ResultOutputSolver instance on-the-fly
       NULLIFY( CurrentModel % Solvers(n) % Values )
       CurrentModel % Solvers(n) % PROCEDURE = 0
       NULLIFY( CurrentModel % Solvers(n) % Matrix )
       NULLIFY( CurrentModel % Solvers(n) % BlockMatrix )
       NULLIFY( CurrentModel % Solvers(n) % Values )
       NULLIFY( CurrentModel % Solvers(n) % Variable )
       NULLIFY( CurrentModel % Solvers(n) % ActiveElements )
       CurrentModel % Solvers(n) % NumberOfActiveElements = 0
       NULLIFY( CurrentModel % Solvers(n) % Values )
       j = CurrentModel % NumberOfBodies
       ALLOCATE( CurrentModel % Solvers(n) % Def_Dofs(10,j,6))
       CurrentModel % Solvers(n) % Def_Dofs(:,1:j,6) = -1
       
       ! Add some keywords to the list
       CALL ListAddString(CurrentModel % Solvers(n) % Values,&
           'Procedure', 'ResultOutputSolve ResultOutputSolver',.FALSE.)
       CALL ListAddString(CurrentModel % Solvers(n) % Values,'Output Format','vtu')
       CALL ListAddString(CurrentModel % Solvers(n) % Values,'Output File Name',str(1:k-1))
       CALL ListAddString(CurrentModel % Solvers(n) % Values,'Exec Solver','after saving')
       CALL ListAddString(CurrentModel % Solvers(n) % Values,'Equation','InternalVtuOutputSolver')
       CALL ListAddLogical(CurrentModel % Solvers(n) % Values,'Save Geometry IDs',.TRUE.)

     END SUBROUTINE AddVtuOutputSolverHack


!------------------------------------------------------------------------------
!> Adds flags for active solvers. 
!------------------------------------------------------------------------------
    SUBROUTINE AddSolvers()
!------------------------------------------------------------------------------
      INTEGER :: i,j,k,nlen
      LOGICAL :: InitSolver, Found
!------------------------------------------------------------------------------

      ! This is a hack that sets Equation flags True for the "Active Solvers".
      ! The Equation flag is the legacy way of setting a Solver active and is still
      ! used internally.
      !----------------------------------------------------------------------------
      DO i=1,CurrentModel % NumberOfSolvers

        eq = ListGetString( CurrentModel % Solvers(i) % Values,'Equation', Found )
     
        IF ( Found ) THEN
          nlen = LEN_TRIM(eq)
          DO j=1,CurrentModel % NumberOFEquations
             ActiveSolvers => ListGetIntegerArray( CurrentModel % Equations(j) % Values, &
                                'Active Solvers', Found )
             IF ( Found ) THEN
                DO k=1,SIZE(ActiveSolvers)
                   IF ( ActiveSolvers(k) == i ) THEN
                      CALL ListAddLogical( CurrentModel % Equations(j) % Values, eq(1:nlen), .TRUE. )
                      EXIT
                   END IF
                END DO
             END IF
          END DO
       END IF
     END DO

     DO i=1,CurrentModel % NumberOfSolvers
        eq = ListGetString( CurrentModel % Solvers(i) % Values,'Equation', Found )

        Solver => CurrentModel % Solvers(i)
        InitSolver = ListGetLogical( Solver % Values, 'Initialize', Found )
        IF ( Found .AND. InitSolver ) THEN
          CALL FreeMatrix( Solver % Matrix )
          CALL ListAddLogical( Solver % Values, 'Initialize', .FALSE. )
        END IF

        IF ( Solver % PROCEDURE == 0 .OR. InitSolver ) THEN
          IF ( .NOT. ASSOCIATED( Solver % Mesh ) ) THEN
            Solver % Mesh => CurrentModel % Meshes
          END IF
          CurrentModel % Solver => Solver
          CALL AddEquationBasics( Solver, eq, Transient )
          CALL AddEquationSolution( Solver, Transient )
        END IF
     END DO
!------------------------------------------------------------------------------
  END SUBROUTINE AddSolvers
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Adds coordinate and time variables to the current mesh structure. 
!------------------------------------------------------------------------------
  SUBROUTINE AddMeshCoordinatesAndTime()
!------------------------------------------------------------------------------
     TYPE(Variable_t), POINTER :: DtVar

     NULLIFY( Solver )

     Mesh => CurrentModel % Meshes 
     DO WHILE( ASSOCIATED( Mesh ) )
       CALL VariableAdd( Mesh % Variables, Mesh,Solver, &
             'Coordinate 1',1,Mesh % Nodes % x )

       CALL VariableAdd(Mesh % Variables,Mesh,Solver, &
             'Coordinate 2',1,Mesh % Nodes % y )

       CALL VariableAdd(Mesh % Variables,Mesh,Solver, &
             'Coordinate 3',1,Mesh % Nodes % z )

       CALL VariableAdd( Mesh % Variables, Mesh, Solver, 'Time', 1, sTime )
       CALL VariableAdd( Mesh % Variables, Mesh, Solver, 'Periodic Time', 1, sPeriodic )
       CALL VariableAdd( Mesh % Variables, Mesh, Solver, 'Timestep', 1, sStep )
       CALL VariableAdd( Mesh % Variables, Mesh, Solver, 'Timestep size', 1, sSize )
       CALL VariableAdd( Mesh % Variables, Mesh, Solver, 'Timestep interval', 1, sInterval )

       ! Save some previous timesteps for variable timestep multistep methods
       DtVar => VariableGet( Mesh % Variables, 'Timestep size' )
       DtVar % PrevValues => sPrevSizes

       CALL VariableAdd( Mesh % Variables, Mesh, Solver, &
               'nonlin iter', 1, nonlinIt )
       CALL VariableAdd( Mesh % Variables, Mesh, Solver, &
               'coupled iter', 1, steadyIt )
       Mesh => Mesh % Next
     END DO
!------------------------------------------------------------------------------
  END SUBROUTINE AddMeshCoordinatesAndTime
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Sets initial conditions for the fields. 
!------------------------------------------------------------------------------
   SUBROUTINE SetInitialConditions()
!------------------------------------------------------------------------------
     USE DefUtils
     INTEGER :: DOFs
     CHARACTER(LEN=MAX_NAME_LEN) :: str
     LOGICAL :: Found
     TYPE(Solver_t), POINTER :: Solver
     INTEGER, ALLOCATABLE :: Indexes(:)
     REAL(KIND=dp),ALLOCATABLE :: Work(:)

     INTEGER :: i,j,k,l,m,vect_dof,real_dof,dim

     REAL(KIND=dp) :: nrm(3),t1(3),t2(3),vec(3),tmp(3),udot
     TYPE(ValueList_t), POINTER :: BC
     TYPE(Nodes_t), SAVE :: Nodes
     LOGICAL :: nt_boundary
     TYPE(Element_t), POINTER :: Element
     TYPE(Variable_t), POINTER :: var, vect_var

     dim = CoordinateSystemDimension()

     IF (GetLogical(GetSimulation(),'Restart Before Initial Conditions',Found)) THEN
       CALL Restart
       CALL InitCond
     ELSE
       CALL InitCond
       CALL Restart
     END IF

!------------------------------------------------------------------------------
!    Make sure that initial values at boundaries are set correctly.
!    NOTE: This overrides the initial condition setting for field variables!!!!
!-------------------------------------------------------------------------------
     InitDirichlet = ListGetLogical( CurrentModel % Simulation, &
            'Initialize Dirichlet Conditions', GotIt ) 
     IF ( .NOT. GotIt ) InitDirichlet = .TRUE.

     vect_var => NULL()
     IF ( InitDirichlet ) THEN
       Mesh => CurrentModel % Meshes
       DO WHILE( ASSOCIATED(Mesh) )
         ALLOCATE( Work(Mesh % MaxElementDOFs) )
         CALL SetCurrentMesh( CurrentModel, Mesh )

         DO t = Mesh % NumberOfBulkElements + 1, &
                 Mesh % NumberOfBulkElements + Mesh % NumberOfBoundaryElements

           Element => Mesh % Elements(t)

           ! Set also the current element pointer in the model structure to
           ! reflect the element being processed:
           ! ---------------------------------------------------------------
           CurrentModel % CurrentElement => Element
           n = Element % TYPE % NumberOfNodes

           BC => GetBC()

           Var => Mesh % Variables
           DO WHILE( ASSOCIATED(Var) )
             Solver => Var % Solver
             IF ( .NOT. ASSOCIATED(Solver) ) Solver => CurrentModel % Solver

             str = ListGetString( Solver % Values, 'Namespace', Found )
             IF (Found) CALL ListSetNamespace(TRIM(str))


             IF ( Var % DOFs <= 1 ) THEN
               Work(1:n) = GetReal( BC,Var % Name, gotIt )
               IF ( GotIt ) THEN

                 nt_boundary = .FALSE.
                 IF ( GetElementFamily() /= 1 ) THEN
                   k = LEN_TRIM(var % name)
                   vect_dof = ICHAR(Var % Name(k:k))-ICHAR('0');
                   IF ( vect_dof>=1 .AND. vect_dof<= 3 ) THEN
                     nt_boundary =  GetLogical( BC, &
                        'normal-tangential '//var % name(1:k-2), gotIt)

                     IF ( nt_boundary ) THEN
                       nt_boundary = .FALSE.
                       Vect_var => Mesh % Variables
                       DO WHILE( ASSOCIATED(Vect_var) )
                         IF ( Vect_var % Dofs>=dim ) THEN
                           DO real_dof=1,Vect_var % Dofs
                             nt_boundary = ASSOCIATED(Var % Values, &
                               Vect_var % Values(real_dof::Vect_var % Dofs))
                             IF ( nt_boundary ) EXIT
                           END DO
                           IF ( nt_boundary ) EXIT
                         END IF
                         Vect_var => Vect_var % Next
                       END DO
                     END IF

                     IF ( nt_boundary ) THEN
                       CALL GetElementNodes(Nodes)
                       nrm = NormalVector(Element,Nodes,0._dp,0._dp,.TRUE.)
                       SELECT CASE(Element % TYPE % DIMENSION)
                       CASE(1)
                         t1(1) =  nrm(2)
                         t1(2) = -nrm(1)
                       CASE(2)
                         CALL TangentDirections(nrm,t1,t2)
                       END SELECT

                       SELECT CASE(vect_dof)
                         CASE(1)
                           vec = nrm
                         CASE(2)
                           vec = t1
                         CASE(3)
                           vec = t2
                       END SELECT
                     END IF
                   END IF
                 END IF

                 DO j=1,n
                   k = Element % NodeIndexes(j)
                   IF ( ASSOCIATED(Var % Perm) ) k = Var % Perm(k)
                   IF ( k>0 ) THEN
                     IF ( nt_boundary ) THEN
                       DO l=1,dim
                         m = l+real_dof-vect_dof
                         tmp(l)=Vect_var % Values(Vect_var % Dofs*(k-1)+m)
                       END DO
                       udot = SUM(vec(1:dim)*tmp(1:dim))
                       tmp(1:dim)=tmp(1:dim)+(work(j)-udot)*vec(1:dim)
                       DO l=1,dim
                         m = l+real_dof-vect_dof
                         Vect_var % Values(Vect_var % Dofs*(k-1)+m)=tmp(l)
                       END DO
                     ELSE
                       Var % Values(k) = Work(j)
                     END IF
                   END IF
                 END DO
               END IF

               IF ( Transient .AND. Solver % TimeOrder==2 ) THEN
                  Work(1:n) = GetReal( BC, TRIM(Var % Name) // ' Velocity', GotIt )
                  IF ( GotIt ) THEN
                    DO j=1,n
                      k = Element % NodeIndexes(j)
                      IF ( ASSOCIATED(Var % Perm) ) k = Var % Perm(k)
                      IF ( k>0 ) Var % PrevValues(k,1) = Work(j)
                    END DO
                  END IF
                  Work(1:n) = GetReal( BC, TRIM(Var % Name) // ' Acceleration', GotIt )
                  IF ( GotIt ) THEN
                    DO j=1,n
                      k = Element % NodeIndexes(j)
                      IF ( ASSOCIATED(Var % Perm) ) k = Var % Perm(k)
                      IF ( k>0 ) Var % PrevValues(k,2) = Work(j)
                    END DO
                  END IF
               END IF
             ELSE
               CALL ListGetRealArray( BC, &
                 Var % Name, WorkA, n, Element % NodeIndexes, gotIt )
               IF ( GotIt ) THEN
                 DO j=1,n
                   k = Element % NodeIndexes(j)
                   DO l=1,MIN(SIZE(WorkA,1),Var % DOFs)
                     IF ( ASSOCIATED(Var % Perm) ) k = Var % Perm(k)
                     IF ( k>0 ) Var % Values(Var % DOFs*(k-1)+l) = WorkA(l,1,j)
                   END DO
                 END DO
               ELSE
               END IF
             END IF

             CALL ListSetNamespace('')
             Var => Var % Next
           END DO
         END DO
         DEALLOCATE( Work )
         Mesh => Mesh % Next
       END DO
     END IF
!------------------------------------------------------------------------------
   END SUBROUTINE SetInitialConditions
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
   SUBROUTINE InitCond()
!------------------------------------------------------------------------------
     USE DefUtils
     TYPE(Element_t), POINTER :: Edge
     INTEGER :: DOFs,i,j,k,l
     CHARACTER(LEN=MAX_NAME_LEN) :: str
     LOGICAL :: Found, ThingsToDO
     TYPE(Solver_t), POINTER :: Solver
     INTEGER, ALLOCATABLE :: Indexes(:)
     REAL(KIND=dp) :: Val
     REAL(KIND=dp),ALLOCATABLE :: Work(:)
     TYPE(ValueList_t), POINTER :: IC
!------------------------------------------------------------------------------

     Mesh => CurrentModel % Meshes
     DO WHILE( ASSOCIATED( Mesh ) )
       ALLOCATE( Indexes(Mesh % MaxElementDOFs), Work(Mesh % MaxElementDOFs) )
       CALL SetCurrentMesh( CurrentModel, Mesh )

       ! First set the global variables and check whether there is anything left to do
       ThingsToDo = .FALSE.
       DO j=1,CurrentModel % NumberOfICs

         IC => CurrentModel % ICs(j) % Values
         
         Var => Mesh % Variables
         DO WHILE( ASSOCIATED(Var) ) 
           
           Solver => Var % Solver
           IF ( .NOT. ASSOCIATED(Solver) ) Solver => CurrentModel % Solver
           
           str = ListGetString( Solver % Values, 'Namespace', Found )
           IF (Found) CALL ListSetNamespace(TRIM(str))
           
           ! global variable
           IF( SIZE( Var % Values ) == Var % DOFs ) THEN
             Val = ListGetCReal( IC, Var % Name, GotIt )
             IF( GotIt ) THEN
               WRITE( Message,'(A,ES12.3)') 'Initializing global variable: > '&
                   //TRIM(Var % Name)//' < to :',Val
               CALL Info('InitCond', Message,Level=8)
               Var % Values = Val
             END IF
           ELSE
             ThingsToDo = ThingsToDo .OR. &
                 ListCheckPresent( IC, TRIM(Var % Name) )
             ThingsToDo = ThingsToDo .OR. &
                 ListCheckPresent( IC, TRIM(Var % Name)//' Velocity' )
             ThingsToDo = ThingsToDo .OR. &
                 ListCheckPresent( IC, TRIM(Var % Name)//' Acceleration' )
             ThingsToDo = ThingsToDo .OR. &
                  ListCheckPresent( IC, TRIM(Var % Name)//' {e}' )
           END IF
           Var => Var % Next
         END DO
       END DO

       ! And now do the ordinary fields
       !--------------------------------
       IF( ThingsToDo ) THEN
         DO t=1, Mesh % NumberOfBulkElements+Mesh % NumberOfBoundaryElements
           
           CurrentElement =>  Mesh % Elements(t)
           
           i = CurrentElement % BodyId 
           IF( i == 0 ) CYCLE
           
           j = ListGetInteger(CurrentModel % Bodies(i) % Values, &
               'Initial Condition',GotIt, 1, CurrentModel % NumberOfICs )           
           IF ( .NOT. GotIt ) CYCLE
           
           IC => CurrentModel % ICs(j) % Values
           CurrentModel % CurrentElement => CurrentElement
           n = GetElementNOFNodes()
           
           Var => Mesh % Variables
           DO WHILE( ASSOCIATED(Var) ) 
             
             
             Solver => Var % Solver
             IF ( .NOT. ASSOCIATED(Solver) ) Solver => CurrentModel % Solver

             str = ListGetString( Solver % Values, 'Namespace', Found )
             IF (Found) CALL ListSetNamespace(TRIM(str))
             
             ! global variables were already set
             IF( SIZE( Var % Values ) == Var % DOFs ) THEN
               CONTINUE
               
             ELSE IF ( Var % DOFs <= 1 ) THEN
               
               Work(1:n) = GetReal( IC, Var % Name, GotIt )
               IF ( GotIt ) THEN
                 DOFs = GetElementDOFs( Indexes, USolver=Var % Solver )
                 DO k=1,n
                   k1 = Indexes(k)
                   IF ( ASSOCIATED(Var % Perm) ) k1 = Var % Perm(k1)
                   IF ( k1>0 ) Var % Values(k1) = Work(k)
                 END DO
               END IF
               
               IF ( Transient .AND. Solver % TimeOrder==2 ) THEN
                 Work(1:n) = GetReal( IC, TRIM(Var % Name) // ' Velocity', GotIt )
                 IF ( GotIt ) THEN
                   DOFs = GetElementDOFs( Indexes, USolver=Var % Solver )
                   DO k=1,n
                     k1 = Indexes(k)
                     IF ( ASSOCIATED(Var % Perm) ) k1 = Var % Perm(k1)
                     IF ( k1>0 ) Var % PrevValues(k1,1) = Work(k)
                   END DO
                 END IF
                 Work(1:n) = GetReal( IC, TRIM(Var % Name) // ' Acceleration', GotIt )
                 IF ( GotIt ) THEN
                   DOFs = GetElementDOFs( Indexes, USolver=Var % Solver )
                   DO k=1,n
                     k1 = Indexes(k)
                     IF ( ASSOCIATED(Var % Perm) ) k1 = Var % Perm(k1)
                     IF ( k1>0 ) Var % PrevValues(k1,2) = Work(k)
                   END DO
                 END IF
               END IF
               
               IF(ASSOCIATED(Mesh % Edges)) THEN
                 IF ( i<=Mesh % NumberOfBulkElements) THEN
                   Gotit = ListCheckPresent( IC, TRIM(Var % Name)//' {e}' )
                   IF ( Gotit ) THEN
                     DO k=1,CurrentElement % TYPE % NumberOfedges
                       Edge => Mesh % Edges(CurrentElement % EdgeIndexes(k))
                       l = Var % Perm(CurrentElement % EdgeIndexes(k)+Mesh % NumberOfNodes)
                       IF ( l>0 ) THEN
                         CALL LocalBcIntegral( IC, &
                             Edge, Edge % TYPE % NumberOfNodes, CurrentElement, n, &
                             TRIM(Var % Name)//' {e}', Work(1) )
                         Var % Values(l) = Work(1)
                       END IF
                     END DO
                   END IF
                 END IF
               END IF
               
             ELSE
               CALL ListGetRealArray( IC, &
                   Var % Name, WorkA, n, CurrentElement % NodeIndexes, gotIt )
               
               IF ( GotIt ) THEN
                 DO k=1,n
                   k1 = Indexes(k)
                   DO l=1,MIN(SIZE(WorkA,1),Var % DOFs)
                     IF ( ASSOCIATED(Var % Perm) ) k1 = Var % Perm(k1)
                     IF ( k1>0 ) Var % Values(Var % DOFs*(k1-1)+l) = WorkA(l,1,k)
                   END DO
                 END DO
               END IF
             END IF
             CALL ListSetNamespace('')
             Var => Var % Next
           END DO
         END DO
       END IF

       DEALLOCATE( Indexes, Work )
       Mesh => Mesh % Next
     END DO

!------------------------------------------------------------------------------
   END SUBROUTINE InitCond
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!> Check if we are restarting are if yes, read in field values.
!------------------------------------------------------------------------------
   SUBROUTINE Restart()
!------------------------------------------------------------------------------
     USE DefUtils
     LOGICAL :: Gotit
     INTEGER :: k,StartTime
!------------------------------------------------------------------------------


     RestartFile = ListGetString( CurrentModel % Simulation, &
         'Restart File', GotIt )

     IF ( GotIt ) THEN
       k = ListGetInteger( CurrentModel % Simulation,'Restart Position',GotIt, &
                  minv=0 )

       Mesh => CurrentModel % Meshes
       DO WHILE( ASSOCIATED(Mesh) ) 

         IF ( LEN_TRIM(Mesh % Name) > 0 ) THEN
           OutputName = TRIM(Mesh % Name) // '/' // TRIM(RestartFile)
         ELSE
           OutputName = TRIM(RestartFile)
         END IF
         IF ( ParEnv % PEs > 1 ) &
           OutputName = TRIM(OutputName) // '.' // TRIM(i2s(ParEnv % MyPe))

         CALL SetCurrentMesh( CurrentModel, Mesh )
         CALL LoadRestartFile( OutputName,k,Mesh )

         StartTime = ListGetConstReal( CurrentModel % Simulation,'Restart Time',GotIt)
         IF( GotIt ) THEN
	   Var  => VariableGet( Mesh % Variables, 'Time' )
           IF ( ASSOCIATED( Var ) )  Var % Values(1)  = StartTime
         END IF

         Mesh => Mesh % Next

       END DO


     END IF
!------------------------------------------------------------------------------
   END SUBROUTINE Restart
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Execute the individual solvers in defined sequence. 
!------------------------------------------------------------------------------
   SUBROUTINE ExecSimulation(TimeIntervals,  CoupledMinIter, &
              CoupledMaxIter, OutputIntervals, Transient, Scanning)
      INTEGER :: TimeIntervals,CoupledMinIter, CoupledMaxIter,OutputIntervals(:)
      LOGICAL :: Transient,Scanning
!------------------------------------------------------------------------------
     INTEGER :: interval, timestep, i, j, k, n
     REAL(KIND=dp) :: dt, ddt, dtfunc
     INTEGER :: timeleft,cum_timestep
     INTEGER, SAVE ::  stepcount=0, RealTimestep
     LOGICAL :: ExecThis,SteadyStateReached=.FALSE.

     REAL(KIND=dp) :: CumTime, MaxErr, AdaptiveLimit, &
           AdaptiveMinTimestep, AdaptiveMaxTimestep, timePeriod
     INTEGER :: SmallestCount, AdaptiveKeepSmallest, StepControl=-1
     LOGICAL :: AdaptiveTime = .TRUE.

     TYPE(Solver_t), POINTER :: Solver

!     REAL(KIND=dp) :: RealTime, newtime, prevtime=0, maxtime, exitcond
     REAL(KIND=dp) :: newtime, prevtime=0, maxtime, exitcond
     REAL(KIND=dp), ALLOCATABLE :: xx(:,:), xxnrm(:), yynrm(:), PrevXX(:,:,:)

    !Something I (Jess) am adding in order to see if I can access
    !the displacements from this level
    LOGICAL :: GotIt
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Variable_t), POINTER :: StressSol
    REAL(KIND=dp), POINTER :: Displacement(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: Equation
    INTEGER :: BoundaryElementCount, ElementCount, t, body_id
    INTEGER :: bc_id, nElementNodes, nt, nk
    INTEGER, POINTER :: Index(:), NodeIndexes(:), MyPerm(:)
    TYPE(Element_t), POINTER :: Element, CurrentElement
    TYPE(Nodes_t) :: ElementNodes
    TYPE(Mesh_t), POINTER :: Mesh
    LOGICAL :: IsFSI
    !End of Jess stuff

!$omp parallel
!$   IF(.NOT.GaussPointsInitialized()) CALL GaussPointsInit
!$omp end parallel

    IF (MyVerbosity > 3) WRITE(*,*) 'NumberOfSolvers = ',&
                         CurrentModel % NumberOfSolvers    
 
     DO i=1,CurrentModel % NumberOfSolvers
        Solver => CurrentModel % Solvers(i)
        IF ( Solver % PROCEDURE==0 ) CYCLE
        IF ( Solver % SolverExecWhen == SOLVER_EXEC_AHEAD_ALL ) THEN
           IF (MyVerbosity > 3) WRITE(*,*) &
                                'Calling SolverActivate for Solver', i
           CALL SolverActivate( CurrentModel,Solver,dt,Transient )
        END IF
     END DO

     DO interval = 1, TimeIntervals
        stepcount = stepcount + Timesteps(interval)
     END DO 

     cum_Timestep = 0
     ddt = 0.0d0
     DO interval = 1,TimeIntervals

!------------------------------------------------------------------------------
       IF ( Transient .OR. Scanning ) THEN
         dt = TimestepSizes(interval,1)
       ELSE
         dt = 1
       END IF
!------------------------------------------------------------------------------
!      go trough number of timesteps within an interval
!------------------------------------------------------------------------------
       timePeriod = ListGetCReal(CurrentModel % Simulation, 'Time Period',gotIt)
       IF(.NOT.GotIt) timePeriod = HUGE(timePeriod)


       RealTimestep = 1
       DO timestep = 1,Timesteps(interval)

         cum_Timestep = cum_Timestep + 1
         sStep(1) = cum_Timestep

         dtfunc = ListGetConstReal( CurrentModel % Simulation, &
                  'Timestep Function', gotIt)
         IF(GotIt) THEN
	   CALL Warn('ExecSimulation','Obsolite keyword > Timestep Function < , use > Timestep Size < instead')
         ELSE	
           dtfunc = ListGetCReal( CurrentModel % Simulation, &
                  'Timestep Size', gotIt)
         END IF
         IF(GotIt) dt = dtfunc

!------------------------------------------------------------------------------
         sTime(1) = sTime(1) + dt
         sPeriodic(1) = sTime(1)
         DO WHILE(sPeriodic(1) > timePeriod)
           sPeriodic(1) = sPeriodic(1) - timePeriod 
         END DO

         ! Move the old timesteps one step down the ladder
         IF(timestep > 1 .OR. interval > 1) THEN
           DO i = SIZE(sPrevSizes,2),2,-1
             sPrevSizes(1,i) = sPrevSizes(1,i-1)
           END DO
           sPrevSizes(1,1) = sSize(1)
         END IF 
         sSize(1) = dt

         sInterval(1) = interval
         IF (.NOT. Transient ) steadyIt(1) = steadyIt(1) + 1
!------------------------------------------------------------------------------
         IF ( ParEnv % MyPE == 0 ) THEN
           CALL Info( 'MAIN', ' ', Level=3 )
           CALL Info( 'MAIN', '-------------------------------------', Level=3 )

           IF ( Transient .OR. Scanning ) THEN
             WRITE( Message, * ) 'Time: ',TRIM(i2s(cum_Timestep)),'/', &
                   TRIM(i2s(stepcount)), sTime(1)
             CALL Info( 'MAIN', Message, Level=3 )

             newtime= RealTime()

             IF( cum_Timestep > 1 ) THEN
               maxtime = ListGetConstReal( CurrentModel % Simulation,'Real Time Max',GotIt)
               IF( GotIt ) THEN
                  WRITE( Message,'(A,F8.3)') 'Fraction of real time left: ',&
                              1.0_dp-RealTime() / maxtime
               ELSE             
                 timeleft = NINT((stepcount-(cum_Timestep-1))*(newtime-prevtime)/60._dp);
                 IF (timeleft > 120) THEN
                   WRITE( Message, *) 'Estimated time left: ', &
                     TRIM(i2s(timeleft/60)),' hours.'
                 ELSE IF(timeleft > 60) THEN
                   WRITE( Message, *) 'Estimated time left: 1 hour ', &
                     TRIM(i2s(MOD(timeleft,60))), ' minutes.'
                 ELSE IF(timeleft >= 1) THEN
                   WRITE( Message, *) 'Estimated time left: ', &
                     TRIM(i2s(timeleft)),' minutes.'
                 ELSE
                   WRITE( Message, *) 'Estimated time left: less than a minute.'
                 END IF
               END IF
               CALL Info( 'MAIN', Message, Level=3 )
             END IF
             prevtime = newtime
           ELSE
             WRITE( Message, * ) 'Steady state iteration: ',cum_Timestep
             CALL Info( 'MAIN', Message, Level=3 )
           END IF

           CALL Info( 'MAIN', '-------------------------------------', Level=3 )
           CALL Info( 'MAIN', ' ', Level=3 )
         END IF

!------------------------------------------------------------------------------
!        Solve any and all governing equations in the system
!------------------------------------------------------------------------------
         AdaptiveTime = ListGetLogical( CurrentModel % Simulation, &
                  'Adaptive Timestepping', GotIt )

         IF ( Transient .AND. AdaptiveTime ) THEN 
            AdaptiveLimit = ListGetConstReal( CurrentModel % Simulation, &
                        'Adaptive Time Error', GotIt )
 
            IF ( .NOT. GotIt ) THEN 
               WRITE( Message, * ) 'Adaptive Time Limit must be given for' // &
                        'adaptive stepping scheme.'
               CALL Fatal( 'ElmerSolver', Message )
            END IF

            AdaptiveMaxTimestep = ListGetConstReal( CurrentModel % Simulation, &
                     'Adaptive Max Timestep', GotIt )
            IF ( .NOT. GotIt ) AdaptiveMaxTimestep =  dt
            AdaptiveMaxTimestep =  MIN(AdaptiveMaxTimeStep, dt)

            AdaptiveMinTimestep = ListGetConstReal( CurrentModel % Simulation, &
                     'Adaptive Min Timestep', GotIt )

            AdaptiveKeepSmallest = ListGetInteger( CurrentModel % Simulation, &
                       'Adaptive Keep Smallest', GotIt, minv=0  )

            n = CurrentModel % NumberOfSolvers
            j = 0
            k = 0
            DO i=1,n
               Solver => CurrentModel % Solvers(i)
               IF ( ASSOCIATED( Solver % Variable  % Values ) ) THEN
                  IF ( ASSOCIATED( Solver % Variable % PrevValues ) ) THEN
                     j = MAX( j, SIZE( Solver % Variable % PrevValues,2 ) )
                  END IF
                  k = MAX( k, SIZE( Solver % Variable % Values ) )
               END IF
            END DO
            ALLOCATE( xx(n,k), yynrm(n), xxnrm(n), prevxx( n,k,j ) )

            CumTime = 0.0d0
            IF ( ddt == 0.0d0 .OR. ddt > AdaptiveMaxTimestep ) ddt = AdaptiveMaxTimestep

            s = sTime(1) - dt
            SmallestCount = 0
            DO WHILE( CumTime < dt-1.0d-12 )
               ddt = MIN( dt - CumTime, ddt )

               DO i=1,CurrentModel % NumberOFSolvers
                  Solver => CurrentModel % Solvers(i)
                  IF ( ASSOCIATED( Solver % Variable % Values ) ) THEN
                     n = SIZE( Solver % Variable % Values )
                     xx(i,1:n) = Solver % Variable % Values
                     xxnrm(i) = Solver % Variable % Norm
                     IF ( ASSOCIATED( Solver % Variable % PrevValues ) ) THEN
                        DO j=1,SIZE( Solver % Variable % PrevValues,2 )
                           prevxx(i,1:n,j) = Solver % Variable % PrevValues(:,j)
                        END DO
                     END IF
                  END IF
               END DO

               sTime(1) = s + CumTime + ddt
               sSize(1) = ddt
               CALL SolveEquations( CurrentModel, ddt, Transient, &
                 CoupledMinIter, CoupledMaxIter, SteadyStateReached, RealTimestep )


               MaxErr = ListGetConstReal( CurrentModel % Simulation, &
                          'Adaptive Error Measure', GotIt )

               DO i=1,CurrentModel % NumberOFSolvers
                  Solver => CurrentModel % Solvers(i)
                  IF ( ASSOCIATED( Solver % Variable % Values ) ) THEN
                     n = SIZE(Solver % Variable % Values)
                     yynrm(i) = Solver % Variable % Norm
                     Solver % Variable % Values = xx(i,1:n)
                     IF ( ASSOCIATED( Solver % Variable % PrevValues ) ) THEN
                        DO j=1,SIZE( Solver % Variable % PrevValues,2 )
                           Solver % Variable % PrevValues(:,j) = prevxx(i,1:n,j)
                        END DO
                     END IF
                  END IF
               END DO

               sStep(1) = ddt / 2
               sTime(1) = s + CumTime + ddt/2
               CALL SolveEquations( CurrentModel, ddt/2, Transient, &
                  CoupledMinIter, CoupledMaxIter, SteadyStateReached, RealTimestep )
               sTime(1) = s + CumTime + ddt
               CALL SolveEquations( CurrentModel, ddt/2, Transient, &
                  CoupledMinIter, CoupledMaxIter, SteadyStateReached, RealTimestep )

               MaxErr = ABS( MaxErr - ListGetConstReal( CurrentModel % Simulation, &
                           'Adaptive Error Measure', GotIt ) )

               IF ( .NOT. GotIt ) THEN
                  MaxErr = 0.0d0
                  DO i=1,CurrentModel % NumberOFSolvers
                     Solver => CurrentModel % Solvers(i)
                     IF ( ASSOCIATED( Solver % Variable % Values ) ) THEN
                        IF ( yynrm(i) /= Solver % Variable % Norm ) THEN
                           Maxerr = MAX(Maxerr,ABS(yynrm(i)-Solver % Variable % Norm)/yynrm(i))
                        END IF
                     END IF
                  END DO
               END IF

               IF ( MaxErr < AdaptiveLimit .OR. ddt <= AdaptiveMinTimestep ) THEN
                 CumTime = CumTime + ddt
                 RealTimestep = RealTimestep+1
                 IF ( SmallestCount >= AdaptiveKeepSmallest .OR. StepControl > 0 ) THEN
                    ddt = MIN( 2*ddt, AdaptiveMaxTimeStep )
                    StepControl   = 1
                    SmallestCount = 0
                  ELSE
                    StepControl   = 0
                    SmallestCount = SmallestCount + 1
                  END IF
               ELSE
                  DO i=1,CurrentModel % NumberOFSolvers
                     Solver => CurrentModel % Solvers(i)
                     IF ( ASSOCIATED( Solver % Variable % Values ) ) THEN
                        n = SIZE(Solver % Variable % Values)
                        Solver % Variable % Norm = xxnrm(i)
                        Solver % Variable % Values = xx(i,1:n)
                        IF ( ASSOCIATED( Solver % Variable % PrevValues ) ) THEN
                           DO j=1,SIZE( Solver % Variable % PrevValues,2 )
                              Solver % Variable % PrevValues(:,j) = prevxx(i,1:n,j)
                           END DO
                        END IF
                     END IF
                  END DO
                  ddt = ddt / 2
                  StepControl = -1
               END IF
               WRITE(*,'(a,3e20.12)') 'Adaptive(cum,ddt,err): ', cumtime, ddt, maxerr
            END DO
            sSize(1) = dt
            sTime(1) = s + dt
  
            DEALLOCATE( xx, xxnrm, yynrm, prevxx )
         ELSE ! Adaptive timestepping
            IF( MyVerbosity > 3) WRITE(*,*) 'Calling SolveEquations &
                                 (no transient and no adaptive)'
            CALL SolveEquations( CurrentModel, dt, Transient, &
              CoupledMinIter, CoupledMaxIter, SteadyStateReached, RealTimestep )
            RealTimestep = RealTimestep+1
         END IF

         !I'm (Jess) trying to access the displacements here
         Solver => CurrentModel % Solvers(2)
         SolverParams => GetSolverParams(Solver)
         Equation = GetString(SolverParams, 'Equation', GotIt)


!------------------------------------------------------------------------------
!        Save results to disk, if requested
!------------------------------------------------------------------------------

         LastSaved = .FALSE.
         IF( OutputIntervals(Interval) /= 0 ) THEN

           CALL SaveToPost(0)
           k = MOD( Timestep-1, OutputIntervals(Interval) )
           IF ( k == 0 .OR. SteadyStateReached ) THEN
            
             DO i=1,CurrentModel % NumberOfSolvers
               Solver => CurrentModel % Solvers(i)
               IF ( Solver % PROCEDURE == 0 ) CYCLE
               ExecThis = ( Solver % SolverExecWhen == SOLVER_EXEC_AHEAD_SAVE)
               When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
               IF ( GotIt ) ExecThis = ( When == 'before saving') 
               IF( ExecThis ) CALL SolverActivate( CurrentModel,Solver,dt,Transient )
             END DO 

             CALL SaveCurrent(Timestep)
             LastSaved = .TRUE.

             DO i=1,CurrentModel % NumberOfSolvers
               Solver => CurrentModel % Solvers(i)
               IF ( Solver % PROCEDURE == 0 ) CYCLE
               ExecThis = ( Solver % SolverExecWhen == SOLVER_EXEC_AFTER_SAVE)
               When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
               IF ( GotIt ) ExecThis = ( When == 'after saving') 
               IF( ExecThis ) CALL SolverActivate( CurrentModel,Solver,dt,Transient )
             END DO 
           END IF
         END IF
!------------------------------------------------------------------------------

         maxtime = ListGetCReal( CurrentModel % Simulation,'Real Time Max',GotIt)
         IF( GotIt .AND. RealTime() - RT0 > maxtime ) THEN
            CALL Info('ElmerSolver','Reached allowed maximum real time, exiting...')
            GOTO 100
         END IF

	 exitcond = ListGetCReal( CurrentModel % Simulation,'Exit Condition',GotIt)
	 IF( GotIt .AND. exitcond > 0.0_dp ) THEN
            CALL Info('ElmerSolver','Found a positive exit condition, exiting...')
            GOTO 100
         END IF
	 
!------------------------------------------------------------------------------

         IF ( SteadyStateReached .AND. .NOT. (Transient .OR. Scanning) ) THEN
            IF ( Timestep >= CoupledMinIter ) EXIT
         END IF

!------------------------------------------------------------------------------
       END DO ! timestep within an iterval
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
     END DO ! timestep intervals, i.e. the simulation
!------------------------------------------------------------------------------

100   DO i=1,CurrentModel % NumberOfSolvers
        Solver => CurrentModel % Solvers(i)
        IF ( Solver % PROCEDURE == 0 ) CYCLE
        When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
        IF ( GotIt ) THEN
           IF ( When == 'after simulation' .OR. When == 'after all' ) THEN
              CALL SolverActivate( CurrentModel,Solver,dt,Transient )
              IF (ASSOCIATED(Solver % Variable % Values) ) LastSaved = .FALSE.
           END IF
        ELSE
           IF ( Solver % SolverExecWhen == SOLVER_EXEC_AFTER_ALL ) THEN
              CALL SolverActivate( CurrentModel,Solver,dt,Transient )
              IF (ASSOCIATED(Solver % Variable % Values) ) LastSaved = .FALSE.
           END IF
        END IF
     END DO

     IF( .NOT. LastSaved ) THEN
        DO i=1,CurrentModel % NumberOfSolvers
           Solver => CurrentModel % Solvers(i)
           IF ( Solver % PROCEDURE == 0 ) CYCLE
           ExecThis = ( Solver % SolverExecWhen == SOLVER_EXEC_AHEAD_SAVE)
           When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
           IF ( GotIt ) ExecThis = ( When == 'before saving') 
           IF( ExecThis ) CALL SolverActivate( CurrentModel,Solver,dt,Transient )
        END DO
     END IF

!------------------------------------------------------------------------------
   END SUBROUTINE ExecSimulation
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Saves current timestep to external files.
!------------------------------------------------------------------------------
  SUBROUTINE SaveCurrent( CurrentStep )
!------------------------------------------------------------------------------
    INTEGER :: i, j,k,l,n,q,CurrentStep,nlen
    TYPE(Variable_t), POINTER :: Var
    LOGICAL :: EigAnal, GotIt
    CHARACTER(LEN=MAX_NAME_LEN) :: Simul
    LOGICAL :: BinaryOutput, SaveAll
    
    Simul = ListGetString( CurrentModel % Simulation, 'Simulation Type' )
    
    OutputFile = ListGetString(CurrentModel % Simulation,'Output File',GotIt)
    IF ( GotIt ) THEN
      IF ( ParEnv % PEs > 1 ) THEN
        DO i=1,MAX_NAME_LEN
          IF ( OutputFile(i:i) == ' ' ) EXIT
        END DO
        OutputFile(i:i) = '.'
        WRITE( OutputFile(i+1:), '(a)' ) TRIM(i2s(ParEnv % MyPE))
      END IF
      
      BinaryOutput = ListGetLogical( CurrentModel % Simulation,'Binary Output',GotIt )
      IF ( .NOT.GotIt ) BinaryOutput = .FALSE.
      
      SaveAll = .NOT.ListGetLogical( CurrentModel % Simulation,&
          'Omit unchanged variables in output',GotIt )
      IF ( .NOT.GotIt ) SaveAll = .TRUE.
      
      Mesh => CurrentModel % Meshes
      DO WHILE( ASSOCIATED( Mesh ) ) 
        IF ( Mesh % OutputActive ) THEN
          nlen = LEN_TRIM(Mesh % Name )
          IF ( nlen > 0 ) THEN
            OutputName = Mesh % Name(1:nlen) // '/' // TRIM(OutputFile)
          ELSE
            OutputName = OutputFile
          END IF
          
          EigAnal = .FALSE.
          DO i=1,CurrentModel % NumberOfSolvers
            IF ( ASSOCIATED( CurrentModel % Solvers(i) % Mesh, Mesh ) ) THEN
              EigAnal = ListGetLogical( CurrentModel % Solvers(i) % Values, &
                  'Eigen Analysis', GotIt )
              EigAnal = EigAnal .OR. ListGetLogical( CurrentModel % Solvers(i) % Values, &
                  'Harmonic Analysis', GotIt )
              
              IF ( EigAnal ) THEN
                Var => CurrentModel % Solvers(i) % Variable
                IF ( ASSOCIATED(Var % EigenValues) ) THEN
                  IF ( TotalTimesteps == 1 ) THEN
                    DO j=1,CurrentModel % Solvers(i) % NOFEigenValues
                      IF ( CurrentModel % Solvers(i) % Matrix % COMPLEX ) THEN

                        n = SIZE(Var % Values)/Var % DOFs
                        DO k=1,n
                          DO l=1,Var % DOFs/2
                            q = Var % DOFs*(k-1)
                            Var % Values(q+l) = REAL(Var % EigenVectors(j,q/2+l))
                            Var % Values(q+l+Var % DOFs/2) = AIMAG(Var % EigenVectors(j,q/2+l))
                          END DO
                        END DO
                      ELSE
                        Var % Values = REAL( Var % EigenVectors(j,:) )
                      END IF
                      SavedSteps = SaveResult( OutputName, Mesh, &
                          j, sTime(1), BinaryOutput, SaveAll )
                    END DO
                  ELSE
                    j = MIN( CurrentStep, SIZE( Var % EigenVectors,1 ) )
                    IF ( CurrentModel % Solvers(i) % Matrix % COMPLEX ) THEN
                      n = SIZE(Var % Values)/Var % DOFs
                      DO k=1,n
                        DO l=1,Var % DOFs/2
                          q = Var % DOFs*(k-1)
                          Var % Values(q+l) = REAL(Var % EigenVectors(j,q/2+l))
                          Var % Values(q+l+Var % DOFs/2) = AIMAG(Var % EigenVectors(j,q/2+l))
                        END DO
                      END DO
                    ELSE
                      Var % Values = REAL(Var % EigenVectors(j,:))
                    END IF
                    SavedSteps = SaveResult( OutputName, Mesh, &
                        CurrentStep, sTime(1), BinaryOutput, SaveAll )
                  END IF
                  Var % Values = 0.0d0
                END IF
              END IF
            END IF
          END DO
          
          IF ( .NOT. EigAnal ) THEN
            SavedSteps = SaveResult( OutputName,Mesh, NINT(sStep(1)), &
                sTime(1), BinaryOutput, SaveAll )
          END IF
        END IF
        Mesh => Mesh % Next
      END DO
    ELSE
      Mesh => CurrentModel % Meshes
      DO WHILE( ASSOCIATED( Mesh ) ) 
        IF ( Mesh % OutputActive ) &
            Mesh % SavesDone=Mesh % SavesDone+1
        Mesh => Mesh % Next
      END DO
    END IF
    CALL SaveToPost(CurrentStep)
!------------------------------------------------------------------------------
  END SUBROUTINE SaveCurrent
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!> Saves results file to post processing file of ElmerPost format, if requested.
!------------------------------------------------------------------------------
  SUBROUTINE SaveToPost(CurrentStep)
!------------------------------------------------------------------------------
    TYPE(Variable_t), POINTER :: Var
    LOGICAL :: EigAnal = .FALSE., Found
    INTEGER :: i, j,k,l,n,q,CurrentStep,nlen,timesteps,SavedEigenValues
    CHARACTER(LEN=MAX_NAME_LEN) :: Simul, SaveWhich
    
    Simul = ListGetString( CurrentModel % Simulation,  'Simulation Type' )

    OutputFile = ListGetString( CurrentModel % Simulation,'Output File',GotIt )
    IF ( Gotit ) THEN
      IF ( ParEnv % PEs > 1 ) THEN
        DO i=1,MAX_NAME_LEN
          IF ( OutputFile(i:i) == ' ' ) EXIT
        END DO
        OutputFile(i:i) = '.'
        WRITE( OutputFile(i+1:), '(a)' ) TRIM(i2s(ParEnv % MyPE))
      END IF
    END IF
    
    PostFile = ListGetString( CurrentModel % Simulation,'Post File',GotIt )
    IF( .NOT. GotIt ) RETURN

    IF ( ParEnv % PEs > 1 ) THEN
      DO i=1,MAX_NAME_LEN
        IF ( PostFile(i:i) == ' ' ) EXIT
      END DO
      PostFile(i:i) = '.'
      WRITE( PostFile(i+1:), '(a)' ) TRIM(i2s(ParEnv % MyPE))
    END IF

    ! Loop over all meshes
    !---------------------------------------------------
    Mesh => CurrentModel % Meshes
    DO WHILE( ASSOCIATED( Mesh ) )
      IF ( CurrentStep == 0 .AND. Mesh % SavesDone > 0) THEN
        Mesh => Mesh % Next
        CYCLE
      END IF

      ! Check whether this mesh is active for saving
      !--------------------------------------------------
      IF ( Mesh % OutputActive ) THEN
        nlen = LEN_TRIM(Mesh % Name)
        IF ( nlen==0 .OR. FileNameQualified(OutputFile) ) THEN
          OutputName = OutputFile
        ELSE
          OutputName = Mesh % Name(1:nlen)//'/'//TRIM(OutputFile)
        END IF
        
        IF ( nlen==0 .OR. FileNameQualified(PostFile) ) THEN
          PostName = PostFile
        ELSE
          Postname = Mesh % Name(1:nlen)//'/'//TRIM(PostFile)
        END IF
        
        IF ( ListGetLogical( CurrentModel % Simulation,'Filename Numbering',GotIt) ) THEN
          IF( CurrentStep == 0 ) THEN
            PostName = NextFreeFilename(PostName)
          ELSE 
            PostName = NextFreeFilename(PostName,LastExisting = .TRUE. ) 
          END IF
        END IF

        ! Set the Mesh pointer in the CurrentModel 
        CALL SetCurrentMesh( CurrentModel, Mesh )

        ! Use number of timesteps or number of eigenmodes
        !------------------------------------------------
        EigAnal = .FALSE.
        timesteps = TotalTimeSteps
        DO i=1,CurrentModel % NumberOfSolvers
          IF (ASSOCIATED(CurrentModel % Solvers(i) % Mesh, Mesh)) THEN
            EigAnal = ListGetLogical( CurrentModel % &
                Solvers(i) % Values, 'Eigen Analysis', GotIt )
            
            EigAnal = EigAnal .OR. ListGetLogical( CurrentModel % &
                Solvers(i) % Values, 'Harmonic Analysis', GotIt )
            
            IF ( EigAnal ) timesteps = MAX( timesteps, &
                CurrentModel % Solvers(i) % NOFEigenValues )
          END IF
        END DO

        DO i=1,CurrentModel % NumberOfSolvers
          IF (ASSOCIATED(CurrentModel % Solvers(i) % Mesh, Mesh)) THEN
            EigAnal = ListGetLogical( CurrentModel % &
                Solvers(i) % Values, 'Eigen Analysis', GotIt )
            
            EigAnal = EigAnal .OR. ListGetLogical( CurrentModel % &
                Solvers(i) % Values, 'Harmonic Analysis', GotIt )
            
            IF ( EigAnal ) THEN
              SaveWhich = ListGetString( CurrentModel % Solvers(i) % Values, &
                  'Eigen and Harmonic Solution Output', Found )
              
              SavedEigenValues = CurrentModel % Solvers(i) % NOFEigenValues
              IF( TotalTimesteps > 1 ) THEN
!                SavedEiegnValues = MIN( CurrentStep, SIZE( Var % EigenVectors,1 ) )
              END IF

              DO j=1, SavedEigenValues
                Var => Mesh % Variables
                DO WHILE(ASSOCIATED(Var))
                  IF ( .NOT. ASSOCIATED(Var % EigenValues) ) THEN
                    Var => Var % Next
                    CYCLE
                  END IF
                  
                  IF ( CurrentModel % Solvers(i) % Matrix % COMPLEX ) THEN
                    IF(Var % DOFs==1) THEN
                      Var => Var % Next
                      CYCLE
                    END IF

                    n = SIZE(Var % Values)/Var % DOFs
                    DO k=1,n
                      DO l=1,Var % DOFs/2
                        q = Var % DOFs*(k-1)
                        Var % Values(q+l) = REAL(Var % EigenVectors(j,q/2+l))
                        Var % Values(q+l+Var % DOFs/2) = AIMAG(Var % EigenVectors(j,q/2+l))
                      END DO
                    END DO

                  ELSE
                    SELECT CASE( SaveWhich )
                    CASE('real part')
                      Var % Values = Var % EigenVectors(j,:)
                    CASE('imag part')
                      Var % Values = AIMAG(Var % EigenVectors(j,:))
                    CASE('abs value')
                      Var % Values = ABS(Var % EigenVectors(j,:))
                    CASE('phase angle')
                      Var % Values = ATAN2(AIMAG(Var % EigenVectors(j,:)), &
                          REAL(Var % EigenVectors(j,:)))
                    CASE DEFAULT
                      Var % CValues => Var % EigenVectors(j,:)
                    END SELECT
                  END IF
                  Var => Var % Next
                END DO
                
                IF ( CurrentStep > 0 ) THEN
                  IF ( Mesh % SavesDone /= 0 ) THEN
                    IF( TotalTimeSteps == 1 ) THEN
                      Mesh % SavesDone = j
                    ELSE
                      Mesh % SavesDone = CurrentStep
                    END IF
                  END IF
                  CALL WritePostFile( PostName,OutputName, CurrentModel, &
                      CurrentModel % Solvers(i) % NOFEigenValues, .TRUE. )
                END IF
              END DO
              EXIT
            END IF
          END IF
        END DO
        
        ! If this mesh has not been saved, then do so
        !----------------------------------------------------------------------------
        IF ( .NOT. EigAnal .OR. CurrentStep == 0 ) THEN
          WRITE(6,*) 'timesteps = ', timesteps
          CALL WritePostFile( PostName, OutputName, CurrentModel, timesteps, .TRUE. )
        END IF

        Var => Mesh % Variables
        DO WHILE(ASSOCIATED(Var))
          IF ( ASSOCIATED(Var % EigenValues) ) THEN
            Var % Values = 0._dp
            Var % CValues => NULL()
          END IF
          Var => Var % Next
        END DO
      END IF
      Mesh => Mesh % Next
    END DO
!------------------------------------------------------------------------------
  END SUBROUTINE SaveToPost
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> Release a mesh from the list of meshes.
!------------------------------------------------------------------------------
!   RECURSIVE SUBROUTINE FreeMesh(Mesh)
! !------------------------------------------------------------------------------
!     TYPE(Mesh_t), POINTER :: Mesh
! !------------------------------------------------------------------------------
!     IF (.NOT.ASSOCIATED(Mesh)) RETURN

!     CALL FreeMesh(Mesh % Next)

!     Mesh % Next   => NULL()
!     Mesh % Child  => NULL()
!     Mesh % Parent => NULL()

!     CALL ReleaseMesh(Mesh)
!     DEALLOCATE(Mesh)
! !------------------------------------------------------------------------------
!   END SUBROUTINE FreeMesh
! !------------------------------------------------------------------------------


! !------------------------------------------------------------------------------
! !> Releases structures related to the Solver. 
! !------------------------------------------------------------------------------
!   SUBROUTINE FreeSolver(Solver)
! !------------------------------------------------------------------------------
!     TYPE(Solver_t) :: Solver
! !------------------------------------------------------------------------------
!     CALL FreeValueList(Solver % Values)
!     CALL FreeMatrix(Solver % Matrix)
!     IF (ALLOCATED(Solver % Def_Dofs)) DEALLOCATE(Solver % Def_Dofs)
!     IF (ASSOCIATED(Solver % ActiveElements)) DEALLOCATE(Solver % ActiveElements)
! !------------------------------------------------------------------------------
!   END SUBROUTINE FreeSolver
! !------------------------------------------------------------------------------


! !------------------------------------------------------------------------------
! !> Releases value list which includes all the sif definitions, for example.
! !------------------------------------------------------------------------------
!   SUBROUTINE FreeValueList(List)
! !------------------------------------------------------------------------------
!     TYPE(ValueList_t), POINTER :: List
! !------------------------------------------------------------------------------
!     TYPE(ValueList_t), POINTER :: ptr
   
!     ptr => List
!     DO WHILE(ASSOCIATED(ptr))
!       IF (ASSOCIATED(ptr % TValues)) DEALLOCATE(ptr % TValues)
!       IF (ASSOCIATED(ptr % FValues)) DEALLOCATE(ptr % FValues)
!       IF (ASSOCIATED(ptr % IValues)) DEALLOCATE(ptr % IValues)
!       ptr => ptr % Next
!     END DO 
! !------------------------------------------------------------------------------
!   END SUBROUTINE FreeValueList
! !------------------------------------------------------------------------------


! !------------------------------------------------------------------------------
! !> Releases the whole model. 
! !------------------------------------------------------------------------------
!  SUBROUTINE FreeModel(Model)
! !------------------------------------------------------------------------------
!    TYPE(Model_t), POINTER :: Model
! !------------------------------------------------------------------------------
!    TYPE(Matrix_t), POINTER :: A,B
!    INTEGER :: i
!    IF (.NOT.ASSOCIATED(Model)) RETURN

!    CALL FreeMesh(Model % Meshes)

!    CALL FreeValueList(Model % Constants)
!    CALL FreeValueList(Model % Simulation)

!    IF (ASSOCIATED(Model % BCs)) THEN
!      DO i=1,Model % NumberOfBCs
! #if 0
!        A => Model % BCs(i) % PMatrix
!        IF (ASSOCIATED(A)) THEN
!          DO WHILE( ASSOCIATED(A) )
!            B => A % Child
!            A % Child => NULL()
!            A => B
!          END DO
!          CALL FreeMatrix(Model % BCs(i) % PMatrix)
!        END IF
! #endif
!        CALL FreeValueList( Model % BCs(i) % Values)
!      END DO
!      DEALLOCATE(Model % BCs)
!    END IF

!    DO i=1,Model % NumberOfSolvers
!      CALL FreeSolver(Model % Solvers(i))
!    END DO
!    DEALLOCATE(Model % Solvers)

!    IF (ASSOCIATED(Model % ICs)) THEN
!      DO i=1,Model % NumberOfICs
!        CALL FreeValueList( Model % ICs(i) % Values)
!      END DO
!      DEALLOCATE(Model % ICs)
!    END IF

!    IF (ASSOCIATED(Model % Bodies)) THEN
!      DO i=1,Model % NumberOfBodies
!        CALL FreeValueList( Model % Bodies(i) % Values)
!      END DO
!      DEALLOCATE(Model % Bodies)
!    END IF

!    IF (ASSOCIATED(Model % Equations)) THEN
!      DO i=1,Model % NumberOfEquations
!        CALL FreeValueList( Model % Equations(i) % Values)
!      END DO
!      DEALLOCATE(Model % Equations)
!    END IF

!    IF (ASSOCIATED(Model % BodyForces)) THEN
!      DO i=1,Model % NumberOfBodyForces
!        CALL FreeValueList( Model % BodyForces(i) % Values)
!      END DO
!      DEALLOCATE(Model % BodyForces)
!    END IF

!    Model=>NULL()
! !------------------------------------------------------------------------------
!  END SUBROUTINE FreeModel
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!> Function to read the complete Elmer model: sif file and mesh files.
!------------------------------------------------------------------------------
  FUNCTION ElmerLoadModel(ModelName,BoundariesOnly,numprocs,mype ) RESULT( Model )
!------------------------------------------------------------------------------
    USE TESTOBJECT

    IMPLICIT NONE

    TYPE(t_global), POINTER :: global

    CHARACTER(LEN=*) :: ModelName
    LOGICAL :: BoundariesOnly

    INTEGER, OPTIONAL :: numprocs,mype
 
    TYPE(Model_t), POINTER :: Model

!------------------------------------------------------------------------------
    TYPE(Mesh_t), POINTER :: Mesh,Mesh1,NewMesh,OldMesh
    INTEGER :: i,j,k,l,s,nlen,eqn,MeshKeep,MeshLevels
    LOGICAL :: GotIt,GotMesh,found,OneMeshName, OpenFile, Transient
    LOGICAL :: stat, single, MeshGrading, NewLoadMesh
    TYPE(Solver_t), POINTER :: Solver
    INTEGER(KIND=AddrInt) :: InitProc
    INTEGER, TARGET :: Def_Dofs(10,6)
    REAL(KIND=dp) :: MeshPower
    REAL(KIND=dp), POINTER :: h(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: Name,ElementDef,ElementDef0
    CHARACTER(LEN=MAX_NAME_LEN) :: MeshDir,MeshName
    TYPE(valuelist_t), POINTER :: lst
    INTEGER, ALLOCATABLE :: EdgeDOFs(:),FaceDOFs(:)
!------------------------------------------------------------------------------

    ALLOCATE( Model )
    CurrentModel => Model
    NULLIFY( Model % Variables )

    MeshDir  = ' '
    MeshName = ' '

    Model % DIMENSION = 0
    Model % NumberOfBoundaries = 0
    Model % NumberOfBodies     = 0
    Model % NumberOfICs        = 0
    Model % NumberOfBCs        = 0
    Model % NumberOfEquations  = 0
    Model % NumberOfSolvers    = 0
    Model % NumberOfMaterials  = 0
    Model % NumberOfBodyForces = 0

    INQUIRE( Unit=InFileUnit, OPENED=OpenFile )
    IF ( .NOT. OpenFile ) OPEN( Unit=InFileUnit, File=Modelname, STATUS='OLD' )
    CALL LoadInputFile( Model,InFileUnit,ModelName,MeshDir,MeshName, .TRUE., .TRUE. )
    REWIND( InFileUnit )
    CALL LoadInputFile( Model,InFileUnit,ModelName,MeshDir,MeshName, .TRUE., .FALSE. )
    IF ( .NOT. OpenFile ) CLOSE( InFileUnit )

    CALL InitializeOutputLevel()

    Transient=ListGetString(Model % Simulation, &
        'Simulation Type',Found)=='transient'

    Def_Dofs = -1; Def_Dofs(:,1)=1

    i = 1
    DO WHILE(i<=Model % NumberOFSolvers)

      Solver => Model % Solvers(i)
      Model % Solver => Solver

      Name = ListGetString( Solver  % Values, 'Procedure', Found )
      IF ( Found ) THEN
        InitProc = GetProcAddr( TRIM(Name)//'_Init0', abort=.FALSE. )
        IF ( InitProc /= 0 ) THEN
          CALL ExecSolver( InitProc, Model, Solver, &
                  Solver % dt, Transient )

          Solver => Model % Solvers(i)
          Model % Solver => Solver
        END IF
      END IF

      GotMesh = ListCheckPresent(Solver % Values, 'Mesh')

      IF(.NOT.ALLOCATED(Solver % Def_Dofs)) THEN
        ALLOCATE(Solver % Def_Dofs(10,Model % NumberOfBodies,6))
        Solver % Def_Dofs = -1; Solver % Def_Dofs(:,:,1)=1
      END IF

      ! Define what kind of element we are working with in this solver
      !-----------------------------------------------------------------
      ElementDef = ListGetString( Solver % Values, 'Element', stat )
   
      IF ( .NOT. stat ) THEN
        IF ( ListGetLogical( Solver % Values, &
             'Discontinuous Galerkin', stat ) ) THEN
           Solver % Def_Dofs(:,:,4) = 0
           IF ( .NOT. GotMesh ) Def_Dofs(:,4) = MAX(Def_Dofs(:,4),0 )
           i=i+1
           CYCLE
        ELSE
           ElementDef = "n:1"
        END IF
      END IF

      ElementDef0 = ElementDef
      DO WHILE(.TRUE.)
        j = INDEX( ElementDef0, '-' )
        IF (j>0) THEN
          ElementDef = ElementDef0(1:j-1)
        ELSE
          ElementDef = ElementDef0
        END IF
        CALL GetDefs( ElementDef )
        IF(j>0) THEN
          ElementDef0 = ElementDef0(j+1:)
         ELSE
          EXIT
        END IF
      END DO

      i = i + 1
    END DO

    ! Check the mesh 
    !--------------------------------------------------------
    Name = ListGetString( Model % Simulation, 'Mesh', GotIt )

    OneMeshName = .FALSE.
    IF ( GotIt ) THEN
      k = 1
      i = 1
      nlen = LEN_TRIM(name)
      DO WHILE( k<=nlen .AND. name(k:k) /= ' ' )
        MeshDir(i:i)  = name(k:k)
        Meshname(i:i) = name(k:k)
        k = k + 1
        i = i + 1
      END DO

      DO WHILE( k<=nlen .AND. Name(k:k) == ' ' )
        k = k + 1
      END DO

      IF ( k<=nlen ) THEN
         MeshName(i:i) = '/'
         i = i + 1
         DO WHILE( name(k:k) /= ' ' )
           MeshName(i:i) = Name(k:k)
           k = k + 1
           i = i + 1
         END DO
      ELSE
         OneMeshName = .TRUE.
         MeshDir = "." // CHAR(0)
      END IF
      MeshName(i:i) = CHAR(0)
    END IF

    NULLIFY( Model % Meshes )
    IF ( MeshDir(1:1) /= ' ' ) THEN

      CALL ResetTimer('LoadMesh') 
      NewLoadMesh = ListGetLogical( Model % Simulation,'New Load Mesh',GotIt) 
      IF( .NOT. GotIt ) NewLoadMesh = .TRUE.
      IF( NewLoadMesh ) THEN
        Model % Meshes => LoadMesh2( Model, MeshDir, MeshName, &
            BoundariesOnly, numprocs, mype, Def_Dofs )
      ELSE
        Model % Meshes => LoadMesh( Model, MeshDir, MeshName, &
            BoundariesOnly, numprocs, mype, Def_Dofs(1,:) )
      END IF

      CALL CheckTimer('LoadMesh',Level=5,Delete=.TRUE.)

      CALL SetCoordinateSystem( Model )

      MeshLevels = ListGetInteger( Model % Simulation, 'Mesh Levels', GotIt )
      IF ( .NOT. GotIt ) MeshLevels=1

      MeshKeep = ListGetInteger( Model % Simulation, 'Mesh keep',  GotIt )
      IF ( .NOT. GotIt ) MeshKeep=MeshLevels

      MeshPower   = ListGetConstReal( Model % Simulation, 'Mesh Grading Power',GotIt)
      MeshGrading = ListGetLogical( Model % Simulation, 'Mesh Keep Grading', GotIt)

      DO i=2,MeshLevels
        OldMesh => Model % Meshes

        IF (MeshGrading) THEN
          ALLOCATE(h(OldMesh % NumberOfNodes))
          Model % Mesh => OldMesh
          CALL GetNodalElementSize(Model,MeshPower,.FALSE.,h)
          NewMesh => SplitMeshEqual(OldMesh,h)
          DEALLOCATE(h)
        ELSE
          NewMesh => SplitMeshEqual(OldMesh)
        END IF

        IF(ASSOCIATED(OldMesh % Faces)) THEN
          CALL FindMeshEdges(NewMesh)

          ALLOCATE( EdgeDOFs(NewMesh % NumberOfBulkElements))
          ALLOCATE( FaceDOFs(NewMesh % NumberOfBulkElements))
          EdgeDOFs = MAX(0,MAXVAL(Def_Dofs(:,2)))
          FaceDOFs = MAX(0,MAXVAL(Def_Dofs(:,3)))
          CALL SetMeshEdgeFaceDofs(NewMesh,EdgeDOFs,FaceDOFs)
          DEALLOCATE(EdgeDOFs,FaceDOFs)

          CALL SetMeshMaxDofs(NewMesh)
          IF(ParEnv % PEs>1) CALL SParEdgeNumbering(NewMesh)
          IF(ParEnv % PEs>1) CALL SParFaceNumbering(NewMesh)
        END IF

        IF ( i>MeshLevels-MeshKeep+1 ) THEN
          NewMesh % Next => OldMesh
          NewMesh % Parent => OldMesh
          OldMesh % Child  => NewMesh
          Newmesh % OutputActive = .TRUE.
          OldMesh % OutputActive = .FALSE.
        ELSE
          CALL ReleaseMesh(OldMesh)
        END IF
        Model % Meshes => NewMesh
      END DO


      IF ( OneMeshName ) THEN
         i = 0
      ELSE
         i = LEN_TRIM(MeshName)
         DO WHILE( i>0 )
            IF (MeshName(i:i) /= '/' ) THEN
               i = i-1
            ELSE	       
               EXIT
            END IF
         END DO
      END IF

      i = i + 1
      k = 1
      Model % Meshes % Name = ' '
      DO WHILE( MeshName(i:i) /= CHAR(0) )
        Model % Meshes % Name(k:k) = MeshName(i:i)
        k = k + 1
        i = i + 1
      END DO

      ! Ok, give name also to the parent meshes as they might be saved too
      OldMesh => Model % Meshes % Parent
      DO WHILE( ASSOCIATED( OldMesh ) )
        OldMesh % Name = TRIM(OldMesh % Child % Name)//'p'
        OldMesh => OldMesh % Parent
      END DO

      DO i=1,Model % NumberOfSolvers
         Model % Solvers(i) % Mesh => Model % Meshes
      END DO
    END IF

    DO s=1,Model % NumberOfSolvers
      Name = ListGetString( Model % Solvers(s) % Values, 'Mesh', GotIt )

      IF( GotIt ) THEN
        WRITE(Message,'(A,I0)') 'Loading solver specific mesh > '//TRIM(Name)// ' < for solver ',s
        CALL Info('ElmerLoadModel',Message,Level=7)

        single=.FALSE.
        IF ( Name(1:8) == '-single ' ) THEN
          single=.TRUE.
          Name=Name(9:)
        END IF
        OneMeshName = .FALSE.
        k = 1
        i = 1
        nlen = LEN_TRIM(name)
        MeshName = ' '
        DO WHILE( k<=nlen .AND. name(k:k) /= ' ' )
          MeshDir(i:i)  = name(k:k)
          Meshname(i:i) = name(k:k)
          k = k + 1
          i = i + 1
        END DO

        DO WHILE( k<=nlen .AND. Name(k:k) == ' ' )
          k = k + 1
        END DO

        IF ( k<=nlen ) THEN
          MeshName(i:i) = '/'
          i = i + 1
          DO WHILE( name(k:k) /= ' ' )
            MeshName(i:i) = Name(k:k)
            k = k + 1
            i = i + 1
          END DO
        ELSE
          OneMeshName = .TRUE.
          MeshDir = "." // CHAR(0)
        END IF
        MeshName(i:i) = CHAR(0)

        IF ( OneMeshName ) THEN
          i = 0
        ELSE
          DO WHILE( i>0 .AND. MeshName(i:i) /= '/' )
            i = i - 1
          END DO
        END IF

        Mesh => Model % Meshes
        Found = .FALSE.
        DO WHILE( ASSOCIATED( Mesh ) )
           Found = .TRUE.
           k = 1
           j = i+1
           DO WHILE( MeshName(j:j) /= CHAR(0) )
              IF ( Mesh % Name(k:k) /= MeshName(j:j) ) THEN
                Found = .FALSE.
                EXIT
              END IF
              k = k + 1
              j = j + 1
           END DO
           IF ( LEN_TRIM(Mesh % Name) /= k-1 ) Found = .FALSE.
           IF ( Found ) EXIT
           Mesh => Mesh % Next
        END DO

        IF ( Found ) THEN
          Model % Solvers(s) % Mesh => Mesh
          CYCLE
        END IF

        DO i=1,6
          DO j=1,8
            Def_Dofs(j,i) = MAXVAL(Model % Solvers(s) % Def_Dofs(j,:,i))
          END DO
        END DO

        NewLoadMesh = ListGetLogical( Model % Simulation,'New Load Mesh',GotIt)
        IF(.NOT. GotIt) NewLoadMesh = .TRUE.
        IF ( Single ) THEN
          IF( NewLoadMesh ) THEN
            Model % Solvers(s) % Mesh => &
              LoadMesh2( Model,MeshDir,MeshName,BoundariesOnly,1,0,def_dofs )
          ELSE
            Model % Solvers(s) % Mesh => &
              LoadMesh( Model,MeshDir,MeshName,BoundariesOnly,1,0,def_dofs(1,:) )
          END IF
        ELSE
          IF( NewLoadMesh ) THEN
            Model % Solvers(s) % Mesh => &
              LoadMesh2( Model,MeshDir,MeshName,BoundariesOnly,numprocs,mype,Def_Dofs )
          ELSE
            Model % Solvers(s) % Mesh => &
              LoadMesh( Model,MeshDir,MeshName,BoundariesOnly,numprocs,mype,Def_Dofs(1,:) )
          END IF
        END IF
        Model % Solvers(s) % Mesh % OutputActive = .TRUE.


        MeshLevels = ListGetInteger( Model % Solvers(s) % Values, 'Mesh Levels', GotIt )
        IF ( .NOT. GotIt ) MeshLevels=1

        MeshKeep = ListGetInteger( Model % Solvers(s) % Values, 'Mesh keep',  GotIt )
        IF ( .NOT. GotIt ) MeshKeep=MeshLevels

        MeshPower   = ListGetConstReal( Model % Simulation, 'Mesh Grading Power',GotIt)
        MeshGrading = ListGetLogical( Model % Simulation, 'Mesh Keep Grading', GotIt)


        DO i=2,MeshLevels
          OldMesh => Model % Solvers(s) % Mesh

          IF (MeshGrading) THEN
            ALLOCATE(h(OldMesh % NumberOfNodes))
            Model % Mesh => OldMesh
            CALL GetNodalElementSize(Model,MeshPower,.FALSE.,h)
            NewMesh => SplitMeshEqual(OldMesh,h)
            DEALLOCATE(h)
          ELSE
            NewMesh => SplitMeshEqual(OldMesh)
          END IF

          IF(ASSOCIATED(OldMesh % Faces)) THEN
            CALL FindMeshEdges(NewMesh)

            ALLOCATE( EdgeDOFs(NewMesh % NumberOfBulkElements))
            ALLOCATE( FaceDOFs(NewMesh % NumberOfBulkElements))
            EdgeDOFs = MAX(0,MAXVAL(Def_Dofs(:,2)))
            FaceDOFs = MAX(0,MAXVAL(Def_Dofs(:,3)))
            CALL SetMeshEdgeFaceDofs(NewMesh,EdgeDOFs,FaceDOFs)
            DEALLOCATE(EdgeDOFs,FaceDOFs)

            CALL SetMeshMaxDofs(NewMesh)
            IF(ParEnv % PEs>1) CALL SParEdgeNumbering(NewMesh)
            IF(ParEnv % PEs>1) CALL SParFAceNumbering(NewMesh)
          END IF

          IF ( i>MeshLevels-MeshKeep+1 ) THEN
            NewMesh % Next => OldMesh
            NewMesh % Parent => OldMesh
            OldMesh % Child  => NewMesh
            NewMesh % Name = OldMesh % Name
            Newmesh % OutputActive = .TRUE.
            OldMesh % OutputActive = .FALSE.
          ELSE
            CALL ReleaseMesh(OldMesh)
          END IF
          Model % Solvers(s) % Mesh => NewMesh
        END DO

        IF ( OneMeshName ) i = 0

        k = 1
        i = i + 1
        Model % Solvers(s) % Mesh % Name = ' '
        DO WHILE( MeshName(i:i) /= CHAR(0) )
          Model % Solvers(s) % Mesh % Name(k:k) = MeshName(i:i)
          k = k + 1
          i = i + 1
        END DO

        IF ( ASSOCIATED( Model % Meshes ) ) THEN
          Mesh1 => Model % Meshes
          DO WHILE( ASSOCIATED( Mesh1 % Next ) ) 
            Mesh1 => Mesh1 % Next
          END DO
          Mesh1 % Next => Model % Solvers(s) % Mesh
        ELSE
          Model % Meshes => Model % Solvers(s) % Mesh
        END IF
      END IF
    END DO

    CALL SetCoordinateSystem( Model )
  
    IF ( OutputPath == ' ' ) THEN
      DO i=1,MAX_NAME_LEN
        IF ( MeshDir(i:i) == CHAR(0) ) EXIT
        OutputPath(i:i) = MeshDir(i:i)
      END DO
    END IF

    Mesh => Model % Meshes
    DO WHILE( ASSOCIATED( Mesh ) )
       CALL MeshStabParams( Mesh )
       Mesh => Mesh % Next
    END DO
!------------------------------------------------------------------------------

  CONTAINS

!------------------------------------------------------------------------------
    SUBROUTINE GetDefs(ElementDef)
!------------------------------------------------------------------------------
      CHARACTER(LEN=*) :: ElementDef
!------------------------------------------------------------------------------
      INTEGER  :: ind(8),i,j,n
      INTEGER, POINTER :: gdofs(:,:), sdofs(:,:,:)

      ind = [1,2,3,4,5,6,7,8]

      IF (ElementDef(1:5) == 'point' )     ind=1
      IF (ElementDef(1:4) == 'line' )      ind=2
      IF (ElementDef(1:3) == 'tri' )       ind=3
      IF (ElementDef(1:4) == 'quad' )      ind=4
      IF (ElementDef(1:5) == 'tetra' )     ind=5
      IF (ElementDef(1:7) == 'pyramid' )   ind=6
      IF (ElementDef(1:5) == 'prism' )     ind=7
      IF (ElementDef(1:5) == 'brick' )     ind=8
      IF (ElementDef(1:8) == 'tri_face' )  ind=9
      IF (ElementDef(1:9) == 'quad_face' ) ind=10

      n = INDEX(ElementDef,'-')
      IF (n<=0) n=LEN_TRIM(ElementDef)
          
      j = INDEX( ElementDef(1:n), 'n:' )
      IF ( j>0 ) THEN
        READ( ElementDef(j+2:), * ) l
        Solver % Def_Dofs(ind,:,1) = l
        IF (.NOT. GotMesh ) Def_Dofs(ind,1) = MAX(Def_Dofs(ind,1), l)
      END IF
          
      j = INDEX( ElementDef(1:n), 'e:' )
      IF ( j>0 ) THEN
        READ( ElementDef(j+2:), * ) l
        Solver % Def_Dofs(ind,:,2) = l
        IF ( .NOT. GotMesh ) Def_Dofs(ind,2) = MAX(Def_Dofs(ind,2), l )
      END IF
          
      j = INDEX( ElementDef(1:n), 'f:' )
      IF ( j>0 ) THEN
        READ( ElementDef(j+2:), * ) l
        Solver % Def_Dofs(ind,:,3) = l
        IF ( .NOT. GotMesh ) Def_Dofs(ind,3) = MAX(Def_Dofs(ind,3), l )
      END IF
          
      j = INDEX( ElementDef(1:n), 'd:' )
      IF ( j>0 ) THEN
        READ( ElementDef(j+2:), * ) l
        Solver % Def_Dofs(ind,:,4) = l
        IF ( .NOT. GotMesh ) Def_Dofs(ind,4) = MAX(Def_Dofs(ind,4), l )
      ELSE 
        IF ( ListGetLogical( Solver % Values, &
            'Discontinuous Galerkin', stat ) ) THEN
          Solver % Def_Dofs(ind,:,4) = 0
          IF ( .NOT. GotMesh ) Def_Dofs(ind,4) = MAX(Def_Dofs(ind,4),0 )
        END IF
      END IF
          
      j = INDEX( ElementDef(1:n), 'b:' )
      IF ( j>0 ) THEN
        READ( ElementDef(j+2:), * ) l
        Solver % Def_Dofs(ind,:,5) = l
        IF ( .NOT. GotMesh ) Def_Dofs(ind,5) = MAX(Def_Dofs(ind,5), l )
      END IF
          
      j = INDEX( ElementDef(1:n), 'p:' )
      IF ( j>0 ) THEN
        IF ( ElementDef(j+2:j+2)=='%' ) THEN
          Solver % Def_Dofs(ind,:,6) = 0
        ELSE
          READ( ElementDef(j+2:), * ) l
          Solver % Def_Dofs(ind,:,6) = l
          IF ( .NOT. GotMesh ) Def_Dofs(ind,6) = MAX(Def_Dofs(ind,6), l )
         END IF
      END IF

!------------------------------------------------------------------------------
    END SUBROUTINE GetDefs
!------------------------------------------------------------------------------

    
    !------------------------------------------------------------------------------
    !      Initialize the log file output system for Messages
    !------------------------------------------------------------------------------
    SUBROUTINE InitializeOutputLevel( )
      
      INTEGER, POINTER :: OutputMask(:)
    
       
      MinOutputLevel = ListGetInteger( CurrentModel % Simulation, &
          'Min Output Level', GotIt )
      
      MaxOutputLevel = ListGetInteger( CurrentModel % Simulation, &
          'Max Output Level', GotIt )
      
      IF ( .NOT. GotIt ) MaxOutputLevel = 10
      
      OutputMask => ListGetIntegerArray( CurrentModel % Simulation, &
          'Output Level', GotIt )
      
      IF ( GotIt ) THEN
        DO i=1,SIZE(OutputMask)
          OutputLevelMask(i-1) = OutputMask(i) /= 0
        END DO
      END IF
      
      DO i=0,31
        OutputLevelMask(i) = OutputLevelMask(i) .AND. &
            i >= MinOutputLevel .AND. i <= MaxOutputLevel
      END DO
      
      OutputPrefix = ListGetLogical( CurrentModel % Simulation, &
          'Output Prefix', GotIt )
      IF ( .NOT. GotIt ) OutputPrefix = .FALSE.
      
      OutputCaller = ListGetLogical( CurrentModel % Simulation, &
          'Output Caller', GotIt )
      IF ( .NOT. GotIt ) OutputCaller = .TRUE.
      
    END SUBROUTINE InitializeOutputLevel
!------------------------------------------------------------------------------
  END FUNCTION ElmerLoadModel
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  END MODULE GeneralModule
!------------------------------------------------------------------------------

!> \} ElmerLib
