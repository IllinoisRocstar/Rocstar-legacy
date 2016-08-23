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
! *  ELMER/FEM Solver finalize program
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
!> The finalize program for Elmer. Solves the equations as defined by the input files.
!------------------------------------------------------------------------------
   SUBROUTINE ElmerFinalize(runs)
!------------------------------------------------------------------------------

     USE GeneralModule

!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------

!     INTEGER :: Initialize

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------

!     INTEGER :: i,j,k,n,l,t,k1,k2,iter,Ndeg,istat,nproc,tlen,nthreads
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
!     REAL(KIND=dp) :: RealTime,CPUTime,CT0,RT0,tt
!
!     LOGICAL :: FirstLoad = .TRUE., FirstTime=.TRUE., Found
!     LOGICAL :: Silent, Version, GotModelName
!
!     INTEGER :: iargc, NoArgs
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

     DO i=1,CurrentModel % NumberOfSolvers
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
!    Always save the last step to output
!------------------------------------------------------------------------------
       IF ( .NOT.LastSaved ) THEN
         DO i=1,CurrentModel % NumberOfSolvers
            Solver => CurrentModel % Solvers(i)
            IF ( Solver % PROCEDURE == 0 ) CYCLE
            ExecThis = ( Solver % SolverExecWhen == SOLVER_EXEC_AHEAD_SAVE)
            When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
            IF ( GotIt ) ExecThis = ( When == 'before saving') 
            IF( ExecThis ) CALL SolverActivate( CurrentModel,Solver,dt,Transient )
         END DO

         CALL SaveToPost(0)
         CALL SaveCurrent(Timestep)

         DO i=1,CurrentModel % NumberOfSolvers
            Solver => CurrentModel % Solvers(i)
            IF ( Solver % PROCEDURE == 0 ) CYCLE
            ExecThis = ( Solver % SolverExecWhen == SOLVER_EXEC_AFTER_SAVE)
            When = ListGetString( Solver % Values, 'Exec Solver', GotIt )
            IF ( GotIt ) ExecThis = ( When == 'after saving') 
            IF( ExecThis ) CALL SolverActivate( CurrentModel,Solver,dt,Transient )
         END DO
       END IF

!       IF ( Initialize >= 2 ) EXIT
    

     
!------------------------------------------------------------------------------
!    THIS IS THE END (...,at last, the end, my friend,...)
!------------------------------------------------------------------------------
     IF ( Initialize /= 1 ) CALL Info( 'MyElmerSolver', '*** Elmer Solver: ALL DONE ***',Level=3 )

     IF ( Initialize <=0 ) CALL FreeModel(CurrentModel)

#ifdef HAVE_TRILINOS
  CALL TrilinosCleanup()
#endif

     IF ( ParEnv % PEs>1 )  CALL ParallelFinalize()
     CALL Info('MyElmerSolver','The end',Level=3)
 
     runs = 1

     RETURN
     

!------------------------------------------------------------------------------
  END SUBROUTINE ElmerFinalize
!------------------------------------------------------------------------------

!> \} ElmerLib
