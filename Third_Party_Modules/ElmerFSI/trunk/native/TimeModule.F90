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
   MODULE TimeModule
!------------------------------------------------------------------------------

     USE GeneralModule

!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------
     REAL(KIND=dp) :: ddt
     INTEGER :: timeleft,cum_timestep
     INTEGER, SAVE ::  stepcount=0, RealTimestep
     LOGICAL :: SteadyStateReached=.FALSE.

     REAL(KIND=dp) :: CumTime, MaxErr, AdaptiveLimit, &
           AdaptiveMinTimestep, AdaptiveMaxTimestep, timePeriod
     INTEGER :: SmallestCount, AdaptiveKeepSmallest, StepControl=-1
     LOGICAL :: AdaptiveTime = .TRUE.

     REAL(KIND=dp) :: newtime, prevtime=0, maxtime, exitcond
     REAL(KIND=dp), ALLOCATABLE :: xx(:,:), xxnrm(:), yynrm(:), PrevXX(:,:,:)

    !Something I (Jess) am adding in order to see if I can access
    !the displacements from this level
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Variable_t), POINTER :: StressSol
    CHARACTER(LEN=MAX_NAME_LEN) :: Equation
    REAL(KIND=dp), POINTER :: Displacement(:)
    INTEGER :: BoundaryElementCount, ElementCount, body_id
    INTEGER :: bc_id, nElementNodes, nt, nk
    INTEGER, POINTER :: MyIndex(:), MyNodeIndexes(:), MyPerm(:)
    TYPE(Element_t), POINTER :: Element, MyCurrentElement
    TYPE(Nodes_t), SAVE :: ElementNodes
    TYPE(Mesh_t), POINTER :: MyMesh
    LOGICAL :: IsFSI
    DOUBLE PRECISION :: FinalTime, PreviousTime
    !End of Jess stuff

!------------------------------------------------------------------------------
  CONTAINS
!------------------------------------------------------------------------------

  SUBROUTINE UpdateLoads(global,runs)
     USE TESTOBJECT
     USE Lists
  
     IMPLICIT NONE
  
     INCLUDE 'comf90.h'
  
     TYPE(t_global), POINTER :: global
     INTEGER :: runs
     REAL(KIND=dp), ALLOCATABLE  :: F(:)
     INTEGER :: MyN, CurrentInterval
     TYPE(ValueList_t), POINTER :: ptr

     !For Rocstar
     INTEGER :: t, bc_id, nt, bndElmCntr, nindex
    
     ! startup blurb 
     IF (MyVerbosity > 1) THEN
       CALL Info('ElimerCSCParallel:UpdateLoads', ' Starting')
     END IF
     IF (MyVerbosity > 3) THEN
       WRITE(6,*) 'sTime(1) = ', sTime(1)
       WRITE(6,*) 'FinalTime = ', FinalTime
       WRITE(6,*) 'PreviousTime = ', PreviousTime
     END IF
 
     !printing the loads as a check
     IF( MyVerbosity > 3) THEN
       WRITE(*,*) 'global%NodeLoads:'
       DO t = 1, global%nNodes
           WRITE(*,*) global%NodeLoads(3*(t-1) + 1), global%NodeLoads(3*(t-1) + 2), &
           global%NodeLoads(3*(t-1) + 3)
       END DO
     END IF
     IF( MyVerbosity > 3) THEN
       WRITE(*,*) 'global%PreviousLoads:'
       DO t = 1, global%nNodes
         DO j =1,3
           WRITE(*,*) global%PreviousLoads(j,t)
         END DO
       END DO
     END IF
  
     !Setting the loads accessible by Elmer to the interpolated value
     !for the current time
     CALL Info('ElmerCSCParallel:UpdateLoads', ' Time Step Summary')
     IF (global%procId == 0) THEN
        WRITE(*,*) '    sTime(1)     = ', sTime(1)
        WRITE(*,*) '    PreviousTime = ', PreviousTime
        WRITE(*,*) '    FinalTime    = ', FinalTime
     END IF

     !ElmerFoamFSI
     ! only works when global%ts_alp (traction vector) is defined over the nodes
     !DO t = 1, global%nNodes
     !  j=1
     !  DO j =1,3
     !      !CurrentModel % NodeLoadsPass(j,t) = global%PreviousLoads(j,t) + &
     !      !  (global%NodeLoads(3*(t-1) + j) - global%PreviousLoads(j,t))&
     !      !  *(sTime(1) - PreviousTime)/(FinalTime - PreviousTime)
     !      CurrentModel % NodeLoadsPass(j,t) = global%ts_alp(3*(t-1) + j)
     !  END DO
     !END DO
     !ElmerFoamFSI End

     !Rocstar
     ! Projecting element center values to all nodes directly
     !Loop over the boundary elements
     bndElmCntr = 0
     MyMesh => CurrentModel % Meshes
     DO t = MyMesh % NumberOfBulkElements + 1, &
            MyMesh % NumberOfBulkElements + &
            MyMesh % NumberOfBoundaryElements
  
        MyCurrentElement => MyMesh % Elements(t)
        bc_id = GetBCId(MyCurrentElement)

        IF ( bc_id == global%FSIbcId ) THEN
           bndElmCntr = bndElmCntr + 1
           MyNodeIndexes => MyCurrentElement % NodeIndexes
           DO nt = 1,MyCurrentElement % TYPE % NumberOfNodes
             nindex = CurrentModel%ElmerToMyNodes(MyNodeIndexes(nt))
             CurrentModel % NodeLoadsPass(1,nindex) = global%ts_alp(3*(bndElmCntr-1) + 1)
             CurrentModel % NodeLoadsPass(2,nindex) = global%ts_alp(3*(bndElmCntr-1) + 2)
             CurrentModel % NodeLoadsPass(3,nindex) = global%ts_alp(3*(bndElmCntr-1) + 3)
           END DO
        END IF
     END DO
     !Rocstar End

     CALL Info('ElmerCSCParallel:UpdateLoads', 'Ending')

  END SUBROUTINE UpdateLoads
!----------------------------------------------------------------------
SUBROUTINE TimeStepper(global,runs)

     USE GeneralModule
     USE TESTOBJECT

!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------
     INCLUDE 'comf90.h'

     TYPE(t_global), POINTER :: global
     INTEGER :: runs

     CALL Info('ElmerCSCParallel:TimeStepper', ' starting')
     DO interval = 1, TimeIntervals
        stepcount = stepcount + Timesteps(interval)
     END DO

     IF (MyVerbosity > 3) THEN
       WRITE(*,*) 'In TimeStepper'
       WRITE(*,*) 'TimeIntervals = ', TimeIntervals 
       WRITE(*,*) 'global%nNodes = ', global%nNodes
     END IF

     cum_Timestep = 0
     ddt = 0.0d0
     DO interval = 1,TimeIntervals

     IF (MyVerbosity > 3) THEN
       WRITE(*,*) 'interval = ', interval
     END IF 
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
      
       IF (MyVerbosity > 3) THEN
          Call Info('ElmerCSCParallel:TimeStepper',' Timesteps(interval) = '//i2s(Timesteps(interval)))
       END IF
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
!        Call UpdateLoads to the get linearly interpolated load value at 
!        the current time
!------------------------------------------------------------------------------
         IF (MyVerbosity > 3) THEN
           WRITE(6,*) 'timestepper timestep = ', timestep
           WRITE(6,*) 'sTime(1) = ', sTime(1)
           WRITE(6,*) 'dtfunc = ', dtfunc
           WRITE(6,*) 'dt = ', dt
           WRITE(*,*) 'TimeStepper Calling UpdateLoads'
         END IF
         CALL UpdateLoads(global,runs)
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
            IF (MyVerbosity > 3) THEN
              WRITE(*,*) 'MyTimeModule:TimeStepper: Calling SolveEquations'
              WRITE(*,*) '   Transient = ', Transient
            END IF
            CALL SolveEquations( CurrentModel, dt, Transient, &
              CoupledMinIter, CoupledMaxIter, SteadyStateReached, RealTimestep )
            RealTimestep = RealTimestep+1
         END IF

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
            IF ( Timestep >= CoupledMinIter ) THEN
              WRITE(6,*) 'SteadyStateReached, exiting'
              EXIT
            END IF
         END IF

!------------------------------------------------------------------------------
       END DO ! timestep within an iterval
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
     END DO ! timestep intervals, i.e. the simulation

     IF (MyVerbosity > 3) THEN
       write(6,*) 'LastSaved = ', LastSaved
     END IF

     runs = 1
     IF (MyVerbosity > 1) THEN
        CALL Info('ElmerCSCParallel:TimeStepper', ' finishing')
     END IF
      
100  RETURN

END SUBROUTINE TimeStepper
!------------------------------------------------------------------------------
  END MODULE TimeModule
!------------------------------------------------------------------------------

!> \} ElmerLib
