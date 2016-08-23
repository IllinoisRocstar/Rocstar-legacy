!-------------------------------------------------------------------------------
!> File: LoadFunctionLibrary.F90
!> Written by: ML, 5 May 2010
!> Modified by: Jessica Kress
!-------------------------------------------------------------------------------
FUNCTION LoadXDirection(Model, inindex, Tvalue) RESULT(load)

   USE DefUtils
   USE GeneralModule
 
   IMPLICIT None

   TYPE(Model_t) :: Model
   INTEGER :: inindex, funci
   REAL(KIND=dp) :: Tvalue, load, diff

   Model%UDFUsed = .TRUE.

   load =  Model%NodeLoadsPass(1,Model%ElmerToMyNodes(inindex))
   !WRITE(*,*) load

   IF (MyVerbosity > 3) WRITE(6,*)'Inside [LoadFunctionLibrary]...[LoadXDirection]'

   !Calculate diff for testing purposes
   IF (Tvalue < 8.001) THEN
     diff = float(Model%ElmerToMyNodes(inindex)-1)*3.0d0
     diff = diff*Tvalue/8.0d0
     If (dabs(load - diff) > 1e-12) Model%GetTestLoads = .FALSE.
   ELSE
     diff = float(Model%ElmerToMyNodes(inindex)-1)*3.0d0
     WRITE(*,*) 'diff = ',diff
     diff = diff + (Tvalue- 8.0d0)/92.0d0
     WRITE(*,*) 'diff = ',diff
     If (dabs(load - diff) > 1e-12) Model%GetTestLoads = .FALSE.
   END IF

   IF (MyVerbosity > 3) THEN
     WRITE(6,*) 'inindex = ', inindex
     WRITE(*,*) 'Load 1 for node:', &
       Model%NodeLoadsPass(1,Model%ElmerToMyNodes(inindex))
     WRITE(*,*) 'Time = ', Tvalue
     WRITE(*,*) 'diff = ',diff
   END IF

END FUNCTION LoadXDirection

FUNCTION LoadYDirection(Model, inindex, Tvalue) RESULT(load)

   USE DefUtils
   USE GeneralModule
 
   IMPLICIT None

   TYPE(Model_t) :: Model
   INTEGER :: inindex, funci
   REAL(KIND=dp) :: Tvalue, load, diff

   Model%UDFUsed = .TRUE.

   load = Model%NodeLoadsPass(2,Model%ElmerToMyNodes(inindex)) 

   IF (MyVerbosity > 3) WRITE(6,*)'Inside [LoadFunctionLibrary]...[LoadYDirection]'

   !Calculate diff for testing purposes
   IF (Tvalue < 8.001) THEN
     diff = float(Model%ElmerToMyNodes(inindex)-1)*3.0d0 + 1.0d0
     diff = diff*Tvalue/8.0d0
     If (dabs(load - diff) > 1e-12) Model%GetTestLoads = .FALSE.
   ELSE
     diff = float(Model%ElmerToMyNodes(inindex)-1)*3.0d0 + 1.0d0
     WRITE(*,*) 'diff = ',diff
     diff = diff + (Tvalue-8.0d0)/92.0d0
     WRITE(*,*) 'diff = ',diff
     If (dabs(load - diff) > 1e-12) Model%GetTestLoads = .FALSE.
   END IF

   IF (MyVerbosity > 3) THEN
     WRITE(6,*) 'inindex = ', inindex
     WRITE(*,*) 'Load 2 for node:', &
       Model%NodeLoadsPass(2,Model%ElmerToMyNodes(inindex))
     WRITE(*,*) 'Time = ', Tvalue
     WRITE(*,*) 'diff = ',diff
   END IF


END FUNCTION LoadYDirection

FUNCTION LoadZDirection(Model, inindex, Tvalue) RESULT(load)

   USE DefUtils
   USE GeneralModule
 
   IMPLICIT None

   TYPE(Model_t) :: Model
   INTEGER :: inindex, funci
   REAL(KIND=dp) :: Tvalue, load, diff

   Model%UDFUsed = .TRUE.

   load =  Model%NodeLoadsPass(3,Model%ElmerToMyNodes(inindex))

   IF (MyVerbosity > 3) WRITE(6,*)'Inside [LoadFunctionLibrary]...[LoadZDirection]'

   !Calculate diff for testing purposes
   IF (Tvalue < 8.001) THEN
     diff = float(Model%ElmerToMyNodes(inindex)-1)*3.0d0 + 2.0d0
     diff = diff*Tvalue/8.0d0
     If (dabs(load - diff) > 1e-12) Model%GetTestLoads = .FALSE.
   ELSE
     diff = float(Model%ElmerToMyNodes(inindex)-1)*3.0d0 + 2.0d0
     WRITE(*,*) 'diff = ',diff
     diff = diff + (Tvalue- 8.0d0)/92.0d0
     WRITE(*,*) 'diff = ',diff
     If (dabs(load - diff) > 1e-12) Model%GetTestLoads = .FALSE.
   END IF

   IF (MyVerbosity > 3) THEN
     WRITE(6,*) 'inindex = ', inindex
     WRITE(*,*) 'Load 3 for node:', &
       Model%NodeLoadsPass(3,Model%ElmerToMyNodes(inindex))
     WRITE(*,*) 'Time = ', Tvalue
     WRITE(*,*) 'diff = ',diff
   END IF

END FUNCTION LoadZDirection

