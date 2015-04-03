! *********************************************************************
! * Rocstar Simulation Suite                                          *
! * Copyright@2015, Illinois Rocstar LLC. All rights reserved.        *
! *                                                                   *
! * Illinois Rocstar LLC                                              *
! * Champaign, IL                                                     *
! * www.illinoisrocstar.com                                           *
! * sales@illinoisrocstar.com                                         *
! *                                                                   *
! * License: See LICENSE file in top level of distribution package or *
! * http://opensource.org/licenses/NCSA                               *
! *********************************************************************
! *********************************************************************
! * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   *
! * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES   *
! * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND          *
! * NONINFRINGEMENT.  IN NO EVENT SHALL THE CONTRIBUTORS OR           *
! * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       *
! * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   *
! * Arising FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE    *
! * USE OR OTHER DEALINGS WITH THE SOFTWARE.                          *
! *********************************************************************
      SUBROUTINE COM_SET_TRUE( i)
        LOGICAL i
        i = .TRUE.
      END SUBROUTINE COM_SET_TRUE

      SUBROUTINE COM_SET_FALSE( i)
        LOGICAL i
        i = .FALSE.
      END SUBROUTINE COM_SET_FALSE

      SUBROUTINE COM_COPY_STRING( str_frm, len_frm, str_to, len_to)
        INTEGER, INTENT(IN) :: len_frm, len_to

        CHARACTER( len_frm), INTENT(IN) :: str_frm
        CHARACTER( len_to), INTENT(OUT) :: str_to

        str_to = str_frm
      END SUBROUTINE COM_COPY_STRING

      SUBROUTINE COM_MAPPTR_CHR1D( p, tonull, x, n) 
        IMPLICIT NONE

        CHARACTER, POINTER :: p(:)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n
        CHARACTER, TARGET :: x(n)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_CHR1D

      SUBROUTINE COM_MAPPTR_INT0D( p, tonull, x) 

        IMPLICIT NONE

        INTEGER, POINTER :: p
        INTEGER, INTENT(IN) :: tonull
        INTEGER, TARGET :: x

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_INT0D

      SUBROUTINE COM_MAPPTR_INT1D( p, tonull, x, n) 

        IMPLICIT NONE

        INTEGER, POINTER :: p(:)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n
        INTEGER, TARGET :: x(n)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_INT1D

      SUBROUTINE COM_MAPPTR_INT2D( p, tonull, x, n1, n2)

        IMPLICIT NONE

        INTEGER, POINTER :: p(:, :)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n1, n2
        INTEGER, TARGET :: x(n1, n2)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_INT2D

      SUBROUTINE COM_MAPPTR_INT3D( p, tonull, x, n1, n2, n3)

        IMPLICIT NONE

        INTEGER, POINTER :: p(:, :, :)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n1, n2, n3
        INTEGER, TARGET :: x(n1, n2, n3)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_INT3D


      SUBROUTINE COM_MAPPTR_FLT0D( p, tonull, x) 

        IMPLICIT NONE

        INTERFACE
           SUBROUTINE COM_MAP_CPTR_FLT( x, y)
             REAL, INTENT(INOUT) :: x, y
           END SUBROUTINE COM_MAP_CPTR_FLT
        END INTERFACE

        REAL, POINTER :: p
        INTEGER, INTENT(IN) :: tonull
        REAL, TARGET :: x

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_FLT0D

      SUBROUTINE COM_MAPPTR_FLT1D( p, tonull, x, n) 

        IMPLICIT NONE

        REAL, POINTER :: p(:)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n
        REAL, TARGET :: x(n)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_FLT1D

      SUBROUTINE COM_MAPPTR_FLT2D( p, tonull, x, n1, n2)

        IMPLICIT NONE

        REAL, POINTER :: p(:, :)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n1, n2
        REAL, TARGET :: x(n1, n2)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_FLT2D

      SUBROUTINE COM_MAPPTR_FLT3D( p, tonull, x, n1, n2, n3)

        IMPLICIT NONE

        REAL, POINTER :: p(:, :, :)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n1, n2, n3
        REAL, TARGET :: x(n1, n2, n3)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_FLT3D


      SUBROUTINE COM_MAPPTR_DBL0D( p, tonull, x) 

        IMPLICIT NONE

        INTERFACE
           SUBROUTINE COM_MAP_CPTR_DBL( x, y)
             DOUBLE PRECISION, INTENT(INOUT) :: x, y
           END SUBROUTINE COM_MAP_CPTR_DBL
        END INTERFACE

        DOUBLE PRECISION, POINTER :: p
        INTEGER, INTENT(IN) :: tonull
        DOUBLE PRECISION, TARGET :: x

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_DBL0D

      SUBROUTINE COM_MAPPTR_DBL1D( p, tonull, x, n) 

        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p(:)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n
        DOUBLE PRECISION, TARGET :: x(n)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_DBL1D

      SUBROUTINE COM_MAPPTR_DBL2D( p, tonull, x, n1, n2)

        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p(:, :)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n1, n2
        DOUBLE PRECISION, TARGET :: x(n1, n2)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_DBL2D

      SUBROUTINE COM_MAPPTR_DBL3D( p, tonull, x, n1, n2, n3)

        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p(:, :, :)
        INTEGER, INTENT(IN) :: tonull
        INTEGER, INTENT(IN) :: n1, n2, n3
        DOUBLE PRECISION, TARGET :: x(n1, n2, n3)

        IF ( tonull /= 0) THEN
           NULLIFY( p)
        ELSE
           p => x
        END IF

      END SUBROUTINE COM_MAPPTR_DBL3D

      SUBROUTINE COM_GETPTR_CHR0D( p, x) 
        IMPLICIT NONE

        INTERFACE
           SUBROUTINE COM_MAP_CPTR_CHR( x, y)
             CHARACTER, INTENT(INOUT) :: x, y
           END SUBROUTINE COM_MAP_CPTR_CHR
        END INTERFACE

        CHARACTER, POINTER :: p
        CHARACTER, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR_CHR( x, x)
        ELSE
           CALL COM_MAP_CPTR_CHR( p, x)
        END IF

      END SUBROUTINE COM_GETPTR_CHR0D

      SUBROUTINE COM_GETPTR_CHR1D( p, x) 
        IMPLICIT NONE

        CHARACTER, POINTER :: p(:)
        CHARACTER, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1)), x)
        END IF

      END SUBROUTINE COM_GETPTR_CHR1D

      SUBROUTINE COM_GETPTR_INT0D( p, x) 
        IMPLICIT NONE

        INTERFACE
           SUBROUTINE COM_MAP_CPTR_INT( x, y)
             INTEGER, INTENT(INOUT) :: x, y
           END SUBROUTINE COM_MAP_CPTR_INT
        END INTERFACE

        INTEGER, POINTER :: p
        INTEGER, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR_INT( x, x)
        ELSE
           CALL COM_MAP_CPTR_INT( p, x)
        END IF

      END SUBROUTINE COM_GETPTR_INT0D

      SUBROUTINE COM_GETPTR_INT1D( p, x) 
        IMPLICIT NONE

        INTEGER, POINTER :: p(:)
        INTEGER, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1)), x)
        END IF

      END SUBROUTINE COM_GETPTR_INT1D

      SUBROUTINE COM_GETPTR_INT2D( p, x) 
        IMPLICIT NONE

        INTEGER, POINTER :: p(:,:)
        INTEGER, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1),LBOUND(p,2)), x)
        END IF

      END SUBROUTINE COM_GETPTR_INT2D
      
      SUBROUTINE COM_GETPTR_INT3D( p, x) 
        IMPLICIT NONE

        INTEGER, POINTER :: p(:,:,:)
        INTEGER, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1),LBOUND(p,2),LBOUND(p,3)), x)
        END IF

      END SUBROUTINE COM_GETPTR_INT3D
      
      SUBROUTINE COM_GETPTR_FLT0D( p, x) 
        IMPLICIT NONE

        REAL, POINTER :: p
        REAL, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR_FLT( x, x)
        ELSE
           CALL COM_MAP_CPTR_FLT( p, x)
        END IF

      END SUBROUTINE COM_GETPTR_FLT0D

      SUBROUTINE COM_GETPTR_FLT1D( p, x) 
        IMPLICIT NONE

        REAL, POINTER :: p(:)
        REAL, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1)), x)
        END IF

      END SUBROUTINE COM_GETPTR_FLT1D

      SUBROUTINE COM_GETPTR_FLT2D( p, x) 
        IMPLICIT NONE

        REAL, POINTER :: p(:,:)
        REAL, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1),LBOUND(p,2)), x)
        END IF

      END SUBROUTINE COM_GETPTR_FLT2D
      
      SUBROUTINE COM_GETPTR_FLT3D( p, x) 
        IMPLICIT NONE

        REAL, POINTER :: p(:,:,:)
        REAL, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1),LBOUND(p,2),LBOUND(p,3)), x)
        END IF

      END SUBROUTINE COM_GETPTR_FLT3D
      
      SUBROUTINE COM_GETPTR_DBL0D( p, x) 
        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p
        DOUBLE PRECISION, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR_DBL( x, x)
        ELSE
           CALL COM_MAP_CPTR_DBL( p, x)
        END IF

      END SUBROUTINE COM_GETPTR_DBL0D

      SUBROUTINE COM_GETPTR_DBL1D( p, x) 
        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p(:)
        DOUBLE PRECISION, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1)), x)
        END IF

      END SUBROUTINE COM_GETPTR_DBL1D

      SUBROUTINE COM_GETPTR_DBL2D( p, x) 
        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p(:,:)
        DOUBLE PRECISION, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1),LBOUND(p,2)), x)
        END IF

      END SUBROUTINE COM_GETPTR_DBL2D
      
      SUBROUTINE COM_GETPTR_DBL3D( p, x) 
        IMPLICIT NONE

        DOUBLE PRECISION, POINTER :: p(:,:,:)
        DOUBLE PRECISION, TARGET :: x

        IF ( .NOT. ASSOCIATED( p)) THEN
           CALL COM_MAP_CPTR( x, x)
        ELSE
           CALL COM_MAP_CPTR( p(LBOUND(p,1),LBOUND(p,2),LBOUND(p,3)), x)
        END IF

      END SUBROUTINE COM_GETPTR_DBL3D
      
      SUBROUTINE COM_setTypeInfo(wrapper)
        USE m_pointers
        IMPLICIT NONE

        TYPE(data_wrapper), INTENT(OUT) :: wrapper
        TYPE(data), ALLOCATABLE, TARGET :: dat(:)
        INTEGER, ALLOCATABLE, TARGET :: a(:)
        INTEGER :: i
        
        ALLOCATE(a(10))
        ALLOCATE(dat(1))
        wrapper%p_data => dat(1)
        wrapper%p_data%a => a
        wrapper%ibegin        = 333331
        wrapper%iend          = 333332
        wrapper%p_data%ibegin = 333333
        wrapper%p_data%iend   = 333334
        
        DO i = 1, 10
           a(i) = i
        END DO

        DEALLOCATE(a)
        DEALLOCATE(dat)
        
      END SUBROUTINE COM_setTypeInfo

      SUBROUTINE COM_CHKPTR_BEGIN
        IMPLICIT NONE
        CHARACTER(17)    :: str1
        CHARACTER(33)    :: str2
        INTEGER, POINTER :: ptr1(:)
        REAL, POINTER    :: ptr2(:)

        INTERFACE
           SUBROUTINE COM_CHKPTR_C( stage, str1, ptr1, str2, ptr2)
             CHARACTER(17), INTENT(IN) :: str1
             CHARACTER(33), INTENT(IN) :: str2
             INTEGER, POINTER          :: ptr1(:)
             REAL, POINTER             :: ptr2(:)
             INTEGER, INTENT(IN)       :: stage
           END SUBROUTINE COM_CHKPTR_C
        END INTERFACE

        ALLOCATE (ptr1(4))
        ALLOCATE (ptr2(3))
        CALL COM_CHKPTR_C( 1, str1, ptr1, str2, ptr2)
      END SUBROUTINE COM_CHKPTR_BEGIN

      SUBROUTINE COM_CHKPTR_END( str1, ptr1, str2, ptr2)
        IMPLICIT NONE
        CHARACTER(17), INTENT(IN) :: str1
        CHARACTER(33), INTENT(IN) :: str2
        INTEGER, POINTER          :: ptr1(:)
        REAL, POINTER             :: ptr2(:)

        INTERFACE
           SUBROUTINE COM_CHKPTR_C( stage, str2, ptr2, str1, ptr1)
             CHARACTER(17), INTENT(IN) :: str1
             CHARACTER(33), INTENT(IN) :: str2
             INTEGER, POINTER          :: ptr1(:)
             REAL, POINTER             :: ptr2(:)
             INTEGER, INTENT(IN)       :: stage
           END SUBROUTINE COM_CHKPTR_C
        END INTERFACE

        DEALLOCATE (ptr1)
        DEALLOCATE (ptr2)

        CALL COM_CHKPTR_C( 2, str2, ptr2, str1, ptr1)
      END SUBROUTINE COM_CHKPTR_END







