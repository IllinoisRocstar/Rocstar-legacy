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
PROGRAM readsdv

  IMPLICIT NONE

  INCLUDE 'roccomf90.h'
  INCLUDE 'mpif.h'

  INTEGER,PARAMETER :: pid = 1 ! will change to input late


  INTEGER :: hdl_read, hdl_obtain, hdl_all

  CHARACTER(*), PARAMETER :: OverlayWin = "Overlay"

  INTEGER :: nsubn, nsubf, nn, nf

  INTEGER :: comm_self
  CHARACTER(*), PARAMETER :: prefix = "A"

  CHARACTER(LEN=5) :: sdv_material, sdv_wname
  CHARACTER(*), PARAMETER :: fname = 'A_101_sdv.hdf'

! obtain function handle ------------------------------------------------------

  PRINT*,'Loading module'

  CALL COM_init

  CALL Rocin_load_module( "SDV_IN")

  PRINT*,'before hdl_read '
  hdl_read   = COM_get_function_handle( 'SDV_IN.read_windows')
  PRINT*,'after hdl_read'
  hdl_obtain = COM_get_function_handle( 'SDV_IN.obtain_attribute')

  PRINT*,'after hdl_obtain'

! Define the base-window and sdv-window names


  sdv_material = prefix//'_sdv'
  sdv_wname = sdv_material

  CALL COM_new_window( OverlayWin )

!  // Read the pane from the given file. Note that the file contains both
!  // the parent and the subdivided windows. Read only the subdivided one.

  comm_self = MPI_COMM_SELF
  CALL COM_call_function( hdl_read, 4, fname, sdv_wname, &
              sdv_material, comm_self)
  hdl_all = COM_get_attribute_handle( "A_sdv.all")
  CALL COM_call_function( hdl_obtain, 3, hdl_all, hdl_all, pid) 

!  // Obtain number of sub-nodes, sub-faces, nodes, and faces
  
  CALL COM_get_size( OverlayWin//'.sn_parent_fcID', pid, nsubn)

  PRINT*,nsubn

!!$  CALL COM_get_size( OverlayWin//'.:t3', pid, nsubf)
!!$
!!$  CALL COM_get_size( OverlayWin//'.sn_subID', pid, nn)
!!$  CALL COM_get_size( OverlayWin//'.sf_offset', pid, nf)


  CALL COM_finalize

END PROGRAM readsdv






