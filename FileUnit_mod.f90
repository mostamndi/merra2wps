!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  FileUnit_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Provides routine to automatically select available file unit number.
!
!------------------------------------------------------------------------------

module FileUnit_mod

   ! Change default behavior
   implicit none
   private
   save

   ! Public routines
   public :: selectFileUnit
   public :: closeFileUnit

contains

   !---------------------------------------------------------------------------
   ! ROUTINE:
   ! selectFileUnit
   !
   ! DESCRIPTION:
   ! Selects available file unit.
   !---------------------------------------------------------------------------

   function selectFileUnit() result (fileUnit)

      ! Return variable
      integer :: fileUnit

      ! Local variables
      logical :: connected
      integer :: i

      ! Determine available file unit
      fileUnit = 0
      connected=.true. ! Assume already in use
      do i = 1,100
         if (i == 5) cycle ! Skip standard input
         if (i == 6) cycle ! Skip standard output
         inquire(unit=i,opened=connected)
         if (.not. connected) then
            fileUnit=i
            exit ! Break out of do loop
         end if
      end do
      if (fileUnit == 0) then
         write(0,*)'ERROR, cannot find available file unit number!'
         stop 1
      end if

   end function selectFileUnit

   !---------------------------------------------------------------------------
   ! ROUTINE:
   ! closeFileUnit
   !
   ! DESCRIPTION:
   ! Closes file and handles error.
   !---------------------------------------------------------------------------

   subroutine closeFileUnit(fileUnit)

      ! Arguments
      integer,intent(in) :: fileUnit

      ! Local variables
      integer :: status

      ! Close the file
      close(fileUnit,iostat=status)
      if (status /= 0) then
         write(0,*)'ERROR, cannot close file unit ',fileUnit
         stop 1
      end if

   end subroutine closeFileUnit

end module FileUnit_mod
