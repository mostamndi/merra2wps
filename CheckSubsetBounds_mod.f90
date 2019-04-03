!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  CheckSubsetBounds_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Utility subroutines for checking dimension indices when subsetting data.
! Used by NetcdfUtils_mod.
!
!------------------------------------------------------------------------------

module CheckSubsetBounds_mod

   implicit none
   private

   public :: checkSubsetBounds1d
   public :: checkSubsetBounds3d
   public :: checkSubsetBounds4d

contains

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  checkSubsetBounds1d
   !
   ! DESCRIPTION:  Checks subset bounds for single dimension.
   !
   !---------------------------------------------------------------------------

   subroutine checkSubsetBounds1d(istart1,iend1)
      implicit none

      ! Arguments
      integer,intent(in) :: istart1,iend1

      if (istart1 < 1 .or. iend1 < istart1) then
         print*,'ERROR with subset bounds for first dimension!'
         print*,'istart1,iend1 = ',istart1,iend1
         stop 1
      end if

   end subroutine checkSubsetBounds1d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  checkSubsetBounds3d
   !
   ! DESCRIPTION:  Checks subset bounds for three dimensions.
   !
   !---------------------------------------------------------------------------

   subroutine checkSubsetBounds3d(istart1,iend1,istart2,iend2,istart3,iend3)
      implicit none

      ! Arguments
      integer,intent(in) :: istart1,iend1,istart2,iend2,istart3,iend3

      call checkSubsetBounds1d(istart1,iend1)
      if (istart2 < 1 .or. iend2 < istart2) then
         print*,'ERROR with subset bounds for second dimension!'
         print*,'istart2,iend2 = ',istart2,iend2
         stop 1
      end if
      if (istart3 < 1 .or. iend3 < istart3) then
         print*,'ERROR with subset bounds for third dimension!'
         print*,'istart3,iend3 = ',istart3,iend3
         stop 1
      end if

   end subroutine checkSubsetBounds3d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  checkSubsetBounds4d
   !
   ! DESCRIPTION:  Checks subset bounds for four dimensions.
   !
   !---------------------------------------------------------------------------

   subroutine checkSubsetBounds4d(istart1,iend1,istart2,iend2,istart3,iend3, &
        istart4,iend4)
      implicit none

      ! Arguments
      integer,intent(in) :: istart1,iend1,istart2,iend2,istart3,iend3
      integer,intent(in) :: istart4,iend4

      call checkSubsetBounds3d(istart1,iend1,istart2,iend2,istart3,iend3)
      if (istart4 < 1 .or. iend4 < istart4) then
         print*,'ERROR with subset bounds for fourth dimension!'
         print*,'istart4,iend4 = ',istart4,iend4
         stop 1
      end if

   end subroutine checkSubsetBounds4d

end module CheckSubsetBounds_mod
