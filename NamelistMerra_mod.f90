!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  NamelistMerra_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Defines data structure and routines for reading namelist input for
! merra2wrf.
!
! REVISION HISTORY:
! 13 Jan 2017:  Started 1.0
!
!------------------------------------------------------------------------------

module NamelistMerra_mod

   ! Import modules
   use FileUnit_mod
   use FileUtils_mod, only : NETCDF_FORMAT


   ! Change defaults
   implicit none
   private

   ! Data structure for namelist entries
   public :: NamelistMerra
   type NamelistMerra
      integer :: numberOfDays
      character(len=132) :: merraDirectory
      integer :: merraFormat_const_2d_asm_Nx
      character(len=132) :: merraFile_const_2d_asm_Nx
      integer :: merraFormat_inst6_3d_ana_Nv
      character(len=132),allocatable :: merraFiles_inst6_3d_ana_Nv(:)
      integer :: merraFormat_inst6_3d_ana_Np
      character(len=132),allocatable :: merraFiles_inst6_3d_ana_Np(:)
      integer :: merraFormat_tavg1_2d_slv_Nx
      character(len=132),allocatable :: merraFiles_tavg1_2d_slv_Nx(:)
      integer :: merraFormat_tavg1_2d_ocn_Nx
      character(len=132),allocatable :: merraFiles_tavg1_2d_ocn_Nx(:)
      integer :: merraFormat_tavg1_2d_lnd_Nx
      character(len=132),allocatable :: merraFiles_tavg1_2d_lnd_Nx(:)
      character(len=10),allocatable :: merraDates(:)
      character(len=132) :: outputDirectory
   end type NamelistMerra

   ! Public methods
   public :: createNamelistMerra
   public :: destroyNamelistMerra

contains

  !----------------------------------------------------------------------------
  !
  ! ROUTINE:  initNamelistMerra
  !
  ! DESCRIPTION:  Initializes data members and deallocates member arrays for
  ! NamelistMerra data type.
  !
  !----------------------------------------------------------------------------

   subroutine initNamelistMerra(this)

      ! Arguments
      type(NamelistMerra),intent(inout) :: this

      this%numberOfDays = 0
      this%merraDirectory = 'NULL'
      this%merraFormat_const_2d_asm_Nx = 0
      this%merraFile_const_2d_asm_Nx = 'NULL'
      this%outputDirectory = 'NULL'

      this%merraFormat_inst6_3d_ana_Nv = 0
      if (allocated(this%merraFiles_inst6_3d_ana_Nv)) &
           deallocate(this%merraFiles_inst6_3d_ana_Nv)
      this%merraFormat_inst6_3d_ana_Np = 0
      if (allocated(this%merraFiles_inst6_3d_ana_Np)) &
           deallocate(this%merraFiles_inst6_3d_ana_Np)
      this%merraFormat_tavg1_2d_slv_Nx = 0
      if (allocated(this%merraFiles_tavg1_2d_slv_Nx)) &
           deallocate(this%merraFiles_tavg1_2d_slv_Nx)
      this%merraFormat_tavg1_2d_ocn_Nx = 0
      if (allocated(this%merraFiles_tavg1_2d_ocn_Nx)) &
           deallocate(this%merraFiles_tavg1_2d_ocn_Nx)
      this%merraFormat_tavg1_2d_lnd_Nx = 0
      if (allocated(this%merraFiles_tavg1_2d_lnd_Nx)) &
           deallocate(this%merraFiles_tavg1_2d_lnd_Nx)
      if (allocated(this%merraDates)) &
           deallocate(this%merraDates)

      return
   end subroutine initNamelistMerra

  !----------------------------------------------------------------------------
  !
  ! ROUTINE:  createNamelistMerra
  !
  ! DESCRIPTION:  Constructor for NamelistMerra data type.
  !
  !----------------------------------------------------------------------------

   function createNamelistMerra(filename) result (this)

      ! Arguments
      character(len=*),intent(in) :: filename

      type(NamelistMerra) :: this
      integer,parameter :: MAX_NUMBER_OF_DAYS=366
      integer :: numberOfDays
      character(len=132) :: merraDirectory
      integer :: merraFormat_const_2d_asm_Nx
      character(len=132) :: merraFile_const_2d_asm_Nx
      integer :: merraFormat_inst6_3d_ana_Nv
      character(len=132) :: merraFiles_inst6_3d_ana_Nv(MAX_NUMBER_OF_DAYS)
      integer :: merraFormat_inst6_3d_ana_Np
      character(len=132) :: merraFiles_inst6_3d_ana_Np(MAX_NUMBER_OF_DAYS)
      integer :: merraFormat_tavg1_2d_slv_Nx
      character(len=132) :: merraFiles_tavg1_2d_slv_Nx(MAX_NUMBER_OF_DAYS)
      integer :: merraFormat_tavg1_2d_ocn_Nx
      character(len=132) :: merraFiles_tavg1_2d_ocn_Nx(MAX_NUMBER_OF_DAYS)
      integer :: merraFormat_tavg1_2d_lnd_Nx
      character(len=132) :: merraFiles_tavg1_2d_lnd_Nx(MAX_NUMBER_OF_DAYS)
      character(len=10)  :: merraDates(MAX_NUMBER_OF_DAYS)
      character(len=132) :: outputDirectory
      namelist /input/ numberOfDays,merraDirectory, &
           merraFormat_const_2d_asm_Nx,merraFile_const_2d_asm_Nx, &
           merraFormat_inst6_3d_ana_Nv,merraFiles_inst6_3d_ana_Nv, &
           merraFormat_inst6_3d_ana_Np,merraFiles_inst6_3d_ana_Np, &
           merraFormat_tavg1_2d_slv_Nx,merraFiles_tavg1_2d_slv_Nx, &
           merraFormat_tavg1_2d_ocn_Nx,merraFiles_tavg1_2d_ocn_Nx, &
           merraFormat_tavg1_2d_lnd_Nx,merraFiles_tavg1_2d_lnd_Nx, &
           merraDates,outputDirectory

      integer :: fileUnit
      integer :: i

      ! Initialize data structure
      call initNamelistMerra(this)

      ! Initialize namelist values
      numberOfDays=0
      merraDirectory='NULL'
      merraFormat_const_2d_asm_Nx=0
      merraFile_const_2d_asm_Nx='NULL'
      merraFormat_inst6_3d_ana_Nv=0
      merraFiles_inst6_3d_ana_Nv(:)='NULL'
      merraFormat_inst6_3d_ana_Np=0
      merraFiles_inst6_3d_ana_Np(:)='NULL'
      merraFormat_tavg1_2d_slv_Nx=0
      merraFiles_tavg1_2d_slv_Nx(:)='NULL'
      merraFormat_tavg1_2d_ocn_Nx=0
      merraFiles_tavg1_2d_ocn_Nx(:)='NULL'
      merraFormat_tavg1_2d_lnd_Nx=0
      merraFiles_tavg1_2d_lnd_Nx(:)='NULL'

      merraDates(:)='NULL'
      outputDirectory='NULL'

      ! Read from namelist file
      fileUnit=selectFileUnit()
      open(unit=fileUnit,file=trim(filename),delim='APOSTROPHE')
      read(unit=fileUnit,nml=input)
      call closeFileUnit(fileUnit)

      ! Sanity check numberOfDays
      if (numberOfDays .lt. 1) then
         print*,'ERROR, numberOfDays should be at least 1'
         print*,'Read in ',numberOfDays
         stop 1
      end if
      if (numberOfDays .gt. MAX_NUMBER_OF_DAYS) then
         print*,'ERROR, numberOfDays cannot exceed ',MAX_NUMBER_OF_DAYS
         print*,'Read in ',numberOfDays
         print*,'Edit namelist or MAX_NUMBER_OF_DAYS in Namelist_mod.f90'
         stop 1
      end if

      ! Copy to data structure
      this%numberOfDays=numberOfDays

      this%merraDirectory=trim(merraDirectory)

      if (merraFormat_const_2d_asm_Nx .ne. NETCDF_FORMAT) then
         print*,'ERROR, invalid format for MERRA const_2d_asm_Nx file!'
         print*,'Valid options:'
         print*,'Found ',merraFormat_const_2d_asm_Nx
         stop 1
      end if
      this%merraFormat_const_2d_asm_Nx = merraFormat_const_2d_asm_Nx

      this%merraFile_const_2d_asm_Nx = &
           trim(merraFile_const_2d_asm_Nx)

      if (merraFormat_inst6_3d_ana_Nv .ne. NETCDF_FORMAT) then
         print*,'ERROR, invalid format for MERRA inst6_3d_ana_Nv file!'
         print*,'Valid options:'
         print*,'  For netCDF:  ',NETCDF_FORMAT
         print*,'Found ',merraFormat_inst6_3d_ana_Nv
         stop 1
      end if
      this%merraFormat_inst6_3d_ana_Nv = merraFormat_inst6_3d_ana_Nv

      allocate(this%merraFiles_inst6_3d_ana_Nv(numberOfDays))
      do i = 1, numberOfDays
         if (trim(merraFiles_inst6_3d_ana_Nv(i)) .eq. 'NULL') then
            print*,'ERROR, invalid file name found!'
            print*,'merraFiles_inst6_3d_ana_Nv(',i,')=', &
                 trim(merraFiles_inst6_3d_ana_Nv(i))
            print*,'Check namelist!'
            stop 1
         else
            this%merraFiles_inst6_3d_ana_Nv(i) = &
                 merraFiles_inst6_3d_ana_Nv(i)
         end if
      end do

      if (merraFormat_inst6_3d_ana_Np .ne. NETCDF_FORMAT) then
         print*,'ERROR, invalid format for MERRA inst6_3d_ana_Np file!'
         print*,'Valid options:'
         print*,'  For netCDF:  ',NETCDF_FORMAT
         print*,'Found ',merraFormat_inst6_3d_ana_Np
         stop 1
      end if
      this%merraFormat_inst6_3d_ana_Np = merraFormat_inst6_3d_ana_Np

      allocate(this%merraFiles_inst6_3d_ana_Np(numberOfDays))
      do i = 1, numberOfDays
         if (trim(merraFiles_inst6_3d_ana_Np(i)) .eq. 'NULL') then
            print*,'ERROR, invalid file name found!'
            print*,'merraFiles_inst6_3d_ana_Np(',i,')=', &
                 trim(merraFiles_inst6_3d_ana_Np(i))
            print*,'Check namelist!'
            stop 1
         else
            this%merraFiles_inst6_3d_ana_Np(i) = &
                 merraFiles_inst6_3d_ana_Np(i)
         end if
      end do

      if (merraFormat_tavg1_2d_slv_Nx .ne. NETCDF_FORMAT) then
         print*,'ERROR, invalid format for MERRA tavg1_2d_slv_Nx file!'
         print*,'Valid options:'
         print*,'  For netCDF:  ',NETCDF_FORMAT
         print*,'Found ',merraFormat_tavg1_2d_slv_Nx
         stop 1
      end if
      this%merraFormat_tavg1_2d_slv_Nx = merraFormat_tavg1_2d_slv_Nx

      allocate(this%merraFiles_tavg1_2d_slv_Nx(numberOfDays))
      do i = 1, numberOfDays
         if (trim(merraFiles_tavg1_2d_slv_Nx(i)) .eq. 'NULL') then
            print*,'ERROR, invalid file name found!'
            print*,'merraFiles_tavg1_2d_slv_Nx(',i,')=', &
                 trim(merraFiles_tavg1_2d_slv_Nx(i))
            print*,'Check namelist!'
            stop 1
         else
            this%merraFiles_tavg1_2d_slv_Nx(i) = &
                 merraFiles_tavg1_2d_slv_Nx(i)
         end if
      end do

      if (merraFormat_tavg1_2d_ocn_Nx .ne. NETCDF_FORMAT) then
         print*,'ERROR, invalid format for MERRA tavg1_2d_ocn_Nx file!'
         print*,'Valid options:'
         print*,'  For netCDF:  ',NETCDF_FORMAT
         print*,'Found ',merraFormat_tavg1_2d_ocn_Nx
         stop 1
      end if
      this%merraFormat_tavg1_2d_ocn_Nx = merraFormat_tavg1_2d_ocn_Nx
      allocate(this%merraFiles_tavg1_2d_ocn_Nx(numberOfDays))
      do i = 1, numberOfDays
         if (trim(merraFiles_tavg1_2d_ocn_Nx(i)) .eq. 'NULL') then
            print*,'ERROR, invalid file name found!'
            print*,'merraFiles_tavg1_2d_ocn_Nx(',i,')=', &
                 trim(merraFiles_tavg1_2d_ocn_Nx(i))
            print*,'Check namelist!'
            stop 1
         else
            this%merraFiles_tavg1_2d_ocn_Nx(i) = &
                 merraFiles_tavg1_2d_ocn_Nx(i)
         end if
      end do

      if (merraFormat_tavg1_2d_lnd_Nx .ne. NETCDF_FORMAT) then
         print*,'ERROR, invalid format for MERRA tavg1_2d_lnd_Nx file!'
         print*,'Valid options:'
         print*,'  For netCDF:  ',NETCDF_FORMAT
         print*,'Found ',merraFormat_tavg1_2d_lnd_Nx
         stop 1
      end if
      this%merraFormat_tavg1_2d_lnd_Nx = merraFormat_tavg1_2d_lnd_Nx
      allocate(this%merraFiles_tavg1_2d_lnd_Nx(numberOfDays))
      do i = 1, numberOfDays
         if (trim(merraFiles_tavg1_2d_ocn_Nx(i)) .eq. 'NULL') then
            print*,'ERROR, invalid file name found!'
            print*,'merraFiles_tavg1_2d_lnd_Nx(',i,')=', &
                 trim(merraFiles_tavg1_2d_lnd_Nx(i))
            print*,'Check namelist!'
            stop 1
         else
            this%merraFiles_tavg1_2d_lnd_Nx(i) = &
                 merraFiles_tavg1_2d_lnd_Nx(i)
         end if
      end do


      allocate(this%merraDates(numberOfDays))
      do i = 1, numberOfDays
         if (trim(merraDates(i)) .eq. 'NULL') then
            print*,'ERROR, invalid date found!'
            print*,'merraDates(',i,')=', &
                 trim(merraDates(i))
            print*,'Check namelist!'
            stop 1
         else
            this%merraDates(i) = merraDates(i)
         end if
      end do

      this%outputDirectory = trim(outputDirectory)

      return
   end function createNamelistMerra

  !----------------------------------------------------------------------------
  !
  ! ROUTINE:  destroyNamelistMerra
  !
  ! DESCRIPTION:  Destructor for NamelistMerra data type.
  !
  !----------------------------------------------------------------------------

   subroutine destroyNamelistMerra(this)

      ! Arguments
      type(NamelistMerra), intent(inout) :: this

      call initNamelistMerra(this)

      return
   end subroutine destroyNamelistMerra

end module NamelistMerra_mod
