!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  File4Utils_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Utility subroutines for calling the netCDF libraries.
!
!------------------------------------------------------------------------------

module FileUtils_mod

   ! Import lower-level wrapper libraries
   use NetcdfUtils_mod

   ! Change defaults
   implicit none
   private

   ! Public subroutines
   public :: openReadfile
   public :: closeFile
   public :: readIntegerArray1d
   public :: readDoubleArray1d
   public :: readRealArray3d
   public :: readRealArray4d

   ! Public constants
   integer,parameter,public :: NETCDF_FORMAT  = 2


contains

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  printError
   !
   ! DESCRIPTION:  Prints error message about file format selection.
   !
   !---------------------------------------------------------------------------

   subroutine printError(fileFormat)

      ! Arguments
      integer,intent(in) :: fileFormat

      print*,'ERROR, invalid file format selected!'
      print*,'For NETCDF  : ',NETCDF_FORMAT
      print*,'Selected    : ',fileFormat

      return
   end subroutine printError

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  openReadFile
   !
   ! DESCRIPTION:  Opens a netCDF file in read mode, returns file ID.
   !
   !---------------------------------------------------------------------------

   function openReadFile(fileFormat,filename) result (fileID)

      ! Arguments
      integer, intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Return value
      integer :: fileID

      ! Open the file
      select case (fileFormat)
      case (NETCDF_FORMAT)
         fileID = openNetcdfReadFile(filename)
      case default
         call printError(fileFormat)
         stop 1
      end select

      return
   end function openReadFile

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  closeFile
   !
   ! DESCRIPTION:  Closes a netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine closeFile(fileFormat,fileID)

      ! Arguments
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Close the file
      select case (fileFormat)
      case (NETCDF_FORMAT)
         call closeNetcdfFile(fileID)
      case default
         call printError(fileFormat)
         stop 1
      end select

      return
   end subroutine closeFile

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readIntegerArray1d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 1D integer array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readIntegerArray1d(fileFormat,fileID,variableName,dim1,array, &
        subset,istart1,iend1)

      ! Arguments
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1
      integer, allocatable, intent(out) :: array(:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,iend1

      ! Read array
      select case (fileFormat)
      case (NETCDF_FORMAT)
         call readNetcdfIntegerArray1d(fileID,variableName,dim1,array, &
              subset,istart1,iend1)
      case default
         call printError(fileFormat)
         stop 1
      end select

      return
   end subroutine readIntegerArray1d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readDoubleArray1d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 1D double array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readDoubleArray1d(fileFormat,fileID,variableName,dim1,array, &
        subset,istart1,iend1)

      ! Arguments
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1
      double precision, allocatable, intent(out) :: array(:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,iend1

      ! Read array
      select case (fileFormat)
      case (NETCDF_FORMAT)
         call readNetcdfDoubleArray1d(fileID,variableName,dim1,array, &
              subset,istart1,iend1)
      case default
         call printError(fileFormat)
         stop 1
      end select

      return
   end subroutine readDoubleArray1d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readRealArray3d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 3D array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readRealArray3d(fileFormat,fileID,variableName,dim1,dim2,dim3, &
        array, &
        subset,istart1,istart2,istart3,iend1,iend2,iend3)

      ! Arguments
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1,dim2,dim3
      real, allocatable, intent(out) :: array(:,:,:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,istart2,istart3
      integer,intent(in),optional :: iend1,iend2,iend3

      ! Read array
      select case (fileFormat)
      case (NETCDF_FORMAT)
         call readNetcdfRealArray3d(fileID,variableName,dim1,dim2,dim3, &
              array, &
              subset,istart1,istart2,istart3,iend1,iend2,iend3)
      case default
         call printError(fileFormat)
         stop 1
      end select

      return
   end subroutine readRealArray3d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readRealArray4d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 4D array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readRealArray4d(fileFormat,fileID,variableName,dim1,dim2,dim3, &
        dim4,array, &
        subset,istart1,istart2,istart3,istart4,iend1,iend2,iend3,iend4)

      ! Arguments
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1,dim2,dim3,dim4
      real, allocatable, intent(out) :: array(:,:,:,:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,istart2,istart3,istart4
      integer,intent(in),optional :: iend1,iend2,iend3,iend4

      ! Read array
      select case (fileFormat)
      case (NETCDF_FORMAT)
         call readNetcdfRealArray4d(fileID,variableName,dim1,dim2,dim3,dim4, &
              array, &
              subset,istart1,istart2,istart3,istart4, &
                 iend1,iend2,iend3,iend4)
      case default
         call printError(fileFormat)
         stop 1
      end select

      return
   end subroutine readRealArray4d

end module FileUtils_mod
