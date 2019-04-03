!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  NetcdfUtils_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Utility subroutines for calling the netCDF library.
!
!------------------------------------------------------------------------------

module NetcdfUtils_mod

   ! Change defaults
   implicit none
   private

   ! Include netCDF constants and routines.  Keep private.
   include 'netcdf.inc'

   ! Public subroutines
   public :: openNetcdfReadfile
   public :: closeNetcdfFile
   public :: readNetcdfIntegerArray1d
   public :: readNetcdfDoubleArray1d
   public :: readNetcdfRealArray3d
   public :: readNetcdfRealArray4d

contains

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  openNetcdfReadfile
   !
   ! DESCRIPTION:  Opens a netCDF file in read mode, returns file ID.
   !
   !---------------------------------------------------------------------------

   function openNetcdfReadfile(filename) result (ncID)

      ! Arguments
      character(len=*),intent(in) :: filename

      ! Return value
      integer :: ncID

      ! Local variables
      integer :: status

      ! Open the file
      status = nf_open(trim(filename),NF_NOWRITE,ncID)
      if (status .ne. NF_NOERR) then
         print*,'ERROR returned when opening file ',trim(filename)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      return
   end function openNetcdfReadfile

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  closeNetcdfFile
   !
   ! DESCRIPTION:  Closes a netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine closeNetcdfFile(ncID)

      ! Arguments
      integer,intent(in) :: ncID

      ! Local variables
      integer :: status

      ! Close the file
      status = nf_close(ncID)
      if (status .ne. NF_NOERR) then
         print*,'ERROR returned when closing netCDF file!'
         print*,trim(nf_strerror(status))
         stop 1
      end if

      return
   end subroutine closeNetcdfFile

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  findVarId
   !
   ! DESCRIPTION:  Finds the ID of a particular netCDF variable.  Private
   ! method.
   !
   !---------------------------------------------------------------------------

   function findVarId(ncID,variableName,rank,dataType) result (varID)

      ! Arguments
      integer,intent(in) :: ncID
      character(len=*),intent(in) :: variableName
      integer,intent(in) :: rank
      integer,intent(in) :: dataType

      ! Return variable
      integer :: varID

      ! Local variables
      integer :: status
      integer :: tmp_rank, tmp_dataType

      ! Get ID of netCDF variable
      status = nf_inq_varid(ncID,trim(variableName),varID)
      if (status .ne. NF_NOERR) then
         print*,'ERROR finding netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      ! Sanity check the rank
      status = nf_inq_varndims(ncID, varID, tmp_rank)
      if (status .ne. NF_NOERR) then
         print*,'ERROR finding rank of netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if
      if (tmp_rank .ne. rank) then
         print*,'ERROR, rank mismatch for netCDF variable ',trim(variableName)
         print*,'Expected ',rank
         print*,'Found ',tmp_rank
         stop 1
      end if

      ! Sanity check the data type
      status = nf_inq_vartype(ncID, varID, tmp_dataType)
      if (status .ne. NF_NOERR) then
         print*,'ERROR finding type of netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      !if (tmp_dataType .ne. dataType) then
      !   print*,'ERROR, type mismatch for netCDF variable ',trim(variableName)
      !   print*,'Expected ',dataType
      !   print*,'Found ',tmp_dataType
      !   stop 1
      !end if

      return
   end function findVarId

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readNetcdfIntegerArray1d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 1D integer array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readNetcdfIntegerArray1d(ncID,variableName,dim1,array, &
        subset,istart1,iend1)

      use CheckSubsetBounds_mod

      ! Arguments
      integer,intent(in) :: ncID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1
      integer, allocatable, intent(out) :: array(:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,iend1

      ! Local variables
      integer :: start(1)
      integer :: count(1)
      integer :: varID
      integer :: dimIDs(1)
      integer :: dimensionSizes(1)
      integer :: rank, dataType
      integer :: status
      integer :: i

      ! Sanity check optional arguments
      if (present(subset)) then
         if (subset) then
            if (.not. present(istart1) .or. &
                .not. present(iend1)) then
               print*,'ERROR, subset is set to true, but missing index bounds!'
               stop 1
            end if
         end if
      end if

      ! Set basic requirements for variable
      rank = 1
      dataType = NF_INT

      ! Get ID of variable
      varID = findVarID(ncID,variableName,rank,dataType)
      ! Retrieve the dimension IDs
      status = nf_inq_vardimid(ncID,varID,dimIDs)
      if (status .ne. NF_NOERR) then
         print*,'ERROR retrieving dimension IDs for variable ', &
              trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      ! Get dimensions
      do i = 1, rank
         status = nf_inq_dimlen(ncID,dimIDs(i),dimensionSizes(i))
         if (status .ne. NF_NOERR) then
            print*,'ERROR retrieving dimension length from netCDF!'
            print*,trim(nf_strerror(status))
            stop 1
         end if
      end do

      ! Subset if requested
      start(1:1) = 1
      if (present(subset)) then
         if (subset) then
            call checkSubsetBounds1d(istart1,iend1)
            dimensionSizes(1) = iend1 - istart1 + 1
            start(1) = istart1
         end if
      end if

      ! Allocate the memory
      dim1 = dimensionSizes(1)
      allocate(array(dim1))

      count(1:1) = dimensionSizes(1:1)
      status = nf_get_vara_int(ncID,varID,start,count,array)
      if (status .ne. NF_NOERR) then
         print*,'ERROR reading netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      return
   end subroutine readNetcdfIntegerArray1d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readNetcdfDoubleArray1d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 1D double array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readNetcdfDoubleArray1d(ncID,variableName,dim1,array, &
        subset,istart1,iend1)

      use CheckSubsetBounds_mod

      ! Arguments
      integer,intent(in) :: ncID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1
      double precision, allocatable, intent(out) :: array(:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,iend1

      ! Local variables
      integer :: start(1)
      integer :: count(1)
      integer :: varID
      integer :: dimIDs(1)
      integer :: dimensionSizes(1)
      integer :: rank, dataType
      integer :: status
      integer :: i

      ! Sanity check optional arguments
      if (present(subset)) then
         if (subset) then
            if (.not. present(istart1) .or. &
                .not. present(iend1)) then
               print*,'ERROR, subset is set to true, but missing index bounds!'
               stop 1
            end if
         end if
      end if

      ! Set basic requirements for variable
      rank = 1
      dataType = NF_DOUBLE

      ! Get ID of variable
      varID = findVarID(ncID,variableName,rank,dataType)
      ! Retrieve the dimension IDs
      status = nf_inq_vardimid(ncID,varID,dimIDs)
      if (status .ne. NF_NOERR) then
         print*,'ERROR retrieving dimension IDs for variable ', &
              trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      ! Get dimensions
      do i = 1, rank
         status = nf_inq_dimlen(ncID,dimIDs(i),dimensionSizes(i))
         if (status .ne. NF_NOERR) then
            print*,'ERROR retrieving dimension length from netCDF!'
            print*,trim(nf_strerror(status))
            stop 1
         end if
      end do

      ! Subset if requested
      start(1:1) = 1
      if (present(subset)) then
         if (subset) then
            call checkSubsetBounds1d(istart1,iend1)
            dimensionSizes(1) = iend1 - istart1 + 1
            start(1) = istart1
         end if
      end if

      ! Allocate the memory
      dim1 = dimensionSizes(1)
      allocate(array(dim1))

      ! Read the data
      count(1:1) = dimensionSizes(1:1)
      status = nf_get_vara_double(ncID,varID,start,count,array)
      if (status .ne. NF_NOERR) then
         print*,'ERROR reading netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      return
   end subroutine readNetcdfDoubleArray1d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readNetcdfRealArray3d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 3D real array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readNetcdfRealArray3d(ncID,variableName,dim1,dim2,dim3,array, &
        subset,istart1,istart2,istart3,iend1,iend2,iend3)

      use CheckSubsetBounds_mod

      ! Arguments
      integer,intent(in) :: ncID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1,dim2,dim3
      real, allocatable, intent(out) :: array(:,:,:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,istart2,istart3
      integer,intent(in),optional :: iend1,iend2,iend3

      ! Local variables
      integer :: start(3)
      integer :: count(3)
      integer :: varID
      integer :: dimIDs(3)
      integer :: dimensionSizes(3)
      integer :: rank, dataType
      integer :: status
      integer :: i

      ! Sanity check optional arguments
      if (present(subset)) then
         if (subset) then
            if (.not. present(istart1) .or. &
                .not. present(istart2) .or. &
                .not. present(istart3) .or. &
                .not. present(iend1) .or. &
                .not. present(iend2) .or. &
                .not. present(iend3)) then
               print*,'ERROR, subset is set to true, but missing index bounds!'
               stop 1
            end if
         end if
      end if

      ! Set basic requirements for variable
      rank = 3
      dataType = NF_FLOAT

      ! Get ID of variable
      varID = findVarID(ncID,variableName,rank,dataType)

      ! Retrieve the dimension IDs
      status = nf_inq_vardimid(ncID,varID,dimIDs)
      if (status .ne. NF_NOERR) then
         print*,'ERROR retrieving dimension IDs for variable ', &
              trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      ! Get dimensions
      do i = 1, rank
         status = nf_inq_dimlen(ncID,dimIDs(i),dimensionSizes(i))
         if (status .ne. NF_NOERR) then
            print*,'ERROR retrieving dimension length from netCDF!'
            print*,trim(nf_strerror(status))
            stop 1
         end if
      end do

      ! Subset if requested
      start(1:3) = 1
      if (present(subset)) then
         if (subset) then
            call checkSubsetBounds3d(istart1,iend1,istart2,iend2,istart3,iend3)
            dimensionSizes(1) = iend1 - istart1 + 1
            dimensionSizes(2) = iend2 - istart2 + 1
            dimensionSizes(3) = iend3 - istart3 + 1
            start(1) = istart1
            start(2) = istart2
            start(3) = istart3
         end if
      end if

      ! Allocate the memory
      dim1 = dimensionSizes(1)
      dim2 = dimensionSizes(2)
      dim3 = dimensionSizes(3)
      allocate(array(dim1,dim2,dim3))

      ! Read the data
      count(1:3) = dimensionSizes(1:3)
      status = nf_get_vara_real(ncID,varID,start,count,array)
      if (status .ne. NF_NOERR) then
         print*,'ERROR reading netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      return
   end subroutine readNetcdfRealArray3d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readNetcdfRealArray4d
   !
   ! DESCRIPTION:  Reads in and returns entire requested 4D real array in
   ! netCDF file.
   !
   !---------------------------------------------------------------------------

   subroutine readNetcdfRealArray4d(ncID,variableName,dim1,dim2,dim3,dim4, &
        array, &
        subset,istart1,istart2,istart3,istart4,iend1,iend2,iend3,iend4)

      use CheckSubsetBounds_mod

      ! Arguments
      integer,intent(in) :: ncID
      character(len=*),intent(in) :: variableName
      integer,intent(out) :: dim1,dim2,dim3,dim4
      real, allocatable, intent(out) :: array(:,:,:,:)
      logical,intent(in),optional :: subset
      integer,intent(in),optional :: istart1,istart2,istart3,istart4
      integer,intent(in),optional :: iend1,iend2,iend3,iend4

      ! Local variables
      integer :: start(4)
      integer :: count(4)
      integer :: varID
      integer :: dimIDs(4)
      integer :: dimensionSizes(4)
      integer :: rank, dataType
      integer :: status
      integer :: i

      ! Sanity check optional arguments
      if (present(subset)) then
         if (subset) then
            if (.not. present(istart1) .or. &
                .not. present(istart2) .or. &
                .not. present(istart3) .or. &
                .not. present(istart4) .or. &
                .not. present(iend1) .or. &
                .not. present(iend2) .or. &
                .not. present(iend3) .or. &
                .not. present(iend4)) then
               print*,'ERROR, subset is set to true, but missing index bounds!'
               stop 1
            end if
         end if
      end if

      ! Set basic requirements for variable
      rank = 4
      dataType = NF_FLOAT

      ! Get ID of variable
      varID = findVarID(ncID,variableName,rank,dataType)

      ! Retrieve the dimension IDs
      status = nf_inq_vardimid(ncID,varID,dimIDs)
      if (status .ne. NF_NOERR) then
         print*,'ERROR retrieving dimension IDs for variable ', &
              trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      ! Get dimensions
      do i = 1, rank
         status = nf_inq_dimlen(ncID,dimIDs(i),dimensionSizes(i))
         if (status .ne. NF_NOERR) then
            print*,'ERROR retrieving dimension length from netCDF!'
            print*,trim(nf_strerror(status))
            stop 1
         end if
      end do

      ! Subset if requested
      start(1:4) = 1
      if ( present(subset)) then
         if (subset) then
            call checkSubsetBounds4d(istart1,iend1,istart2,iend2, &
                 istart3,iend3, &
                 istart4,iend4)
            dimensionSizes(1) = iend1 - istart1 + 1
            dimensionSizes(2) = iend2 - istart2 + 1
            dimensionSizes(3) = iend3 - istart3 + 1
            dimensionSizes(4) = iend4 - istart4 + 1
            start(1) = istart1
            start(2) = istart2
            start(3) = istart3
            start(4) = istart4
         end if
      end if

      ! Allocate the memory
      dim1 = dimensionSizes(1)
      dim2 = dimensionSizes(2)
      dim3 = dimensionSizes(3)
      dim4 = dimensionSizes(4)
      allocate(array(dim1,dim2,dim3,dim4))

      ! Read the data
      count(1:4) = dimensionSizes(1:4)
      status = nf_get_vara_real(ncID,varID,start,count,array)
      if (status .ne. NF_NOERR) then
         print*,'ERROR reading netCDF variable ',trim(variableName)
         print*,trim(nf_strerror(status))
         stop 1
      end if

      return
   end subroutine readNetcdfRealArray4d

end module NetcdfUtils_mod
