!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  MerraData_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Defines structure for MERRA data from NASA GES DISC
!
! REVISION:
! 13 Jan 2017:  Started 1.0
! 22 Feb 2017:  Added new fields for soil temperature and moisture
!               soil levels; 1)0 -10cm; 2) 10-20cm; 3) 20-40cm; 4) 40-100cm
!------------------------------------------------------------------------------

module MerraData_mod

   ! Import modules
   use FileUnit_mod
   use FileUtils_mod
   use WrfRH_mod

   ! Reset defaults
   implicit none
   private

   ! Define data structure storing MERRA data
   public :: MerraData
   type MerraData

      integer :: longitudeDimension
      integer :: latitudeDimension
      integer :: verticalDimension
      integer :: timeDimension

      real :: southwestLongitude
      real :: southwestLatitude
      real :: deltaLongitude
      real :: deltaLatitude

      ! From const_2d_asm_Nx file
      real, allocatable :: surfaceGeopotentials(:,:)
      real, allocatable :: landSea(:,:)
      real, allocatable :: terrainHeights(:,:)

      ! From inst6_3d_ana_Nv files
      integer, allocatable :: synopticHours(:)
      real, allocatable :: surfacePressures(:,:,:)
      real, allocatable :: layerTemperatures(:,:,:,:)
      real, allocatable :: layerUWinds(:,:,:,:)
      real, allocatable :: layerVWinds(:,:,:,:)
      real, allocatable :: layerPressures(:,:,:,:)
      real, allocatable :: layerGeopotentialHeights(:,:,:,:)
      real, allocatable :: layerRelativeHumidities(:,:,:,:)

      ! From inst6_3d_ana_Np files
      real, allocatable :: meanSeaLevelPressures(:,:,:)

      ! From tavg1_2d_slv_Nx files
      real, allocatable :: uWinds10M(:,:,:)
      real, allocatable :: vWinds10M(:,:,:)
      real, allocatable :: temperatures2M(:,:,:)
      real, allocatable :: relativeHumidities2M(:,:,:)
      real, allocatable :: skinTemperatures(:,:,:)

      ! From tavg1_2d_ocn_Nx files
      real, allocatable :: seaIceFractions(:,:,:)

      ! From tavg1_2d_lnd_Nx files
      real, allocatable :: soilTemperature1(:,:,:)
      real, allocatable :: soilTemperature2(:,:,:)
      real, allocatable :: soilTemperature3(:,:,:)
      real, allocatable :: soilTemperature4(:,:,:)
      real, allocatable :: soilTemperature5(:,:,:)

      real, allocatable :: soilMoisture1(:,:,:)
      real, allocatable :: soilMoisture2(:,:,:)
      real, allocatable :: soilMoisture3(:,:,:)
      real, allocatable :: soilMoisture4(:,:,:)
      real, allocatable :: soilMoisture5(:,:,:)

      real, allocatable :: snowDepth(:,:,:)
      real, allocatable :: swe(:,:,:)

   end type MerraData

   ! Public routines
   public :: createMerraData
   public :: destroyMerraData
   public :: readConst2dAsmNx
   public :: readInst63dAnaNv
   public :: readInst63dAnaNp
   public :: readTavg12dSlvNx
   public :: readTavg12dOcnNx
   public :: readTavg2dlndNx
   public :: writeOutput

   ! TODO:  Put in external module
   real, parameter :: G_0 = 9.80665 ! Gravity at mean sea level
   real, parameter :: R_d = 287.    ! Dry air gas constant
   real, parameter :: P_TOP=1. ! MERRA model top is at 1 Pa = 0.01 hPa

   character(len=32), parameter :: MAP_SOURCE='MERRA/KAUST PSE'

contains

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  createMerraData
   !
   ! DESCRIPTION:  Public constructor for MerraData structure.
   !
   !---------------------------------------------------------------------------

   function createMerraData() result (this)

      ! Return variable
      type(MerraData) :: this

      ! Initialize scalars.  Allocatable arrays are guaranteed to be
      ! in "uninitialized" state.
      call initMerraDataScalars(this)

      return
   end function createMerraData

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  destroyMerraData
   !
   ! DESCRIPTION:  Public destructor for MerraData structure.
   !
   !---------------------------------------------------------------------------

   subroutine destroyMerraData(this)

      ! Arguments
      type(MerraData),intent(inout) :: this

      ! Reset the scalar data members
      call initMerraDataScalars(this)

      ! Deallocate arrays
      if (allocated(this%surfaceGeopotentials)) &
           deallocate(this%surfaceGeopotentials)
      if (allocated(this%landSea)) &
           deallocate(this%landSea)
      if (allocated(this%terrainHeights)) &
           deallocate(this%terrainHeights)

      if (allocated(this%surfacePressures)) &
           deallocate(this%surfacePressures)
      if (allocated(this%layerTemperatures)) &
           deallocate(this%layerTemperatures)
      if (allocated(this%layerUWinds)) &
           deallocate(this%layerUWinds)
      if (allocated(this%layerVWinds)) &
           deallocate(this%layerVWinds)
      if (allocated(this%layerPressures)) &
           deallocate(this%layerPressures)
      if (allocated(this%layerGeopotentialHeights)) &
           deallocate(this%layerGeopotentialHeights)
      if (allocated(this%layerRelativeHumidities)) &
           deallocate(this%layerRelativeHumidities)

      if (allocated(this%meanSeaLevelPressures)) &
           deallocate(this%meanSeaLevelPressures)

      if (allocated(this%uWinds10M)) &
           deallocate(this%uWinds10M)
      if (allocated(this%vWinds10M)) &
           deallocate(this%vWinds10M)
      if (allocated(this%temperatures2M)) &
           deallocate(this%temperatures2M)
      if (allocated(this%relativeHumidities2M)) &
           deallocate(this%relativeHumidities2M)
      if (allocated(this%skinTemperatures)) &
           deallocate(this%skinTemperatures)

      if (allocated(this%seaIceFractions)) &
           deallocate(this%seaIceFractions)

      if (allocated(this%soilMoisture1)) &
           deallocate(this%soilMoisture1)
      if (allocated(this%soilMoisture2)) &
           deallocate(this%soilMoisture2)
      if (allocated(this%soilMoisture3)) &
           deallocate(this%soilMoisture3)
      if (allocated(this%soilMoisture4)) &
           deallocate(this%soilMoisture4)
      if (allocated(this%soilMoisture5)) &
           deallocate(this%soilMoisture5)

      if (allocated(this%soilTemperature1)) &
           deallocate(this%soilTemperature1)
      if (allocated(this%soilTemperature2)) &
           deallocate(this%soilTemperature2)
      if (allocated(this%soilTemperature3)) &
           deallocate(this%soilTemperature3)
      if (allocated(this%soilTemperature4)) &
           deallocate(this%soilTemperature4)
      if (allocated(this%soilTemperature5)) &
           deallocate(this%soilTemperature5)
      return
   end subroutine destroyMerraData

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readConst2dAsmNx
   !
   ! DESCRIPTION:  Public method for processing a MERRA const_2d_asm_Nx file.
   !
   !---------------------------------------------------------------------------

   subroutine readConst2dAsmNx(this,fileFormat,filename)

      ! Arguments
      type(MerraData),intent(inout) :: this
      integer,intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Local variables
      integer :: fileID
      integer :: dim1,dim2,dim3
      real, allocatable :: tmp3d(:,:,:)
      real, allocatable :: oceanFractions(:,:)
      real, allocatable :: lakeFractions(:,:)
      integer :: i,j

      ! Open file
      fileID = openReadFile(fileFormat,filename)

      ! Get lat/lon info
      call getLatLon_merra2(this,fileFormat,fileID)

      ! Read surface geopotententials
      call readRealArray3d(fileFormat,fileID,"PHIS",dim1,dim2,dim3,tmp3d)
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 1) then
         print*,'ERROR, dimension mismatch for PHIS'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension,1
         print*,'Found ',dim1,dim2,dim3
         stop 1
      end if
      allocate(this%surfaceGeopotentials(dim1,dim2))
      do j = 1, this%latitudeDimension
         do i = 1, this%longitudeDimension
            this%surfaceGeopotentials(i,j) = tmp3d(i,j,1)
         end do
      end do
      deallocate(tmp3d)

      ! Read lake fraction
      call readRealArray3d(fileFormat,fileID,"FRLAKE",dim1,dim2,dim3,tmp3d)
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 1) then
         print*,'ERROR, dimension mismatch for FRLAKE'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension,1
         print*,'Found ',dim1,dim2,dim3
         stop 1
      end if
      allocate(lakeFractions(dim1,dim2))
      do j = 1, this%latitudeDimension
         do i = 1, this%longitudeDimension
            lakeFractions(i,j) = tmp3d(i,j,1)
         end do
      end do
      deallocate(tmp3d)

      ! Read ocean fraction
      call readRealArray3d(fileFormat,fileID,"FROCEAN",dim1,dim2,dim3,tmp3d)
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
         dim3 .ne. 1) then
         print*,'ERROR, dimension mismatch for FROCEAN'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension,1
         print*,'Found ',dim1,dim2,dim3
         stop 1
      end if
      allocate(oceanFractions(dim1,dim2))
      do j = 1, this%latitudeDimension
         do i = 1, this%longitudeDimension
            oceanFractions(i,j) = tmp3d(i,j,1)
         end do
      end do
      deallocate(tmp3d)

      ! Close the NETCDF4 file
      call closeFile(fileFormat,fileID)

      ! Calculate landsea mask
      call calculateLandSea(this,oceanFractions,lakeFractions)
      deallocate(oceanFractions)
      deallocate(lakeFractions)

      ! Calculate terrain heights
      call calculateTerrainHeights(this)

      return
   end subroutine readConst2dAsmNx

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readInst63dAnaNv
   !
   ! DESCRIPTION:  Public method for processing MERRA inst6_3d_ana_Nv files.
   !
   !---------------------------------------------------------------------------

   subroutine readInst63dAnaNv(this,fileFormat,filename)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Local variables
      integer :: fileID
      real, allocatable :: layerPressureThicknesses(:,:,:,:)
      real, allocatable :: layerSpecificHumidities(:,:,:,:)
      real, allocatable :: edgePressures(:,:,:,:)
      real, allocatable :: edgeGeopotentialHeights(:,:,:,:)
      integer :: dim1,dim2,dim3,dim4
      logical :: startAtTop

      ! Open the MERRA file
      fileID = openReadFile(fileFormat,filename)
      ! Make sure latitudes and longitudes agree
      call checkLatLon(this,fileFormat,fileID)
      ! Get time and vertical level information
      call getTimesVerticalLevels(this,fileFormat,fileID)

      ! See if the data start at the model top or at the terrain height.
      startAtTop = dataStartsAtModelTop(fileFormat,fileID)

      ! Get surface pressures
      if (fileFormat == NETCDF_FORMAT ) then
         call readRealArray3d(fileFormat,fileID,"PS",dim1,dim2,dim3, &
              this%surfacePressures)
           else
         call readRealArray3d(fileFormat,fileID,"ps",dim1,dim2,dim3, &
              this%surfacePressures)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for ps array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%timeDimension
         print*,'Found ',dim1,dim2,dim3
         stop 1
      end if

      ! Get Layer pressure thicknesses
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray4d(fileFormat,fileID,"DELP",dim1,dim2,dim3,dim4, &
              layerPressureThicknesses)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%verticalDimension .or. &
          dim4 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for delp array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%verticalDimension, &
              this%timeDimension
         print*,'Found ',dim1,dim2,dim3,dim4
         stop 1
      end if
      if (.not. startAtTop) then
         print*,'Flipping layerPressureThickness...'
         call flipRealArray4d(dim1,dim2,dim3,dim4,layerPressureThicknesses)
      end if

      ! Get layer air temperatures
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray4d(fileFormat,fileID,"T",dim1,dim2,dim3,dim4, &
              this%layerTemperatures)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%verticalDimension .or. &
          dim4 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for t array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%verticalDimension, &
              this%timeDimension
         print*,'Found ',dim1,dim2,dim3,dim4
         stop 1
      end if
      if (.not. startAtTop) then
         print*,'Flipping layerTemperatures...'
         call flipRealArray4d(dim1,dim2,dim3,dim4,this%layerTemperatures)
      end if

      ! Get layer U-winds
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray4d(fileFormat,fileID,"U",dim1,dim2,dim3,dim4, &
              this%layerUWinds)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%verticalDimension .or. &
          dim4 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for u array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%verticalDimension, &
              this%timeDimension
         print*,'Found ',dim1,dim2,dim3,dim4
         stop 1
      end if
      if (.not. startAtTop) then
         print*,'Flipping layerUWinds...'
         call flipRealArray4d(dim1,dim2,dim3,dim4,this%layerUWinds)
      end if

      ! Get layer V-winds
      if (fileFormat == NETCDF_FORMAT ) then
         call readRealArray4d(fileFormat,fileID,"V",dim1,dim2,dim3,dim4, &
              this%layerVWinds)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%verticalDimension .or. &
          dim4 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for v array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%verticalDimension, &
              this%timeDimension
         print*,'Found ',dim1,dim2,dim3,dim4
         stop 1
      end if
      if (.not. startAtTop) then
         print*,'Flipping layerVWinds...'
         call flipRealArray4d(dim1,dim2,dim3,dim4,this%layerVWinds)
      end if

      ! Get layer specific humidities
      if (fileFormat == NETCDF_FORMAT ) then
         call readRealArray4d(fileFormat,fileID,"QV",dim1,dim2,dim3,dim4, &
              layerSpecificHumidities)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%verticalDimension .or. &
          dim4 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for qv array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%verticalDimension, &
              this%timeDimension
         print*,'Found ',dim1,dim2,dim3,dim4
         stop 1
      end if
      if (.not. startAtTop) then
         print*,'Flipping layerSpecificHumidities...'
         call flipRealArray4d(dim1,dim2,dim3,dim4,layerSpecificHumidities)
      end if

      ! Close the NETCDF4 file
      call closeFile(fileFormat,fileID)

      ! Calculate pressures at edges of model layers.
      allocate(edgePressures(this%longitudeDimension, this%latitudeDimension, &
           this%verticalDimension+1, this%timeDimension))
      call calculateEdgePressures(this,layerPressureThicknesses, &
           edgePressures)
      deallocate(layerPressureThicknesses)

      ! Calculate the mean layer pressures.  But keep the edge pressures for
      ! calculating geopotential heights further below.
      ! Note:  this%layerPressures is allocated in the subroutine.
      call calculateLayerPressures(this,edgePressures)

      ! Calculate edge geopotential heights
      allocate(edgeGeopotentialHeights(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension))
      call calculateEdgeGeopotentialHeights(this,edgePressures, &
           layerSpecificHumidities,edgeGeopotentialHeights)

      ! Calculate layer geopotential heights.
      ! Note:  this%layerGeopotentialHeights is allocated in the subroutine.
      call calculateLayerGeopotentialHeights(this,edgePressures, &
           layerSpecificHumidities,edgeGeopotentialHeights)
      deallocate(edgeGeopotentialHeights)
      deallocate(edgePressures)

      ! Calculate layer relative humidities.
      ! Note:  this%layerRelativeHumidities is allocated in the subroutine.
      call calculateLayerRelativeHumidities(this,layerSpecificHumidities)
      deallocate(layerSpecificHumidities)

      return
   end subroutine readInst63dAnaNv

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readInst63dAnaNp
   !
   ! DESCRIPTION:  Public method for processing MERRA inst6_3d_ana_Np files.
   !
   !---------------------------------------------------------------------------

   subroutine readInst63dAnaNp(this,fileFormat,filename)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Local variables
      integer :: fileID
      integer :: dim1,dim2,dim3

      ! Open the MERRA file
      fileID = openReadFile(fileFormat,filename)

      ! Sanity check latitudes and longitudes
      call checkLatLon(this,fileFormat,fileID)

      ! Check the times
      call checkTimes(this,fileFormat,fileID)
      ! Get sea level pressures
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'SLP',dim1,dim2,dim3, &
              this%meanSeaLevelPressures)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for slp array!'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, this%timeDimension
         print*,'Found ',dim1,dim2,dim3
         stop 1
      end if

      ! Close file
      call closeFile(fileFormat,fileID)

      return
   end subroutine readInst63dAnaNp

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readTavg12dSlvNx
   !
   ! DESCRIPTION:  Public method for processing MERRA tavg1_2d_slv_Nx files.
   !
   !---------------------------------------------------------------------------

   subroutine readTavg12dSlvNx(this,fileFormat,filename)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Local variables
      integer :: fileID
      integer :: ii,jj,mm
      real, allocatable :: u10m(:,:,:)
      real, allocatable :: v10m(:,:,:)
      real, allocatable :: t2m(:,:,:)
      real, allocatable :: qv2m(:,:,:)
      real, allocatable :: specificHumidities2m(:,:,:)
      real, allocatable :: ts(:,:,:)
      integer,allocatable :: indices(:)
      integer :: fileTimeDimension
      integer :: dim1,dim2,dim3

      ! Open the MERRA file
      fileID = openReadFile(fileFormat,filename)

      ! Get array indices of common time levels.
      call getTimeIndices(this,fileFormat,fileID,indices, &
           fileTimeDimension)

      ! Now check longitude and latitude grids
      call checkLatLon(this,fileFormat,fileID)

      ! Get 10-meter U Winds
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'U10M',dim1,dim2,dim3,u10m)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. fileTimeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%uWinds10M(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%uWinds10M(ii,jj,mm) = u10m(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(u10m)

      ! Get 10-meter V Winds
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'V10M',dim1,dim2,dim3,v10m)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. fileTimeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%vWinds10M(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%vWinds10M(ii,jj,mm) = v10m(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(v10m)

      ! Get 2-meter temperatures
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'T2M',dim1,dim2,dim3,t2m)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. fileTimeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%temperatures2M(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%temperatures2M(ii,jj,mm) = t2m(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(t2m)

      ! Get 2-meter specific humidities
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'QV2M',dim1,dim2,dim3,qv2m)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. fileTimeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(specificHumidities2M(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               specificHumidities2M(ii,jj,mm) = qv2m(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(qv2m)

      ! Get skin temperatures
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'TS',dim1,dim2,dim3,ts)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. fileTimeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%skinTemperatures(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%skinTemperatures(ii,jj,mm) = ts(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(ts)

      ! Close MERRA file
      call closeFile(fileFormat,fileID)

      ! Convert 2 meter specific humidity to 2 m relative humidity.
      ! Note that this%relativeHumidities2M is allocated in the subroutine.
      call calculateRelativeHumidities2M(this,specificHumidities2M)
      deallocate(specificHumidities2M)

      deallocate(indices)
      return
   end subroutine readTavg12dSlvNx

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readTavg12dOcnNx
   !
   ! DESCRIPTION:  Public method for processing MERRA tavg1_2d_ocn_Nx files.
   !
   !---------------------------------------------------------------------------

   subroutine readTavg12dOcnNx(this,fileFormat,filename)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Local variables
      integer :: fileID
      integer :: ii,jj,mm
      real,allocatable :: frseaice(:,:,:)
      integer,allocatable :: indices(:)
      integer :: fileTimeDimension
      integer :: dim1,dim2,dim3

      ! Open the MERRA file
      fileID = openReadFile(fileFormat,filename)

     ! Get indices of common time levels.
      call getTimeIndices(this,fileFormat,fileID,indices, &
           fileTimeDimension)

      ! Now check longitude and latitude grids
      call checkLatLon(this,fileFormat,fileID)

      ! Get seaice fractions
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'FRSEAICE',dim1,dim2,dim3, &
              frseaice)
      end if
      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. fileTimeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%seaIceFractions(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%seaIceFractions(ii,jj,mm) = &
                    frseaice(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(frseaice)

      ! Close MERRA file
      call closeFile(fileFormat,fileID)

      deallocate(indices)

      return
   end subroutine readTavg12dOcnNx

  !---------------------------------------------------------------------------
   !
   ! ROUTINE:  readTav2dlndNx
   !
   ! DESCRIPTION:  Public method for processing MERRA tavg1_2d_lnd_Nx files.
   !
   !---------------------------------------------------------------------------

   subroutine readTavg2dlndNx(this,fileFormat,filename)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      character(len=*),intent(in) :: filename

      ! Local variables
      integer :: fileID
      integer :: ii,jj,mm
      real,allocatable :: sm1(:,:,:), sm2(:,:,:), sd(:,:,:), swe(:,:,:)
      real,allocatable :: st1(:,:,:), st2(:,:,:), st3(:,:,:), st4(:,:,:),st5(:,:,:)
      integer,allocatable :: indices(:)
      integer :: fileTimeDimension
      integer :: dim1,dim2,dim3

      ! Open the MERRA file
      fileID = openReadFile(fileFormat,filename)

     ! Get indices of common time levels.
      call getTimeIndices(this,fileFormat,fileID,indices, &
           fileTimeDimension)

      ! Now check longitude and latitude grids
      call checkLatLon(this,fileFormat,fileID)

      ! Get soil temperature first depth
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'TSOIL1',dim1,dim2,dim3, &
              st1)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilTemperature1(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilTemperature1(ii,jj,mm) = st1(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(st1)

      ! Get soil temperature second depth
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'TSOIL2',dim1,dim2,dim3, &
              st2)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilTemperature2(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      print *, "Time Dimension=" , this%timeDimension
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilTemperature2(ii,jj,mm) = st2(ii,jj,indices(mm))
            end do
         end do
         print *, indices
         print *, "indices(mm)=", indices(mm), "mm=", mm
         print *, MAXVAL(st2(:,:,mm))
      end do
      deallocate(st2)

      !print '(41F9.3)', this%soilTemperature2(344:384,204:239,2)

      ! Get soil temperature third depth
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'TSOIL3',dim1,dim2,dim3, &
              st3)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilTemperature3(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilTemperature3(ii,jj,mm) = st3(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(st3)

      ! Get soil temperature fourth depth
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'TSOIL4',dim1,dim2,dim3, &
              st4)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilTemperature4(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilTemperature4(ii,jj,mm) = st4(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(st4)

      ! Get soil temperature fifth depth
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'TSOIL5',dim1,dim2,dim3, &
              st5)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilTemperature5(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilTemperature5(ii,jj,mm) = st5(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(st5)

      ! Get soil moisture first depth (water surface layer )
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'SFMC',dim1,dim2,dim3, &
              sm1)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilMoisture1(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilMoisture1(ii,jj,mm) = sm1(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(sm1)

! Get soil moisture second depth (water surface layer )
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'RZMC',dim1,dim2,dim3, &
              sm2)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%soilMoisture2(this%longitudeDimension,this%latitudeDimension, this%timeDimension))
      allocate(this%soilMoisture3(this%longitudeDimension,this%latitudeDimension, this%timeDimension))
      allocate(this%soilMoisture4(this%longitudeDimension,this%latitudeDimension, this%timeDimension))
      allocate(this%soilMoisture5(this%longitudeDimension,this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%soilMoisture2(ii,jj,mm) = sm2(ii,jj,indices(mm))
               this%soilMoisture3(ii,jj,mm) = sm2(ii,jj,indices(mm))
               this%soilMoisture4(ii,jj,mm) = sm2(ii,jj,indices(mm))
               this%soilMoisture5(ii,jj,mm) = sm2(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(sm2)

! Get Snow depth
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'SNODP',dim1,dim2,dim3, &
              sd)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%snowDepth(this%longitudeDimension,this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%snowDepth(ii,jj,mm) = sd(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(sd)

! Get Snow water Equivalent
      if (fileFormat == NETCDF_FORMAT) then
         call readRealArray3d(fileFormat,fileID,'SNOMAS',dim1,dim2,dim3, &
              swe)
      end if

      if (dim1 .ne. this%longitudeDimension .or. &
          dim2 .ne. this%latitudeDimension .or. &
          dim3 .ne. 24.) then
         print*,'ERROR, dimension mismatch between MERRA files'
         print*,'Expected ',this%longitudeDimension, &
              this%latitudeDimension, fileTimeDimension
         print*,'Got ',dim1,dim2,dim3
      end if
      allocate(this%swe(this%longitudeDimension,this%latitudeDimension, this%timeDimension))
      do mm = 1, this%timeDimension
         do jj = 1, this%latitudeDimension
            do ii = 1, this%longitudeDimension
               this%swe(ii,jj,mm) = swe(ii,jj,indices(mm))
            end do
         end do
      end do
      deallocate(swe)


      ! Close MERRA file
      call closeFile(fileFormat,fileID)

      deallocate(indices)

      return
   end subroutine readTavg2dlndNx

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  writeOutput
   !
   ! DESCRIPTION:  Public method for writing MERRA data to WPS intermediate
   ! format.
   !
   !---------------------------------------------------------------------------

   subroutine writeOutput(this, merraDate, outputDirectory)

      use FieldWPS_mod, only:  FieldWPS, createFieldWPS, destroyFieldWPS, &
           IPROJ_LATLON, SWCORNER
      use FileWPS_mod, only:  FileWPS, createFileWPS, destroyFileWPS, &
           writeFileWPS

      ! Arguments
      type(MerraData),intent(in) :: this
      character(len=10),intent(in) :: merraDate
      character(len=*),intent(in) :: outputDirectory

      ! Local variables
      type(FileWPS) :: wpsFileData
      type(FieldWPS) :: wpsFieldData
      real :: earthRadius
      integer :: fileUnit
      integer :: year,month,day,hour
      integer :: m,k

      ! Calculate earth radius in km
      earthRadius = 111.111111*180./(4.*atan(1.))

      do m = 1, this%timeDimension
         read(merraDate,'(i4.4,1x,i2.2,1x,i2.2)') year,month,day
         hour = this%synopticHours(m)

         write(*,'(a,i2,a)') 'Writing time level ',m,' ...'

         ! Open WPS file
         fileUnit=selectFileUnit()
         wpsFileData = createFileWPS(fileUnit=fileUnit, &
              outputDirectory=trim(outputDirectory), &
              prefix='MERRA', &
              year=year, &
              month=month, &
              day=day, &
              hour=hour)

         write(*,'(a)') 'Writing vertical levels...'
         do k = 1, this%verticalDimension

            ! Pressures
            wpsFieldData = createFieldWPS( &
                 year=year, &
                 month=month, &
                 day=day, &
                 hour=hour, &
                 xfcst=0., &
                 map_source=MAP_SOURCE, &
                 field='PRESSURE', &
                 units='Pa', &
                 desc='Pressure', &
                 xlvl=real(k), &
                 nx=this%longitudeDimension, &
                 ny=this%latitudeDimension, &
                 iproj=IPROJ_LATLON, &
                 startloc=SWCORNER, &
                 startlat=this%southwestLatitude, &
                 startlon=this%southwestLongitude, &
                 earth_radius=earthRadius, &
                 is_wind_grid_rel=.false., &
                 slab=this%layerPressures(1,1,k,m), &
                 deltaLat=this%deltaLatitude, &
                 deltaLon=this%deltaLongitude)
            call writeFileWPS(wpsFileData,wpsFieldData)
            call destroyFieldWPS(wpsFieldData)

            ! Geopotential heights
            wpsFieldData = createFieldWPS( &
                 year=year, &
                 month=month, &
                 day=day, &
                 hour=hour, &
                 xfcst=0., &
                 map_source=MAP_SOURCE, &
                 field='HGT', &
                 units='m', &
                 desc='Height', &
                 xlvl=real(k), &
                 nx=this%longitudeDimension, &
                 ny=this%latitudeDimension, &
                 iproj=IPROJ_LATLON, &
                 startloc=SWCORNER, &
                 startlat=this%southwestLatitude, &
                 startlon=this%southwestLongitude, &
                 earth_radius=earthRadius, &
                 is_wind_grid_rel=.false., &
                 slab=this%layerGeopotentialHeights(1,1,k,m), &
                 deltaLat=this%deltaLatitude, &
                 deltaLon=this%deltaLongitude)
            call writeFileWPS(wpsFileData,wpsFieldData)
            call destroyFieldWPS(wpsFieldData)

            ! Temperatures
            wpsFieldData = createFieldWPS( &
                 year=year, &
                 month=month, &
                 day=day, &
                 hour=hour, &
                 xfcst=0., &
                 map_source=MAP_SOURCE, &
                 field='TT', &
                 units='K', &
                 desc='Temperature', &
                 xlvl=real(k), &
                 nx=this%longitudeDimension, &
                 ny=this%latitudeDimension, &
                 iproj=IPROJ_LATLON, &
                 startloc=SWCORNER, &
                 startlat=this%southwestLatitude, &
                 startlon=this%southwestLongitude, &
                 earth_radius=earthRadius, &
                 is_wind_grid_rel=.false., &
                 slab=this%layerTemperatures(1,1,k,m), &
                 deltaLat=this%deltaLatitude, &
                 deltaLon=this%deltaLongitude)
            call writeFileWPS(wpsFileData,wpsFieldData)
            call destroyFieldWPS(wpsFieldData)

            ! U winds
            wpsFieldData = createFieldWPS( &
                 year=year, &
                 month=month, &
                 day=day, &
                 hour=hour, &
                 xfcst=0., &
                 map_source=MAP_SOURCE, &
                 field='UU', &
                 units='m s-1', &
                 desc='U', &
                 xlvl=real(k), &
                 nx=this%longitudeDimension, &
                 ny=this%latitudeDimension, &
                 iproj=IPROJ_LATLON, &
                 startloc=SWCORNER, &
                 startlat=this%southwestLatitude, &
                 startlon=this%southwestLongitude, &
                 earth_radius=earthRadius, &
                 is_wind_grid_rel=.false., &
                 slab=this%layerUWinds(1,1,k,m), &
                 deltaLat=this%deltaLatitude, &
                 deltaLon=this%deltaLongitude)
            call writeFileWPS(wpsFileData,wpsFieldData)
            call destroyFieldWPS(wpsFieldData)

            ! V winds
            wpsFieldData = createFieldWPS( &
                 year=year, &
                 month=month, &
                 day=day, &
                 hour=hour, &
                 xfcst=0., &
                 map_source=MAP_SOURCE, &
                 field='VV', &
                 units='m s-1', &
                 desc='V', &
                 xlvl=real(k), &
                 nx=this%longitudeDimension, &
                 ny=this%latitudeDimension, &
                 iproj=IPROJ_LATLON, &
                 startloc=SWCORNER, &
                 startlat=this%southwestLatitude, &
                 startlon=this%southwestLongitude, &
                 earth_radius=earthRadius, &
                 is_wind_grid_rel=.false., &
                 slab=this%layerVWinds(1,1,k,m), &
                 deltaLat=this%deltaLatitude, &
                 deltaLon=this%deltaLongitude)
            call writeFileWPS(wpsFileData,wpsFieldData)
            call destroyFieldWPS(wpsFieldData)

            ! Relative Humidity
            wpsFieldData = createFieldWPS( &
                 year=year, &
                 month=month, &
                 day=day, &
                 hour=hour, &
                 xfcst=0., &
                 map_source=MAP_SOURCE, &
                 field='RH', &
                 units='%', &
                 desc='Relative Humidity', &
                 xlvl=real(k), &
                 nx=this%longitudeDimension, &
                 ny=this%latitudeDimension, &
                 iproj=IPROJ_LATLON, &
                 startloc=SWCORNER, &
                 startlat=this%southwestLatitude, &
                 startlon=this%southwestLongitude, &
                 earth_radius=earthRadius, &
                 is_wind_grid_rel=.false., &
                 slab=this%layerRelativeHumidities(1,1,k,m), &
                 deltaLat=this%deltaLatitude, &
                 deltaLon=this%deltaLongitude)
            call writeFileWPS(wpsFileData,wpsFieldData)
            call destroyFieldWPS(wpsFieldData)
         end do

         ! Surface pressure
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='PSFC', &
              units='Pa', &
              desc='Surface Pressure', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%surfacePressures(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! Mean sea level pressure
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='PMSL', &
              units='Pa', &
              desc='Sea-level Pressure', &
              xlvl=201300., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%meanSeaLevelPressures(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! 10-meter U-Winds
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='UU', &
              units='m s-1', &
              desc='U                 at 10 m', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%uWinds10M(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! 10-meter V-Winds
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='VV', &
              units='m s-1', &
              desc='V                 at 10 m', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%vWinds10M(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! 2-meter temperatures
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='TT', &
              units='K', &
              desc='Temperature       at 2 m', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%temperatures2M(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! 2-meter relative humidity
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='RH', &
              units='%', &
              desc='Relative Humidity at 2 m', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%relativeHumidities2M(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! Skin temperature
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SKINTEMP', &
              units='K', &
              desc='Skin temperature (can use for SST also)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%skinTemperatures(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! Sea ice fraction
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SEAICE', &
              units='proprtn', &
              desc='Ice flag', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%seaIceFractions(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! Terrain height
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SOILHGT', &
              units='m', &
              desc='Terrain field of source analysis', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%terrainHeights, &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! Land Sea mask
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='LANDSEA', &
              units='proprtn', &
              desc='Land/Sea flag (1=land, 0 or 2=sea)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%landSea, &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

          ! Soil temperature layer 1
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='ST000010', &
              units='K', &
              desc='T 0-10 cm below ground layer (Upper)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilTemperature1(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil temperature layer 1
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='ST010020', &
              units='K', &
              desc='T 10-20 cm below ground layer (Upper)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilTemperature2(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil temperature layer 3
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='ST020040', &
              units='K', &
              desc='T 20-40 cm below ground layer (Upper)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilTemperature3(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil temperature layer 4
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='ST040080', &
              units='K', &
              desc='T 40-80 cm below ground layer (Upper)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilTemperature4(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil temperature layer 5
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='ST080150', &
              units='K', &
              desc='T 80-150 cm below ground layer (Bottom)', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilTemperature5(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil moisture layer 1
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SM000010', &
              units='m3 m-3', &
              desc='Soil moisture of 0-10 cm ground layer', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilMoisture1(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil moisture layer 2
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SM010020', &
              units='m3 m-3', &
              desc='Soil moisture of 10-20 cm ground layer', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilMoisture2(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil moisture layer 3
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SM020040', &
              units='m3 m-3', &
              desc='Soil moisture of 20-40 cm ground layer', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilMoisture3(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil moisture layer 4
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SM040080', &
              units='m3 m-3', &
              desc='Soil moisture of 40-80 cm ground layer', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilMoisture4(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Soil moisture layer 5
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SM080150', &
              units='m3 m-3', &
              desc='Soil moisture of 80-150 cm ground layer', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%soilMoisture5(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Snow Depth (SNOWH)
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SNOWH', &
              units='m', &
              desc='Physical Snow Depth', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%snowDepth(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

           ! Snow Water Equivalent (SNOW)
         wpsFieldData = createFieldWPS( &
              year=year, &
              month=month, &
              day=day, &
              hour=hour, &
              xfcst=0., &
              map_source=MAP_SOURCE, &
              field='SNOW', &
              units='kg m-2', &
              desc='Water equivalent snow depth', &
              xlvl=200100., &
              nx=this%longitudeDimension, &
              ny=this%latitudeDimension, &
              iproj=IPROJ_LATLON, &
              startloc=SWCORNER, &
              startlat=this%southwestLatitude, &
              startlon=this%southwestLongitude, &
              earth_radius=earthRadius, &
              is_wind_grid_rel=.false., &
              slab=this%swe(1,1,m), &
              deltaLat=this%deltaLatitude, &
              deltaLon=this%deltaLongitude)
         call writeFileWPS(wpsFileData,wpsFieldData)
         call destroyFieldWPS(wpsFieldData)

         ! Clean up
         call destroyFileWPS(wpsFileData)
         call closeFileUnit(fileUnit)
      end do
      write(*,*)'Done'
      return
   end subroutine writeOutput

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  initMerraDataScalars
   !
   ! DESCRIPTION:  Private method for initializing scalar members of MerraData
   ! structure.
   !
   !---------------------------------------------------------------------------

   subroutine initMerraDataScalars(this)

      ! Arguments
      type(MerraData),intent(inout) :: this

      this%longitudeDimension = 0
      this%latitudeDimension = 0
      this%verticalDimension = 0
      this%timeDimension = 0
      this%southwestLongitude = 0
      this%southwestLatitude = 0
      this%deltaLongitude = 0
      this%deltaLatitude = 0

      return
   end subroutine initMerraDataScalars

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateLandSea
   !
   ! DESCRIPTION:  Private method for calculating LANDSEA table from ocean
   ! and lake fractions.
   !
   !---------------------------------------------------------------------------

   subroutine calculateLandSea(this,oceanFractions,lakeFractions)

      ! Arguments
      type(MerraData), intent(inout) :: this
      real,intent(in) :: oceanFractions(this%longitudeDimension, &
           this%latitudeDimension)
      real,intent(in) :: lakeFractions(this%longitudeDimension, &
           this%latitudeDimension)

      ! Local variables
      real :: waterFraction
      integer :: i,j

      allocate(this%landSea(this%longitudeDimension,this%latitudeDimension))

      do j = 1, this%latitudeDimension
         do i = 1, this%longitudeDimension

            waterFraction = oceanFractions(i,j) + lakeFractions(i,j)
            if ( waterFraction < 0.5) then
               this%landSea(i,j) = 1.
            else
               this%landSea(i,j) = 0.
            end if
         end do
      end do

      return
   end subroutine calculateLandSea

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateTerrainHeights
   !
   ! DESCRIPTION:  Private method for calculating terrain heights from
   ! surface geopotentials.
   !
   !---------------------------------------------------------------------------

   subroutine calculateTerrainHeights(this)

      ! Arguments
      type(MerraData), intent(inout) :: this

      ! Local variables
      real :: inverse_G_0
      integer :: i,j

      inverse_G_0 = 1./G_0

      allocate(this%terrainHeights(this%longitudeDimension, &
           this%latitudeDimension))

      do j = 1, this%latitudeDimension
         do i = 1, this%longitudeDimension
            this%terrainHeights(i,j) = &
                 inverse_G_0 * this%surfaceGeopotentials(i,j)
         end do
      end do

      return
   end subroutine calculateTerrainHeights

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateEdgePressures
   !
   ! DESCRIPTION:  Private method for calculating pressures at each model
   ! layer edge.
   !
   !---------------------------------------------------------------------------

   subroutine calculateEdgePressures(this,layerPressureThicknesses, &
        edgePressures)

      ! Arguments
      ! Note that each model layer has two edges, so the total of
      ! all edges will be one greater than the total number of layers, i.e.,
      ! verticalDimension+1.
      type(MerraData),intent(in) :: this
      real, intent(in) :: layerPressureThicknesses(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, this%timeDimension)
      real,intent(out) :: edgePressures(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension)

      ! Local variables
      integer :: i,j,k,n

      ! According to Section 4.2 of the "File Specification for MERRA
      ! Products" document, the model top is set at 0.01 hPa, and the
      ! vertical dimension starts at model top.  The model top is the
      ! first edge; subsequent edges are determined using the pressure
      ! thickness of each layer.

      do n = 1, this%timeDimension
         do k = 1, this%verticalDimension+1
            do j = 1, this%latitudeDimension
               do i = 1, this%longitudeDimension
                  if (k == 1) then
                     edgePressures(i,j,k,n) = P_TOP
                  else
                     edgePressures(i,j,k,n) = &
                          edgePressures(i,j,k-1,n) + &
                          layerPressureThicknesses(i,j,k-1,n)
                  end if
               end do
            end do
         end do
      end do

      return
   end subroutine calculateEdgePressures

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateLayerPressures
   !
   ! DESCRIPTION:  Private method for calculating mean pressure of each model
   ! layer.
   !
   !---------------------------------------------------------------------------

   subroutine calculateLayerPressures(this,edgePressures)

      ! Arguments
      ! Note that each model layer has two edges, so the total of
      ! all edges will be one greater than the total number of layers, i.e.,
      ! verticalDimension+1.
      type(MerraData),intent(inout) :: this
      real, intent(in) :: edgePressures(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension)

      ! Internal variables
      integer :: i,j,k,n

      allocate(this%layerPressures(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, this%timeDimension))

      ! We'll define the layer pressure as the mean pressure in the layer,
      ! i.e., the mean of the two edge pressures.
      do n = 1, this%timeDimension
         do k = 1, this%verticalDimension
            do j = 1, this%latitudeDimension
               do i = 1, this%longitudeDimension
                  this%layerPressures(i,j,k,n) = (edgePressures(i,j,k,n) + &
                       edgePressures(i,j,k+1,n))*0.5
               end do
            end do
         end do
      end do

      return
   end subroutine calculateLayerPressures

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateLayerRelativeHumidities
   !
   ! DESCRIPTION:  Private method for calculating relative humidity w.r.t.
   ! liquid for each model layer.
   !
   !---------------------------------------------------------------------------

   subroutine calculateLayerRelativeHumidities(this,layerSpecificHumidities)

      ! Arguments
      type(MerraData),intent(inout) :: this
      real,intent(in) :: layerSpecificHumidities(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, this%timeDimension)

      ! Local variables
      real :: temperatureK
      real :: pressurePA
      real :: specificHumidity
      real :: satSpecificHumidity
      real :: relativeHumidity
      real :: satMixingRatio
      real :: mixingRatio
      integer :: i,j,k,n

      allocate(this%layerRelativeHumidities(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, this%timeDimension))

      do n = 1, this%timeDimension
         do k = 1, this%verticalDimension
            do j = 1, this%latitudeDimension
               do i = 1, this%longitudeDimension

                  temperatureK = this%layerTemperatures(i,j,k,n)
                  pressurePa = this%layerPressures(i,j,k,n)
                  specificHumidity = layerSpecificHumidities(i,j,k,n)
                  mixingRatio = calcMixingRatio(specificHumidity)

                  satSpecificHumidity = &
                       calcSatSpecificHumidity(temperatureK, pressurePa)
                  satMixingRatio = calcMixingRatio(satSpecificHumidity)

                  relativeHumidity = mixingRatio/satMixingRatio*100.

                  relativeHumidity = max(0.,relativeHumidity)
                  relativeHumidity = min(100.,relativeHumidity)

                  this%layerRelativeHumidities(i,j,k,n) = relativeHumidity
               end do
            end do
         end do
      end do

      return
   end subroutine calculateLayerRelativeHumidities

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateEdgeGeopotentialHeights
   !
   ! DESCRIPTION:  Private method for calculating geopotential heights at
   ! model layer edges.
   !
   !---------------------------------------------------------------------------

   subroutine calculateEdgeGeopotentialHeights(this,edgePressures, &
        layerSpecificHumidities,edgeGeopotentialHeights)

      ! Arguments
      ! Note that each model layer has two edges, so the total of
      ! all edges will be one greater than the total number of layers, i.e.,
      ! verticalDimension+1.
      type(MerraData),intent(in) :: this
      real, intent(in) :: edgePressures(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension)
      real, intent(in) :: layerSpecificHumidities(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, &
           this%timeDimension)
      real, intent(out) :: edgeGeopotentialHeights(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension)

      ! Internal variables
      real :: inverse_G_0
      real :: layerVirtualTemperature
      integer :: i,j,k,n

      inverse_G_0 = 1./G_0

      do n = 1, this%timeDimension
         ! k=1 is the model top in MERRA, but we have to start at the terrain
         ! height where surface geopotential is defined.
         do k = this%verticalDimension+1, 1, -1
            do j = 1, this%latitudeDimension
               do i = 1, this%longitudeDimension

                  if (k == this%verticalDimension+1) then
                     edgeGeopotentialHeights(i,j,k,n) = &
                          this%surfaceGeopotentials(i,j) * inverse_G_0
                  else
                     layerVirtualTemperature = &
                          calculateVirtualTemperature( &
                          this%layerTemperatures(i,j,k,n), &
                          layerSpecificHumidities(i,j,k,n))
                     edgeGeopotentialHeights(i,j,k,n) = &
                          edgeGeopotentialHeights(i,j,k+1,n) + &
                          R_d*layerVirtualTemperature*inverse_G_0 * &
                          ( log(edgePressures(i,j,k+1,n)) - &
                            log(edgePressures(i,j,k  ,n)) )
                  end if

               end do
            end do
         end do
      end do

      return
   end subroutine calculateEdgeGeopotentialHeights

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateLayerGeopotentialHeights
   !
   ! DESCRIPTION:  Private method for calculating geopotential heights at
   ! middle of each model layer.
   !
   !---------------------------------------------------------------------------

   subroutine calculateLayerGeopotentialHeights(this,edgePressures, &
        layerSpecificHumidities,edgeGeopotentialHeights)

      ! Arguments
      ! Note that each model layer has two edges, so the total of
      ! all edges will be one greater than the total number of layers, i.e.,
      ! verticalDimension+1.
      type(MerraData),intent(inout) :: this
      real,intent(in) :: edgePressures(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension)
      real,intent(in) :: layerSpecificHumidities(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, &
           this%timeDimension)
      real,intent(in) :: edgeGeopotentialHeights(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension+1, &
           this%timeDimension)

      ! Internal variables
      real :: inverse_G_0
      real :: layerVirtualTemperature
      integer :: i,j,k,n

      inverse_G_0 = 1./G_0

      allocate(this%layerGeopotentialHeights(this%longitudeDimension, &
           this%latitudeDimension, this%verticalDimension, this%timeDimension))

      do n = 1, this%timeDimension
         ! k=1 is the model top in MERRA, but we have to start at the terrain
         ! height where surface geopotential is defined.
         do k = this%verticalDimension, 1, -1
            do j = 1, this%latitudeDimension
               do i = 1, this%longitudeDimension
                  layerVirtualTemperature = &
                       calculateVirtualTemperature( &
                       this%layerTemperatures(i,j,k,n), &
                       layerSpecificHumidities(i,j,k,n))
                  this%layerGeopotentialHeights(i,j,k,n) = &
                       edgeGeopotentialHeights(i,j,k+1,n) + &
                       R_d*layerVirtualTemperature*inverse_G_0 * &
                       ( log(edgePressures(i,j,k+1,n)) - &
                         log(this%layerPressures(i,j,k,n)) )
               end do
            end do
         end do
      end do

      return
   end subroutine calculateLayerGeopotentialHeights

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateRelativeHumidities2M
   !
   ! DESCRIPTION:  Private method for calculating relative humidity w.r.t.
   ! liquid at 2 meters above model terrain.
   !
   !---------------------------------------------------------------------------

   subroutine calculateRelativeHumidities2M(this,specificHumidities2M)

      ! Arguments
      type(MerraData),intent(inout) :: this
      real,intent(in) :: specificHumidities2M(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension)

      ! Local variables
      real :: temperatureK
      real :: pressurePA
      real :: specificHumidity
      real :: satSpecificHumidity
      real :: mixingRatio
      real :: satMixingRatio
      real :: relativeHumidity
      integer :: i,j,n

      allocate(this%relativeHumidities2M(this%longitudeDimension, &
           this%latitudeDimension, this%timeDimension))
      do n = 1, this%timeDimension
         do j = 1, this%latitudeDimension
            do i = 1, this%longitudeDimension

               temperatureK = this%temperatures2M(i,j,n)
               pressurePa = this%surfacePressures(i,j,n)
               specificHumidity = specificHumidities2M(i,j,n)
               mixingRatio = calcMixingRatio(specificHumidity)

               satSpecificHumidity = &
                    calcSatSpecificHumidity(temperatureK, pressurePa)
               satMixingRatio = calcMixingRatio(satSpecificHumidity)

               relativeHumidity = mixingRatio/satMixingRatio*100.

               relativeHumidity = max(0.,relativeHumidity)
               relativeHumidity = min(100.,relativeHumidity)

               this%relativeHumidities2M(i,j,n) = relativeHumidity
            end do
         end do
      end do
      return
   end subroutine calculateRelativeHumidities2M

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  getTimeIndices
   !
   ! DESCRIPTION:  Private method for creating list of time indices, with each
   ! array index valid for a particular synoptic hour.
   !
   !---------------------------------------------------------------------------

   subroutine getTimeIndices(this,fileFormat,fileID,indices, &
        fileTimeDimension)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID
      integer,allocatable,intent(inout) :: indices(:)
      integer,intent(out) :: fileTimeDimension

      ! Local variables
      double precision, allocatable :: times(:)
      integer, allocatable :: times1(:)
      integer :: counter
      integer :: i,j

      ! Get times array
      if (fileFormat == NETCDF_FORMAT) then
         call readIntegerArray1d(fileFormat,fileID,'time', &
              fileTimeDimension, times1)
         allocate(times(fileTimeDimension))
         times(:) = dble(times1)
         do i = 1, fileTimeDimension
            times(i) = times(i)/60
         end do
      end if

      ! Count common time levels
      counter = 0
      do j = 1, this%timeDimension
         do i = 1, fileTimeDimension
            if (nint(times(i)) .eq. this%synopticHours(j)) then
               counter = counter + 1
               exit ! Get out of i do loop
            end if
         end do
      end do
      if (counter .ne. this%timeDimension) then
         print*,'ERROR, synoptic hour mismatch between MERRA files!'
         print*,'Need ',this%synopticHours(:)
         print*,'Found ',times(:)
         stop 1
      end if
      allocate(indices(this%timeDimension))
      indices(:) = 0
      do j = 1, this%timeDimension
         do i = 1, fileTimeDimension
            if (nint(times(i)) .eq. this%synopticHours(j)) then
               indices(j) = i
               exit ! Get out of i do loop
            end if
         end do
      end do
      deallocate(times)

      return
   end subroutine getTimeIndices

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  checkLatLon
   !
   ! DESCRIPTION:  Private method for checking lat/lon from MERRA file.
   !
   !---------------------------------------------------------------------------

   subroutine checkLatLon(this,fileFormat,fileID)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Local variables
      double precision, allocatable :: longitudes(:)
      double precision, allocatable :: latitudes(:)
      real :: diff
      integer :: dim1

      ! Check longitudes
      if (fileFormat == NETCDF_FORMAT) then
         call readDoubleArray1d(fileFormat,fileID,'lon',dim1,longitudes)
      end if
      if (dim1 .ne. this%longitudeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files!'
         print*,'For longitude, expected ',this%longitudeDimension, &
              ', got ',dim1
         stop 1
      end if
      diff = longitudes(2) - longitudes(1)
      if (abs(this%deltaLongitude - diff) > 0.001) then
         print*,'ERROR, resolutions conflict between MERRA files!'
         print*,'For longitude, expected ',this%deltaLongitude, &
              ', got ', diff
         stop 1
      end if
      diff = this%southwestLongitude - longitudes(1)
      if (abs(diff) > 0.001) then
         print*,'ERROR, SW longitudes conflict between MERRA files!'
         print*,'Expected ',this%southwestLongitude, &
              ', got ',longitudes(1)
         stop 1
      end if
      deallocate(longitudes)

      ! Check latitudes
      if (fileFormat == NETCDF_FORMAT) then
         call readDoubleArray1d(fileFormat,fileID,'lat',dim1,latitudes)
      end if
      if (dim1 .ne. this%latitudeDimension) then
         print*,'ERROR, dimension mismatch between MERRA files!'
         print*,'For latitude, expected ',this%latitudeDimension, &
              ', got ',dim1
         stop 1
      end if
      diff = latitudes(2) - latitudes(1)
      if (abs(this%deltaLatitude - diff) > 0.001) then
         print*,'ERROR, resolutions conflict between MERRA files!'
         print*,'For latitude, expected ',this%deltaLatitude, &
              ', got ', diff
         stop 1
      end if
      diff = this%southwestLatitude - latitudes(1)
      if (abs(diff) > 0.001) then
         print*,'ERROR, SW latitudes conflict between MERRA files!'
         print*,'Expected ',this%southwestLatitude, &
              ', got ',latitudes(1)
         stop 1
      end if
      deallocate(latitudes)

      return
   end subroutine checkLatLon

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  getLatLon
   !
   ! DESCRIPTION:  Private method for obtaining latitude and longitude data
   ! from const_2d_asm_Nx file.
   !
   !---------------------------------------------------------------------------

   subroutine getLatLon(this,fileFormat,fileID)

      ! Arguments
      type(MerraData),intent(inout) :: this
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Local variables
      double precision, allocatable :: longitudes(:)
      double precision, allocatable :: latitudes(:)
      integer :: dim1

      ! Get longitudes and latitudes
      call readDoubleArray1d(fileFormat,fileID,'XDim',dim1,longitudes)
      this%longitudeDimension = dim1
      this%deltaLongitude = longitudes(2) - longitudes(1)
      this%southwestLongitude = longitudes(1)
      deallocate(longitudes)

      call readDoubleArray1d(fileFormat,fileID,'YDim',dim1,latitudes)
      this%latitudeDimension = dim1
      this%deltaLatitude = latitudes(2) - latitudes(1)
      this%southwestLatitude = latitudes(1)
      deallocate(latitudes)

      return
   end subroutine getLatLon

   subroutine getLatLon_merra2(this,fileFormat,fileID)

      ! Arguments
      type(MerraData),intent(inout) :: this
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Local variables
      double precision, allocatable :: longitudes(:)
      double precision, allocatable :: latitudes(:)
      integer :: dim1

      ! Get longitudes and latitudes
      call readDoubleArray1d(fileFormat,fileID,'lon',dim1,longitudes)
      this%longitudeDimension = dim1
      this%deltaLongitude = longitudes(2) - longitudes(1)
      this%southwestLongitude = longitudes(1)
      deallocate(longitudes)

      call readDoubleArray1d(fileFormat,fileID,'lat',dim1,latitudes)
      this%latitudeDimension = dim1
      this%deltaLatitude = latitudes(2) - latitudes(1)
      this%southwestLatitude = latitudes(1)
      deallocate(latitudes)

      return
   end subroutine getLatLon_merra2

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  getTimesVerticalLevels
   !
   ! DESCRIPTION:  Private method for reading times and levels from MERRA
   ! file.
   !
   !---------------------------------------------------------------------------

   subroutine getTimesVerticalLevels(this,fileFormat,fileID)

      ! Arguments
      type(MerraData),intent(inout) :: this
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Local variables
      double precision, allocatable :: times(:)
      integer, allocatable :: times1(:)
      double precision, allocatable :: levels(:)
      integer :: dim1
      integer :: m


      ! Get times
      if (fileFormat == NETCDF_FORMAT) then
          call readIntegerArray1d(fileFormat,fileID,'time',dim1,times1)
          allocate(times(dim1))
          times = dble(times1)
      end if
      this%timeDimension = dim1
      allocate(this%synopticHours(dim1))
      if (fileFormat == NETCDF_FORMAT) then
         do m = 1, this%timeDimension
            this%synopticHours(m) = nint(times(m))/60
         end do
      end if
      deallocate(times)

      ! Get levels
      if (fileFormat == NETCDF_FORMAT) then
         call readDoubleArray1d(fileFormat,fileID,'lev',dim1,levels)
      end if

      this%verticalDimension = dim1
      deallocate(levels)

      return
   end subroutine getTimesVerticalLevels

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  checkTimes
   !
   ! DESCRIPTION:  Private method for reading in times from MERRA file and
   ! making sure they match what is expected.
   !
   !---------------------------------------------------------------------------

   subroutine checkTimes(this,fileFormat,fileID)

      ! Arguments
      type(MerraData), intent(inout) :: this
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Local variables
      double precision, allocatable :: times(:)
      Integer, allocatable :: times1(:)
      integer :: dim1
      integer :: n,m

      ! Get times
      if (fileFormat == NETCDF_FORMAT) then
         call readIntegerArray1d(fileFormat,fileID,'time',dim1,times1)
         allocate(times(dim1))
         times = dble(times1)
         do n = 1, dim1
            times(n) = times(n)/60
         end do
      end if

      if (dim1 .ne. this%timeDimension) then
         print*,'ERROR, dimension mismatch for time array!'
         print*,'Expected ',this%timeDimension
         print*,'Got ',dim1
         stop 1
      end if

      ! Check the times
      do n = 1, this%timeDimension
         if (this%synopticHours(n) .ne. nint(times(n))) then
            print*,'ERROR, synoptic hour mismatch between MERRA files'
            print*,'Expected ',this%synopticHours(:)
            print*,'Found ', (nint(times(m)),m=1,this%timeDimension)
            stop 1
         end if
      end do
      deallocate(times)

      return
   end subroutine checkTimes

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  dataStartsAtModelTop
   !
   ! DESCRIPTION:  Private method for reading levels from MERRA data and
   ! determining if 3D data starts at model top or at terrain level.
   !
   !---------------------------------------------------------------------------

   function dataStartsAtModelTop(fileFormat,fileID) result &
        (startAtTop)

      ! Arguments
      integer,intent(in) :: fileFormat
      integer,intent(in) :: fileID

      ! Return variable
      logical :: startAtTop

      ! Local variables
      double precision, allocatable :: levels(:)
      integer :: dim1

      ! Get levels
      if (fileFormat == NETCDF_FORMAT) then
         call readDoubleArray1d(fileFormat,fileID,'lev',dim1,levels)
      end if
      if ((levels(2) - levels(1)) .gt. 0) then
         startAtTop = .true.
      else
         startAtTop = .false.
      end if
      deallocate(levels)

      return
   end function dataStartsAtModelTop

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  flipRealArray4d
   !
   ! DESCRIPTION:  Private method for flipping vertical dimension of
   ! 4D real array.
   !
   !---------------------------------------------------------------------------

   subroutine flipRealArray4d(dim1,dim2,dim3,dim4,array)

      ! Arguments
      integer,intent(in) :: dim1,dim2,dim3,dim4
      real,intent(inout) :: array(dim1,dim2,dim3,dim4)

      ! Local variables
      real,allocatable :: tmp4d(:,:,:,:)
      integer :: i,j,k,n

      allocate(tmp4d(dim1,dim2,dim3,dim4))
      do n = 1,dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
                  tmp4d(i,j,dim3-k+1,n) = array(i,j,k,n)
               end do
            end do
         end do
      end do
      do n = 1,dim4
         do k = 1, dim3
            do j = 1, dim2
               do i = 1, dim1
                  array(i,j,k,n) = tmp4d(i,j,k,n)
               end do
            end do
         end do
      end do
      deallocate(tmp4d)
      return
   end subroutine flipRealArray4d

   !---------------------------------------------------------------------------
   !
   ! ROUTINE:  calculateVirtualTemperature
   !
   ! DESCRIPTION:  Private function for calculate virtual temperature from
   ! temperature and specific humidity.
   !
   !---------------------------------------------------------------------------

   function calculateVirtualTemperature(temperature,specificHumidity) &
        result(virtualTemperature)

      ! Arguments
      real,intent(in) :: temperature
      real,intent(in) :: specificHumidity

      ! Return argument
      real :: virtualTemperature

      ! Local variables
      real :: mixingRatio

      ! Calculate mixing ratio from specific humidity
      mixingRatio = calcMixingRatio(specificHumidity)

      ! Now calculate virtual temperature
      virtualTemperature = temperature*(1. + 0.61*mixingRatio)

      return
   end function calculateVirtualTemperature

end module MerraData_mod
