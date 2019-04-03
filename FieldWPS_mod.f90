!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! MODULE:  FieldWPS_mod
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Defines FieldWPS data type and associated routines, for writing data in
! WPS intermediate format.
!
! REVISION HISTORY:
! 05 Feb 2017 - Initial version
!
!------------------------------------------------------------------------------

module FieldWPS_mod

  ! Force explicit variable declarations
  implicit none

  ! Force explicit public declarations
  private

  ! Public parameters
  public :: IPROJ_LATLON, IPROJ_MERCATOR, IPROJ_LAMBERT_CONFORMAL, &
       IPROJ_GAUSSIAN, IPROJ_POLAR_STEREOGRAPHIC
  public :: SWCORNER, CENTER
  integer,parameter :: IPROJ_LATLON = 0
  integer,parameter :: IPROJ_MERCATOR = 1
  integer,parameter :: IPROJ_LAMBERT_CONFORMAL = 3
  integer,parameter :: IPROJ_GAUSSIAN = 4
  integer,parameter :: IPROJ_POLAR_STEREOGRAPHIC = 5
  character(len=8) :: SWCORNER = 'SWCORNER'
  character(len=8) :: CENTER =   'CENTER  '

  ! Internal parameters
  integer,parameter :: VERSION_WPS = 5
  integer,parameter :: FIELD_WPS_MISSING = -9999

  ! Public data type for slab of data going to WPS intermediate
  ! format file.
  public :: FieldWPS
  type FieldWPS
     integer :: version ! Format version (must =5 for WPS format)
     integer :: nx      ! x-dimension for 2-d array
     integer :: ny      ! y-dimension for 2-d array
     integer :: iproj   ! Code for projection of data in array
                        ! = 0, cylindrical equidistant (lat/lon)
                        ! = 1, Mercator
                        ! = 3, Lambert conformal conic
                        ! = 4, Gaussian (global only)
                        ! = 5, Polar stereographic
     real :: nlats      ! Number of latitudes north of equator
                        ! (for Gaussian grids)
     real :: xfcst      ! Forecast hour of data
     real :: xlvl       ! Vertical level of data in 2-d array
     real :: startlat   ! Latitude of point in array indicated by startloc
     real :: startlon   ! Longitude of point in array indicated by startloc
     real :: deltalat   ! Grid spacing, degrees
     real :: deltalon   ! Grid spacing, degrees
     real :: dx         ! Grid spacing, km
     real :: dy         ! Grid spacing, km
     real :: xlonc      ! Standard longitude of projection
     real :: truelat1   ! True latitude 1 of projection
     real :: truelat2   ! True latitude 2 of projection
     real :: earth_radius ! Earth radius, km
     real, allocatable :: slab(:,:) ! 2-d array holding the data
     logical :: is_wind_grid_rel ! Flag indicating whether winds are relative
                                 ! to source grid (TRUE) or relative to earth
                                 ! (FALSE)
     character(len=8) :: startloc ! Which point in array is given by startlat/
                                  ! startlon; set either to 'SWCORNER' or
                                  ! 'CENTER  '
     character(len=9) :: field ! Name of the field
     character(len=24) :: hdate ! Valid data for data YYYY:MM:DD_HH:mm:ss
     character(len=25) :: units ! Units of data
     character(len=32) :: map_source ! Source model / originating center
     character(len=46) :: desc ! Short description of data
  end type FieldWPS

  ! Public methods
  public :: createFieldWPS
  public :: destroyFieldWPS
  public :: checkFieldWPS
  public :: copyFieldWPS
  public :: compareGridsFieldWPS
  public :: checkDateTimeFieldWPS

contains

  !----------------------------------------------------------------------------
  ! ROUTINE:
  ! createFieldWPS
  !
  ! DESCRIPTION:
  ! Public "constructor method" for FieldWPS data type.
  !----------------------------------------------------------------------------

  function createFieldWPS(year,month,day,hour, &
       xfcst,map_source,field,units,desc,xlvl,nx,ny,iproj, &
       startloc,startlat,startlon,earth_radius,is_wind_grid_rel,slab, &
       deltalat,deltalon,dx,dy,nlats,xlonc,truelat1,truelat2, &
       minute,second,includeMinute,includeSecond) result (this)

    ! Force explicit variable declarations
    implicit none

    ! Arguments
    integer,intent(in) :: year,month,day,hour
    real,intent(in) :: xfcst
    character(len=*),intent(in) :: map_source
    character(len=*),intent(in) :: field
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: desc
    real,intent(in) :: xlvl
    integer,intent(in) :: nx,ny
    integer,intent(in) :: iproj
    character(len=*),intent(in) :: startloc
    real,intent(in) :: startlat,startlon
    real,intent(in) :: earth_radius
    logical,intent(in) :: is_wind_grid_rel
    real,intent(in) :: slab(nx,ny)
    real,intent(in),optional :: deltalat,deltalon
    real,intent(in),optional :: dx,dy
    real,intent(in),optional :: nlats
    real,intent(in),optional :: xlonc
    real,intent(in),optional :: truelat1,truelat2
    integer,intent(in),optional :: minute,second
    logical,intent(in),optional :: includeMinute,includeSecond

    ! Return variable
    type(FieldWPS) :: this

    ! Local variables
    integer :: i,j

    ! Assignments
    this%xfcst = xfcst
    this%map_source = trim(map_source)
    this%field = trim(field)

    ! Default case
    write(this%hdate,'(I4.4,A,I2.2,A,I2.2,A,I2.2,A)') &
         year,':',month,':',day,'_',hour,':00:00'
    if (present(minute) .and. present(includeMinute)) then
       if (includeMinute) then
          ! Higher-res case
          write(this%hdate,'(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') &
               year,':',month,':',day,'_',hour,':',minute,':00'
          if (present(second) .and. present(includeSecond)) then
             if (includeSecond) then
                ! Highest-res case
                write(this%hdate,'(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
                     year,':',month,':',day,'_',hour,':',minute,':',second
             end if
          end if
       end if
    end if

    this%units = trim(units)
    this%desc = trim(desc)
    this%xlvl = xlvl
    this%nx = nx
    this%ny = ny
    this%iproj = iproj
    this%startloc = startloc
    this%startlat = startlat
    this%startlon = startlon
    this%earth_radius = earth_radius
    this%is_wind_grid_rel = is_wind_grid_rel

    if (nx < 1 .or. ny < 1) then
       print*,'ERROR, bad dimensions for slab!'
       print*,'nx = ',nx
       print*,'ny = ',ny
       stop 1
    end if
    allocate(this%slab(nx,ny))
    do j = 1,ny
       do i = 1,nx
          this%slab(i,j) = slab(i,j)
       end do
    end do

    this%deltalat = FIELD_WPS_MISSING
    if (present(deltalat)) then
       this%deltalat = deltalat
    end if

    this%deltalon = FIELD_WPS_MISSING
    if (present(deltalon)) then
       this%deltalon = deltalon
    end if

    this%dx = FIELD_WPS_MISSING
    if (present(dx)) then
       this%dx = dx
    end if

    this%dy = FIELD_WPS_MISSING
    if (present(dy)) then
       this%dy = dy
    end if

    this%nlats = FIELD_WPS_MISSING
    if (present(nlats)) then
       this%nlats = nlats
    end if

    this%xlonc = FIELD_WPS_MISSING
    if (present(xlonc)) then
       this%xlonc = xlonc
    end if

    this%truelat1 = FIELD_WPS_MISSING
    if (present(truelat1)) then
       this%truelat1 = truelat1
    end if

    this%truelat2 = FIELD_WPS_MISSING
    if (present(truelat2)) then
       this%truelat2 = truelat2
    end if

    this%version = VERSION_WPS

    return
  end function createFieldWPS

  !----------------------------------------------------------------------------
  ! ROUTINE:
  ! destroyFieldWPS
  !
  ! DESCRIPTION:
  ! Public "destructor method" for FieldWPS data type.
  !----------------------------------------------------------------------------

  subroutine destroyFieldWPS(this)

    ! Force explicit declarations
    implicit none

    ! Arguments
    type(FieldWPS),intent(inout) :: this

    ! Clean up data structure
    this%nx = FIELD_WPS_MISSING
    this%ny = FIELD_WPS_MISSING
    this%iproj = FIELD_WPS_MISSING
    this%nlats = FIELD_WPS_MISSING
    this%xfcst = FIELD_WPS_MISSING
    this%xlvl = FIELD_WPS_MISSING
    this%startlat = FIELD_WPS_MISSING
    this%startlon = FIELD_WPS_MISSING
    this%deltalat = FIELD_WPS_MISSING
    this%deltalon = FIELD_WPS_MISSING
    this%dx = FIELD_WPS_MISSING
    this%dy = FIELD_WPS_MISSING
    this%xlonc = FIELD_WPS_MISSING
    this%truelat1 = FIELD_WPS_MISSING
    this%truelat2 = FIELD_WPS_MISSING
    this%earth_radius = FIELD_WPS_MISSING
    this%is_wind_grid_rel = .false.
    this%startloc = "NULL"
    this%field = "NULL"
    this%hdate = "NULL"
    this%units = "NULL"
    this%map_source = "NULL"
    this%desc = "NULL"
    if (allocated(this%slab)) deallocate(this%slab)

    return
  end subroutine destroyFieldWPS

  !----------------------------------------------------------------------------
  ! ROUTINE:
  ! checkFieldWPS
  !
  ! DESCRIPTION:
  ! Sanity checks data in FieldWPS.
  !----------------------------------------------------------------------------

  subroutine checkFieldWPS(this)

    ! Force explicit variable declarations
    implicit none

    ! Arguments
    type(FieldWPS),intent(in) :: this

    ! Check version
    if (this%version /= VERSION_WPS) then
       print*,'ERROR, bad format version number, should be 5 for WPS!'
       print*,'this%version = ',this%version
       stop 1
    end if

    ! Check projection
    if (this%iproj /= IPROJ_LATLON .and. &
        this%iproj /= IPROJ_MERCATOR .and. &
        this%iproj /= IPROJ_LAMBERT_CONFORMAL .and. &
        this%iproj /= IPROJ_GAUSSIAN .and. &
        this%iproj /= IPROJ_POLAR_STEREOGRAPHIC) then
       print*, 'ERROR, illegal map projection selected for output!'
       print*, 'this%iproj = ',this%iproj
       stop 1
    end if

    ! Check slab dimensions
    if (this%nx < 1 .or. this%ny < 1) then
       print*,'ERROR, bad dimensions for slab!'
       print*,'this%nx = ',this%nx
       print*,'this%ny = ',this%ny
       stop 1
    end if

    ! Check startloc
    if (this%startloc /= SWCORNER .and. this%startloc /= CENTER) then
       print*,'ERROR, invalid entry for startloc!'
       print*,'this%startloc = ',this%startloc
       stop 1
    end if

    ! Check startlat
    if (this%startLat < -90 .or. this%startLat > 90) then
       print*,'ERROR, startLat not physical!'
       print*,'this%startLat = ',this%startLat
       stop 1
    end if

    ! Check startlon
    if (this%startLon < -180 .or. this%startLat > 360) then
       print*,'ERROR, startLon not physical!'
       print*,'this%startLon = ',this%startLon
       stop 1
    end if

    ! Check deltaLat
    if (this%iproj == IPROJ_LATLON) then
       if (this%deltaLat < 0) then
          print*,'ERROR, invalid deltaLat value!'
          print*,'this%deltaLat = ',this%deltaLat
          stop 1
       end if
    end if

    ! Check deltaLon
    if (this%iproj == IPROJ_LATLON .or. &
        this%iproj == IPROJ_GAUSSIAN) then
       if (this%deltaLon < 0) then
          print*,'ERROR, invalid deltaLon value!'
          print*,'this%deltaLon = ',this%deltaLon
          stop 1
       end if
    end if

    ! Check dx and dy
    if (this%iproj == IPROJ_MERCATOR .or. &
        this%iproj == IPROJ_LAMBERT_CONFORMAL .or. &
        this%iproj == IPROJ_POLAR_STEREOGRAPHIC) then
       if (this%dx < 0 .or. this%dy < 0) then
          print*,'ERROR, invalid dx, dy value!'
          print*,'this%dx = ',this%dx
          print*,'this%dy = ',this%dy
          stop 1
       end if
    end if

    ! Check nlats
    if (this%iproj == IPROJ_GAUSSIAN) then
       if (this%nlats < 0) then
          print*,'ERROR, invalid nlats value!'
          print*,'this%nlats = ',this%nlats
          stop 1
       end if
    end if

    ! Check xlonc
    if (this%iproj == IPROJ_LAMBERT_CONFORMAL .or. &
        this%iproj == IPROJ_POLAR_STEREOGRAPHIC) then
       if (this%xlonc < -180. .or. this%xlonc > 360.) then
          print*,'ERROR, invalid xlonc value!'
          print*,'this%xlonc = ',this%xlonc
          stop 1
       end if
    end if

    ! Check truelat1
    if (this%iproj == IPROJ_MERCATOR .or. &
        this%iproj == IPROJ_LAMBERT_CONFORMAL .or. &
        this%iproj == IPROJ_POLAR_STEREOGRAPHIC) then
       if (this%truelat1 < -90. .or. this%truelat1 > 90.) then
          print*,'ERROR, invalid truelat1 value!'
          print*,'this%truelat1 = ',this%truelat1
          stop 1
       end if
    end if

    ! Check truelat2
    if (this%iproj == IPROJ_LAMBERT_CONFORMAL) then
       if (this%truelat2 < -90. .or. this%truelat2 > 90.) then
          print*,'ERROR, invalid truelat2 value!'
          print*,'this%truelat2 = ',this%truelat2
          stop 1
       end if
    end if

    ! Check earth_radius
    if (this%earth_radius < 0) then
       print*,'ERROR, invalid value of earth_radius!'
       print*,'this%earth_radius = ',this%earth_radius
       stop 1
    end if

    ! Check slab
    if (.not. allocated(this%slab)) then
       print*,'ERROR, this%slab is not allocated!'
       stop 1
    end if

    return
  end subroutine checkFieldWPS

  !----------------------------------------------------------------------------
  ! ROUTINE:
  ! copyFieldWPS
  !
  ! DESCRIPTION:
  ! Public method for copying FieldWPS data type.
  !----------------------------------------------------------------------------

  function copyFieldWPS(field) result (copy)

     ! Arguments
     type(FieldWPS),intent(in) :: field

     ! Return variable
     type(FieldWPS) :: copy

     ! Local variables
     integer :: i,j

     ! Initialize variable
     call destroyFieldWPS(copy)

     ! Now copy the field
     copy%version = field%version
     copy%hdate = field%hdate
     copy%xfcst = field%xfcst
     copy%map_source = field%map_source
     copy%field = field%field
     copy%units = field%units
     copy%desc = field%desc
     copy%xlvl = field%xlvl
     copy%nx = field%nx
     copy%ny = field%ny
     copy%iproj = field%iproj

     if (field%iproj == IPROJ_LATLON) then
        copy%startloc = field%startloc
        copy%startlat = field%startlat
        copy%startlon = field%startlon
        copy%deltalat = field%deltalat
        copy%deltalon = field%deltalon
        copy%earth_radius = field%earth_radius
     else if (field%iproj == IPROJ_MERCATOR) then
        copy%startloc = field%startloc
        copy%startlat = field%startlat
        copy%startlon = field%startlon
        copy%dx = field%dx
        copy%dy = field%dy
        copy%truelat1 = field%truelat1
        copy%earth_radius = field%earth_radius
     else if (field%iproj == IPROJ_LAMBERT_CONFORMAL) then
        copy%startloc = field%startloc
        copy%startlat = field%startlat
        copy%startlon = field%startlon
        copy%dx = field%dx
        copy%dy = field%dy
        copy%xlonc = field%xlonc
        copy%truelat1 = field%truelat1
        copy%truelat2 = field%truelat2
        copy%earth_radius = field%earth_radius
     else if (field%iproj == IPROJ_GAUSSIAN) then
        copy%startloc = field%startloc
        copy%startlat = field%startlat
        copy%startlon = field%startlon
        copy%nlats = field%nlats
        copy%deltalon = field%deltalon
        copy%earth_radius = field%earth_radius
     else if (field%iproj == IPROJ_POLAR_STEREOGRAPHIC) then
        copy%startloc = field%startloc
        copy%startlat = field%startlat
        copy%startlon = field%startlon
        copy%xlonc = field%xlonc
        copy%truelat1 = field%truelat1
        copy%earth_radius = field%earth_radius
     else
        print*,'ERROR, invalid iproj value!'
        print*,'field%iproj = ',field%iproj
        stop 1
     end if

     copy%is_wind_grid_rel = field%is_wind_grid_rel

     allocate(copy%slab(field%nx,field%ny))
     do j = 1, field%ny
        do i = 1, field%nx
           copy%slab(i,j) = field%slab(i,j)
        end do
     end do

     ! Sanity check the field
     call checkFieldWPS(copy)

     return
  end function copyFieldWPS

  !----------------------------------------------------------------------------
  ! ROUTINE:
  ! compareGridsFieldWPS
  !
  ! DESCRIPTION:
  ! Public subroutine for checking grids (including map projections) between
  ! two FieldWPS data types.
  !----------------------------------------------------------------------------

  subroutine compareGridsFieldWPS(field1,field2)

     ! Arguments
     type(FieldWPS), intent(in) :: field1
     type(FieldWPS), intent(in) :: field2

     ! Make sure data types contain valid values
     call checkFieldWPS(field1)
     call checkFieldWPS(field2)

     ! Make sure dimensions and map projections match
     if (field1%nx .ne. field2%nx .or. &
         field1%ny .ne. field2%ny) then
        print*,'Grid dimension mismatch between FieldWPS data types!'
        print*,'Field1 nx, ny:  ',field1%nx, field1%ny
        print*,'Field2 nx, ny:  ',field2%nx, field2%ny
        stop 1
     end if
     if (field1%iproj .ne. field2%iproj) then
        print*,'Map projection mismatch between FieldWPS data types!'
        print*,'Field1 map projection code: ',field1%iproj
        print*,'Field2 map projection code: ',field2%iproj
        stop 1
     end if

     select case (field1%iproj)
     case (IPROJ_LATLON)
        if (field1%startloc .ne. field2%startloc .or. &
            abs(field1%startlat - field2%startlat) > 0.001 .or. &
            abs(field1%startlon - field2%startlon) > 0.001 .or. &
            abs(field1%deltalat - field2%deltalat) > 0.001 .or. &
            abs(field1%deltalon - field2%deltalon) > 0.001) then
           print*,'Map projection mismatch between FieldWPS data types!'
           print*,'Startloc for Field1 and Field2:  ',field1%startloc, &
                field2%startloc
           print*,'Startlat for Field1 and Field2:  ',field1%startlat, &
                field2%startlat
           print*,'Startlon for Field1 and Field2:  ',field1%startlon, &
                field2%startlon
           print*,'Deltalat for Field1 and Field2:  ',field1%deltalat, &
                field2%deltalat
           print*,'Deltalon for Field1 and Field2:  ',field1%deltalon, &
                field2%deltalon
           stop 1
        end if

     case (IPROJ_MERCATOR)
        if (field1%startloc .ne. field2%startloc .or. &
            abs(field1%startlat - field2%startlat) > 0.001 .or. &
            abs(field1%startlon - field2%startlon) > 0.001 .or. &
            abs(field1%dx - field2%dx) > 0.001 .or. &
            abs(field1%dy - field2%dy) > 0.001 .or. &
            abs(field1%truelat1 - field2%truelat1) > 0.001) then
           print*,'Map projection mismatch between FieldWPS data types!'
           print*,'Startloc for Field1 and Field2:  ',field1%startloc, &
                field2%startloc
           print*,'Startlat for Field1 and Field2:  ',field1%startlat, &
                field2%startlat
           print*,'Startlon for Field1 and Field2:  ',field1%startlon, &
                field2%startlon
           print*,'Dx for Field1 and Field2:  ',field1%dx, &
                field2%dx
           print*,'Dy for Field1 and Field2:  ',field1%dy, &
                field2%dy
           print*,'Truelat1 for Field1 and Field2:  ',field1%truelat1, &
                field2%truelat1
           stop 1
        end if

     case (IPROJ_LAMBERT_CONFORMAL)
        if (field1%startloc .ne. field2%startloc .or. &
            abs(field1%startlat - field2%startlat) > 0.001 .or. &
            abs(field1%startlon - field2%startlon) > 0.001 .or. &
            abs(field1%dx - field2%dx) > 0.001 .or. &
            abs(field1%dy - field2%dy) > 0.001 .or. &
            abs(field1%xlonc - field1%xlonc) > 0.001 .or. &
            abs(field1%truelat1 - field2%truelat1) > 0.001 .or. &
            abs(field1%truelat2 - field2%truelat2) > 0.001) then
           print*,'Map projection mismatch between FieldWPS data types!'
           print*,'Startloc for Field1 and Field2:  ',field1%startloc, &
                field2%startloc
           print*,'Startlat for Field1 and Field2:  ',field1%startlat, &
                field2%startlat
           print*,'Startlon for Field1 and Field2:  ',field1%startlon, &
                field2%startlon
           print*,'Dx for Field1 and Field2:  ',field1%dx, &
                field2%dx
           print*,'Dy for Field1 and Field2:  ',field1%dy, &
                field2%dy
           print*,'Xlonc for Field1 and Field2:  ',field1%xlonc, field2%xlonc
           print*,'Truelat1 for Field1 and Field2:  ',field1%truelat1, &
                field2%truelat1
           print*,'Truelat2 for Field1 and Field2:  ',field1%truelat2, &
                field2%truelat2

           stop 1
        end if

     case (IPROJ_GAUSSIAN)
        if (field1%startloc .ne. field2%startloc .or. &
            abs(field1%startlat - field2%startlat) > 0.001 .or. &
            abs(field1%startlon - field2%startlon) > 0.001 .or. &
            abs(field1%nlats - field2%nlats) > 0.001 .or. &
            abs(field1%deltalon - field2%deltalon) > 0.001) then

           print*,'Map projection mismatch between FieldWPS data types!'
           print*,'Startloc for Field1 and Field2:  ',field1%startloc, &
                field2%startloc
           print*,'Startlat for Field1 and Field2:  ',field1%startlat, &
                field2%startlat
           print*,'Startlon for Field1 and Field2:  ',field1%startlon, &
                field2%startlon
           print*,'Nlats for Field1 and Field2:  ',field1%nlats, &
                field2%nlats
           print*,'Deltalon for Field1 and Field2:  ',field1%deltalon, &
                field2%deltalon
           stop 1
        end if

     case (IPROJ_POLAR_STEREOGRAPHIC)
        if (field1%startloc .ne. field2%startloc .or. &
            abs(field1%startlat - field2%startlat) > 0.001 .or. &
            abs(field1%startlon - field2%startlon) > 0.001 .or. &
            abs(field1%xlonc - field2%xlonc) > 0.001 .or. &
            abs(field1%truelat1 - field2%truelat1) > 0.001) then

           print*,'Map projection mismatch between FieldWPS data types!'
           print*,'Startloc for Field1 and Field2:  ',field1%startloc, &
                field2%startloc
           print*,'Startlat for Field1 and Field2:  ',field1%startlat, &
                field2%startlat
           print*,'Startlon for Field1 and Field2:  ',field1%startlon, &
                field2%startlon
           print*,'Nlats for Field1 and Field2:  ',field1%xlonc, &
                field2%xlonc
           print*,'Truelat1 for Field1 and Field2:  ',field1%truelat1, &
                field2%truelat1
           stop 1
        end if

     case default

        print*,'ERROR, invalid map projection for FieldWPS data types!'
        print*,'Value is ',field1%iproj
        print*,'Valid options are: '
        print*,'    Latitude-Longitude:   ',IPROJ_LATLON
        print*,'    Mercator:             ',IPROJ_MERCATOR
        print*,'    Lambert conformal:    ',IPROJ_LAMBERT_CONFORMAL
        print*,'    Gaussian:             ',IPROJ_GAUSSIAN
        print*,'    Polar stereographic:  ',IPROJ_POLAR_STEREOGRAPHIC
        stop 1
     end select

     if (field1%earth_radius .ne. field2%earth_radius) then
        print*,'Earth radius mismatch between FieldWPS data types!'
        print*,'Earth radius for Field1 and Field2:  ',field1%earth_radius, &
             field2%earth_radius
        stop 1
     end if

     return
  end subroutine compareGridsFieldWPS

  !----------------------------------------------------------------------------
  ! ROUTINE:
  ! checkDateTimeFieldWPS
  !
  ! DESCRIPTION:
  ! Public method for checking date and time of FieldWPS data type.
  !----------------------------------------------------------------------------

  function checkDateTimeFieldWPS(this,year,month,day,hour,minute,second) &
       result(match)

     ! Arguments
     type(FieldWPS),intent(in) :: this
     integer,intent(in) :: year
     integer,intent(in) :: month
     integer,intent(in) :: day
     integer,intent(in) :: hour
     integer,intent(in) :: minute
     integer,intent(in) :: second

     ! Return variable
     logical :: match

     ! Local variables
     integer :: year_field,month_field,day_field,hour_field, &
          minute_field,second_field

     read(this%hdate,'(I4.4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2)') &
          year_field, month_field, day_field, &
          hour_field,minute_field,second_field

     match = .true.
     if (year .ne. year_field .or. &
         month .ne. month_field .or. &
         day .ne. day_field .or. &
         hour .ne. hour_field .or. &
         minute .ne. minute_field .or. &
         second .ne. second_field) then
!         print*,'ERROR, date/time mismatch for FieldWPS data type!'
!         write(6,'(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') &
!              'Expected ',year,':',month,':',day,'_',hour,':00:00'
!         write(6,'(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') &
!              'Found ',year_field,':',month_field,':',day_field,'_', &
!              hour_field,':00:00'
!         write(6,'(A,A,A,F7.0)') 'Field:  ',trim(this%field), &
!              '  Level:  ',this%xlvl
        match = .false.
     end if

  end function checkDateTimeFieldWPS
end module FieldWPS_mod
