!------------------------------------------------------------------------------
! KAUST/ESEP, Climate Modeling
!------------------------------------------------------------------------------
!
! PROGRAM:  merra2wrf
!
! AUTHOR:
! Suleiman Mostamandi, KAUST ESEP/PSE
! This code is modified version of Eric Kemp, NASA code
!
! DESCRIPTION:
! Reads model-level MERRA NetCDF4 files from GES DISC web site and creating WPS
! intermediate files.
!
! REVISION:
! 13 Jan 2017:  Started 1.0
!
!------------------------------------------------------------------------------

program merra2wrf

   use MerraData_mod
   use NamelistMerra_mod
   use FileUtils_mod

   implicit none

   ! Input variables
   type(NamelistMerra) :: namelist
   character(len=132) :: filename
   character(len=132) :: inputs
   character (len=132) arg
   type(MerraData) :: merra
   integer :: i, iargc, numarg

   numarg = iargc()
   if (numarg < 1) then
      print *,' First argument should be namelist file name:'
      print *,' merra2wrf <namelist>'
      stop 1
   end if
   call getarg(1, arg)
   write(inputs, '(a)') arg

   ! Process namelist input
   namelist = createNamelistMerra(trim(inputs))

   ! Loop through the MERRA files
   do i = 1, namelist%numberOfDays

      ! Create the merra object
      merra = createMerraData()

      write(*,'(a,i2,a)')'Reading file set ',i,' ...'

      ! Process the const_2d_asm_Nx file
      write(filename,'(a,a,a)') &
           trim(namelist%merraDirectory),'/', &
           trim(namelist%merraFile_const_2d_asm_Nx)
      print*,'   const_2d_asm_Nx'

      call readConst2dAsmNx(merra,namelist%merraFormat_const_2d_asm_Nx, &
           filename)

      ! Process the inst6_3d_ana_Nv file
      write(filename,'(a,a,a)') trim(namelist%merraDirectory),'/', &
           trim(namelist%merraFiles_inst6_3d_ana_Nv(i))
      print*,'   inst6_3d_ana_Nv'

      call readInst63dAnaNv(merra,namelist%merraFormat_inst6_3d_ana_Nv, &
           filename)

      ! Process the inst6_3d_ana_Np file
      write(filename,'(a,a,a)') trim(namelist%merraDirectory),'/', &
           trim(namelist%merraFiles_inst6_3d_ana_Np(i))
      print*,'   inst6_3d_ana_Np'

      call readInst63dAnaNp(merra,namelist%merraFormat_inst6_3d_ana_Np, &
           filename)

      ! Process the tavg1_2d_slv_Nx file
      write(filename,'(a,a,a)') trim(namelist%merraDirectory),'/', &
           trim(namelist%merraFiles_tavg1_2d_slv_Nx(i))
      print*,'   tavg1_2d_slv_Nx'

      call readTavg12dSlvNx(merra,namelist%merraFormat_tavg1_2d_slv_Nx, &
           filename)

      ! Process the tavg1_2d_flx_Nx file
      write(filename,'(a,a,a)') trim(namelist%merraDirectory),'/', &
           trim(namelist%merraFiles_tavg1_2d_ocn_Nx(i))
      print*,'   tavg1_2d_ocn_Nx'

      call readTavg12dOcnNx(merra, namelist%merraFormat_tavg1_2d_ocn_Nx, &
           filename)

      ! Process the tavg1_2d_lnd_Nx file
      write(filename,'(a,a,a)') trim(namelist%merraDirectory),'/', &
           trim(namelist%merraFiles_tavg1_2d_lnd_Nx(i))
      print*,'   tavg1_2d_lnd_Nx'

      call readTavg2dlndNx(merra, namelist%merraFormat_tavg1_2d_ocn_Nx, &
           filename)

      ! Write out variables to WPS
      call writeOutput(merra, namelist%merraDates(i), namelist%outputDirectory)

      ! Clean up
      call destroyMerraData(merra)

   end do

   ! Clean up
   call destroyNamelistMerra(namelist)

end program merra2wrf
