Merra2wrf is a utility for preparing intermediate files that can be read by metgrid.exe in the same way as ungrib.exe do it. 
It reads MERRA2 datasets files in netcdf4 format. 
It uses 73 model levels and 5 soil depths: 
Soil moisture interpolated to 5th soil levels 
(originally it has only two soil moisture level: 0-5 cm; 0-100 cm) 
but now it has these 5 levels : 0-10; 10-20; 20-40; 40-80 and  80-150 cm. 

Requirement:
NETCDF Library 
NETCDF must be compiled with hdf and hdf4 support

optional: 
HDF5, HDF4 Library 
HDF must be compiled with zlib and zslib support 
Code is written if Fortran 90

How to compile: 
First you have to edit Makefile and set the correct path of your libraries   
then from command line samply type make by the end you will get merra2wrf executable file

How to run:
You can run either manually or using already prepared script. 
Manually run  
Make sure you have downloaded all these files : 
Files are shown for only 01.01.2015 
MERRA2_400.inst6_3d_ana_Nv.20150101.nc4
For (P, H, T, Qv in 73 model levels)
MERRA2_400.inst6_3d_ana_Np.20150101.nc4
For (Ps, SLP)
MERRA2_400.tavg1_2d_slv_Nx.20150101.nc4
For (LandSea mask, HGT, T2m, RH2m, U10m, V10m, skin temperature)
MERRA2_400.tavg1_2d_ocn_Nx.20150101.nc4
For (SST, SeaIce fraction)
MERRA2_400.tavg1_2d_lnd_Nx.20150101.nc4
For (soil temperature in fifth depths, and soil moisture in two depths)
Now edit namelist.merra2wrf
&input
    outputDirectory = '/home/suleiman/WRF/DATA/DOMAINS/merra2/boundary/m2wOut',       # Folder where be located created intermediate files  
    merraDirectory = '/home/suleiman/WRF/DATA

After creating namelist.merra2wrf
Just type merra2wrf namelist.merra2wrf
After success ending of program you will get :
-rw-rw-r-- 1 suleiman suleiman 377717104 Feb 25 19:30 MERRA:2015-01-01_00
-rw-rw-r-- 1 suleiman suleiman 377717104 Feb 25 19:30 MERRA:2015-01-01_06
-rw-rw-r-- 1 suleiman suleiman 377717104 Feb 25 19:30 MERRA:2015-01-01_12
-rw-rw-r-- 1 suleiman suleiman 377717104 Feb 25 19:30 MERRA:2015-01-01_18

Using script.
The script for running merra2wrf was written
Script looks for necessary files if don’t find then try to download 
Creates namelist.merra2wrf 
Creates input and output folder 
Starts run merra2wrf
 How to run script:
 Usage: proc_merra2wrf.sh STARTDATE ENDDATE RUNDIR OUTDIR
STARTDATE – YYYYMMDD
ENDDATE     – YYYYMMDD
RUNDIR        - folder where your executable files and merra2 files
OUTDIR        - path to output files.


Before to run metgrid.exe and real.exe
Copy the modified METGRID.TLB
In namelist.input 
Edit num_metgrid_soil_levels = 5,
num_metgrid_levels           = 73, 
In namelist.wps
fg_name = 'MERRA', 





 

