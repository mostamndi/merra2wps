#!/bin/sh
#------------------------------------------------------------------------------
# KAUST/PSPE, Climate Modiling,
#
#------------------------------------------------------------------------------
#
# SCRIPT: proc_merra2_ges_disc.sh
#
# AUTHOR:
# Suleiman Mostamandi
#
# DESCRIPTION:
# Downloading & Converting MERRA2 data to WPS Intermidiate Format
#
# REVISION HISTORY:
# 18 Sep 2017 - version 2.
#
#------------------------------------------------------------------------------

# Process command line arguments.

###if [ "$#" -ne 4 ] ; then
###    echo 'Usage: proc_merra2.sh STARTDATE ENDDATE RUNDIR NUWRFDIR'
###    echo 'Example: ./proc_merra2.sh 20160101 20160104 ./workdir ./'
###    exit 1
###fi
STARTDATE=${1-"20160101"}
ENDDATE=${2-"20160102"}
RUNDIR=${3-"./"}
NUWRFDIR=${4-"./bnd"}

# Set WORKDIR and ensure it is an absolute path. Create directory and 
# subdirectories as needed.
WORKDIR="$RUNDIR"
if [ ! -e $WORKDIR ] ; then
    mkdir $WORKDIR || exit 1
fi
cd $WORKDIR
WORKDIR=`pwd` # Absolute path.
if [ ! -e $WORKDIR/m2wIn ] ; then
    mkdir $WORKDIR/m2wIn || exit 1
fi
if [ ! -e $WORKDIR/m2wOut ] ; then
    mkdir $WORKDIR/m2wOut || exit 1
fi

# Link executables from NUWRFDIR
##ln -fs $WORKDIR/merra2wrf $WORKDIR/merra2wrf || exit 1
if [ ! -e $WORKDIR/merra2wrf ] ; then
    echo "ERROR, $WORKDIR/merra2wrf does not exist!"
    exit 1
fi
#ln -fs $NUWRFDIR/utils/geos2wrf_2/RUN_MERRA2/Get_dates_daily.py \
   $WORKDIR/Get_dates_daily.py $STARTDATE $ENDDATE 1 ${STARTDATE}.dat || exit 1
if [ ! -e $WORKDIR/Get_dates_daily.py ] ; then
    echo "ERROR, $WORKDIR/Get_dates_daily.py does not exist!"
    exit 1
fi

# Construct list of dates.
cd $WORKDIR || exit 1
DT=1 # Day
$WORKDIR/Get_dates_daily.py $STARTDATE $ENDDATE $DT ${STARTDATE}.dat || exit 1
if [ ! -e "$WORKDIR/${STARTDATE}.dat" ] ; then
    echo "ERROR, Datetime.dat not generated!"
    exit 1
fi

# Get const_2d_asm_Nx file. This is used for all dates.
cd $WORKDIR/m2wIn || exit 1
# Updated 21 March 2016
#file1=MERRA2_100.const_2d_asm_Nx.00000000.nc4 
file1=MERRA2_101.const_2d_asm_Nx.00000000.nc4 
echo $PWD

if [ ! -e "$file1" ] ; then
    WEBSITE=https://goldsmr4.gesdisc.eosdis.nasa.gov
    path=data/MERRA2_MONTHLY/M2C0NXASM.5.12.4/1980/$file1
    wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --auth-no-challenge=on --content-disposition --keep-session-cookies $WEBSITE/$path || exit 1
fi

# Loop through the dates, fetch date specific files, and run MERRA2WRF.
for date in $(cat $WORKDIR/${STARTDATE}.dat) ; do

    YEAR=`echo $date | cut -c1-4`
    MONTH=`echo $date | cut -c5-6`
    DAY=`echo $date | cut -c7-8`

    # File prefix depends on MERRA2 "data stream" which depends on year.
    if [ "$YEAR" -lt 1992 ] ; then
	MERRANAME="MERRA2_100"
    elif [ "$YEAR" -lt 2001 ] ; then
	MERRANAME="MERRA2_200"
    elif [ "$YEAR" -lt 2011 ] ; then
	MERRANAME="MERRA2_300"
    else
	MERRANAME="MERRA2_400"
    fi

    # Get inst6_3d_ana_Nv file.
    file2=$MERRANAME.inst6_3d_ana_Nv.$YEAR$MONTH${DAY}.nc4
    if [ ! -e "$file2" ] ; then
	WEBSITE=https://goldsmr4.gesdisc.eosdis.nasa.gov
	path=data/MERRA2/M2I6NVANA.5.12.4/$YEAR/$MONTH/$file2
	wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --auth-no-challenge=on --content-disposition --keep-session-cookies $WEBSITE/$path || exit 1
    fi

    # Get inst6_3d_ana_Np file.
    file3=$MERRANAME.inst6_3d_ana_Np.$YEAR$MONTH${DAY}.nc4
    if [ ! -e "$file3" ] ; then
	WEBSITE=https://goldsmr4.gesdisc.eosdis.nasa.gov/
	path=data/MERRA2/M2I6NPANA.5.12.4/$YEAR/$MONTH/$file3
	wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --auth-no-challenge=on --content-disposition --keep-session-cookies $WEBSITE/$path || exit 1
    fi

    # Get tavg1_2d_slv_Nx file.
    file4=$MERRANAME.tavg1_2d_slv_Nx.$YEAR$MONTH${DAY}.SUB.nc4
    if [ ! -e "$file4" ] ; then
	WEBSITE=https://goldsmr4.gesdisc.eosdis.nasa.gov/
	path=data/MERRA2/M2T1NXSLV.5.12.4/$YEAR/$MONTH/$file4
	wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --auth-no-challenge=on --content-disposition --keep-session-cookies $WEBSITE/$path || exit 1
    fi

    # Get tavg1_2d_ocn_Nx file.
    file5=$MERRANAME.tavg1_2d_ocn_Nx.$YEAR$MONTH${DAY}.SUB.nc4
    if [ ! -e "$file5" ] ; then
	WEBSITE=https://goldsmr4.gesdisc.eosdis.nasa.gov/
	path=data/MERRA2/M2T1NXOCN.5.12.4/$YEAR/$MONTH/$file5
	wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --auth-no-challenge=on --content-disposition --keep-session-cookies $WEBSITE/$path || exit 1
    fi

    # Get tavg1_2d_lnd_Nx file.
    file6=$MERRANAME.tavg1_2d_lnd_Nx.$YEAR$MONTH${DAY}.nc4
    if [ ! -e "$file6" ] ; then
	WEBSITE=https://goldsmr4.gesdisc.eosdis.nasa.gov/
	path=data/MERRA2/M2T1NXOCN.5.12.4/$YEAR/$MONTH/$file6
	wget --load-cookies ./.urs_cookies --save-cookies ./.urs_cookies --auth-no-challenge=on --content-disposition --keep-session-cookies $WEBSITE/$path || exit 1
    fi

    
    MERRADATE="$YEAR-$MONTH-$DAY"
    MERRADATE2="$YEAR$MONTH$DAY"

    # Now run MERRA2WRF for the collected data.
    cd $WORKDIR || exit 1
    cat > namelist.merra2wrf_$STARTDATE <<EOF
&input
    outputDirectory = '$WORKDIR/m2wOut',
    merraDirectory = '$WORKDIR/m2wIn',
    merraFormat_const_2d_asm_Nx = 2,
    merraFile_const_2d_asm_Nx = '$file1',
    numberOfDays=1,
    !withAerosol=0,
    merraDates(1)="$MERRADATE",
    merraFormat_inst6_3d_ana_Nv = 2,
    merraFiles_inst6_3d_ana_Nv(1) = '$file2',
    merraFormat_inst6_3d_ana_Np = 2,
    merraFiles_inst6_3d_ana_Np(1) = '$file3',
    merraFormat_tavg1_2d_slv_Nx = 2,
    merraFiles_tavg1_2d_slv_Nx(1) = '$file4',
    merraFormat_tavg1_2d_ocn_Nx = 2,
    merraFiles_tavg1_2d_ocn_Nx(1) = '$file5',
    merraFormat_tavg1_2d_lnd_Nx = 2,
    merraFiles_tavg1_2d_lnd_Nx(1) = '$file6',
    !merraFormat_inst3_3d_aer_Nv = 2,
    !merraFiles_inst3_3d_aer_Nv(1) = '$file7',
/
EOF
    $WORKDIR/merra2wrf namelist.merra2wrf_$STARTDATE || exit 1

    cd $WORKDIR/m2wIn || exit 1

done

# The End.
echo "MERRA2WRF output files are in $WORKDIR/m2wOut"
echo "Completed MERRA2 processing"
exit 0


