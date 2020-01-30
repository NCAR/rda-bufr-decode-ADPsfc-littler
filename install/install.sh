#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make the following executables:
#
#  bufr_sfc2ob.x:       read ADPSFC BUFR input files and
#                       write output to formatted ASCII files
#  bufr_ship2ob.x:      read SFCSHIP BUFR input files and 
#                       write output to formatted ASCII files
#  dumpbufr.x:          used to dump all contents of a BUFR file.
#  runob2lit_imd_obs.x: combine and convert obs formatted files into 
#                       a single file in little_r format
#  ------------------------------------------------------------------------

set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=/path/to/BUFRLIB
EXE=../exe
INSTALL=.

#  different platforms use different link name protocols
#  -----------------------------------------------------

cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   export FC=gfortran
   export CC=gcc
   fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
   cflag=" -O3 -DUNDERSCORE -w"
fi

cd $INSTALL

#  Compile the decode programs
#  ---------------------------------------
 
echo "Compiling bufr_decode_ADPsfc programs..."
$FC $fflag -c $SRC/dumpbufr.f
$FC $fflag -c $SRC/bufr_sfc2ob.f
$FC $fflag -c $SRC/bufr_ship2ob.f

$FC $fflag -c $SRC/runob2lit_imd_obs.f
 
#  link and load the executables
#  -----------------------------

echo "Linking..."
$FC $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a

$FC $fflag -o $EXE/bufr_sfc2ob.x bufr_sfc2ob.o $LIB/bufrlib.a
$FC $fflag -o $EXE/bufr_ship2ob.x bufr_ship2ob.o $LIB/bufrlib.a

$FC $fflag -o $EXE/runob2lit_imd_obs.x runob2lit_imd_obs.o
#  clean up
#  --------

rm -f *.o

echo "Finished."
