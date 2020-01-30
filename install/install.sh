#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrupprair.x which to extract data from ADP BUFR
#  input files, and place the data into a basic text file.  It is used to
#  extract data from these kinds of files:
#      gdas.adpupa.tHHz.YYYYMMDD.bufr 
#      gdas.aircft.tHHz.YYYYMMDD.bufr
#      gdas.satwnd.tHHz.YYYYMMDD.bufr 
#      gdas.aircar.tHHz.YYYYMMDD.bufr
#
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#  ** Make sure the "ar" command location has been set in your path
#  environment variable.  Type "which ar" to check if this is done. **
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=../lib
EXE=../exe
INSTALL=.

#  different platforms use different link name protocols
#  -----------------------------------------------------

# if using linux, BUFR files must be run through the "grabbufr/grabbufr.sh" script
# with the resulting output used as input for the decoders.  Set appropriate compiler
# in grabbufr.sh, and exe/convert.csh
 
cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   export FC=gfortran
   export CC=gcc
   fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
   cflag=" -O3 -DUNDERSCORE -w"
fi

#  Compile and archive the Bufr Library
#  ------------------------------------
echo "Compiling BUFRLIB Library..."
cd $LIB
if [ -e bufrlib.a ]
then
  rm bufrlib.a
fi
$LIB/makebufrlib.sh
cd $INSTALL

#  Compile the decode programs
#  ---------------------------------------
 
echo "Compiling bufr_configdecode_ADPupa programs..."
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
