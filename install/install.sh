#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make executables to extract data from ADP BUFR
#  input files, write the data into a basic text file, and convert
#  the text files into LITTLE_R format.  It is used to
#  extract data from these kinds of files:
#      gdas.adpsfc.tHHz.YYYYMMDD.bufr 
#      gdas.sfcshp.tHHz.YYYYMMDD.bufr
#
#  bufr_adpsfc2ob.x: decodes ADPSFC observations and writes to a text file
#  bufr_sfcshp2ob.x: decodes SFCSHP observations and writes to a text file
#  surface_obs2littler.x: reads observation text files and converts to little_r format
#
#  Note: this script assumes this will be run and compiled on a linux 
#        system with the Gnu Fortran compiler
#  ------------------------------------------------------------------------
 
set -eua
 
SRC=../src
LIB=/path/to/libbufr_4.a
INCL=/path/to/bufrlib/include/directory
EXE=../exe
INSTALL=.
 
export FC=gfortran
export CC=gcc
fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
cflag=" -O3 -DUNDERSCORE -w"

#  Compile the decode programs
#  --------------------------------------- 
echo "Compiling bufr decoding programs..."
$FC $fflag -c $SRC/bufr_adpsfc2ob.f -I $INCL
$FC $fflag -c $SRC/bufr_sfcshp2ob.f -I $INCL
$FC $fflag -c $SRC/surface_obs2littler.f
 
#  link and load the executables
#  -----------------------------
echo "Linking..."
$FC $fflag -o $EXE/bufr_adpsfc2ob.x bufr_adpsfc2ob.o $LIB -I $INCL
$FC $fflag -o $EXE/bufr_sfcshp2ob.x bufr_sfcshp2ob.o $LIB -I $INCL
$FC $fflag -o $EXE/surface_obs2littler.x surface_obs2littler.o

#  clean up
#  --------
rm -f *.o *.mod
echo "Finished."