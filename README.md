# rda-bufr-decode-ADPsfc-littler

This project contains Fortran source code to read BUFR files containing NCEP ADP surface observations,
convert the observations to ASCII format, and then convert the ASCII data to little_r format. The 
little_r formatted files then can be used as input to the [WRF model](https://www2.mmm.ucar.edu/wrf/users/).  
BUFR observation files are archived and available for download in the 
[NCAR Research Data Archive (RDA) dataset ds461.0](https://doi.org/10.5065/4F4P-E398).

To compile, run the `install.sh` script under the `/install` directory to complete the compilations.
The most recent version of BUFRLIB is required to compile this code; software download and 
installation instructions are provided at:

https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php

The executables will be placed in the `/exe` directory.  Run
the desired executable and enter the BUFR input file name to extract
the basic meteorological varibles into formatted text format.  
```
exe/bufr_adpsfc2ob.x:        used to convert gdas.adpsfc.tHHz.YYYYMMHH.bufr files to ASCII format.
exe/bufr_sfcshp2ob.x:        used to convert gdas.sfcshp.tHHz.YYYYMMHH.bufr files to ASCII format.
exe/surface_obs2littler.x:  used to convert/combine obs format files into one little_r format file.
```

# References
A guide to the NCEP BUFR libraries can be found at:
https://emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib.php

Definitions for BUFR MNEMONIC headers can be found in the doc directory 
and at: 
https://www.emc.ncep.noaa.gov/emc/pages/infrastructure/bufrlib/tables/bufrtab_tableb.html

WRF documentation on the LITTLE_R format:
https://www2.mmm.ucar.edu/wrf/users/wrfda/OnlineTutorial/Help/littler.html
