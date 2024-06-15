#!/bin/csh

# Script to process all BUFR tar files located in "bufrdecodelr/bufrobs"

# !! Edit procdir directory to reflect your local system !!

set procdir=`pwd`/..

cd $procdir/bufrobs

foreach file (gdassfcobs.*????.tar*)

# gdassfcobs.yyyymmdd.tar.gz
   if ("$file" =~ *.gz)  then
       tar -xvzf $file
   else
       tar -xvf $file
   endif
end

foreach dir (sfcobs.*????)

 set date=`echo $dir | awk -F. '{print $2}'` 

  foreach hour (`ls -1 $dir | cut -d. -f3 | sort | uniq`)
      set hh=`echo $hour | sed 's|[A-Za-z]||g'`
    
      set datehh=$date$hh
    
      echo $datehh
      echo $hour
    
      echo "$procdir/exe/bufr_adpsfc2ob.x $procdir/bufrobs/sfcobs.$date/gdas.adpsfc.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_adpsfc2ob.x $procdir/bufrobs/sfcobs.$date/gdas.adpsfc.$hour.$date.bufr $datehh 
      echo "$procdir/exe/bufr_sfcshp2ob.x $procdir/bufrobs/sfcobs.$date/gdas.sfcshp.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_sfcshp2ob.x $procdir/bufrobs/sfcobs.$date/gdas.sfcshp.$hour.$date.bufr $datehh   

      echo adpsfc$datehh.obs >files.txt
      echo sfcshp$datehh.obs >>files.txt 
    
      echo "$procdir/exe/surface_obs2littler.x files.txt $datehh"
      $procdir/exe/surface_obs2littler.x files.txt $datehh
    
      rm adpsfc$datehh.obs
      rm sfcshp$datehh.obs
    
      rm files.txt
    
      mv *OBS* $procdir/lrobs
  end
   
end
