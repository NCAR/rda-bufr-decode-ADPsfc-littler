!  This program prepare observation data file little_r from all data set iwritten
!  in a standard format. All input data files are given for a single time. A file
!  is required which will contain the names of all data files.
!================================================================================
      program makeobs
!================================================================================
!     ... this is a little testing routine that is supposed to generate a 
!         single sounding that the objective analysis program will ingest

!     ... pressure is in Pa, height in m, temperature and dew point are in
!         K, speed is in m/s, and direction is in degrees

!     ... sea level pressure is in Pa, terrain elevation is in m, latitude
!         is in degrees N, longitude is in degrees E

!     ... to put in a surface observation, make only a single level "sounding"
!         and make the value of the height equal the terrain elevation -- PRESTO!

!     ... the first 40 character string may be used for the description of
!         the station (i.e. name city country, etc)

!     ... the second character string we use for our source

!     ... the third string should be left alone, it uses the phrase "FM-35 TEMP"
!         for an upper air station, and should use "FM-12 SYNOP" for surface data

!     ... the fourth string is unused, feel free to experiment with labels!

!     ... bogus data are not subject to quality control
!=================================================================================
      parameter (kx=500)
      integer i, iargc, n
      real p(kx),z(kx),t(kx),td(kx),spd(kx),dir(kx)
      logical bogus
      character*30 fin(20),fout1,fout2
      character*6 dname,staid
      character   argv*200, mdate*12
      character*13 time_tag
      character*40 fn,stndesc,codestr
      data iunit,iounit1,iounit2/114,150,151/
      parameter (maxob=9999999)

      n = iargc()

      call getarg( 1, argv )
      fn=argv
      call getarg(2,argv)
      time_tag=argv

c*      write(*,*)'give the filename for datafile list :'
c*	read(*,fmt='(a30)')fn
	open(1,file=fn,form='formatted',status='old')

	m = iunit
	do i=1,10
      read(1,fmt='(a30)',end=222)fin(i)
	open(m,file=fin(i),status='old',form='formatted')
	m = m + 1
	enddo

c*222   write(*,*)'What is time tag (YYYY-MM-DD_HH): '
c*      read(*,fmt='(a13)')time_tag

222      fout1 = 'OBS:'//time_tag
      fout2 = 'SURFACE_OBS:'//time_tag
	open(iounit1,file=fout1,status='unknown')
	open(iounit2,file=fout2,status='unknown')
!
!  start of file reading loop
!
      i = 1

      do 333 iu=iunit,m-1
      read(iu,fmt='(i10)') mdatea
!
!  reading a file in the list


      do 444 iter=1,maxob

      call miss(kx,p,z,t,td,spd,dir,slp,ter,dname,staid)
      call getdat(iu,isurf,nlev,p,z,t,td,spd,dir,slp,ter,
     & xlat,xlon,dname,staid,bogus,iflag,mdate)
      call wmo_codename(isurf,dname,codestr) 
      stndesc = ' '//staid//'     get data information here.  ' 
	if (iflag .eq. 0)then
        write(*,111)fin(i)
111   format('finished file : ',a30)
        i=i+1
        goto 333
        endif

! Set desired area

      if(xlat .ge. -90.0 .and. xlat .le. 90.0 ) then
      if(xlon .ge. -180.0 .and. xlon .le. 180.0 ) then 

      if ( isurf .eq. 1 ) then
         call write_obs (p,z,t,td,spd,dir, 
     &                   slp, ter, xlat, xlon, mdate, nlev, stndesc,
     &         'SURFACE DATA FROM ??????????? SOURCE    ',
     &         codestr                                   ,
     &         '                                        ',
     &         bogus , iseq_num , isurf, iounit1, iounit2 )
      else
         call write_obs (p,z,t,td,spd,dir, 
     &                   slp, ter, xlat, xlon, mdate, nlev, stndesc,
     &         'SOUNDINGS FROM ????????? SOURCE         ',
     &         codestr                                   ,
     &         '                                        ',
     &         bogus , iseq_num , isurf, iounit1, iounit2 )
      endif
     
      endif
      endif

444   continue
     
333   continue
  
      close(iounit1)
      close(iounit2)

20    stop 99999
      end
!
!  Subroutine to fill -888888. in place of missing data
!
      subroutine miss(kx,p,z,t,td,spd,dir,slp,ter,dname,staid)
      real p(kx),z(kx),t(kx),td(kx),spd(kx),dir(kx)
      character*6 dname,staid

      do k=1,kx
      p(k)=-888888.
      z(k)=-888888.
      t(k)=-888888.
      td(k)=-888888.
      spd(k)=-888888.
      dir(k)=-888888.
      enddo
      slp=-888888.
      ter=-888888.
      dname = '99001 '
      staid = '99001 '
      return
      end
!
! subroutine to read data from file unit
!
      subroutine getdat(iunit,isurf,nlev,p,z,t,td,spd,dir,slp,ter,
     &  xlat,xlon,dname,staid,bogus,iflag,mdate)

      parameter (kx=500)
      real p(kx),z(kx),t(kx),td(kx),spd(kx),dir(kx)
      real px(kx),zx(kx),tx(kx),tdx(kx),spdx(kx),dirx(kx)
      character*6 dname,staid
      character*12 mdate
      logical bogus

      bogus=.TRUE.
      dmiss = 99999.9

      read(iunit,113,end=1000)isurf,dname,staid,mdate,xlat,
     &     xlon,xter,xslp,nlev,ibogus
113    format(i1,1x,a6,1x,a6,1x,a12,4(f7.1,1x),i3,1x,i1)
      if (xter .ne. dmiss) ter = xter
      if (xslp .ne. dmiss) slp = xslp*100.

      if(ibogus.eq.0) bogus=.FALSE.

      if (isurf .eq. 1)then
        do kk=1,nlev

          read(iunit,114,end=1000) px(kk),zx(kk),tx(kk),tdx(kk),
     &    dirx(kk),spdx(kk)

          if (px(kk) .ne. dmiss) p(kk) = px(kk)*100.
     
	  if (zx(kk) .ne. dmiss) z(kk) = zx(kk)

	  if (tx(kk) .ne. dmiss) t(kk) = tx(kk)
  
          if (tdx(kk) .ne. dmiss) td(kk) = tdx(kk) 

	  if (dirx(kk) .ne. dmiss) dir(kk) = dirx(kk)

	  if (spdx(kk) .ne. dmiss) spd(kk) = spdx(kk)

        enddo

      else

	do kk=1,nlev

	  read(iunit,114,end=1000) px(kk),zx(kk),tx(kk),tdx(kk),
     &  dirx(kk),spdx(kk)
          
        if (px(kk) .ne. dmiss) p(kk) = px(kk)*100.

	  if (zx(kk) .ne. dmiss) z(kk) = zx(kk)

	  if (tx(kk) .ne. dmiss) t(kk) = tx(kk)

          if (tdx(kk) .ne. dmiss) td(kk) = tdx(kk) 

	  if (dirx(kk) .ne. dmiss) dir(kk) = dirx(kk)

	  if (spdx(kk) .ne. dmiss) spd(kk) = spdx(kk)

	enddo

      endif

	iflag = 1
	goto 2000

114    format(6(f7.1,1x))
1000	iflag = 0
2000  return
      end
!   Subroutine to put WMO code for each type of data
      
      subroutine wmo_codename(isurf,dname,codestr) 
      
      character dname*6, codestr*40
      integer isurf

      if (isurf .eq. 1 ) then

      if (dname .eq. '  SHIP' ) then
        codestr = 'FM-13 SHIP'
      elseif(dname .eq. '  BUOY') then
        codestr = 'FM-18 BUOY' 
      elseif(dname .eq. ' QSCAT') then 
        codestr = 'FM-281 QSCAT'
      elseif(dname .eq. ' METAR' .or. dname .eq. 'SPECI ') then
        codestr = 'FM-16 METAR' 
      elseif(dname .eq. '  SSMI') then
        codestr = 'FM-125 SSMI'
      else
        codestr = 'FM-12 SYNOP'
      endif 

      else

      if(dname .eq. 'PILOT ' ) then
        codestr = 'FM-32 PILOT'
      elseif(dname .eq. ' AMDAR' ) then
        codestr = 'FM-42 AMDAR'
      elseif(dname .eq. ' AIREP' ) then
        codestr = 'FM-96 AIREP'
      elseif(dname .eq. ' SATOB' ) then
        codestr = 'FM-88 SATOB'
      elseif(dname .eq. ' SATEM' ) then
        codestr = 'FM-86 SATEM'
      elseif(dname .eq. '  TOVS' ) then
        codestr = 'FM-131 TOVS'
      else
        codestr ='FM-35 TEMP'
      endif

      endif
!------------------------------------------------------------------------------!
! Given the WMO code fm, return the observation platform type and increment
! the corresponding counter if present.
!
! Returned platforms are reduced to 13 output classes:
!
!   Name    WMO Codes     WMO Code names
!   synop    12,14       'SYNOP','SYNOP MOBIL'
!   ship     13          'SHIP'
!   metar    15,16       'METAR','SPECI'
!   buoy     18          'BUOY'
!   pilot    32,33,34    'PILOT','PILOT SHIP','PILOT MOBIL'
!   sound    35,36,37,38 'TEMP','TEMP SHIP, 'TEMP DROP','TEMP MOBIL'
!   amdar    42          'AMDAR'
!   satem    86          'SATEM'
!   satob    88          'SATOB'
!   airep    96,97       'AIREP'
!   gpspw    111         'GPSPW'
!   ssmt1    121         'SSMT1'
!   ssmt2    122         'SSMT2'
!   ssmi     125,126     'SSMI'
!   tovs     131         'TOVS'
!   qscat    281         'Quikscat'
!   profl    132         'Profilers'
!   other Any other code 'UNKNOWN'
!------------------------------------------------------------------------------!
      return
      end

!
!  Subroutine to write data in a specified format for little_r
!
      SUBROUTINE write_obs ( p , z , t , td , spd , dir , 
     &                      slp , ter , xlat , xlon , mdate , kx , 
     & string1 , string2 , string3 , string4 , bogus , iseq_num ,
     & isurf, iounit1, iounit2 )

      dimension p(kx), z(kx),t(kx),td(kx),spd(kx),dir(kx)

      integer isurf, iounit1, iounit2
      character *20 date_char
      character *12 mdate
      character *40 string1, string2 , string3 , string4
      CHARACTER *84  rpt_format 
      CHARACTER *22  meas_format 
      CHARACTER *14  end_format
      logical bogus
! changed Osuri
      iseq_num =0      
! changed Osuri end


      rpt_format =  ' ( 2f20.5 , 2a40 , '
     &             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '
     &             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) ' 
      write (date_char(7:18),fmt='(a12)') mdate
!     if (mdate/1000000 .GT. 70 ) then
!        date_char(7:8)='19'
!     else
!        date_char(7:8)='20'
!     endif
      date_char(19:20)='00'
      date_char(1:6)='      '

      if ( isurf .eq. 1 ) then

      WRITE ( UNIT = iounit2 , ERR = 19 , FMT = rpt_format )
     &        xlat,xlon, string1 , string2 , 
     &        string3 , string4 , ter, kx, 0,0,iseq_num,0, 
     &        .true.,bogus,.false., 
     &         -888888, -888888, date_char , 
     &         slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0, 
     &               -888888.,0, 
     &               -888888.,0, -888888.,0, -888888.,0, -888888.,0, 
     &               -888888.,0, 
     &               -888888.,0, -888888.,0
   
      do 200 k = 1 , kx
         WRITE ( UNIT = iounit2 , ERR = 19 , FMT = meas_format ) 
     &          p(k), 0, z(k),0, t(k),0, td(k),0, 
     &          spd(k),0, dir(k),0, 
     &          -888888.,0, -888888.,0,-888888.,0, -888888.,0
200   continue
      WRITE ( UNIT = iounit2 , ERR = 19 , FMT = meas_format ) 
     & -777777.,0, -777777.,0,float(kx),0,
     & -888888.,0, -888888.,0, -888888.,0, 
     & -888888.,0, -888888.,0, -888888.,0, 
     & -888888.,0
      WRITE ( UNIT = iounit2 , ERR = 19 , FMT = end_format )  kx, 0, 0
      
      endif

      WRITE ( UNIT = iounit1 , ERR = 19 , FMT = rpt_format ) 
     &        xlat,xlon, string1 , string2 , 
     &        string3 , string4 , ter, kx, 0,0,iseq_num,0, 
     &        .true.,bogus,.false., 
     &         -888888, -888888, date_char , 
     &         slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0, 
     &               -888888.,0, 
     &               -888888.,0, -888888.,0, -888888.,0, -888888.,0, 
     &               -888888.,0, 
     &               -888888.,0, -888888.,0
   
      do 100 k = 1 , kx
         WRITE ( UNIT = iounit1 , ERR = 19 , FMT = meas_format ) 
     &          p(k), 0, z(k),0, t(k),0, td(k),0, 
     &          spd(k),0, dir(k),0, 
     &          -888888.,0, -888888.,0,-888888.,0, -888888.,0
100   continue
      WRITE ( UNIT = iounit1 , ERR = 19 , FMT = meas_format ) 
     & -777777.,0, -777777.,0,float(kx),0,
     & -888888.,0, -888888.,0, -888888.,0, 
     & -888888.,0, -888888.,0, -888888.,0, 
     & -888888.,0
      WRITE ( UNIT = iounit1 , ERR = 19 , FMT = end_format )  kx, 0, 0

      return
19    continue
      print *,'troubles writing a sounding'
      stop 19
      END
