      PARAMETER (MXMN = 10)
      PARAMETER (MXLV = 255)

      REAL*8 idarr(MXMN, MXLV), 
     +       locarrt(MXMN, MXLV),
     +       locarrh(MXMN, MXLV), 
     +       locarrv(MXMN, MXLV),
     +       obsarr(MXMN,MXLV),
     +       obsarrh(MXMN,MXLV) 

c  BUFR mnemonics
      CHARACTER*40 idstr, locstrt, locstrh, locstrv, obstr, obstrh
      DATA idstr  /'WMOB WMOS RPID SMID STSN SSTN LSTN      '/
      DATA locstrt/'YEAR MNTH DAYS HOUR MINU                '/
      DATA locstrh/'CLAT CLON CLATH CLONH                   '/
      DATA locstrv/'SELV PRES PMSL                          '/
      DATA obstr  /'WDIR WSPD TMDB TMDP                     '/
!      DATA obstrh /'REHU TP03 TP24 HOCB CLTP                '/

      parameter(iu=9, iou=10, lunit=11)
      parameter(nz=999999)
      parameter(dumm=99999.9)

      real wmob, wmos
      character*40 rpid, smid, stsn, sstn, lstn
      real selv, pres, pmsl
      real wspd, wdir, tmdb, tmdp
      real lat, lon
      real wlon, elon, slat, nlat

      integer nlev, iargc, n
      integer i, idate

      character*8 csubset
      character*200 inf
      character*30 fout
      character*10 date_tag, date
      character*6 dname
      character*300 argv
      character*2 mins

      INTEGER lun, il, im
      CHARACTER*40 adpsfcname, adpsfcid, adpsfcsource

      integer iogce, mtyp, msbt, lcmmsbt, iermsbt
      character*80 cmmsbt

C*-----------------------------------------------------------------------
c*    Read the command-line arguments
c*      
        n = iargc()
        IF (n .GE. 2) THEN
          call getarg( 1, argv )
          inf=argv
          call getarg(2,argv)
          date_tag=argv
          IF (n .eq. 6) THEN  ! User-specified lat/lon boundaries
            call getarg(3,argv)
            read(argv,*) wlon
            call getarg(4,argv)
            read(argv,*) elon
            call getarg(5,argv)
            read(argv,*) slat
            call getarg(6,argv)
            read(argv,*) nlat
            write(*,*) 'Lon/lat boundaries: ',wlon,elon,slat,nlat
          ELSE  ! Default lon/lat boundaries
            slat = -90.
            nlat = 90.
            wlon = -180.
            elon = 180.
          END IF
        ELSE
          write(*,*) 'Usage: bufr_adpsfc2ob.x gdas.adpsfc.t<HH>z.
     +<YYYYMMDD>.bufr <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
          STOP
        END IF

C*-----------------------------------------------------------------------
C*    Open BUFR input file
      OPEN (UNIT=lunit, FILE=inf, form='unformatted')

C*    Open output file
      fout= "adpsfc"//date_tag//'.obs'
      open(iou,file=fout, status='unknown', form='formatted')
      write(iou, fmt='(a10)') date_tag

      adpsfcsource='NCEP GDAS BUFR ADPSFC observations      '

      nlev = 1
      isurf = 1
      ibogus = 0
      date='MMMMMMMMMM'
      mins='MM'
      lat=dumm
      lon=dumm
      selv=dumm
      pmsl=dumm
      pres=dumm
      tmdb=dumm
      tmdp=dumm
      wdir=dumm
      wspd=dumm

C* Connect BUFR file to the BUFRLIB software for input operations.
      CALL OPENBF(lunit, 'IN', lunit)

C*    Specify that we would like IDATE values returned using 10 digits
C*    (i.e. YYYYMMDDHH ).
      CALL DATELEN(10)

c  Include code and flag table information from master BUFR tables
      CALL codflg('Y')

C*-----------------------------------------------------------------------
c  Loop through BUFR subsets
      DO WHILE(.true.)

c  Get file ID (lun) associated with the BUFR file
        CALL status(lunit, lun, il, im)

c  Read next subset
        call readns(lunit, csubset, idate, ierr)

        IF(ierr .eq. -1) THEN
          write(*,*) '....all records read, Exit'
          CALL CLOSBF(lunit)
          GOTO 2000
        END IF

c Get current message and data subset number
        call ufbcnt(lunit, irec, isub)

c        print'(''MESSAGE: '',A8,2(2X,I6),i12 )',
c     +           csubset,irec,isub,idate

        write(date, '(I10)') idate

c Get data local subtype
        write(cmmsbt, '(A40)') repeat(' ', 40)
        write(adpsfcname, '(A40)') repeat(' ', 40)
        iogce = iupvs01(lunit, 'OGCE')
        mtyp = iupvs01(lunit, 'MTYP')
        msbt = iupvs01(lunit, 'MSBT')
        call getcfmng(lunit, 'TABLASL', msbt, 'TABLAT', mtyp, 
     +                cmmsbt, lcmmsbt, iermsbt)

        if (iermsbt .eq. 0) then
           write(adpsfcname, '(A40)') cmmsbt
        else
           write (adpsfcname, '(A,A8)') 'BUFR MESSAGE TYPE ', csubset
        end if

c Check message type and set obs type accordingly
        if(csubset .eq. 'NC000007') then
            dname=' METAR'
        else
            dname=' SYNOP'
        endif

C* Read data values into arrays

        CALL UFBINT(lunit, idarr, MXMN, MXLV, nlev, idstr)
        CALL UFBINT(lunit, locarrt, MXMN, MXLV, nlev, locstrt)
        CALL UFBINT(lunit, locarrh, MXMN, MXLV, nlev, locstrh)
        CALL UFBINT(lunit, locarrv, MXMN, MXLV, nlev, locstrv)
        CALL UFBINT(lunit, obsarr, MXMN, MXLV, nlev, obstr)
!        CALL UFBINT(lunit, obsarrh, MXMN, MXLV, nlev, obstrh)

        if (ibfms(locarrt(5,1)) .eq. 1) then
           mins='00'
        else
           write (mins, FMT='(I2.2)') int(locarrt(5,1))
        endif

C*-----------------------------------------------------------------------
c  Prepare output
        CALL get_val(idarr(1,1), wmob)
        CALL get_val(idarr(2,1), wmos)
        CALL get_charval(idarr(3,1), rpid)
        CALL get_charval(idarr(4,1), smid)
        CALL get_charval(idarr(5,1), stsn)
        CALL get_charval(idarr(6,1), sstn)
        CALL get_charval(idarr(7,1), lstn)

        CALL get_val(locarrv(1,1), selv)
        CALL get_val(locarrv(2,1), pres)
        CALL get_val(locarrv(3,1), pmsl)
        CALL get_val(obsarr(1,1), wdir)
        CALL get_val(obsarr(2,1), wspd)
        CALL get_val(obsarr(3,1), tmdb)
        CALL get_val(obsarr(4,1), tmdp)
        CALL get_lat_lon(locarrh(1,1), locarrh(3,1), lat)
        CALL get_lat_lon(locarrh(2,1), locarrh(4,1), lon)

        if(pres .ne. 0 .and. pres .ne. dumm) then
            pres=pres/100.
        endif
        if(pmsl .ne. 0 .and. pmsl .ne. dumm) then
            pmsl=pmsl/100.
        endif

        if((rpid .ne. 'MISSING') .and. (stsn .ne. 'MISSING')) then
            write(adpsfcid, '(A,1X,A,1X,A)') 
     +            'RPID:',trim(rpid),trim(stsn)
        else if (rpid .ne. 'MISSING') then
            write(adpsfcid, '(A,1X,A)') 'RPID:',trim(rpid)
        else if (stsn .ne. 'MISSING') then
            write(adpsfcid, '(A,1X,A)') 'Station name: ',trim(stsn)
        else
            write(adpsfcid, '(A40)') 'RPID: MISSING'
        endif

c------------------------------------------------------------------------
c         Write output 
          if(slat<=lat .and. nlat>=lat .and.
     +       wlon<=lon .and. elon>=lon) then
               write(iou,111) isurf,
     +                        dname,
     +                        adpsfcid,
     +                        adpsfcname,
     +                        adpsfcsource,
     +                        date,
     +                        mins,
     +                        lat,
     +                        lon,
     +                        selv,
     +                        pmsl,
     +                        nlev,
     +                        ibogus
             write(iou,112) pres, selv, tmdb, tmdp, wdir, wspd
           endif

111       format(i1,1x,a6,1x,3(a40,1x),a10,a2,1x,
     +           3(f20.5,1x),f13.5,1x,i10,1x,i1)
112       format(6(f13.5,1x))

        ENDDO

2000  continue
        
      END

C*-----------------------------------------------------------------------
       SUBROUTINE get_val(mval, retval)

C      Checks variable value returned by UFBINT and returns either the 
c      observation value or missing.
c
c      Input:
c         mval: BUFR parameter value returned by UFBINT
c      Output:
c         retval: observation value

       real*8 mval
       real retval
       parameter(missing=99999.9)

       IF (ibfms(mval) .EQ. 0) THEN
          retval = mval
       ELSE
          retval = missing
       ENDIF
       
       RETURN
       END
C*-----------------------------------------------------------------------
       SUBROUTINE get_charval(mval, retval)

C      Checks character value returned by UFBINT and returns either the 
c      string value or missing.
c
c      Input:
c         mval: BUFR character value returned by UFBINT
c      Output:
c         retval: observation string

       real*8 mval
       character*40 retval

       IF (ibfms(mval) .EQ. 0) THEN
          write(retval, '(A)') mval
       ELSE
          write(retval, '(A)') 'MISSING'
       ENDIF
       
       RETURN
       END
C*-----------------------------------------------------------------------
       SUBROUTINE get_lat_lon(clatlon, clatlonh, retval)

C      Get latitude and longitude from either CLAT/CLON or CLATH/CLONH
c      Input:
c         clatlon: CLAT or CLON returned by UFBINT
c         clatlonh: CLATH or CLONH returned by UFBINT
c      Output:
c         retval: latitude or longitude value

       real*8 clatlon, clatlonh
       real retval
       parameter(missing=99999.9)

       IF (ibfms(clatlon) .EQ. 0) THEN
          retval = clatlon
       ELSE IF (ibfms(clatlonh) .EQ. 0) THEN
          retval = clatlonh
       ELSE
          retval = missing
       ENDIF
       
       RETURN
       END