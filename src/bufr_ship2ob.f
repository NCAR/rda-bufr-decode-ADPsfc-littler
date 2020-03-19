      PARAMETER (MXMN = 8)
      PARAMETER (MXLV = 86)
      PARAMETER (NVAR = 17)
      PARAMETER (NSTR = 5)

      REAL*8 r8arr(MXMN, MXLV), r8arr2(MXMN, MXLV),
     +       r8arr3 (MXMN, MXLV), r8arr4(MXMN, MXLV),
     +       r8arr5 (MXMN, MXLV) 

      parameter(iu=9, iou=10)

      real pr,tt,td,dslp
      integer xht,nlev,iargc,n,minu,k
      real xpr,xu,xv
      real  temp,v,zx,d
      real  lat, lon, ter
      real xt,xtd
      character*30 fin,fout
      character*10  date_tag,date
      character*6 dname,staid,M20
      character   argv*300,minute*2,M11*2,mins*2
      character*12 ilev,xy,xm,xd,xh,xmin,M5,M6,M7,M8
      character*12 M10,M0,M1,M2,min,M3,M4,xn1,xn2,xn3,xn4,M9
      real wlon,elon,slat,nlat
      
      CHARACTER csubset*8, inf*200, outstg*200
      INTEGER code, z, y, i, idate, iflag

      CHARACTER*80   ostr(NSTR)      
      
      ostr(1)='RPID YEAR MNTH'
      ostr(2)='DAYS HOUR MINU CLAT CLON'
      ostr(3)='TMDB TMDP SST1 PRES PMSL'
      ostr(4)='WDIR WSPD 3HPC 24PC TP06 TOCC'
      ostr(5)='HOCB'
      
c-----7---------------------------------------------------------------72
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
         write(*,*) 'Usage: bufr_ship2ob.x gdas.sfcshp.t<HH>z.
     +<YYYYMMDD>.bufr.be <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
         STOP
      END IF

c-----7---------------------------------------------------------------72

C*      Open the BUFR messages file.

      fout = "Ship"//date_tag//'.obs'
      OPEN (UNIT=11, FILE=inf, form='unformatted' )

C*      Open output file

      open(iou, file=fout, status='unknown', form='formatted')

      iflag = 0
      nlev = 1
      dumm=99999.9

      isurf=1
      ibogus=0
      date='MMMMMMMMMM'
      mins='MM'
      staid='MMMMMM' 
      dname='  BUOY'
      ter=dumm
      dslp=dumm
      pr=dumm
      zx=dumm
      tt=dumm
      td=dumm
      d=dumm
      v=dumm

C*    Identify BUFR file to the BUFRLIB software.  DX BUFR tables
C*    are embedded within the first few messages of the BUFR file
C*    itself, thus we logical unit for the BUFR tables file is the 
C*    same as the BUFR file itself.

      CALL OPENBF  ( 11, 'IN', 11 )

C*    Specify that we would like IDATE values returned using 10 digits
C*    (i.e. YYYYMMDDHH ).

      CALL DATELEN  ( 10 )
     
c-----7---------------------------------------------------------------72
      DO WHILE  ( .true. )

C*          Read the next BUFR message.

           call readns(11,csubset,idate,ierr)
c            write(*,*)' idate: ',idate,'  ',csubset
            IF (ierr .eq.  -1) THEN
                write(*,*) '[bufr_ship2ob]....all records read, Exit'
                CALL CLOSBF (11)
                Goto 2000
            END IF

C*            At this point, we have a data subset within the
C*            internal arrays of BUFRLIB, and we can now begin
C*            reading actual data values:

              CALL UFBINT(11, r8arr, MXMN, MXLV, nlv, ostr(1))
              CALL UFBINT(11, r8arr2, MXMN, MXLV, nlv, ostr(2))
              CALL UFBINT(11, r8arr3, MXMN, MXLV, nlv, ostr(3))
              CALL UFBINT(11, r8arr4, MXMN, MXLV, nlv, ostr(4))
              CALL UFBINT(11, r8arr5, MXMN, MXLV, nlv, ostr(5)) 

            minu=int(r8arr2(3,1))
            write (unit=minute, FMT='(I2)') minu

            DO k=1,2
               IF ( minute (k:k) .eq. ' ') THEN
                 minute (k:k) = '0'
               ENDIF
            ENDDO

            DO z = 1,1 
              WRITE (UNIT=outstg, FMT='(I10,1X, A8, 1X,A6, 
     +          1X,F6.1,4(1x,F4.1),1X,F6.1,1X,F6.1,
     +          3(1X,F5.1),2(1X,F8.1),1X,F5.1,1X,F4.1,
     +          4(1X,F5.1),1X,F7.1)') idate,csubset,
     +          (r8arr(i,z), i = 1,3),(r8arr2(i,z), i = 1,5),
     +          (r8arr3(i,z), i = 1,5),(r8arr4(i,z), i = 1,6),
     +          (r8arr5(i,z), i = 1,1)

              DO y = 1,151
               IF ( outstg (y:y) .eq. '*') THEN
                 outstg (y:y) = 'm'
               ENDIF
              ENDDO
              
              read(outstg,21) M10,M0,M20,M1,M2,
     &          M3,M4,M5,M6,M7,M8
              read(minute,22) M11
!             write(*,*)M10,M0, M1,M2,M3,M4,M5,M6,M7,M8
21            format(A10,1X,A8,1X,A6,28X,A6,
     &         1X,A6,1X,A5,1X,A5,
     &         7X,A8,1X,A8,1x,A5,1X,A5)
22            format(A2)

c-----7---------------------------------------------------------------72
c             Prepare output

              CALL READMval(M1,lat)
              CALL READMval(M2,lon)
              CALL READMval(M6,dslp)
              CALL READMval(M5,pr)
              CALL READMval(M3,tt)
              CALL READMval(M4,td)
              CALL READMval(M7,d)
              CALL READMval(M8,v)

              if(pr.ne.0 .and. pr.ne.99999.9) then
                 pr=pr/100.
              end if

              if(dslp.ne.0 .and. dslp.ne.99999.9) then
                dslp=dslp/100.
              end if

              if(M0.eq.'NC001001') then
                dname='  SHIP'
              end if

              date=M10
              mins=M11
              staid=M20

c             Write to output file
              if (iflag.eq.0) then
                 write(iou,fmt='(a10)') date_tag
                 iflag=1
              endif
              if(slat<=lat .and. nlat>=lat .and.
     &           wlon<=lon .and. elon>=lon) then
                 write(iou,111) isurf,dname,staid,date,mins,
     &                          lat,lon,ter,dslp,nlev,ibogus
                 write(iou,112) pr,zx,tt,td,d,v
              endif

111           format(i1,1x,a6,1x,a6,1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112           format(6(f7.1,1x))

            END DO
        END DO

c-----7---------------------------------------------------------------72

       endif

c-----7---------------------------------------------------------------72
2000  stop 99999

        END

c-----7---------------------------------------------------------------72
      SUBROUTINE READMval(M1,fl)
      character*8 M1
      dumm=99999.9
      if(M1(1:1) ==  'm') then
         fl = dumm
      else
         read(M1,*)fl
      endif
      RETURN
      END