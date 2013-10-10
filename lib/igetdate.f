	FUNCTION IGETDATE(MBAY,IYR,IMO,IDY,IHR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IGETDATE
C   PRGMMR: ATOR             ORG: NP12       DATE: 2005-11-29
C
C ABSTRACT: THIS FUNCTION UNPACKS AND RETURNS THE SECTION 1 DATE-TIME
C   FROM THE BUFR MESSAGE STORED IN ARRAY MBAY.  IT WILL WORK ON ANY
C   MESSAGE ENCODED USING BUFR EDITION 2, 3 OR 4.  THE START OF THE
C   BUFR MESSAGE, (I.E. THE STRING "BUFR") MUST BE ALIGNED ON THE FIRST
C   FOUR BYTES OF MBAY.
C
C PROGRAM HISTORY LOG:
C 2005-11-29  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    IGETDATE (MBAY, IYR, IMO, IDY, IHR)
C   INPUT ARGUMENT LIST:
C     MBAY     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING
C                BUFR MESSAGE
C
C   OUTPUT ARGUMENT LIST:
C     IYR      - INTEGER: SECTION 1 YEAR (YYYY OR YY, DEPENDING ON
C                DATELEN() VALUE)
C     IMO      - INTEGER: SECTION 1 MONTH (MM)
C     IDY      - INTEGER: SECTION 1 DAY (DD)
C     IHR      - INTEGER: SECTION 1 HOUR (HH)
C     IGETDATE - INTEGER: SECTION 1 DATE-TIME (YYYYMMDDHH OR YYMMDDHH,
C                DEPENDING ON DATELEN() VALUE)
C
C REMARKS:
C    THIS ROUTINE CALLS:        IUPBS01
C    THIS ROUTINE IS CALLED BY: CKTABA   DATEBF   DUMPBF
C                               Normally not called by application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	COMMON /DATELN/ LENDAT

	DIMENSION MBAY(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IYR = IUPBS01(MBAY,'YEAR')
	IMO = IUPBS01(MBAY,'MNTH')
	IDY = IUPBS01(MBAY,'DAYS')
	IHR = IUPBS01(MBAY,'HOUR')
	IF(LENDAT.NE.10) THEN
	    IYR = MOD(IYR,100)
	ENDIF
	IGETDATE = (IYR*1000000) + (IMO*10000) + (IDY*100) + IHR

	RETURN
	END
