      SUBROUTINE DBMRD (WTRDIR,ST,IUWE,IWEATH,IYEAR,IDAY,
     &                  GEOP,ELEV,TMIN,TMAX,AVRAD,VAPOUR,WIND,RAIN)
      IMPLICIT REAL (A-Z)
*     This routine automatically detects changes in input data such
*     as weather station, year etc.

*     WTRDIR - Directory where weather data are                      I
*     ST     - Station name of weather data                          I
*     IUWE   - Unit number by which data file may be opened          I
*     IWEATH - Indicates which of the two possible formats is 
*              requested (=3: DBMETEO, =4: 'repaired DBMETO')        I
*     IYEAR  - Requested year of weather data                        I
*     IDAY   - Requested day number                                  I
*     GEOP   - Latitude of the station                               I
*     ELEV   - Altitude of the station                               I
*     TMIN   - Daily minimum temperature (Celsius)                   O
*     TMAX   - Daily maximum temperature (Celsius)                   O
*     AVRAD  - Daily shortwave radiation (J/m2/d)                   O
*     VAPOUR - Daily average vapour pressure (mbar)                   O
*     WIND   - Daily average wind speed (m/s)                        O
*     RAIN   - Daily total rainfall (cm/d)                           O
*
*     Subroutines called: 
*      - from library TTUTIL: IFINDC, ILEN, FOPENG, UPPERC, ERROR

*     Author: Daniel van Kraalingen
*     Date  : April 1991

*     formal parameters
      CHARACTER*(*) WTRDIR, ST
      INTEGER IUWE, IWEATH, IYEAR, IDAY

*     local parameters
      CHARACTER CLFILE*80, WTRDIO*80, STNAM*8, STNAMO*8
      INTEGER I1, I2, ILEN, IOS, IREC, IWEATO, IYEARF, IDAYF, IMOF
      LOGICAL OPNF, RDREF

*     special declarations
      INTEGER*2 IVARRE(18), IVAR2(7), IYR2
      INTEGER*1 IMO1,IDAY1

      INTEGER IMNST, IFINDC, INSTAT

*     arrays to hold contents of index file of reference weather

      PARAMETER (IMNST=500)
*      REAL LATSTA(IMNST), LONSTA(IMNST), ALTSTA(IMNST)
      INTEGER IRECNA(IMNST)
*      CHARACTER*8 STNAMA(IMNST)

      SAVE

      DATA OPNF /.FALSE./, RDREF /.FALSE./
      DATA WTRDIO /' '/, STNAMO /' '/, IWEATO /0/
      
*     open new file if new weather directory, station name or data format
*     is requested

      IF (WTRDIR.NE.WTRDIO.OR.ST.NE.STNAMO.OR.IWEATH.NE.IWEATO) THEN
*         IF (.NOT.RDREF) THEN
*           open index file once
*            CALL RDRFIN (IUWE,STNAMA,LATSTA,LONSTA,ALTSTA,IRECNA,IMNST,
*     &                   INSTAT)
*            RDREF = .TRUE.
*         END IF
         STNAM = ST
*        find requested station in the list, check if found
*         I1 = IFINDC (STNAMA,IMNST,1,INSTAT,STNAM)
*         IF (I1.EQ.0) CALL ERROR ('DBMRD','station not in list')
*         GEOP = LATSTA (I1)
*         ELEV = ALTSTA (I1)

*        close data file if it is open
         IF (OPNF) CLOSE (IUWE)
         I1 = LEN_TRIM (WTRDIR)
         I2 = LEN_TRIM (STNAM)
         IF (IWEATH.EQ.3) THEN
*           DBMETEO format
            IF (I1.GT.0) THEN
*              directory not empty
               CLFILE = WTRDIR(1:I1)//'W'//STNAM(1:I2)//'.MET'
            ELSE
*              directory empty
               CLFILE = 'W'//STNAM(1:I2)//'.MET'
            END IF
*           open unformatted, direct access data file
            CALL FOPENG (IUWE,CLFILE,'OLD','UD',40,'NVT')
         ELSE IF (IWEATH.EQ.4) THEN
*           'repaired DBMETEO' format
            IF (STNAM(1:I2).EQ.'03066') THEN
            GEOP= 57.65
            ELEV=  7.00
            ELSE IF (STNAM(1:I2).EQ.'03222') THEN
            GEOP= 54.92
            ELEV= 26.00
            ELSE IF (STNAM(1:I2).EQ.'03354') THEN
            GEOP= 53.00
            ELEV= 117.00
            ELSE IF (STNAM(1:I2).EQ.'03566') THEN
            GEOP= 52.35
            ELEV= 41.00
            ELSE IF (STNAM(1:I2).EQ.'03971') THEN
            GEOP= 53.52
            ELEV= 104.00
            ELSE IF (STNAM(1:I2).EQ.'06030') THEN
            GEOP= 57.10
            ELEV=  2.00
            ELSE IF (STNAM(1:I2).EQ.'06260') THEN
            GEOP= 52.10
            ELEV=  2.00
            ELSE IF (STNAM(1:I2).EQ.'06261') THEN
            GEOP= 52.10
            ELEV=  2.00
            ELSE IF (STNAM(1:I2).EQ.'07015') THEN
            GEOP= 50.57
            ELEV= 181.00
            ELSE IF (STNAM(1:I2).EQ.'07249') THEN
            GEOP= 47.97
            ELEV= 126.00
            ELSE IF (STNAM(1:I2).EQ.'07630') THEN
            GEOP= 43.62
            ELEV= 153.00
            ELSE IF (STNAM(1:I2).EQ.'08023') THEN
            GEOP= 43.45
            ELEV= 64.00
            ELSE IF (STNAM(1:I2).EQ.'08181') THEN
            GEOP= 41.27
            ELEV=  4.00
            ELSE IF (STNAM(1:I2).EQ.'08221') THEN
            GEOP= 40.45
            ELEV= 664.00
            ELSE IF (STNAM(1:I2).EQ.'08391') THEN
            GEOP= 37.42
            ELEV= 34.00
            ELSE IF (STNAM(1:I2).EQ.'08546') THEN
            GEOP= 41.12
            ELEV= 93.00
            ELSE IF (STNAM(1:I2).EQ.'08579') THEN
            GEOP= 38.77
            ELEV= 104.00
            ELSE IF (STNAM(1:I2).EQ.'10224') THEN
            GEOP= 53.05
            ELEV=  5.00
            ELSE IF (STNAM(1:I2).EQ.'10729') THEN
            GEOP= 49.52
            ELEV= 96.00
            ELSE IF (STNAM(1:I2).EQ.'10866') THEN
            GEOP= 48.12
            ELEV= 529.00
            ELSE IF (STNAM(1:I2).EQ.'16080') THEN
            GEOP= 45.42
            ELEV= 107.00
            ELSE IF (STNAM(1:I2).EQ.'16140') THEN
            GEOP= 44.52
            ELEV= 36.00
            ELSE IF (STNAM(1:I2).EQ.'16230') THEN
            GEOP= 42.42
            ELEV= 10.00
            ELSE IF (STNAM(1:I2).EQ.'16320') THEN
            GEOP= 40.65
            ELEV= 15.00
            ELSE IF (STNAM(1:I2).EQ.'16622') THEN
            GEOP= 40.52
            ELEV=  8.00
            ELSE IF (STNAM(1:I2).EQ.'16726') THEN
            GEOP= 37.07
            ELEV=  6.00
            ElSE 
            WRITE(*,999)
 999        FORMAT(/,' CANNOT READ DATA IN DBMRD')
            END IF
            
            IF (I1.GT.0) THEN
*              directory not empty
               CLFILE = WTRDIR(1:I1)//'W'//STNAM(1:I2)//'.REP'
            ELSE
*              directory empty
               CLFILE = 'W'//STNAM(1:I2)//'.REP'
            END IF
*           open unformatted, direct access data file
            CALL FOPENG (IUWE,CLFILE,'OLD','UD',20,'NVT')
         ELSE
            CALL ERROR ('DBMRD','wrong file type')
         END IF

         WTRDIO = WTRDIR
         STNAMO = STNAM
         IWEATO = IWEATH
         OPNF = .TRUE.
         IYEARF = -99
         IDAYF  = -99
         IREC = 0
      END IF


*     find requested year and day if day and year during previous call
*     are different

      IOS = 0
      IF (IDAYF.LT.IDAY) IREC = IREC+1
10    IF (IOS.EQ.0.AND.((IYEAR+1900).NE.IYEARF.OR.IDAY.NE.IDAYF)) THEN
         IF (IWEATH.EQ.3) THEN
*           dbmeteo format
            READ (IUWE,REC=IREC,IOSTAT=IOS) IYR2,IMO1,IDAY1,IVARRE
            IYEARF = IYR2
         ELSE IF (IWEATH.EQ.4) THEN
*           repaired dbmeteo format
            READ (IUWE,REC=IREC,IOSTAT=IOS) IYEARF,IMO1,IDAY1,IVAR2

         END IF

*        test if read statement was succesful
         IF (IOS.EQ.0) THEN
           
            IMOF = IMO1
            IDAYF = IDAY1
*           convert month and day in month to day in year
            CALL DATES ('TO_YEAR',IYEARF,IDAYF,IMOF)
            
            IF ((IYEAR+1900).GT.IYEARF) THEN
*              if requested year is greater than year number that was
*              just read, the next record should be read
               IREC = IREC+1

           
            ELSE IF ((IYEAR+1900).EQ.IYEARF) THEN
               IF (IDAY.GT.IDAYF) THEN
*                 if requested day is greater than day number that was
*                 just read, the next record should be read
                  IREC = IREC+1
               ELSE IF (IDAY.LT.IDAYF) THEN
*                 if requested day is smaller than day number that was
*                 just read, the previous record should be read
                  IREC = IREC-1
               END IF
            ELSE IF ((IYEAR+1900).LT.IYEARF) THEN
*                 if requested year is smaller than year number that was
*                 just read, the previous record should be read
               IREC = IREC-1
            END IF
*           stop reading if top of file is reached
            IF (IREC.LT.1) IOS = 1
         END IF

      GOTO 10
      END IF

      IF (IOS.NE.0) CALL ERROR
     &   ('DBMRD','requested weather data not found')

*     convert coded integers to real values
      IF (IWEATH.EQ.3) THEN
         CALL CONVRT ('DECODE_DBMETEO',IVARRE,IVAR2,
     &                SSD,AVRAD,TMIN,TMAX,VAPOUR,WIND,RAIN)
      ELSE
         CALL CONVRT ('DECODE_NORMAL',IVARRE,IVAR2,
     &                SSD,AVRAD,TMIN,TMAX,VAPOUR,WIND,RAIN)
      END IF

      AVRAD  = AVRAD*1.E3
      VAPOUR = VAPOUR*10.
      RAIN= RAIN/10.
      RETURN
      END

      SUBROUTINE CONVRT (ACTION,IVARRE,IVAR2,
     &                   SSD,RAD,TMN,TMX,HUM,WIN,RAI)

*     Converts weather data from DBMETEO or 'repaired DBMETEO' files
*     to appropriate units. The method by which data are stored in these
*     files is with INTEGER*2 variables to reduce file size. The records
*     of the DBMETEO files, however, contain more data than the 
*     'repaired DBMETEO format' so that the conversion to REAL's is 
*     slightly different.The routine can undertake three different
*     actions, 1) convert DBMETEO file record to REAL's
*     (ACTION='DECODE_DBMETEO'), 2) convert 'repaired DBMETO' format to
*     REAL's (ACTION='DECODE_NORMAL'), 3) convert REAL's to 'repaired
*     DBMETEO' file record (ACTION='CODE_NORMAL').

*     ACTION - Conversion to undertake                               I
*     IVARRE - INTEGER*2 array with coded weather variables from
*              DEMETEO file                                          I
*     IVAR2  - INTEGER*2 array with coded weather variables from
*              'repaired DEMETEO file'                               I
*     SSD    - Daily sunshine duration (h/d)                         O
*     RAD    - Daily shortwave radiation (kJ/m2/d)                   O
*     TMN    - Daily average minimum temperature (Celsius)           O
*     TMX    - Daily average maximum temperature (Celsius)           O
*     HUM    - Daily average vapour pressure (kPa)                   O
*     WIN    - Daily average windspeed (m/s)                         O
*     RAI    - Daily rainfall (mm/d)                                 O
*
*     Subroutine called: 
*      - from library TTUTIL: ERROR

*     Author: Daniel van Kraalingen
*     Date  : April 1991

*     formal parameters
      IMPLICIT REAL (A-Z)
      INTEGER*2 IVARRE(18), IVAR2(7)
      CHARACTER*(*) ACTION

**    local parameters
      SAVE

      IF (ACTION.EQ.'DECODE_DBMETEO') THEN
         SSD = FLOAT (IVARRE(2))/10.
         IF (IVARRE(4).GT.-98.) THEN
            RAD = FLOAT (IVARRE(4))*10.
         ELSE
            RAD = FLOAT (IVARRE(4))
         END IF
         TMN = FLOAT (IVARRE(5))/10.
         TMX = FLOAT (IVARRE(6))/10.
         HUM = FLOAT (IVARRE(7))/100.
         WIN = FLOAT (IVARRE(9))/10.
         RAI = FLOAT (IVARRE(10))/10.

      ELSE IF (ACTION.EQ.'DECODE_NORMAL') THEN

         SSD = FLOAT (IVAR2(1))/10.
         IF (IVARRE(2).GT.-98) THEN
            RAD = FLOAT (IVAR2(2))*10.
         ELSE
            RAD = FLOAT (IVAR2(2))
         END IF
         TMN = FLOAT (IVAR2(3))/10.
         TMX = FLOAT (IVAR2(4))/10.
         HUM = FLOAT (IVAR2(5))/100.
         WIN = FLOAT (IVAR2(6))/10.
         RAI = FLOAT (IVAR2(7))/10.

      ELSE IF (ACTION.EQ.'CODE_NORMAL') THEN

         IVAR2(1) = NINT (SSD*10.)
         IF (IVAR2(2).GT.-98) THEN
            IVAR2(2) = NINT (RAD/10.)
         ELSE
            IVAR2(2) = NINT (RAD)
         END IF
         IVAR2(3) = NINT (TMN*10.)
         IVAR2(4) = NINT (TMX*10.)
         IVAR2(5) = NINT (HUM*100.)
         IVAR2(6) = NINT (WIN*10.)
         IVAR2(7) = NINT (RAI*10.)
      ELSE
         CALL ERROR ('CONVRT','unknown action')
      END IF

      RETURN
      END

      SUBROUTINE DATES (ACTION,IYEAR,IDAY,IMONTH)

*     Converts either a year and day number to a month number 
*     and a day number within that month (ACTION='TO_MONTHS')
*     or the inverse, from a day number in a certain month to
*     a day number within the year (ACTION='TO_YEAR'). Leap years are
*     recognised.

*     ACTION - Type of action to undertake                           I
*     IYEAR  - Year number                                           I
*     IDAY   - Day number (counted from 1st or Jan or 1st of month)  I
*     IMONTH - Month number                                          I
*
*     Subroutine called: 
*      - from library TTUTIL: ERROR

*     Author: Daniel van Kraalingen
*     Date  : April 1991

*     formal parameters
      INTEGER IYEAR, IDAY, IMONTH
      CHARACTER*(*) ACTION

**    local parameters
      LOGICAL LEAP
      INTEGER IMOTB1(12), IMOTB2(12), IDM1(365), IDM2(366)
      SAVE

*     arrays are filled with data for normal year (1) or leap year (2)

      DATA IMOTB1 /0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IMOTB2 /0,31,60,91,121,152,182,213,244,274,305,335/
      DATA IDM1 /31*1,28*2,31*3,30*4,31*5,30*6,
     &           31*7,31*8,30*9,31*10,30*11,31*12/
      DATA IDM2 /31*1,29*2,31*3,30*4,31*5,30*6,
     &           31*7,31*8,30*9,31*10,30*11,31*12/

*     determine if IYEAR is leap year
      IF (MOD (IYEAR,4).EQ.0.AND.IYEAR.NE.1000) THEN
         LEAP = .TRUE.
      ELSE
         LEAP = .FALSE.
      END IF

      IF (ACTION.EQ.'TO_MONTHS') THEN
*        convert day number counted from 1st of Jan to 1st of month
         IF (.NOT.LEAP) THEN
            IMONTH = IDM1(IDAY)
            IDAY   = IDAY-IMOTB1(IMONTH)            
         ELSE
            IMONTH = IDM2(IDAY)
            IDAY   = IDAY-IMOTB2(IMONTH)            
         END IF
      ELSE IF (ACTION.EQ.'TO_YEAR') THEN
*        convert day number counted from 1st of month to 1st of Jan
         IF (.NOT.LEAP) THEN
            IDAY = IMOTB1(IMONTH)+IDAY
         ELSE
            IDAY = IMOTB2(IMONTH)+IDAY
         END IF
      ELSE
         CALL ERROR ('DATES','unkown action')
      END IF

      RETURN
      END

      SUBROUTINE RDRFIN (IU1,STNAM,LATST,LONST,ALTST,IRECN,IMNST,
     &                   INSTAT)

*     Reads in index file of reference weather and returns station
*     names, latitudes, longitudes, altitudes and record number where
*     reference weather starts (Jan 1) in reference weather file
*     DBMREF.DAT (this file contains weather for all stations).

*     IU1    - Unit number by which index file can be opened         I
*     STNAM  - Array with station names                              O
*     LATST  - Array with latitudes                                  O
*     LONST  - Array with longitudes                                 O
*     ALTST  - Array with altitudes                                  O
*     IRECN  - Array with record numbers                             O
*     IMNST  - Maximum number of stations that can be read           I
*     INSTAT - Number of stations found on index file                O
*
*     Subroutines called: 
*      - from library TTUTIL: ERROR, ILEN, UPPERC

*     Author: Daniel van Kraalingen
*     Date  : April 1991

*     formal parameters
      IMPLICIT REAL (A-Z)
      INTEGER IU1, IMNST, IRECN(IMNST), INSTAT
      REAL LATST(IMNST), LONST(IMNST), ALTST(IMNST)
      CHARACTER*(*) STNAM(IMNST)

*     local parameters
      INTEGER IOS, IREC, IDUM
      SAVE

*     open index file for reference weather
*!!andere file naam
      CALL FOPENG  (IU1,'DBMREFIN.DAT',
     &   'OLD','UD',28,'NVT')

*     read in meteostation data into arrays until end_of_file
      IOS  = 0
      IREC = 1
10    IF (IOS.EQ.0) THEN
         READ (IU1,REC=IREC,IOSTAT=IOS) STNAM(IREC),IDUM,
     &        LATST(IREC),LONST(IREC),ALTST(IREC),IRECN(IREC)
         IF (IOS.EQ.0) THEN
            IREC = IREC+1
            IF (IREC.GT.IMNST) CALL ERROR ('RDRFIN',
     &        'array sizes not sufficient to hold meteo station data')
         ELSE
            IREC = IREC-1
         END IF
      GOTO 10
      END IF

      CLOSE (IU1)
      INSTAT = IREC

      RETURN
      END

