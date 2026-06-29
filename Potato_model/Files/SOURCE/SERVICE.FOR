*---------------------------------------------------------------------------*
*  REAL FUNCTION INTGRL                                                     *
*                                                                           *
*  Date   : 18-FEB-1987                                                     *
*  Purpose: The INTGRL (Integral) function updates state variables          *
*	    on the basis of their rate of change. The initial value         *
*	    of the integral is given in the calling routine or              *
*	    calling program                                                 *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  GR      rate of change of state variable              =      I           *
*  H       input value of state variable that is                            *
*	   updated                                       =      I           *
*  INTGRL  output value of state variable that is                           *
*	   updated                                       =      O           *
*---------------------------------------------------------------------------*

      REAL FUNCTION INTGRL(GR,H)
	
      IMPLICIT REAL (A-Z)
      DELT = 1.


*---- multiplication of rate and time step and addition to previous     
*     value

      INTGRL = H+DELT*GR

      RETURN                                                            
      END







*---------------------------------------------------------------------------*
* FUNCTION LIMIT                                                            *
* Function limits X between P1 and P2                                       *
*---------------------------------------------------------------------------*

      REAL FUNCTION LIMIT(P1,P2,X)                                      
	

      IMPLICIT REAL (A-Z)

      IF (P1.GT.P2) THEN                                                
         WRITE (*,'(A)') ' LIMIT function arguments error'
         WRITE (*,'(A,F8.2,A,F8.2)')
     $           ' Lower bound =',P1,';  Upper bound =',P2
         WRITE (*,'(A)') ' Run deleted!!'
         STOP
      END IF

      IF (X.GT.P2) THEN                                                 
         LIMIT = P2
      ELSE
         IF (X.LT.P1) THEN
            LIMIT = P1
         ELSE
            LIMIT = X
         END IF
      END IF

      RETURN                                                            
      END

*---------------------------------------------------------------------------*
*  REAL FUNCTION AFGEN                                                      *
*  Author : Daniel  van Kraalingen                                          *
*  Date   : 18-FEB-1987                                                     *
*  Purpose: This function is a linear interpolation function.  The          *
*           function does not extrapolate : in case of X below or           *
*           above the region defined by TABLE, the first                    *
*           respectively the last Y-value is returned and a message         *
*           is generated.                                                   *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  AFGEN  function name, result of the interpolation     =      O           *
*  TABLE  A one-dimensional array with paired            =      I           *
*         data: x,y,x,y, etc.                                               *
*  ILTAB  The number of elements of the array            -      I           *
*         TABLE                                                             *
*  X      The value at which interpolation should        =      I           *
*         take place                                                        *
*                                                                           *
*  FATAL ERROR CHECKS (execution terminated, message)                       *
*  condition                                                                *
*  ---------                                                                *
*  TABLE(I) < TABLE(I-2) , for I odd                                        *
*  ILTAB odd                                                                *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      REAL FUNCTION AFGEN (ILTAB,TABLE,X)
      IMPLICIT REAL (A-Z)
      INTEGER I, IUP, ILTAB
      DIMENSION TABLE(ILTAB)


*---- check on odd ILTAB

      IF (MOD(ILTAB,2).NE.0) THEN
         WRITE (*,'(A,I4/,A)')
     $        ' ERROR in function AFGEN : ILTAB=',ILTAB,
     $        ' ILTAB must be even !'
         STOP
      END IF

      IUP = 0



      DO 10 I=3,ILTAB,2      

*------- check on ascending order of X-values in function

         IF (TABLE(I).LE.TABLE(I-2)) THEN
            WRITE (*,'(A,I4/,A,I4,A/,A)')
     $           ' X-coordinates not in ascending order at element',I,
     $           ' AFGEN-function contains',ILTAB,' points',
     $           ' Run deleted!'
            STOP
         END IF
      
         IF (IUP.EQ.0.AND.TABLE(I).GE.X) IUP = I

10    CONTINUE


      IF (X.LT.TABLE(1)) THEN
         WRITE (*,'(A/A,I4,A/A,G8.2)') 
     $           ' Interpolation below defined region!!',
     $           ' AFGEN-function contains ',ILTAB,' points,',
     $           ' Interpolation at X=',X
         AFGEN = TABLE(2)
         GOTO 40
      END IF


      IF (X.GT.TABLE(ILTAB-1)) THEN
         WRITE (*,'(A/A,I4,A/A,G8.2)')
     $           ' Interpolation above defined region!!',
     $           ' AFGEN-function contains ',ILTAB,' points,',
     $           ' Interpolation at X=',X
         AFGEN = TABLE(ILTAB)
         GO TO 40
      END IF


*---- normal interpolation

      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      AFGEN = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE

40    RETURN
      END




*--------------------------------------------------------------------------*
*  SUBROUTINE MOFILP                                                       *
*  Author : Daniel van Kraalingen                                          *
*  Date   : december 1986                                                  *
*  Purpose: This subroutine moves the filepointer across comment lines     *
*           of data files and places the file pointer at the first non     *
*	    comment record. Comment lines are marked by an asterisk (*)    *
*	    in their first column.                                         *
*                                                                          *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)         *
*  name    meaning                                     units  class        *
*  ----    -------                                     -----  -----        *
*  IUNIT   unit number from which data are read          -      I          * 
*                                                                          *
*  SUBROUTINES called : none                                               *
*  FILE usage : none                                                       *
*--------------------------------------------------------------------------*

      SUBROUTINE MOFILP (IUNIT)                                         

      IMPLICIT REAL (A-Z)
      INTEGER IUNIT
      CHARACTER LINE*80,CHR*1

*---- the file with unit number IUNIT is read as long as the first      
*     character of a record is an asterisk (*).

      CHR = '*'
10    IF (CHR.EQ.'*') THEN
         READ (IUNIT,'(A)') LINE
         CHR = LINE(1:1)
         GOTO 10
      ENDIF

*---- the filepointer backspaced one line                               

      BACKSPACE (IUNIT)

      RETURN                                                            
      END



*      INTEGER FUNCTION ILEN (STRING)

*     this subroutine determines the significant length (ILEN)
*     of a string (STRING), if the string is empty a zero is returned

*     ILEN, returned length (O)
*     STRING, input string  (I)

*     Author: Daniel van Kraalingen
*     Date: Aug 87


*      CHARACTER*(*) STRING

*      DO 10 ILEN=LEN(STRING),1,-1
*         IF (STRING(ILEN:ILEN).NE.' ') RETURN
*  10    CONTINUE

*      RETURN
*      END



*---------------------------------------------------------------------------*
*  SUBROUTINE RADIAT                                                        *
*  Author : Daniel van Kraalingen                                           *
*           documented by Rob Groot                                         *
*  Date   : 15 march 1987                                                   *
*  Purpose: This subroutine calculates for three moments during a           *
*           day, chosen according to a three point Gaussian integra-        *
*           tion routine, the sine of the solar elevation (SINB) and        *
*           the flux densities of the direct (PARDIR, in W/m2 and           *
*           diffuse (PARDIF, in W/m2) components of PAR.                    *
*                                                                           *
*           References to literature:                                       *
*           Spitters, C.J.T., H.A.J.M. Toussaint & J.Goudriaan, 1986.       *
*	         Agric.and For.Met.38(1986):217-229                         *
*           Spitters, C.J.T. 1986.                                          *
*	         Agric.and For.Met.38(1986):217-229                         *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  IDAY     Julian daynumber                             =      I,T         *
*  DAYL     astronomical daylength                       h      I           *
*  SINLD    intermediate variable                        =      I           *
*  COSLD    intermediate variable                        =      I           *
*  DTR      global radiation                         J m-2 d-1  I           *
*  AVRAD    global radiation                         J m-2 d-1  O           *
*  SINB     sine of the solar elevation                  =      O           *
*  PARDIR   flux density of direct  component of PAR   W m-2    O           *
*  PARDIF   flux density of diffuse component of PAR   W m-2    O           * 
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE RADIAT(IDAY,DAYL,SINLD,COSLD,DTR,SINB,PARDIR,
     $                  PARDIF,ATMTR,AVRAD)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IT
      DIMENSION XGAUS(3), WGAUS(3)
      DIMENSION PARDIR(3),PARDIF(3),SINB(3)
      LOGICAL INIT
      PARAMETER (PI=3.1415926)
      DATA XGAUS /0.1127, 0.5000, 0.8873/
      DATA WGAUS /0.2778, 0.4444, 0.2778/


*---- sine of solar elevation (SINB), integral of SINB (DSINB)          
*     and integral of SINB with correction for lower atmospheric
*     transmission at low solar elevations (DSINBE) 

      AOB   =SINLD/COSLD
      DSINB =3600.*(DAYL*SINLD+24.*COSLD*SQRT(1.-AOB*AOB)/PI)
      DSINBE=3600.*(DAYL*(SINLD+0.4*(SINLD*SINLD+COSLD*COSLD*0.5))+  
     $       12.0*COSLD*(2.0+3.0*0.4*SINLD)*SQRT(1.-AOB*AOB)/PI)


*---- solar constant (SC) and daily extraterrestrial radiation (ANGOT)  

      SC =1370.*(1.+0.033*COS(2.*PI*IDAY/365.))
      ANGOT=SC*DSINB


*---- diffuse light fraction (FRDIF) from atmospheric                   
*     transmission (ATMTR)
      AVRAD = AMIN1(0.80*ANGOT,DTR)
      ATMTR=AVRAD/ANGOT
      IF (ATMTR.GT.0.75) FRDIF=0.23
      IF (ATMTR.LE.0.75.AND.ATMTR.GT.0.35) FRDIF=1.33-1.46*ATMTR
      IF (ATMTR.LE.0.35.AND.ATMTR.GT.0.07) FRDIF=1.-2.3*(ATMTR-0.07)**2
      IF (ATMTR.LE.0.07) FRDIF=1.


*---- three hours of the day are generated                              

      DO 10,IT=1,3

         HOUR=12.0+0.5*DAYL*XGAUS(IT)
         SINB(IT)=AMAX1(0.,SINLD+COSLD*COS(2.*PI*(HOUR+12.)/24.))       
 
*------- diffuse PAR (PARDIF) and direct PAR (PARDIR) 

         PAR   =0.5*AVRAD*SINB(IT)*(1.+0.4*SINB(IT))/DSINBE
         PARDIF(IT)=AMIN1(PAR,SINB(IT)*FRDIF*ATMTR*0.5*SC)
         PARDIR(IT)=PAR-PARDIF(IT)

10    CONTINUE


      RETURN                                                           
      END




*---------------------------------------------------------------------------*
*  SUBROUTINE ASTRO                                                         *
*  Author : Daniel van Kraalingen                                           *
*           documented by Rob Groot                                         * 
*  Date   : 15 march 1987                                                   *
*  Purpose: This subroutine calculates astronomical daylength (DAYL)        *
*           and photoperiodically active daylength (DAYLP), which           *
*           are used in the calculation of canopy assimilation and in the   *
*           calculation of the development rate.                            *
*                                                                           *
*           References to literature:                                       *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  IDAY     Julian daynumber                                    I,T         *
*  LAT      latitude                                  degrees   I           *
*  DAYL     astronomical daylength                       h      O           *
*  DAYLP    photoperiodically active daylength           h      O           *
*  SINLD    intermediate variable                        =      O           *
*  COSLD    intermediate variable                        =      O           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE ASTRO (IDAY,LAT,DAYL,DAYLP,SINLD,COSLD)                     
 
      IMPLICIT REAL (A-Z)
      INTEGER IDAY
      LOGICAL INIT
      PARAMETER (PI=3.1415926)


*---- conversion factor from degrees to radians                         

      RAD=PI/180.


*---- declination of the sun as function of daynumber (IDAY)

      DEC=-ASIN(SIN(23.45*RAD)*COS(2.*PI*(IDAY+10.)/365.))


*---- SINLD, COSLD are intermediate variables

      SINLD=SIN(RAD*LAT)*SIN(DEC)
      COSLD=COS(RAD*LAT)*COS(DEC)


*---- daylength (DAYL) and photoperiodically active daylength (DAYLP)

      DAYL =12.0*(1.+2.*ASIN(SINLD/COSLD)/PI)
      DAYLP=12.0*(1.+2.*ASIN((-SIN(-4.*RAD)+SINLD)/COSLD)/PI)

      RETURN                                                            
      END





      SUBROUTINE CLS

      PRINT '(1X,A1,A3)', CHAR(27),'[2J'

      RETURN
      END





*---------------------------------------------------------------------------*
*  SUBROUTINE TIMER                                                         *
*  Author : Rob Groot                                                       *
*  Date   : december 1986                                                   *
*  Purpose: This subroutine updates TIME with DELT.                         *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  TIME     running value for time                       =      I,T         *
*  DELT     time step of integration                   days     I,T         *
*  FINTIM   time at which simulation is finished         =      I,T         *
*  STYEAR   year in which simulation starts              =      I,T         *
*  IDAY     julian daynumber                             =      O,T         *
*  YEAR     running value for year                       =      O,T         *
*  TERMIN   logical, simulation is finished when.TRUE.          O           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE TIMER(IDAY,IYEAR,IFINIT,TERMIN,INITI)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IYEAR,IFINIT,IRUN
      LOGICAL TERMIN,INITI


*---- write daynumber to screen

      CALL CLS      
      WRITE (*,*) IDAY
      IF (INITI) IRUN = 0

*---- update daynumber

      IDAY = IDAY+1
      IRUN = IRUN+1


*---- end of simulation is induced when finish time is reached          

      IF (IRUN.EQ.IFINIT) THEN
         TERMIN=.TRUE.
         WRITE (*,'(///A)') ' finish conditions reached'
      ENDIF


*---- January 1 is defined as DAY=1

      IF (IDAY.GT.365) THEN
         IDAY = 1
         IYEAR = IYEAR+1
      END IF


      RETURN 
      END



*---------------------------------------------------------------------------*
*  SUBROUTINE PENMAN                                                        *
*  Author : Daniel van Kraalingen                                           *
*           based on an earlier version written by: Kees van Diepen         *
*	    and Gert-Jan Nooy                                               *
*  Date   : 9-JAN-1987                                                      *
*  Purpose: This subroutine calculates potential evaporation from a free    *
*	    water surface (E0), a bare soil surface (ES0) and the potential *
*	    transpiration of a closed crop canopy (ET0)                     *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  ALTI    Altitude                                      m      I           *
*  ATMTR   Atmospheric transmission                      =      I           *
*  TMIN    Minimum temperature during day                C      I           *
*  TMAX    Maximum temperature during day                C      I           *
*  VAP     Vapour pressure                             mbar     I           *
*  WIND    Average windspeed                           m s-1    I           *
*  CO      Atmospheric CO2 concentration               mbar     I           *
*  E0      Potential evaporation of open water         cm d-1   O           *
*  ES0     Potential evaporation of soil               cm d-1   O           *
*  ETC     Potential evapotranspiration of crop (CO2 corr.)  cm d-1   O     *
*                                                                           *
*  FATAL ERROR CHECKS (execution terminated, message)                       *
*  condition                                                                *
*  ---------                                                                *
*  ATMTR < 0  or  ATMTR > 1                                                 *
*  TMIN > TMAX                                                              *
*  WIND < 0                                                                 *
*  AVRAD < 0                                                                *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE PENMAN (ALTI,ATMTR,TMIN,TMAX,AVRAD,WIND,VAP,CO,
     $                     E0,ES0,ETC)

      IMPLICIT REAL (A-Z)
      INTEGER IFPEN
      DIMENSION FPENMTB(10)
      LOGICAL INIT

      DATA INIT/.TRUE./

*----- correction factor for Penman evapo-transpiration rate as a function
*       of atmospheric CO2 concentration
      DATA FPENMTB/40.,1.10,355.,1.00,710.,0.90,1000.,0.85,2000.,0.85/
      DATA IFPEN/10/

*---- initial section

      IF (.NOT.INIT) GOTO 100
      INIT = .FALSE.
      A = 0.20
      B = 0.56

*---- Albedo for water surface, soil surface and canopy

      REFCFW = 0.05
      REFCFS = 0.15
      REFCFC = 0.25

*---- Latent heat of evaporation of water (J kg-1=J mm-1 m-2) and 
*     Stefan Boltzmann constant (J m-2 d-1 K-1)) Psychrometric 
*     instrument constant (K-1) 

      LHVAP  = 2.45E6
      STBC   = 4.9E-3
      PSYCON = 0.000662

100   CONTINUE

*---- average daily temperature

      TMPA = (TMIN+TMAX)/2.

*---- error checks on some input variable ranges

      IF (ATMTR.LT.0..OR.ATMTR.GT.1.) STOP 'ERR in PENMAN:ATMTR<0 or >1'  
      IF (TMIN.GT.TMAX) STOP 'ERR in PENMAN:TMIN > TMAX'
      IF (WIND.LT.0.)   STOP 'ERR in PENMAN:WIND < 0'
      IF (AVRAD.LT.0.)  STOP 'ERR in PENMAN:AVRAD < 0'

*---- Temperature difference (Celsius) 

      TDIF = TMAX-TMIN
 
*---- Coefficient Bu in wind function, dependent on temperature difference

      BU = 0.54+0.35*LIMIT(0.,1.,(TDIF-12.)/4.)

*---- Barometric pressure (mbar), Psychrometric constant (mbar K-1)

      PBAR = 1013.*EXP(-0.034*ALTI/(TMPA+273.))
      GAMMA = PSYCON*PBAR

*---- Saturated vapour pressure according to equation of Goudriaan (1977)

      SVAP = 6.11*EXP(17.4*TMPA/(TMPA+239.))

*---- Measured vapour pressure should not be greater than saturated
*     vapour pressure

      VAP  = AMIN1(SVAP,VAP)

*---- Derivative of SVAP with respect to temperature, i.e. slope of the 
*     SVAP-temperature curve (mbar K-1)

      DELTA = 239.*17.4*SVAP/(TMPA+239.)**2

*---- The expression n/N (RELSSD) from the Penman formula is estimated
*     from the Angstrom formula: RI=RA(A+B.n/N) -> n/N=(RI/RA-A)/B,
*     where AVRAD=RI and DSO=RA, the Angot radiation, obtained by a CALL 
*     to ASTRO and three statements from RADIAT:

      RELSSD = LIMIT (0.,1.,(ATMTR-A)/B)

*---- Terms of the Penman formula, for water surface, soil 
*     surface and canopy 
*     Net outgoing long-wave radiation (J m-2 d-1) according to Brunt (1932)

      RB  = STBC*(TMPA+273.)**4*(0.56-0.079*SQRT(VAP))*(0.1+0.9*RELSSD)  

*---- Net absorbed radiation

      RNW = AVRAD*(1.-REFCFW)-RB
      RNS = AVRAD*(1.-REFCFS)-RB
      RNC = AVRAD*(1.-REFCFC)-RB

*---- Evaporative demand of the atmosphere (mm d-1)

      EA  = 0.26*(SVAP-VAP)*(0.5+BU*WIND)
      EAC = 0.26*(SVAP-VAP)*(1.0+BU*WIND)

*---- Penman formula (1948), and conversion to cm d-1

      E0  = 0.1*(DELTA*(RNW/LHVAP)+GAMMA*EA)/(DELTA+GAMMA)
      ES0 = 0.1*(DELTA*(RNS/LHVAP)+GAMMA*EA)/(DELTA+GAMMA)	
      ET0 = 0.1*(DELTA*(RNC/LHVAP)+GAMMA*EAC)/(DELTA+GAMMA)


*---- Evapo(transpi)ration is limited in order to prevent negative
*     values

      E0  = AMAX1(0.,E0)
      ES0 = AMAX1(0.,ES0)
      ET0 = AMAX1(0.,ET0)

*---- correction of potential evapo-transpiration for atmospheric 
*     CO2 concentration
      ETC = ET0 * AFGEN(IFPEN,FPENMTB,CO)

      RETURN
      END




*---------------------------------------------------------------------------*
*  SUBROUTINE ASSIM                                                         *
*  Author : Daniel van Kraalingen                                           *
*  Date   : 15 march 1987                                                   *
*  Purpose: This subroutine calculates daily totals of gross                *
*	    assimilation from environmental parameters: daylength           *
*	    (DAYL) the level of diffuse and direct photosynthetically       *
*	    active radiation (PARDIR, PARDIF), sine of the solar            *
*	    elevation, maximum rate of assimilation (PAMAX), initial        *
*	    light use efficiency (EFF), light extinction of diffuse         *
*	    radiation (KDIF) and the leaf area index of the crop            *
*	    (LAI).                                                          *
*                                                                           *
*           References to literature:                                       *
*           Spitters, C.J.T., H.A.J.M. Toussaint & J.Goudriaan, 1986.       *
*	         Agric.and For.Met.38(1986):217-229                         *
*           Spitters, C.J.T. 1986.                                          *
*	         Agric.and For.Met.38(1986):217-229                         *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                        units  class     *
*  ----     -------                                        -----  -----     *
*  DAYL     astronomical daylength                           h      I,T     *
*  PAMAX    potential maximum rate of photosynthesis                        *
*	    at light saturation                        kg ha-1 h-1  I       *
*  EFF      initial light use efficiency               kg ha-1 h-1  I       *
*                                                       J m-2 s-1   I       *
*  KDIF     extinction coefficient for diffuse light         =      I       *  
*  LAI      leaf area index                              m2 m-2     I       *
*  PARDIR   array,flux density of direct  component of                      *
*           PAR                                         J m-2 s-1   I       *
*  PARDIF   array,flux density of diffuse component of                      *
*           PAR                                         J m-2 s-1   I       * 
*  SINB     array,sine of the solar elevation                =      I       *
*  DTGA     daily total gross assimilation             kg ha-1 d-1  O       *
*  PARABT   total intercepted PAR                       J m-2 d-1   O       *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE ASSIM (DAYL,PAMAX,EFF,KDIF,LAI,PARDIR,PARDIF,
     $                  SINB,DTGA,PARABT) 

      IMPLICIT REAL (A-Z)
      INTEGER IHOUR, ILAI
      DIMENSION XGAUS(3), WGAUS(3)
      DIMENSION PARDIR(3), PARDIF(3),SINB(3)
      LOGICAL INIT
      PARAMETER (PI=3.1415926)
      DATA XGAUS /0.1127, 0.5000, 0.8873/
      DATA WGAUS /0.2778, 0.4444, 0.2778/

      SCV =0.2                                                          

*---- reflection coefficient of a canopy of horizontal leaves
*    (Goudriaan, 1974, p14,31)

      REFH=(1.-SQRT(1.-SCV))/(1.+SQRT(1.-SCV))

*---- assimilation set to zero and three different times of             
*     the day (HOUR) 

      DTGA=0.
      PARABT = 0.
      DO 100,IHOUR=1,3

*------- reflection coefficient for a spherical leaf angle distribution

         REFS = REFH*2./(1.+1.6*SINB(IHOUR))                                

*------- extinction coefficient for direct radiation and total             
*        direct flux

         KDIRBL = (0.5/SINB(IHOUR))*KDIF/(0.8*SQRT(1.-SCV))
         KDIRT  = KDIRBL*SQRT(1.-SCV)

*------- selection of depth of canopy, canopy assimilation                 
*        is set to zero

         FGROS = 0.
         PARABS = 0.
         DO 10,ILAI=1,3

            LAIC = LAI*XGAUS(ILAI)

*---------- absorbed fluxes per unit leaf area: diffuse flux (VISDF), 
*           total direct flux (VIST), and direct component of direct
*           flux (VISD).

            VISDF = (1.-REFS)*PARDIF(IHOUR)*KDIF*EXP(-KDIF*LAIC)
            VIST  = (1.-REFS)*PARDIR(IHOUR)*KDIRT*EXP(-KDIRT*LAIC)
            VISD  = (1.-SCV)*PARDIR(IHOUR)*KDIRBL*EXP(-KDIRBL*LAIC)

*---------- absorbed flux (J M-2 s-1) for shaded leaves (VISSHD) and                 
*           assimilation of shaded leaves (FGRSH)

            VISSHD = VISDF+VIST-VISD
            FGRSH  = PAMAX*(1.-EXP(-VISSHD*EFF/PAMAX))

*---------- direct flux absorbed by leaves perpendicular on                  
*           direct beam and assimilation of sunlit leaf area (FGRSUN)

            VISPP = (1.-SCV)*PARDIR(IHOUR)/SINB(IHOUR)
            IF (VISPP.GT.0.) THEN
                FGRSUN = PAMAX*(1.-(PAMAX-FGRSH)*(1.-EXP(-VISPP*EFF/
     $                   PAMAX))/(EFF*VISPP))
            ELSE
                FGRSUN = FGRSH
            END IF

*---------- fraction sunlit leaf area (FSLLA) and local                       
*           assimilation rate (FGL)

            FSLLA = EXP(-KDIRBL*LAIC)
            FGL   = FSLLA*FGRSUN+(1.-FSLLA)*FGRSH

*---------- intercepted radiation
            PARTOT = VISDF + VIST

*---------- calculation of average assimilation rate of canopy (FGROS)

            FGROS = FGROS+FGL*WGAUS(ILAI)

*---------- integration of intercepted radiation
            PARABS = PARABS + PARTOT*WGAUS(ILAI)

10       CONTINUE

*------- integration of average rate over crop LAI

         FGROS = FGROS*LAI
         PARABS = PARABS*LAI

*------- calculation of average assimilation rate during day

         DTGA = DTGA+FGROS*WGAUS(IHOUR)
         PARABT = PARABT + PARABS*WGAUS(IHOUR)         
100   CONTINUE                                         

*---- integration of average rate over daylength to obtain total gross
*     assimilation rate and total intercepted radiation

      DTGA = DTGA*DAYL
      PARABT = PARABT*DAYL*3600.
      RETURN                                                            
      END






*---------------------------------------------------------------------------*
*  FUNCTION INSW                                                            *
*  Purpose: Input switch depending on sign of X1                            *
*---------------------------------------------------------------------------*

      FUNCTION INSW (X1,X2,X3)                                          

      IMPLICIT REAL (A-Z)

      IF (X1.LT.0.) THEN                                                
         INSW = X2
      ELSE
         INSW = X3
      END IF

      RETURN                                                            
      END
