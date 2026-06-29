

*--------------------------------------------------------------------------*
*  SUBROUTINE SOLTMP                                                       *
*  Author : J.J.R. Groot                                                   *
*  Date   : 11-february-1987                                               *
*  Purpose: This subroutine generates temperatures of different soil       *
*	    compartments, based on the assumption that the temperature-    *
*           fluctuations are smaller in deeper soil compartments, and that *
*	    the temperature of a compartment can be described by a delay-  *
*	    function of the average daily air temperature                  *
*                                                                          *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)         *
*  name    meaning                                     units  class        *
*  ----    -------                                     -----  -----        *
*  TMIN    minimum air temperature                       C      I          *
*  TMAX    maximum air temperature                       C      I          *
*  ILAYER  number of soil compartments                   -      I          *
*  SOILTP  array with generated temperature per                            *
*          compartment                                   C      O          *
*  INITI   indicates initialization                      -      C, I       *                                                    *
*  subroutines called : none                                               *
*  file usage : none                                                       *
*--------------------------------------------------------------------------*

      SUBROUTINE SOLTMP (TMIN,TMAX,ILAYER,SOILTP,INITI)

      IMPLICIT REAL (A-Z)
      INTEGER ILAG,IDAY,ILAYER,IL,ISTART,INIDAY,I
      LOGICAL INITI,INIT
      DIMENSION SOILTP(10),ILAG(10),AVTMP(1000)
      
*---- temperature delay for soil compartments

      DATA ILAG/1,4,8,16,26,38,52,68,84,100/

      IF (INITI) INIT = .TRUE.
      IF (.NOT. INIT) GOTO 5
      IDAY = 0
5     CONTINUE    

*---- average daily temperature

      TMPA = (TMIN+TMAX)/2.

      IDAY  = IDAY + 1
      AVTMP(IDAY) = TMPA

      DO 100,IL = 1,ILAYER

*-------  when the delay is one day, compartment-temperature equals 
*         average air temperature

         IF (ILAG(IL).EQ.1) THEN
            SOILTP(IL) = AVTMP(IDAY)
         ELSE

*--------- calculate temperature of compartment IL. For days prior to 
*          initialization, average daily air temperatures are not 
*          available. Therefore the temperature of all soil compartments
*          at the start of the simulation is assumed to be equal to the air
*          temperature 

           ISTART = MAX(IDAY-ILAG(IL)+1,1)

*          length of delay period before start of the simulation, for which temperature
*          is assumed to be equal to the air temperature at start 

           INIDAY = MAX(ILAG(IL)-IDAY,0)

*          summarizing air temperature over the delay period for each soil compartment
     
           SUMTMP = 0.
           DO 10,I = ISTART, IDAY
              SUMTMP = SUMTMP + AVTMP(I)
10         CONTINUE

           SOILTP(IL) = ((INIDAY*AVTMP(1)+SUMTMP)/ILAG(IL))

        ENDIF

100   CONTINUE

      RETURN
      END
