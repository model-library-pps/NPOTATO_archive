*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*

*---------------------------------------------------------------------------*
*  SUBROUTINE WATBAL                                                        *
*  Author : Rob Groot                                                       *
*  Date   : december 1986                                                   *
*  Purpose: This subroutine simulates soil water status. The soil is        *
*	    devided into a specified number of soil compartments of         *
*	    variable thickness.                                             *
*	    The following processes are taken into account:                 *
*	    - soil surface evaporation                                      *
*	    - distribution of soil evaporative losses over the soil         *
*	      compartments                                                  *
*	    - infiltration and rainfall                                     *
*	    - crop transpiration                                            *
*                                                                           *
*           References to literature:                                       *
*                van Keulen & Seligman, 1987. Simulation of water use,      *
*	         nitrogen nutrition and growth of a spring wheat crop.      *
*                Pudoc, Wageningen, in press.                               *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  RD       rooting depth                               cm      I           *
*  IDAY     julian daynumber                             =      I,T         *
*  REMARK   output remarks                               =      I           *
*  RUNNAME  first 5 characters for name of output files         I           *
*  ETC      pot. evapo-transpiration                  cm d-1    I           *
*  ES0      pot. evaporation of bare soil surface     cm d-1    I           *
*  LAI      leaf area index of crop                   m2 m-2    I           *
*  RAIN     daily precipitation                       cm d-1    I           *
*  TRR      array, transpiration rate per soil depth                        *
*           from each soil compartment             cm cm-1 d-1  O           *
*  TRTOT    total transpiration during one day          cm      O           *
*  WCREL    array, rel. watercontent of soil comp.    cm cm-3   O           *
*  RDMSOL   maximum rooting depth                       cm      O           *
*  THCKN    array, thickness of soil compartments       cm      O           *
*  DEPTH    array, middle of soil compartment measured                      *
*	    from top of soil profile                    cm      O           *
*  ILAYER   number of soil compartment                   =      O           *
*  TTRANS   total transpiration over sim. period        cm      O           *
*  TDRAIN   total drainage over simulated period        cm      O           *
*  TRAIN    total rainfall over simulated period        cm      O           *
*  FLOW     array, rate of waterflow between soil                           *
*	    compartments                              cm d-1    O           *
*  FLOWR    infiltration from precipitation           cm d-1    O           *
*  TESOIL   total evaporation over simulated period     cm      O           *
*  WCACT    array, act.water content of compartment   cm cm-1   O           *
*  WCACT0   array, act.water content of compartment                         *
*	    before routine is executed                cm cm-1   O           *
*  RHOD     array, bulk density of soil compartments   g cm-3   O           *
*  IRRI     0 = no irrigation; 1 = irrigation                   I           *
*  RUNFR    fraction of rainfall lost by run off                I           *
*  RESOIL   array, evaporation rate per soil depth from                     *
*           each soil compartment                   cm cm-1 d-1 O           *
*  WCWILT   array, water content at wilting point for                       *
*	    each soil compartment                     cm cm-3   O           *
*  WCFLDC   array, soil moisture content at field                           *
*	    capacity for each soil compartment        cm cm-1   O           *
*  TRMAX    maximum transpiration rate of crop        cm d-1    O           *
*  TERMIN   logical, indicates end of simulation       -        I           *
*  KDIF     extinction coefficient for diffuse visible light -  I           *   
*  START    logical, indicates crop growth             -        I           *
*  IDEMERG  date of crop emergence                     -        I           *
*  INITI    logical, indicates start of simulation     -        I           *
*  STATR    name of weather station (CHAR*5)           -        I           *
*  ISOIL    number of soil type                        -        I           *
*  ICROP    number of crop species                     -        I           *
*  IORGM    number of soil organic matter data base    -        I           *
*  INSOIL   number of soil nitrogen data base          -        I           *
*  IPM      1 = Penman-Monteith for crop transpiration -        I           *
*  CO       atmospheric CO2 concentration             ppmv      I           *
*  IOUT     frequency of output                        d        I           *
*                                                                           *
*  SUBROUTINES called :                                                     *
*  SOILR    reads physical soil data from data file                         *
*	    SOIL.DAT                                                        *
*  EVSOIL   calculates soil surface evaporation and distributes             *
*	    evaporation over soil compartments                              *
*  WATOUT   writes soil water status to output file WATOUT.DAT              *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE WATBAL (ISTEPW,RD,IDAY,REMARK,RUNNAME,ETC,ES0,LAI,
     $                   RAIN,TRR,TRTOT,WCREL,RDMSOL,THCKN,DEPTH,
     $                   ILAYER,TTRANS,TDRAIN,TRAIN,FLOW,TESOIL,WCACT,
     $                   WCACT0,IRRI,RUNFR,RHOD,RESOIL,WCWILT,WCFLDC,
     $                   TRMAX,FLOWR,TERMIN,KDIF,
     $                   START,IDEMERG,INITI,STATR,ISOIL,ICROP,IORGM,
     $                   INSOIL,IPM,CO,IOUT)
	
      IMPLICIT REAL (A-Z)
      INTEGER ISTEPW,ILAYER,ISOIL,ICROP,IORGM,INSOIL
      INTEGER IRRI1,I, ITEXCL,ISMTB1,ISMTB2,ISMTB3,ISMTB4
      INTEGER ISMTB5, IDAY,IN,IPM,IRRI,IOUT,IDEMERG
      LOGICAL INIT,TERMIN,START,INITI
      CHARACTER REMARK*80,RUNNAME*5,STATR*5

      COMMON /WATBOUT/ TTRANS1,TDRAIN1,TRAIN1,TESOIL1,DWAVT
      COMMON /WATBOUT/ TRUNOF1,TIRR1,RUNFR1,IRRI1

      DIMENSION WCMAX(10) , WCFLDC(10), WCWILT(10), WCAIR(10)
      DIMENSION WCINI(10) , WCACT(10) , WCREL(10) , WCACT0(10)
      DIMENSION RESOIL(10), TRR(10)   , THCKN(10)  , DEPTH(10)
      DIMENSION SMTB1(30) , SMTB2(30) , SMTB3(30)  ,ITEXCL(10)
      DIMENSION SMTB4(30) , SMTB5(30) , FLOW(11)   ,RHOD(10)
      DIMENSION WCCR(10)   ,RED(10)
      

*----------------------------------------------------------------------------   
*     Data initialization of soil water balance
*----------------------------------------------------------------------------   

      IF (INITI .AND. ISTEPW .EQ. 1) INIT = .TRUE.
      IF (.NOT.(INIT) .OR. INIT .AND. ISTEPW .EQ. 2) GO TO 200


*---- read physical soil properties from soil data file SOIL.DAT


      CALL SOILR (ISOIL,ISMTB1,ISMTB2,ISMTB3,ISMTB4,
     $      ISMTB5,SMTB1,SMTB2,SMTB3,SMTB4,SMTB5,THCKN,
     $      RHOD,ITEXCL,ILAYER,RDMSOL,PROP,WCINI,RUNFR)

       

*---- pF characteristics of the boundary water contents                


      PF_MAX =  0.
      PF_FLDC = 2.3
      PF_WILT = 4.2

    
*----------------------------------------------------------------------------   
*     Initialization  of water balance
*----------------------------------------------------------------------------   

      TDRAIN = 0.0
      TRAIN  = 0.0
      TTRANS = 0.0
      TRTOT  = 0.0
      TESOIL = 0.0
      TIRR = 0.0
      DIRR = 0.0
      TRUNOF = 0.0
      ESOIL  = 0.0
      RD = 0.0
      LAI = 0.0

      DO 5 I = 1,ILAYER
      TRR(I) = 0.0
      RESOIL(I) = 0.0
5     CONTINUE 

*---- for the soil, the water holding characteristics are 
*     computed


      DO 10,I=1,ILAYER

*------- in case a compartment is of texture class one

         IF (ITEXCL(I).EQ.1) THEN
            WCMAX(I)  = AFGEN (ISMTB1,SMTB1,PF_MAX)
            WCFLDC(I) = AFGEN (ISMTB1,SMTB1,PF_FLDC)
            WCWILT(I) = AFGEN (ISMTB1,SMTB1,PF_WILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class two

         IF (ITEXCL(I).EQ.2) THEN
            WCMAX(I)  = AFGEN (ISMTB2,SMTB2,PF_MAX)
            WCFLDC(I) = AFGEN (ISMTB2,SMTB2,PF_FLDC)
            WCWILT(I) = AFGEN (ISMTB2,SMTB2,PF_WILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class three

         IF (ITEXCL(I).EQ.3) THEN
            WCMAX(I)  = AFGEN (ISMTB3,SMTB3,PF_MAX)
            WCFLDC(I) = AFGEN (ISMTB3,SMTB3,PF_FLDC)
            WCWILT(I) = AFGEN (ISMTB3,SMTB3,PF_WILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class four

         IF (ITEXCL(I).EQ.4) THEN
            WCMAX(I)  = AFGEN (ISMTB4,SMTB4,PF_MAX)
            WCFLDC(I) = AFGEN (ISMTB4,SMTB4,PF_FLDC)
            WCWILT(I) = AFGEN (ISMTB4,SMTB4,PF_WILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

*------- in case a compartment is of texture class five

         IF (ITEXCL(I).EQ.5) THEN
            WCMAX(I)  = AFGEN (ISMTB5,SMTB5,PF_MAX)
            WCFLDC(I) = AFGEN (ISMTB5,SMTB5,PF_FLDC)
            WCWILT(I) = AFGEN (ISMTB5,SMTB5,PF_WILT)
            WCAIR(I)  = WCWILT(I)/3.
         END IF

10    CONTINUE


*---- calculation of depth of subsequent compartments                        


      DEPTH(1)=0.5*THCKN(1)
      DO 20,I=2,ILAYER
         DEPTH(I)=DEPTH(I-1)+0.5*THCKN(I-1)+0.5*THCKN(I)
20    CONTINUE


*----------------------------------------------------------------------------   
*     initialization of water contents
*----------------------------------------------------------------------------   

*---- initialization of actual water contents cm3 cm-3


      DO 30,I=1,ILAYER
         WCACT(I) = WCINI(I)
30    CONTINUE


*---- total amount of water in profile after calculations              


      WTOT = 0.
      DO 33,I=1,ILAYER
      WTOT = WTOT+WCACT(I)*THCKN(I)
33    CONTINUE
      
       
*---- total available amount of water in profile (above wilting point)

      WAVT = 0.
      DO 35, I=1,ILAYER
      WAVT = WAVT + (WCACT(I) - WCWILT(I)) * THCKN(I)
35    CONTINUE

      WAVTS= WAVT
*---- total available amount of water in rooted zone (above wilting point)

      WAVR = 0.

        CALL WATOUT (IDAY,WCACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $        RUNFR,IRRI,WTOT,WAVT,
     $        WAVR,REMARK,RUNNAME,TERMIN,IDEMERG,INIT,STATR,ISOIL,ICROP,
     $        IORGM,INSOIL,CO,IOUT)

*----------------------------------------------------------------------------   
*     dynamic section of water balance
*----------------------------------------------------------------------------   

200   CONTINUE

*---- subroutine WATOUT is called to write soil water balance data to 
*     output file *****WAT.OUT

      IF (ISTEPW .EQ. 1 .AND. .NOT.INIT)
     $  CALL WATOUT (IDAY,WCACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $        RUNFR,IRRI,WTOT,WAVT,
     $        WAVR,REMARK,RUNNAME,TERMIN,IDEMERG,INIT,STATR,ISOIL,ICROP,
     $        IORGM,INSOIL,CO,IOUT)



      INIT = .FALSE.
      

      IF (ISTEPW .EQ. 2) GOTO 1000

*------ data for summary output
        
        IF (TERMIN) THEN

        TTRANS1= TTRANS
        TDRAIN1= TDRAIN
        TRAIN1= TRAIN
        TESOIL1= TESOIL
        TRUNOF1= TRUNOF
        TIRR1= TIRR
        IRRI1= IRRI
        RUNFR1= RUNFR       
        DWAVT= WAVTS - WAVT

        ENDIF
        
*-----------------------------------------------------------------------------
*     STEP1 
*
*     soil water balance is executed twice during one time step:
*     in step1 the redistribution of pricipitation is calculated
*     and moisture contents of compartments when soil is at its moist 
*     are transferred to main program. 
*     mineralization and immobilization are calculated at these moisture
*     contents. in step2, moisture withdrawal through transpiration and
*     evaporation are subtracted from moisture contents
*-----------------------------------------------------------------------------

*---- total amount of water in profile before calculations              


      TWAT1 = 0.
      DO 40,I=1,ILAYER
        TWAT1 = TWAT1+WCACT(I)*THCKN(I)
40    CONTINUE
      
*---- save actual water contents in array WCACT0 for use in soil
*     nitrogen routine NITBAL


      DO 50,I=1,ILAYER
         WCACT0(I) = WCACT(I)
50    CONTINUE	


*---- 'waterfall' infiltration of precipitation and irrigation
*     water. This procedure is 
*     executed only when infiltration occurs


      RIRR = LIMIT(0.,1., DIRR)

      DO 60,I=1,ILAYER+1
         FLOW(I) = 0.
60    CONTINUE

      IF ((RAIN + RIRR) .GT.0.) THEN
         FLOW(1) = (1.-RUNFR) * RAIN + RIRR
                
         DO 70,I=1,ILAYER

*---------- water holding capacity of the compartment (cm)


            CAP = (WCFLDC(I)-WCACT(I))*THCKN(I)


*---------- if the capacity of the compartment is smaller than the 
*           infiltration, the layer is filled up to field capacity, 
*           if not the layer is filled with all that is left of the 
*           infiltration.


            IF (CAP.LE.FLOW(I)) THEN
               WCACT(I) = WCFLDC(I)
               FLOW(I+1)= FLOW(I)-CAP
            ELSE
               WCACT(I) = WCACT(I)+FLOW(I)/THCKN(I)
               FLOW(I+1)=0.
            END IF

70       CONTINUE

      END IF
                 
      RUNOF = RUNFR * RAIN
      FLOWR= (1.-RUNFR) * RAIN

*---- integrals for drainage and precipitation


      TDRAIN = INTGRL (FLOW(11),TDRAIN)
      TRAIN  = INTGRL (RAIN,TRAIN)
      TRUNOF = INTGRL (RUNOF,TRUNOF)
      TIRR   = INTGRL (RIRR,TIRR)

*---- relative water content, available for crop moisture uptake


      DO 75,I=1,ILAYER
         WCREL(I)=MAX(0.,(WCACT(I)-(WCWILT(I)+1.0E-9))/
     $           (WCFLDC(I)-(WCWILT(I)+1.0E-9)))
75    CONTINUE


      GOTO 2000


*----------------------------------------------------------------------------
*     STEP2
*----------------------------------------------------------------------------


1000  continue


*---- Rooting depth


*-----search for compartment where root growth takes place                    
*     plus penetration in deepest compartment


      IF (RD.GT.0.AND.LAI.GT.0.) THEN


         DO 11,I=1,ILAYER
            IF (RD.GT.(DEPTH(I)-0.5*THCKN(I)).AND.
     $      RD.LE.(DEPTH(I)+0.5*THCKN(I))) IN=I
11       CONTINUE

         RDINL = RD-(DEPTH(IN-1)+0.5*THCKN(IN-1))


*------- calculate transpiration rates

         TRMAX = ETC*(1.-EXP(-0.75*KDIF*LAI))

         IF (IPM .EQ. 1) TRMAX= ETC
                         
                       
*------  soil water depletion depends on potential transpiration rate and
*        crop species

         SWDEP = LIMIT(0.10, 0.95, 1./(0.76 + 1.5*ETC) - 0.20)
       
         DO 25,I=1,ILAYER
         WCCR(I) = (1. - SWDEP) * (WCFLDC(I) - WCWILT(I)) + WCWILT(I)
         RED(I) = 0.
25       CONTINUE

         REDT= 0.
         DO 27, I=1,IN-1
         RED(I)= LIMIT(0.,1., (WCACT(I) - WCWILT(I))/
     $           ( WCCR(I) - WCWILT(I)) )
         REDT = REDT + THCKN(I) * RED(I) 
27       CONTINUE
         
         RED(IN)= LIMIT(0.,1.,(WCACT(IN) - WCWILT(IN))/
     $          ( WCCR(IN) - WCWILT(IN)) ) 
         REDT = REDT + RDINL * RED(IN)

         REDTR = AMIN1(1., REDT/AMIN1(50., RD))
         IF (IRRI .EQ. 1) REDTR = 1.
         TRTOT = REDTR * TRMAX

         TRPCM = TRTOT/AMAX1(0.000001,REDT)

         TRTOT = 0.                                                        
         DO 31,I=1,ILAYER
            IF (I.LE.IN-1) THEN
               TRR(I) = TRPCM*RED(I)
               TRTOT  = TRTOT+TRR(I)*THCKN(I)  
            ELSE
               TRR(I) = 0.
            END IF
31       CONTINUE
       
         TRR(IN) = TRPCM*RED(IN)*RDINL/THCKN(IN)
         TRTOT   = TRTOT+TRR(IN)*THCKN(IN)

      ELSE

         DO 41, I=1,ILAYER
            TRR(I) = 0.            
41       CONTINUE
         TRTOT= 0.
      END IF


*---- transpiration is subtracted from the soil water status


      DO 80,I=1,ILAYER
         WCACT(I) = WCACT(I)-TRR(I)


80    CONTINUE

      

*---- subroutine EVSOIL is called to compute the soil evaporation 
*     from each soil compartment   


      CALL EVSOIL (LAI,ES0,KDIF,ILAYER,WCACT,WCFLDC,WCAIR,THCKN,
     $             PROP,DEPTH,ESOIL,RESOIL)


*---- soil surface evaporation is subtracted from the soil water status


      DO 85,I=1,ILAYER
         WCACT(I) = WCACT(I)-RESOIL(I)
85    CONTINUE

      
*---- integrals of soil water balance                                  


      TTRANS = INTGRL (TRTOT,TTRANS)
      TESOIL = INTGRL (ESOIL,TESOIL)


*---- total amount of water in profile after calculations              


      TWAT2 = 0.
      DO 90,I=1,ILAYER
         TWAT2 = TWAT2+WCACT(I)*THCKN(I)
90    CONTINUE
      WTOT = TWAT2

       
*---- total available amount of water in profile (above wilting point)

      WAVT = 0.
      DO 110, I=1,ILAYER
      WAVT = WAVT + (WCACT(I) - WCWILT(I)) * THCKN(I)
110   CONTINUE

*---- total available amount of water in rooted zone (above wilting point)
*     and total available amounts of water at critical point for irrigation 
*     and at field capacity

      WAVR = 0.
      WAVCR = 0.
      WAVFC = 0.
      DIRR = 0.

      IF (RD.GT.0. .AND. LAI.GT.0. .AND. START) THEN

        DO 120, I = 1,IN-1
        WAVR = WAVR + (WCACT(I) - WCWILT(I)) * THCKN(I)
120     CONTINUE
        WAVR = WAVR + RDINL * (WCACT(IN) - WCWILT(IN)) 

        DO 130, I = 1,IN-1
        WAVCR = WAVCR + (WCCR(I) - WCWILT(I)
     $          + 0.5*(WCFLDC(I) - WCCR(I))) * THCKN(I)
130     CONTINUE
        WAVCR = WAVCR + RDINL * (WCCR(IN) - WCWILT(IN)
     $          + 0.5*(WCFLDC(IN) - WCCR(IN))) 

        DO 140, I = 1,IN-1
        WAVFC = WAVFC + (WCFLDC(I) - WCWILT(I)) * THCKN(I)
140     CONTINUE
        WAVFC = WAVFC + RDINL * (WCFLDC(IN) - WCWILT(IN)) 

        IF (WAVR .LT. WAVCR .AND. IRRI .EQ. 1) THEN
                DIRR= 0.7 * (WAVFC - WAVR)
        ELSE
                DIRR= 0.
        END IF
      ELSE
      WAVR = 0.
      DIRR = 0.

      ENDIF


      

*---- check soil water balance: WBAL should always be zero!!!
       
      WBAL = ABS(TWAT1-TRTOT-ESOIL+RAIN+RIRR-RUNOF-FLOW(ILAYER+1)-TWAT2)


      IF (WBAL.GE.1.E-4) STOP
     $   ' soil moisture balance not 0, program aborted'



2000  CONTINUE
      
      RETURN                                                           
      END
