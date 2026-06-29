*--------------------------------------------------------------------*
*  SUBROUTINE NITBAL                                                 *
*  Authors: Rob Groot                                                *
*  Date of last revision  : 10-01-89                                 *
*  Purpose: This subroutine calculates the soil nitrogen status      *
*           For each soil compartment. Decomposition of crop         *
*           residues and old organic matter are calculated in a      *
*           separate subroutine OMDYN, and calculated mineralization *
*           and immobilization are input to this routine.            *
*           Mixing of soil water and nitrogen is described according *
*           to Burns (1974). Nitrogen losses from soil compartments  *
*           occur by leaching, immobilization and nitrogen uptake    *
*           by the crop.                                             *
*                                                                    *
*           References to literature:                                *
*           Burns, I.G.,1984. J.soil Sci.,Vol25.No2:165-178          *
*                                                                    *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)   *
*  name     meaning                                    units  class  *
*  ----     -------                                    -----  -----  *
*  ISTEP    Integer*1, routine is executed for first,                *
*           second or third time during a day IDAY       =      I,T  *
*  IDAY     Julian daynumber                             =      I,T  *
*  REMARK   output remark                                =      I    *
*  RUNNAME  first 5 char's of output-file name           =      I    *
*  FLOW     array, rate of waterflow between soil                    *
*	    compartments                               cm d-1   I    *
*  WCACT    array, actual watercontent of soil                       *
*	    compartments                               cm cm-1  I    *
*  THCKN    array, thickness of soil compartments        cm     I    *
*  ILAYER   number of soil compartments                  =      I    *
*  WCACT0   array, actual watercontent of soil                       *
*  IN       nr. of compartment in which roots grow              I    *
*  RDINL    rooting depth in comp. were roots grow       cm     I    *
*  TRR      array, transpiration rate per soil comp.   cm d-1   I    *
*  RD       rooting depth                                cm     I    *
*  DVS      crop development stage                        =     I    *
*  NDEM     nitrogen demand of the crop             kg ha-1 d-1 I    *
*  WCWILT   array, water content of soil compartments                *
*  ANLAY    array, amount of N in soil compartments kg(N)/ha-1  O    *
*  NUPTR    nitrogen uptake rate of the crop        kg ha-1 d-1 O    *
*  NIMM     soil mineral nitrogen lost by immob     kg ha-1 d-1 I    *
*  MINFM    nitrogen mineralized from FOM           kg ha-1 d-1 I    *
*  MINOM    nitrogen mineralized from old OM        kg ha-1 d-1 I    *
*  FMTOT    total nitrogen in fresh organic matter  kg(N) ha-1       *
*  NBIOM    nitrogen immobilized by growing biomass kg ha-1 d-1 I    *
*  FLOWR    infiltration from precipitation         cm d-1      I    *
*  TERMIN   logical, indicates end of simulation     -          I    *
*  IDEMERG  date of crop emergence                   d          I    *
*  INITI    logical, indicates start of simulation   -          I    *
*  STATR    name of weather station (CHAR*5)         -          I    *
*  ISOIL    number of soil type                      -          I    *
*  ICROP    number of crop species                   -          I    *
*  IORGM    number of soil organic matter data base  -          I    *
*  INSOIL   number of soil nitrogen data base        -          I    * 
*  IRRI     0 = no irrgiation; 1 = irrigation        -          I    *
*  CO       atmospheric CO2 concentration           ppmv        I    *
*  IOUT     frequency of output                      d          I    *
*  IPOT     potential production (no N shortage)     -          I    *
*                                                                    *
*  SUBROUTINES called :                                              *
*  NITDAT   reads soil nitrogen data from file                       *
*	    NSOIL.DAT                                                *
*  NITOUT   writes soil nitrgen status to the output file NITOUT.DAT *
*  FILE usage : none                                                 *
*--------------------------------------------------------------------*


      SUBROUTINE NITBAL (ISTEPN,IDAY,REMARK,RUNNAME,FLOW,
     $                   WCACT,THCKN,ILAYER,WCACT0,IN,RDINL,
     $                   TRR,RD,DVS,NDEM,WCWILT,ANLAY,NUPTR,NIMM,
     $                   MINFM,MINOM,NBIOM,FMTOT,FLOWR,TERMIN,IDEMERG,
     $                   INITI,STATR,ISOIL,ICROP,IORGM,INSOIL,
     $                   IRRI,CO,IOUT,IPOT)

      IMPLICIT REAL (A-Z)
      INTEGER ILAYER, IDAYN, INUMAP, IDAY, ISOIL, ICROP
      INTEGER IORGM, INSOIL, ISTEPN, IT, I,IN, IRRI,IOUT,IDEMERG, IPOT
      LOGICAL INIT,TERMIN,INITI
      CHARACTER REMARK*80,RUNNAME*5,STATR*5

      COMMON /NITBOUT/ TNUPTC1,TMINOM1,TMINFM1,TBIOM1
      COMMON /NITBOUT/ TNIMM1,TLEACH1,TNRAIN1,TNGIFT1,DFMTOT,DNTOT

      DIMENSION FLOW(11)  ,WCACT(10) ,ANLAY(10) ,THCKN(10) ,WCACT0(10)
      DIMENSION TRR(10)   ,MINOM(10) ,WCWILT(10),NTRL(10)  ,NDFL(10)  
      DIMENSION IDAYN(5)  ,ANAP(5)   ,NCON(10)  ,NBIOM(10)  ,MINFM(10) 
      DIMENSION NIMM(10) 

      IF (INITI .AND. ISTEPN .EQ. 1) INIT = .TRUE.
     
      IF (.NOT.INIT .OR. INIT .AND. ISTEPN .NE. 1) GOTO 100

      NUPTR  = 0.      
      TNUPTC = 0.
      TMINOM = 0.
      TMINFM = 0.
      TBIOM  = 0.
      TNIMM  = 0.
      TLEACH = 0.
      TNRAIN = 0.
      TNGIFT = 0.
      NDEM = 0.0

*--------------------------------------------------------------------------
*     initialization of the soil nitrogen balance
*--------------------------------------------------------------------------


*---- Read soil nitrogen data 


      CALL NITDAT (INSOIL,ILAYER,NTOT,ANLAY,IDAYN,ANAP,NCRAIN,TCND,
     $             ADC,FMAX,INUMAP)

      DO 5,I=1,ILAYER
      NTRL(I) = 0.0
      NDFL(I) = 0.0
5     CONTINUE

*     potential production (no nitrogen shortage)
  
      IF (IPOT .EQ. 1) THEN
        ANAP(1) = 400.
        ANAP(2) = 0.
        ANAP(3) = 0.
      ENDIF


      CALL OMDAT (IORGM,PLAYER,PERCOM,RDROM,REFTMP,FM,RDRFM,CCFM,NCFM,
     $    DISASS,NCRB)

      FMTOT = FM * NCFM
      FMTOTS= FMTOT
      NTOTS= NTOT

        CALL NITOUT (IDAY,ANLAY,NTOT,NDF,
     $             NUPTR,TNUPTC,TMINOM,TMINFM,TBIOM,TNIMM,
     $             TLEACH,FMTOT,REMARK,RUNNAME,TERMIN,IDEMERG,INIT,
     $             TNRAIN,TNGIFT,STATR,ISOIL,ICROP,IORGM,INSOIL,
     $             IRRI,IPOT,CO,IOUT) 


100   CONTINUE
 
*---------------------------------------------------------------------------
*     write soil nitrogen status to output-file
*---------------------------------------------------------------------------
      
      IF (ISTEPN .EQ. 1 .AND. .NOT.INIT )
     $  CALL NITOUT (IDAY,ANLAY,NTOT,NDF,
     $             NUPTR,TNUPTC,TMINOM,TMINFM,TBIOM,TNIMM,
     $             TLEACH,FMTOT,REMARK,RUNNAME,TERMIN,IDEMERG,INIT,
     $             TNRAIN,TNGIFT,STATR,ISOIL,ICROP,IORGM,INSOIL,
     $             IRRI,IPOT,CO,IOUT) 

      INIT = .FALSE.



*--------------------------------------------------------------------------
*     dynamic part
*--------------------------------------------------------------------------

      

*---- reset daily totals to zero

      NUPTR   = 0.0  
      NS      = 0.0  
      NTR     = 0.0  
      NDF     = 0.0  
      PNTR    = 0.0   
      MINOMP  = 0.0  
      NBIOMP   = 0.0  
      MINFMP  = 0.0  
      NIMMP  = 0.0  
      LEACH   = 0.0  
      NRAIN   = 0.0
      NGIFT   = 0.0

      
      IF (ISTEPN.EQ.2) GOTO 999
      IF (ISTEPN.EQ.3) GOTO 1111

*----- variables for summary output

      IF (TERMIN) THEN
      
      TNUPTC1 = TNUPTC 
      TMINOM1 = TMINOM 
      TMINFM1 = TMINFM
      TBIOM1 = TBIOM 
      TNIMM1 = TNIMM 
      TLEACH1 = TLEACH 
      TNRAIN1 = TNRAIN 
      TNGIFT1 = TNGIFT 
      DFMTOT= FMTOTS - FMTOT
      DNTOT= NTOTS - NTOT
      ENDIF

*-------------------------------------------------------------------------
*     STEP1: calculate N-transport by moisture redistribution
*-------------------------------------------------------------------------


*---- nitrogen applications

      IF (IPOT .EQ. 1) IDAYN(1)= IDEMERG + 1

30    IT=IT+1
      IF(IDAY.NE.IDAYN(IT).AND.IT.LT.INUMAP) THEN
         GOTO 30
      ELSE
        IF(IDAY.EQ.IDAYN(IT)) THEN
           NGIFT=ANAP(IT)
        ELSE
           NGIFT=0.
        ENDIF
        IT=0
      ENDIF


*---------------------------------------------------------------------------
*     according to the complete mixing theory (Burns, 1974), all 
*     water and nitrogen entering a soil compartment is completely mixed 
*     with water and nitrogen already present in the compartment, with the 
*     nitrogen resulting from mineralization, and with nitrogen from 
*     fertilizer application. The resulting nitrogen concentration is 
*     subject to crop uptake in the transpiration stream
*---------------------------------------------------------------------------


      NTOTI = NTOT
      NTOT  = 0.0
      NRAIN = FLOWR*NCRAIN
      PLD= THCKN(1) + THCKN(2) + THCKN(3) + 0.5*THCKN(4)    

      DO 60,I=1,ILAYER

         WATERM   = WCACT0(I)*THCKN(I)+FLOW(I)
         IF(I.EQ.1) THEN
            ANLAYM   = ANLAY(I) + NRAIN + NGIFT*THCKN(I)/PLD
         ENDIF
         IF(I.EQ.2 .OR. I.EQ.3) THEN
            ANLAYM   = ANLAY(I) + FLOW(I)*NCONM + NGIFT*THCKN(I)/PLD
         ENDIF
         IF(I.EQ.4) THEN
            ANLAYM   = ANLAY(I) + FLOW(I)*NCONM + NGIFT*0.5*THCKN(I)/PLD
         ENDIF
         IF(I.GT.4) THEN
            ANLAYM   = ANLAY(I) + FLOW(I)*NCONM
         ENDIF
         NCONM    = ANLAYM/WATERM

*------- transport out of compartment I now occurs with the concentration
*        that results from complete mixing

         ANLAY(I) = ANLAYM-FLOW(I+1)*NCONM
         NCON(I)  = ANLAY(I)/(WATERM-FLOW(I+1))
         NTOT     = NTOT + ANLAY(I)
60    CONTINUE


      LEACH = FLOW(11)*NCONM

*---- check soil nitrogen balance: NBAL1 should always be zero!!!

      NBAL = ABS(NTOTI+NGIFT+NRAIN-LEACH-NTOT)


      IF (NBAL.GE.1.E-3) STOP
     $    ' soil nitrogen balance (1) not 0, program aborted'


      GOTO 9999


*-------------------------------------------------------------------------
*     STEP2: subtract nitrogen lost by immobilization and add nitrogen
*     from mineralization 
*-------------------------------------------------------------------------


999   CONTINUE 


*---- add amounts of nitrogen from mineralization


      NTOTI = NTOT
      NTOT  = 0.0


      DO 111,I=1,ILAYER

         ANLAY(I) = ANLAY(I)+MINOM(I)+MINFM(I)-NIMM(I)
         MINOMP   = MINOMP + MINOM(I)
         MINFMP   = MINFMP + MINFM(I)
         NIMMP   = NIMMP + NIMM(I)
         NBIOMP    = NBIOMP  + NBIOM(I)
         NTOT     = NTOT + ANLAY(I)
111   CONTINUE


*---- check soil nitrogen balance: NBAL2 should always be zero!!!


      NBAL = ABS(NTOTI+MINOMP+MINFMP-NIMMP-NTOT)


      IF (NBAL.GE.1.E-3) STOP
     $    ' soil nitrogen balance (2) not 0, program aborted'


      GOTO 9999


*-------------------------------------------------------------------------
*     STEP3: subtract nitrogen uptake by plant 
*-------------------------------------------------------------------------


1111  IF (NDEM.EQ.0) GOTO 9999
      

*---- nitrogen that can be taken up in the transpiration stream is 
*     calculated. It is assumed that water transport caused by 
*     evaporation is in the gas phase, and does not induce upward 
*     nitrogen transport. The nitrogen uptake in the transpiration
*     stream can never exceed the demand of the crop.

      DO 66,I=1,ILAYER
         
         NTRL(I)  = TRR(I)*NCON(I)*THCKN(I)
         PNTR     = PNTR+NTRL(I)

66    CONTINUE


*---- if the nitrogen demand of the crop is smaller then the 
*     amount that can be taken up by transpiration, NTRL(I) is reduced

      IF (NDEM.LT.PNTR) THEN

         DO 65,I=1,ILAYER
            NTRL(I) = NTRL(I)*(NDEM/PNTR)
            NTR     = NTR+NTRL(I)
65       CONTINUE

      ELSE

         NTR = PNTR

      ENDIF


*---------------------------------------------------------------------------
*     nitrogen uptake by nitrogen diffusion towards roots
*---------------------------------------------------------------------------

*---- diffusion is only calculated when roots are present


      IF (RD.GT.0.) THEN


*------- calculate potential diffusion rate:


         IF (DVS.LT.1.8) THEN
            NDF  = AMAX1(0.,(NDEM-NTR))
         ELSE 
            NDF  = 0.
         ENDIF

         

         IF (NDF .GT. 0.) THEN

*---------- nitrogen available for diffusion in compartments that
*           have been penetrated by roots


            DO 70,I=1,IN-1

               IF(WCACT(I).GT.WCWILT(I)) THEN
                  NS = NS+(ANLAY(I)-NTRL(I))/TCND
               ENDIF
70          CONTINUE

             

            IF(WCACT(IN).GT.WCWILT(IN)) THEN
               NS = NS + ((ANLAY(IN)-NTRL(IN))*(RDINL/THCKN(IN))/TCND)
            ENDIF

               
*---------- potential rate of uptake by diffusion is calculated for
*	    each soil compartment that has been penetrated by roots


            DO 80,I=1,IN-1

               IF(WCACT(I).GT.WCWILT(I)) THEN
                  NDFL(I) = NDF*(((ANLAY(I)-NTRL(I))/TCND)/NS)
               ELSE
                  NDFL(I) = 0.0
               ENDIF

80          CONTINUE


            IF(WCACT(IN).GT.WCWILT(IN)) THEN
               NDFL(IN) = NDF*(((ANLAY(IN)-NTRL(IN))*(RDINL/THCKN(IN))
     $                    /TCND)/NS)
            ELSE
               NDFL(IN) = 0.0
            ENDIF
         
     
           
*---------- if the amount of nitrogen available for diffusion is smaller
*           then the potential uptake rate by diffusion, potential rates
*           will be reduced


            IF (NS.LT.NDF) THEN
      
               DO 90, I=1,ILAYER
                  NDFL(I) = (NS/NDF)*NDFL(I)
90             CONTINUE

           
            ENDIF

*---------- total nitrogen uptake by diffusion during one time step

 
            NDF = 0.
       
            DO 95, I= 1,ILAYER
               NDF = NDF + NDFL(I)
95          CONTINUE

         ENDIF

      ENDIF
         
             
*---------------------------------------------------------------------------
*     Amounts of nitrogen in each compartment and nitrogen uptake rate
*     of crop are calculated
*---------------------------------------------------------------------------


      NTOTI = NTOT
      NTOT  = 0.0


      DO 110,I=1,ILAYER

         IF (NDF.GT.0.) THEN
            ANLAY(I)= ANLAY(I)-NTRL(I)-NDFL(I)
         ELSE
            ANLAY(I)= ANLAY(I)-NTRL(I)
         ENDIF

         NTOT    = NTOT+ANLAY(I)

110   CONTINUE


      NUPTR = NTR+NDF

*---- check soil nitrogen balance: NBAL3 should always be zero!!!


      NBAL = ABS(NTOTI-NTR-NDF-NTOT)


      IF (NBAL.GE.1.E-3) STOP
     $    ' soil nitrogen balance (3) not 0, program aborted'


9999  CONTINUE


*---------------------------------------------------------------------------
*     total amount of nitrogen taken up by the crop (TNUPTC), total 
*     mineralization from old organic material (TMINOM), total 
*     mineralization from crop residues (TMINFM), total amount of
*     nitrogen immobilized by biomass (TBIOM) and total amount of
*     soil nitrate lost by immobilization (TNIMM), in kg(N).ha-1
*---------------------------------------------------------------------------


      TNUPTC = TNUPTC + NUPTR
      TMINOM = TMINOM + MINOMP
      TMINFM = TMINFM + MINFMP
      TBIOM  = TBIOM  + NBIOMP
      TNIMM  = TNIMM  + NIMMP
      TLEACH = TLEACH + LEACH
      TNRAIN = TNRAIN + NRAIN
      TNGIFT = TNGIFT + NGIFT
      
      
303   RETURN


      END
