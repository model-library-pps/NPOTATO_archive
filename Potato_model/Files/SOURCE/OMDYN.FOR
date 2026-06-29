*--------------------------------------------------------------------------*
*  SUBROUTINE OMDYN                                                        *
*  Author : Rob Groot                                                      *
*  Date of last revision: 19-01-89                                         *
*  Purpose: This subroutine simulates soil organic matter dynamics         *
*           Old organic matter (humus) and young organic matter            *
*           are distinguised. Decomposition of both old and young          *
*           organic matter are discribed as a first order process.         *
*           The specific rates of decomposition are corrected for          *
*           the effects of soil moisture content and soil temperature      *
*           according to a response-surface proposed by Verbruggen         *
*           (1985). Decomposition of organic matter results in             *
*           mineralization or immobilization of nitrogen, depending        *
*           on the C/N-ratio of the subtrate                               *
*                                                                          *
*           References:                                                    *
*           Johsson,Bergstrom,Jansson&Paustiaan,1988.Simulated             *
*           nitrogen dynamics and losses in a layered agricultural         *
*           soil. Ecosystems and Environment, 1988.                        *
*           Verbruggen,1981. Simulatie van het denitrificatie-             *
*           proces in de bodem. Doctoraatproefschrift nr.140, KU,          *
*           Leuven.                                                        *
*                                                                          *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)         *
*  name    meaning                                     units       class   *
*  ----    -------                                     -----       -----   *
*  ILAYER  number of soil compartments                   -          I      *
*  WCACT   water content of soil compartments         cm cm-1       I      *
*  ANLAY   amount of nitrogen is soil compartments    kg ha-1       I      *
*  THCKN   thickness of soil compartments               cm          I      *
*  WCFLDC  water content at field capacity            cm cm-1       I      *
*  RHOD    bulk density of soil compartments           g cm-3       I      *
*  SOILTP  temperature of soil compartments              C          I      *
*  MINFM   N from mineralization of crop residues    kg N ha-1 d-1  O      *
*  NIMM    loss of soil mineral N by immobilization  kg N ha-1 d-1  O      *
*  MINOM   N from mineralization of old org. mat.    kg N ha-1 d-1  O      *
*  NBIOM   N immobilized in biomass                  kg N ha-1 d-1  O      *
*                                                                          *
*  SUBROUTINES and FUNCTIONS called :                                      *
*  OMDAT   reads soil specific data on organic matter                      *
*          from file OMSOILN.DAT                                           *
*  FILE usage : none                                                       *
*--------------------------------------------------------------------------*


      SUBROUTINE OMDYN (IORGM,ILAYER,WCACT,ANLAY,THCKN,WCFLDC,RHOD,
     $                  SOILTP,MINFM,NIMM,MINOM,NBIOM,FMTOT,INITI)

      IMPLICIT REAL (A-Z)
      INTEGER ILAYER,IORGM,I
      LOGICAL INIT,INITI
      CHARACTER REMARK*80,RUNNAME*5

      DIMENSION WCACT(10) ,ANLAY(10) ,THCKN(10), WCMAX(10),SOILTM(10)
      DIMENSION OMLAY(10) ,MINOM(10) ,SOILTP(10)
      DIMENSION TRDMIN(10),FMLAY(10) ,NBIOM(10)  ,MINFM(10), FMN(10) 
      DIMENSION NIMM(10) ,RHOD(10)  ,MR0(10)   ,WCFLDC(10)
      
      IF (INITI) INIT = .TRUE.
      
      IF (.NOT.INIT) GOTO 100


      INIT = .FALSE.


*--------------------------------------------------------------------------
*     initialization 
*--------------------------------------------------------------------------
      I = 0
      DEPT = 0.

*---- Read soil organic matter data 

      CALL OMDAT (IORGM,PLAYER,PERCOM,RDROM,REFTMP,
     $           FM,RDRFM,CCFM,NCFM,DISASS,NCRB)


*---- Calculate initial amounts of old organic matter  and crop
*     residues for the soil compartments

      OMCM = PERCOM * 1.E3
      FMCM = FM/PLAYER

10    I = I+1
      DEPT = DEPT + THCKN(I)

      IF (PLAYER.GT.DEPT) THEN

*---- OMLAY in kg C ha-1 and FMLAY in kg DM ha-1 per compartment

         OMLAY(I) = THCKN(I)*OMCM*RHOD(I)
         FMLAY(I) = THCKN(I)*FMCM
         GOTO 10
      ELSE
         OMLAY(I) = (PLAYER-(DEPT-THCKN(I)))*OMCM*RHOD(I)
         FMLAY(I) = (PLAYER-(DEPT-THCKN(I)))*FMCM
      ENDIF

               
*---- calculate reference mineralization rate according to Verbruggen
*     (1985). Mineralization is assumed to be optimum at field capacity

      DO 15,I=1,ILAYER
         A0     = 0.22*(REFTMP+0.00064)**1.4425
         B0     = 0.1737*EXP(-0.119*REFTMP)+0.1107
         MR0(I) = A0*(1.-EXP(-B0*WCFLDC(I)/RHOD(I)))
15    CONTINUE


100   CONTINUE


*---------------------------------------------------------------------------
*     dynamic section
*---------------------------------------------------------------------------


*---- mineralization is a function of soil temperature and soil 
*     humidity according to Verbruggen (1985). The temperature and soil 
*     moisture reduction factor for mineralization (TRDMIN(I)) is 
*     calculated for each soil compartment. For soil temperature 30C and 
*     for soil moisture content that at field capacity are used as maximum 
*     values for calculating TRDMIN.

      DO 40,I=1,ILAYER

         IF (SOILTP(I).LE.0.) THEN
            TRDMIN(I)=0.
         ELSE
            WCMAX(I)= AMIN1 (WCFLDC(I),WCACT(I))
            SOILTM(I)= AMIN1 (SOILTP(I),30.)
            A         = 0.22*((SOILTM(I)+0.00064)**1.4425)
            B         = 0.1737*EXP(-0.119*SOILTM(I))+0.1107
            MR        = A*(1.-EXP(-B*WCMAX(I)/RHOD(I)))
            TRDMIN(I) = MR/MR0(I)
         
         ENDIF

40    CONTINUE


*---------------------------------------------------------------------------
*     nitrogen from mineralization of crop residues (MINFM(I))
*     immobilization of nitrogen in biomass (NBIOM(I))
*     loss of soil mineral nitrogen by immobilization (NIMM(I))
*---------------------------------------------------------------------------

      DO 200,I=1,ILAYER

         IF (SOILTP(I).LE.0.) THEN

            NBIOM(I)   = 0.
            NIMM(I)  = 0.
            MINFM(I)  = 0.

         ELSE

*---------- calculate decomposition rate of fresh organic material and 
*           potential mineralization rates for fresh organic material
*           (kg DM ha-1 d-1)

            PDRFM = RDRFM * TRDMIN(I) * FMLAY(I)
            PMINN = PDRFM * NCFM

*---------- dissimilation:assimilation ratio for growth of microbial biomass
*           (DISASS) and N:C ratio of biomass (NCRB) have been read from 
*           data-file NSOIL.DAT

            NDEMB = (PDRFM*CCFM/(DISASS+1.))*NCRB
  
            IF (NDEMB.LE.PMINN) THEN

*------------- nitrogen demand of biomass (NDEMB) is supplied by nitrogen from
*              decay of fresh organic matter (kg N ha- d-1)

               NBIOM(I)  = NDEMB
               MINFM(I) = PMINN-NDEMB
               NIMM(I) = 0.
               FMLAY(I) = AMAX1(0.,FMLAY(I)-PDRFM)

            ELSE

               NDEMBS = AMAX1(0.,NDEMB-PMINN)

               IF (ANLAY(I).GE.NDEMBS) THEN

*---------------- if crop residues do not cover nitrogen demand of biomass,
*                 soil mineral nitrogen is partly immobilized (kg N ha-1 d-1)

                  NBIOM(I)  = NDEMB
                  MINFM(I) = 0.
                  NIMM(I) = NDEMBS
                  FMLAY(I) = AMAX1(0.,FMLAY(I)-PDRFM)

               ELSE

*---------------- soil mineral nitrogen does not cover the demand.
*                 mineralization is reduced

                  NBIOM(I)  = ANLAY(I) + PMINN
                  MINFM(I) = 0.
                  NIMM(I) = ANLAY(I)
                  FMLAY(I) = AMAX1(0.,(FMLAY(I)-(ANLAY(I)/NDEMBS)*
     $                       PDRFM))

               ENDIF

            ENDIF

         ENDIF	

200   CONTINUE


*----- nitrogen in fresh organic material (kg N ha-1)
        FMTOT = 0.
        DO 45, I= 1, ILAYER
        FMN(I)= FMLAY(I) * NCFM
        FMTOT = FMTOT + FMN(I)
45      CONTINUE
       
*---------------------------------------------------------------------------
*     mineralization of old organic material (humus)
*---------------------------------------------------------------------------


*---- A C/N ratio of 10 is assumed. The total OM pool does not decrease!!
*     The rel. min. rate is from Johnsson (1987)

      DO 50,I=1,ILAYER

         IF (SOILTP(I).LE.0.) THEN
            MINOM(I) = 0.
         ELSE
            CNOM = 0.1
            MINOM(I) = OMLAY(I)*TRDMIN(I)*RDROM*CNOM
         ENDIF

         MINOMP = MINOMP + MINOM(I)
      
50    CONTINUE


      RETURN
      END
