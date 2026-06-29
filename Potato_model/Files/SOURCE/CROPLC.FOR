*---------------------------------------------------------------------------*
*  SUBROUTINE CROPLC                                                        *
*  Author : Joost Wolf, based on routine by Rob Groot (Jan. 1989)           *
*  Date of last revision: January 1997                                      *
*  Purpose: This subroutine simulates the dry matter increase and           *
*	    the nitrogen distribution for a potato crop. This routine       *
*           is equal to CROP but calculates leaf death from life span       *
*           of leaves using daily leaf classes.                             *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  REMARK   output remark                                -                  *
*  RUNNAME  first 5 characters for name of output files         I           *
*  IDAY     Julian daynumber                             -      I           *
*  TMIN     minimum air temperature                      C      I           *
*  RDINL    rooting depth in comp. where roots grow      cm     O           *
*  TMAX     maximum air temperature                      C      I           *
*  DAYL     astronomical daylength                       h      I           *
*  DAYLP    photoperiodically active daylength           h      I           *
*  PARDIR   array, flux density of direct  component of                     *
*	    PAR                                      J m-2 s-1  I           *
*  PARDIF   array, flux density of diffuse component of                     *
*	    PAR                                      J m-2 s-1  I           * 
*  SINB     array, sine of the solar elevation           -      I           *
*  IN       nr.of compartment where root growth occurs   -      O           *
*  ILAYER   number of soil compartments                  -      I           *
*  WCREL    array, relative water content of soil                           *
*	    compartment                               cm cm-3   I           *
*  DEPTH    array, middle of soil compartment measured                      *
*	    from top of soil profile                    cm      I           *
*  RDMSOL   maximum rooting depth (soil limitation)     cm      I           *
*  IDEM     date of crop emergence                       -      I,T         *
*  IDPL     date of planting                             -      I,T         *
*  IDEMERG  date of crop emergence                       -      O,T         *
*  THCKN    array, thickness of soil compartments       cm      I           *
*  TRTOT    total transpiration during one day          cm      I           *
*  TRMAX    max. transpiration rate of crop           cm d-1    I           *
*  KDIF     extinction coefficient for diffuse visible light -  O           *
*  SOILTP   array, temperature of soil compartments     C       I           *
*  NUPTR    nitrogen uptake rate of the crop        kg ha-1 d-1 I           *
*  RD       rooting depth                               cm      O           *
*  LAI      leaf area index of the crop               m2 m-2    O           *
*  DVS      crop development stage                       =      O           *
*  CROPHT   crop height                                  m      O           *
*  NDEM     nitrogen demand of the crop             kg ha-1 d-1 O           *
*  LRV      root density                              cm cm-3   O           *
*  AVRAD    global incoming radiation               J m-2 d-1   I           *
*  CO       atmospheric CO2 concentration              ppmv     I           *
*  TERMIN   logical, simulation is finished when.TRUE.          I,O         *
*  START    logical, start of crop growth                       O           *
*  INITI    logical, indicates start of simulation              I,C         *
*  PL       logical, indicates planting as start of simulation  I,C         *
*  STATR    name of weather station (CHAR*5)            -       I           *
*  ISOIL    number of soil type                         -       I           *
*  ICROP    number of crop species                      -       I           *
*  IORGM    number of soil organic matter data base     -       I           *
*  INSOIL   number of soil nitrogen data base           -       I           *
*  ITUBS    assimil.distrib. fixed(=0) or dep. on tuber gr.(=1) I           *  
*  ILEAFC   1 = with daily leaf classes                         I           *
*  IPOT     1 = potential production (no N shortage)            I           *
*  IRRI     0 = no irrigation; 1 = irrigation            -      I           *
*  IOUT     frequency of output                          d      I           *
*                                                                           *
*  SUBROUTINES called :                                                     *
*  CROPR    reads crop-specific data from file CROP.DAT                     *
*  PHENO1   calculates crop development stage                               *
*  CRPOUT   writes crop status to output file CRPOUT.DAT                    *
*  ASSIM    calculates gross assimilation                                   *
*                                                                           *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE CROPLC (REMARK,RUNNAME,IDAY,TMIN,RDINL,
     $                 TMAX,DAYL,DAYLP,PARDIR,PARDIF,SINB,IN,
     $                 ILAYER,WCREL,DEPTH,RDMSOL,IDEM,IDPL,IDEMERG,
     $                 THCKN,TRTOT,TRMAX,KDIF,
     $                 SOILTP,NUPTR,RD,LAI,DVS,CROPHT,NDEM,LRV,
     $                 AVRAD,CO,TERMIN,START,INITI,PL,STATR,ISOIL,ICROP,
     $                 IORGM,INSOIL,ITUBS,ILEAFC,IPOT,IRRI,IOUT)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,ILAYER,ISOIL,ICROP,IORGM,INSOIL,IDAYL,ITMPN,ITUBN
      INTEGER IFRN,IFLN,ISLAN, ICRPN,ITRDRT,IWRDRT,INMXSN,INMXLN,IOUT
      INTEGER IFSN,IFTN,ITUBS,ILEAFC,IDEM,IDTUBI
      INTEGER INMXRN,INMXTN,IFAMN,IFSLN,IDPL1,IDEMERG1,IDHALT
      INTEGER I,IN,IDEMERG,IDPL,IPOT,IRRI,IDTUBI1
      INTEGER IRLT,IRST,IRRT,ILVOLD,IFRNX
      LOGICAL INIT,TERMIN,START,INITI,TUB,PL
      CHARACTER REMARK*80,RUNNAME*5,STATR*5


      DIMENSION PARDIR(3),PARDIF(3),SINB(3),WCREL(ILAYER)
      DIMENSION DEPTH(ILAYER),THCKN(ILAYER)
      DIMENSION SOILTP(ILAYER),ART(10),LRV(10),DIST(10)

      DIMENSION FRRTB(30),FRLTB(30),FRSTB(30),FRTTB(30),TTUBTB(30)
      DIMENSION TMPFTB(30),SLAT(30)
      DIMENSION TRDRTT(30),NMAXST(30)
      DIMENSION NMAXRT(30), NMAXLT(30),NMAXTU(30)
      DIMENSION WRDRTT(30),FAMAXTB(30), FSLATB(30),CROPHTB(30)
      DIMENSION RDRTB(30),RDLTB(30),RDSTB(30),FRNXTB(8)
      DIMENSION LV(366),SLAC(366),LVAGE(366)

      COMMON/CROPOUT/ IDPL1,IDEMERG1,IDTUBI1, IDHALT, TWRT,TWST,TWLV
      COMMON/CROPOUT/ TBMR1,HI,LAIMAX1,TPARINT1,RUE,NUPTMR1,TWTU


      
*--------------------------------------------------------------------------
*     initialization 
*--------------------------------------------------------------------------
      IF (INITI) INIT = .TRUE.
      IF (INITI) TUB  = .FALSE.      
      IF (INITI) START = .FALSE.

      IF (.NOT.INIT) GOTO 100

*---- read crop specific data from data-file CROP.DAT

      CALL CROPR (ICROP,TDWI,WTUI,RGRL,RGRT,
     $            RDI,TSUMEM,TSUMTI,TSUMCS,TBASE,IDAYL,DAYMAX,DAYMIN,
     $            MAINLV,MAINST,MAINRT,MAINTU,REFTMP,
     $            Q10RM,PMXUPR,RDMCR,LAICR,EFCPR,EFCCH,EFF,KDIF,
     $            FC,TCNT,RNFLV,RNFST,RNFRT,RNFTU,RGM,CHAMAX,
     $            DISTF,TMPFTB,TTUBTB,FRRTB,FRLTB,FRSTB,FRTTB,
     $            RDRTB,RDSTB,RDLTB,SPAN,
     $            SLAT,CROPHTB, TRDRTT,WRDRTT,
     $            NMAXST,NMAXLT,NMAXRT,NMAXTU,FAMAXTB,FSLATB,
     $            ITMPN,ITUBN,IFRN,IFLN,IFSN,IFTN,ISLAN,ICRPN,
     $            ITRDRT,IWRDRT,INMXSN,INMXLN,INMXRN,
     $            INMXTN,IFAMN,IFSLN,IRLT,IRST,IRRT)


      WRT   = 0.
      WLV   = 0.
      WLD   = 0.
      WST   = 0.
      WTU   = 0.
      RD= 0.

      IDEMERG = 999
      AVASS = 0.0
      GPHOT = 0.0
      RMAINT = 0.0
      DVS = 0.0
      DVS1 = 0.0
      DVS2 = 0.0
      WSD = 0.0
      WRD = 0.0
      ANTU = 0.0
      ANLD = 0.0
      ANSD = 0.0
      ANRD = 0.0
      GRST = 0.0
      DRST = 0.0
      GRRT = 0.0
      DRRT = 0.0
      GRLV = 0.0
      DRLV1 = 0.0
      DRLV2 = 0.0
      DRLV  = 0.0
      GRTU = 0.0
      RRD = 0.0
      
      RCTRT = 0.0
      RCTLV = 0.0
      RCTST = 0.0
      RCTTU = 0.0

      RNULV = 0.0
      RNTLV = 0.0
      RNLDLV = 0.0
      RNUST = 0.0
      RNTST = 0.0
      RNLDST = 0.0
      RNURT = 0.0
      RNTRT = 0.0
      RNLDRT = 0.0
      RNATU = 0.0
      NDEM = 0.0
      NUPTR = 0.0

      NFST = 0.0
      TAGB = 0.
      TB = 0.
      TBMR = 0.
      WSTOT = 0.
      SLAC(1) = 0.
      LV(1) = 0.
      LVAGE(1) = 0.
      ILVOLD = 1
      LAI = 0.
      LAIMAX = 0.0
      RDM= AMIN1(RDMSOL, RDMCR)

      NMAXL = 0.
      NMAXS = 0.
      NMAXR = 0.
      NFLV = 0.
      NFST = 0.
      NFRT = 0.

      NFLD = 0.0
      NFTOT = 0.0
      NFSTOT = 0.0
      NFTU = 0.0

      ANLV = 0.
      ANST = 0.
      ANRT = 0.
      NUPTMR = 0.
      NUPTT = 0.

      RED = 1.0
      REDN = 1.0 
      TPARINT = 0.0
      PARABT = 0.0
      TPAR = 0.0
      AVRAD = 0.0
      IN= 0      
*---------------------------------------------------------------------------
*     Dynamic part of the model
*---------------------------------------------------------------------------
     
100   CONTINUE

*---- Subroutine PHENO1 is called to calculate the crop
*     development stage DVS
     
      TMPA = (TMIN+TMAX)/2.

      CALL PHENO1 (IDAY,IDEM,TMPA,TBASE,TSUMEM,TSUMTI,TSUMCS,
     $  IDAYL,DAYMAX,DAYMIN,TSUTE,
     $  DAYLP,DVR1,DVR2,INIT,TUB,PL,IDPL,IDEMERG,IDTUBI)

      INIT = .FALSE.

      
*---- development stages

      DVS1   = AMIN1(1.,INTGRL(DVR1,DVS1))
      DVS2   = INTGRL(DVR2,DVS2)
      DVS    = DVS1 + DVS2


      IF (IDAY .EQ. IDEMERG) THEN
        START= .TRUE.
      
        RD= RDI
        FRT   = AFGEN(IFRN,FRRTB,DVS)
        WRT = FRT * TDWI
        FSH   = 1.-FRT
        FLT   = FSH*AFGEN(IFLN,FRLTB,DVS)
        WLV   = FLT * TDWI
        FST   = FSH*AFGEN(IFSN,FRSTB,DVS)
        WST   = FST * TDWI
        WTU   = WTUI
        TBMR = WLV + WLD + WST + WSD + WTU   
        TB = WLV + WLD + WST + WSD + WTU + WRT + WRD   
        WSTOT = WST + WSD 
        SLAC(1)  = AFGEN(IFSLN,FSLATB,CO) * AFGEN(ISLAN,SLAT,DVS)
     $   * 1.0E-4 
        LAI    = WLV * SLAC(1)
        LV(1) = WLV 

        NMAXL  = AFGEN(INMXLN,NMAXLT,DVS)
        NMAXS  = AFGEN(INMXSN,NMAXST,DVS)
        NMAXR  = AFGEN(INMXRN,NMAXRT,DVS)
        NFLV = NMAXL * 0.8
        NFST = NMAXS * 0.8
        NFRT = NMAXR * 0.8
        NFSTOT= NFST
        ANLV = WLV * NFLV
        ANST = WST * NFST
        ANRT = WRT * NFRT 
        NUPTMR = ANLV + ANST 
        NUPTT = ANLV + ANST + ANTU + ANRT 
        TRMAX= 0.3
        TRTOT= TRMAX
      ENDIF

*---------------------------------------------------------------------------   
*     Integrations
*---------------------------------------------------------------------------   

*---- weights
      
      WST    = INTGRL(GRST-DRST,WST)
      WSD    = INTGRL(DRST-RCTST,WSD)
      WSTOT  = WST + WSD
      WRT    = INTGRL(GRRT-DRRT,WRT)
      WRD    = INTGRL(DRRT-RCTRT,WRD)
      

      WLVS= 0.
      DO 20 I1=1, ILVOLD
      WLVS= WLVS + LV(I1)
20    CONTINUE
      WLV= WLVS

      WTU    = INTGRL(GRTU+RCTTU,WTU)
      WLD    = INTGRL(DRLV-RCTLV,WLD)

      RD     = INTGRL(RRD,RD)


*---- nitrogen uptake rates of plant organs (rates preceding integrals to accelerate 
*      uptake-reaction to nitrogen demand)

      RNULV  = (NDEMLO/(NDEMTO+1.0E-9))*NUPTR
      RNUST  = (NDEMSO/(NDEMTO+1.0E-9))*NUPTR
      RNURT  = (NDEMRO/(NDEMTO+1.0E-9))*NUPTR


*---- amounts of nitrogen

      ANTU   = INTGRL(RNATU,ANTU)
      ANLV   = INTGRL(RNULV-RNTLV-RNLDLV,ANLV)
      
      ANLD   = INTGRL(RNLDLV,ANLD)
      ANST   = INTGRL(RNUST-RNTST-RNLDST,ANST)
     
      ANSD   = INTGRL(RNLDST,ANSD)
      ANRT   = INTGRL(RNURT-RNTRT-RNLDRT,ANRT)
      ANRD   = INTGRL(RNLDRT,ANRD)

*----- total incoming and intercepted PAR

       TPAR = INTGRL(0.5*AVRAD,TPAR)
       TPARINT = INTGRL(PARABT,TPARINT)
      
*---------------------------------------------------------------------------
*     some output parameters to compare model output with observed
*     field data
*---------------------------------------------------------------------------

      TB     = WLV +WLD +WST +WSD +WTU +WRT +WRD
      TBMR   = WLV +WLD +WST +WSD +WTU 
      NUPTT  = ANLV+ANLD+ANST+ANSD+ANTU+ANRT+ANRD
      NUPTMR = ANLV+ANLD+ANST+ANSD+ANTU
      NFTOT  = NUPTMR/(TBMR+1.0E-9)

*---- Nitrogen fractions of the organs

      NFTU   = ANTU/(WTU+1.0E-9)
      NFLV   = ANLV/(WLV+1.0E-9)
      NFST   = ANST/(WST+1.0E-9)
      NFSTOT = (ANST + ANSD)/(WSTOT+1.0E-9)	
      NFRT   = ANRT/(WRT+1.0E-9)
      NFLD   = ANLD/(WLD+1.0E-9)	


*     total green leaf area 

      LAISUM= 0.
      DO 30 I1= 1, ILVOLD
      LAISUM= LAISUM + LV(I1) * SLAC(I1)
30    CONTINUE
      LAI= LAISUM      

      LAIMAX= AMAX1(LAI,LAIMAX)

*----- crop height (m)
       CROPHT= AFGEN(ICRPN,CROPHTB,DVS)


      CALL CRPOUT (IDAY,IDEM,IDPL,IDEMERG,WLV,WLD,WST,
     $     WSTOT,WRT,WTU,LAI,DVS,NFLV,NFLD,NFSTOT,NFTU,RD,NUPTMR,
     $     NUPTT,TBMR,TB,TPARINT,TPAR,REMARK,RUNNAME,RED,REDN,
     $     TERMIN,STATR,ISOIL,ICROP,IORGM,
     $     INSOIL,IRRI,ITUBS,ILEAFC,IPOT,CO,IOUT)	

      
      IF (.NOT. START) GOTO 999
      IF (TERMIN) GOTO 999

*---------------------------------------------------------------------------   
*     canopy photosynthesis 
*---------------------------------------------------------------------------   

*     optimum N concentration in leaves for photosynthesis as fraction of
*     maximum N concentration (determines N demand); N concentrations at 
*     maximum maintenance respiration in various crop organs

      DATA FRNXTB/0.,0.8,1.0,0.8,1.5,0.9,2.0,1.0/
      IFRNX= 8
      FRNX= AFGEN(IFRNX,FRNXTB,DVS)
      NOPTL= FRNX*NMAXL

      NOPTL1  = 0.8*AFGEN(INMXLN,NMAXLT,1.)
      NOPTS1  = 0.8*AFGEN(INMXSN,NMAXST,1.)
      NOPTR1  = 0.8*AFGEN(INMXRN,NMAXRT,1.)
      NOPTT1  = 0.8*AFGEN(INMXTN,NMAXTU,1.)


*---- AMAX depends on the nitrogen content of the leaves,
*     on the average day temperature, crop variety (CHAMAX),
*     and on the atmospheric CO2 concentration. 
      
      FAMAX = AFGEN(IFAMN,FAMAXTB, CO) 
      EAVT  = TMAX-0.25*(TMAX-TMIN)
      REDAMAX= AFGEN(ITMPN,TMPFTB,EAVT)
      LAMAX = AMAX1(0.,1140.*AMIN1(NFLV,NOPTL) - 18.) * 
     $  FAMAX * (1. + CHAMAX) 

*     AMAX at optimum leaf nitrogen content for photosynthesis
      IF (IPOT .EQ. 1) THEN
        LAMAX = AMAX1(0.,1140.*NOPTL - 18.) * FAMAX * (1. + CHAMAX) 
      ENDIF

      PAMAX = AMAX1(0.5,(LAMAX*REDAMAX))

*--- EFF depends on the atmospheric CO2 concentration and crop variety
     
      COREF= 355.
      EFF1= EFF * ((CO - 40.)/(CO + 80.))/((COREF - 40.)/(COREF + 80.))
     $   * (1. + 0.50 * CHAMAX)

      CALL ASSIM (DAYL,PAMAX,EFF1,KDIF,LAI,PARDIR,PARDIF,SINB,
     $            DTGA,PARABT)
             
*---------------------------------------------------------------------------   
*     Rooting depth
*---------------------------------------------------------------------------   

*---- search for compartment where root growth takes place                    
*     plus penetration in deepest compartment

      DO 10,I=1,ILAYER

        IF (RD.GT.(DEPTH(I)-0.5*THCKN(I)).AND.
     $      RD.LE.(DEPTH(I)+0.5*THCKN(I))) IN=I

10    CONTINUE
      

      RDINL = RD-(DEPTH(IN-1)+0.5*THCKN(IN-1))


*---------------------------------------------------------------------------   
*     maintenance respiration
*---------------------------------------------------------------------------   

      
      ACTLV  =AMAX1(0.,AMIN1(1.,(NFLV-RNFLV)/(NOPTL1-RNFLV)))      
      ACTST  =AMAX1(0.,AMIN1(1.,(NFST-RNFST)/(NOPTS1-RNFST)))
      ACTRT  =AMAX1(0.,AMIN1(1.,(NFRT-RNFRT)/(NOPTR1-RNFRT)))
      ACTTU  =AMAX1(0.,AMIN1(1.,(NFTU-RNFTU)/(NOPTT1-RNFTU)))

      TEF    = Q10RM**((TMPA-REFTMP)/10.)

      RMNLV  =WLV*MAINLV*TEF*ACTLV
      RMNST  =WST*MAINST*TEF*ACTST
      RMNRT  =WRT*MAINRT*TEF*ACTRT
      RMNTU  =WTU*MAINTU*TEF*ACTTU
      RMAINT =RMNLV+RMNST+RMNRT+RMNTU

*---------------------------------------------------------------------------   
*     Actual Carbohydrate flow
*---------------------------------------------------------------------------   

*---- Photosynthesis is reduced in case of water stress

      IF (IRRI .NE. 1) THEN
         RED = LIMIT(0.0,1.0,TRTOT/(TRMAX+1.0E-9))
      ELSE
         RED = 1.
      ENDIF

      GPHOT = DTGA*(30./44.)
      AVASS = AMAX1(0.,GPHOT-RMAINT)*RED
      RMAINT= AMIN1(GPHOT,RMAINT)
 

*---------------------------------------------------------------------------   
*     growth section
*---------------------------------------------------------------------------   

*---- Efficiency coefficients for the conversion of primary 
*     photosynthates into structural material

      PRCLV  = 6.25*NFLV
      PRCST  = 6.25*NFST
      PRCRT  = 6.25*NFRT
      PRCTU  = INSW(DVS-1.,0.00,6.25*NFTU)
      EFCGLV = PRCLV*EFCPR + (1.-PRCLV)*EFCCH
      EFCGST = PRCST*EFCPR + (1.-PRCST)*EFCCH
      EFCGRT = PRCRT*EFCPR + (1.-PRCRT)*EFCCH
      EFCGTU = PRCTU*EFCPR + (1.-PRCTU)*EFCCH


*---- Assimilate distribution 

      FRT   = AFGEN(IFRN,FRRTB,DVS)
      FSH   = 1.-FRT
      REDN= LIMIT(0.0, 1.0,(NFLV - RNFLV)/(2.0*RNFLV - RNFLV))
      IF (IPOT .EQ. 1) REDN= 1.

*     distribution above-ground without effect of tuber growth limitation
     
      IF (ITUBS .NE. 1) THEN

         FL   = AFGEN(IFLN,FRLTB,DVS) 
         FS   = AFGEN(IFSN,FRSTB,DVS)

*     more allocation to tubers in case of water stress or nitrogen shortage
   
         REDWN = AMIN1(REDN,RED) 
         FTU  = AMIN1(1., AFGEN(IFTN,FRTTB,DVS) / AMAX1(0.1, REDWN) )

         FLD  = (FL / AMAX1(0.001,FL + FS)) 
         FLT  = FLD * (1. - FTU) * FSH
         FST  = (1. - FLD) * (1. - FTU) * FSH
         FTUT = FTU * FSH   
 
      ELSE

*     with effect of tuber growth limitation
      
         REDTUB= AFGEN(ITUBN,TTUBTB,EAVT)

         IF (TSUTE .GT. TSUMTI) THEN

*     more allocation to tubers in case of nitrogen shortage
    
            FTU = ((WTU * RGRT * REDTUB)/AMAX1(0.1,REDN)) / 
     $            (AVASS * FSH * EFCGTU  + 
     $            (WTU * RGRT *REDTUB)/AMAX1(0.1,REDN))
            FTUT = FTU * FSH

            FL1  = AFGEN(IFLN,FRLTB,1.0)
            FL   = AMAX1(0.,(FL1 - (FL1 * FTU))) 
            FLT   =  FL * (1. - FTU)  * FSH
            FST  = (1. - FL) * (1. - FTU) * FSH

 
         ELSE
            FTUT = 0.
            FTU= 0.
            FLT   = AFGEN(IFLN,FRLTB,AMIN1(1.,DVS)) * FSH
            FST   = AFGEN(IFSN,FRSTB,AMIN1(1.,DVS)) * FSH

         ENDIF
      ENDIF

*----- check assimilate distribution
      FBAL = FRT + FLT + FST + FTUT - 1.0 

*      IF (ABS(FBAL) .GE. 1.E-3) STOP
*     $   ' assimilate distribution balance not 0, program aborted'
 
      IF (ABS(FBAL) .GE. 1.E-3) THEN
         WRITE (*,*) FRT,FLT,FST,FTUT,FTU,FSH,FBAL,REDTUB,EAVT
         WRITE(*,'(A)')
     $   ' assimilate distribution balance not 0, program aborted'
         STOP
       ENDIF
              
         IF(FRT*AVASS.GT.0.) THEN


*---------- Rate of root extension is calculated. Extension only occurs 
*           when the rooting depth is less then the maximum rooting depth 
*           (RDM) which depends on soil and crop. Rate of root 
*           extension depends on soil temperature 

            IF (RD.LT.RDM) THEN
*               WREDRT = AFGEN (IWRDRT,WRDRTT,WCREL(IN))
               TREDRT = AFGEN (ITRDRT,TRDRTT,SOILTP(IN))
               RRD = AMIN1(RGM * TREDRT,RDM - RD)
            ELSE
               RRD = 0.
            END IF

         ELSE
          RRD = 0.
                
         END IF


*------- growth rate of plant organs

         GRLV  = FLT*AVASS*EFCGLV
         GRRT  = FRT*AVASS*EFCGRT
         GRST  = FST*AVASS*EFCGST
         GRTU  = FTUT*AVASS*EFCGTU


*---- check carbon balance in plant

      CBAL = (GPHOT - RMAINT)*RED -GRLV/EFCGLV -GRRT/EFCGRT
     $      -GRST/EFCGST -GRTU/EFCGTU
    
    
      IF (ABS(CBAL) .GE. 1.E-3) STOP
     $   '  carbon balance in plant not 0, program aborted'

*---- Calculate root distribution. ASSUME root density decreases
*     exponentially down the profile. Upper two layers are treated
*     as a single compartment in this calculation
*     This root distribution is only used when detailed nitrogen
*     uptake routine UPTA is used


      ARTT  = 0.
      SUM   = 0.
      DISTUP = EXP(-DISTF*0.01*((THCKN(1)+THCKN(2))/2.))
      SUM    = SUM +DISTUP

      IF (IN.GT.3) THEN

         DO 121, I=3,IN-1
            DIST(I) = EXP(-DISTF*0.01*DEPTH(I))
            SUM     = SUM + DIST(I)
121      CONTINUE

      ENDIF

      DIST(IN) = EXP(-DISTF*0.01*(DEPTH(IN)-0.5*THCKN(IN)
     $           +RDINL*0.5))
      SUM      = SUM + DIST(IN)

      ART(1) = (WRT*DISTUP/SUM)*THCKN(1)/(THCKN(1)+THCKN(2))
      ART(2) = (WRT*DISTUP/SUM)*THCKN(2)/(THCKN(1)+THCKN(2))
      ARTT   = ART(1)+ART(2)

      IF (IN.GT.3) THEN
         DO 131, I=3,IN-1
            ART(I) = WRT*DIST(I)/SUM
            ARTT   = ARTT+ART(I)
131      CONTINUE
      ENDIF

      ART(IN) = WRT*DIST(IN)/SUM
      ARTT    = ARTT+ART(IN)

      IF (ABS(ARTT-WRT).GT.1.0E-3) STOP
     $  ' ERROR IN CALC. OF ROOT DISTIB.'

*---- Assume a Specific Root Length of 200m/g
*     the factor 1.exp-3 is to convert kg/ha to
*     cm/cm2

      DO 141, I=1,IN
         LRV(I) = ART(I) * 200. *1.E-3 / THCKN(I)
141   CONTINUE


*---------------------------------------------------------------------------   
*     nitrogen distribution 
*---------------------------------------------------------------------------   

*---- Maximum nitrogen levels in various organs 

      NMAXL  = AFGEN(INMXLN,NMAXLT,DVS)
      NMAXS  = AFGEN(INMXSN,NMAXST,DVS)
      NMAXR  = AFGEN(INMXRN,NMAXRT,DVS)
      NMAXT  = AFGEN(INMXTN,NMAXTU,DVS)

*---- Calculation of N-demands of the various organs

      NDEML  = AMAX1(NMAXL*WLV - ANLV,0.)
      NDEMS  = AMAX1(NMAXS*WST - ANST,0.)
      NDEMR  = AMAX1(NMAXR*WRT - ANRT,0.)
      NDEMTU = INSW(DVS -1., 0., AMAX1(NMAXT*WTU - ANTU,0.))

      IF (IDAY .EQ. IDEMERG) THEN
         NDEMLO = NDEML
         NDEMSO = NDEMS
         NDEMRO = NDEMR
         NDEMTO = NDEMLO + NDEMSO + NDEMRO 
      ENDIF

*---- Total N-Demand 

      NDEML1  = NMAXL*WLV - ANLV
      NDEMS1  = NMAXS*WST - ANST
      NDEMR1  = NMAXR*WRT - ANRT

      NDEMT = AMAX1(NDEML1 + NDEMS1 + NDEMR1, 0.) 

*---- Amount of translocatable nitrogen of the various organs

      ATNLV  = AMAX1(0.,ANLV - WLV*RNFLV)
      ATNST  = AMAX1(0.,ANST - WST*RNFST)
      ATNRT  = AMAX1(0.,ANRT - WRT*RNFRT)
      ATN    = ATNLV + ATNST + ATNRT 

*---- N-supply for the tubers

      NSUPTU  = INSW(DVS-1.,0.,ATN/TCNT * TEF)

*---- Rate of nitrogen accumulation in tubers

      RNATU  = AMIN1(NDEMTU,NSUPTU)

*---- Rate of nitrogen translocation is calculated for the organs

      RNTRT  = RNATU * ATNRT/(ATN+1.0E-9)
      RNTLV  = RNATU * ATNLV/(ATN+1.0E-9)
      RNTST  = RNATU * ATNST/(ATN+1.0E-9)

 
*---- Death rates of the various organs

*---- death rate of leaves due to water stress, high LAI (mutual shading of leaves),
*     or nitrogen shortage

      DSLV1= (1. - RED) * WLV * 0.03
      DSLV2= LIMIT(0.0, 1.0, (LAI - LAICR)/ LAICR ) * 0.03 * WLV
      DSLV3= (1. - REDN) * WLV * 0.03
      DSLV= AMAX1(DSLV1,DSLV2,DSLV3)     

*     leaf death by stress or high LAI imposed on leaf array
      DELT= 1.
      REST= DSLV*DELT
      I1= ILVOLD

200   IF (REST .GT. LV(I1) .AND. I1 .GE. 1) THEN
        REST= REST - LV(I1)
        I1= I1 -1
        GOTO 200
      ENDIF


*---- death due to ageing of remaining leaves (weights are summed) 
*     i.e. if life span of leaves (SPAN) is exceeded
*     (leaf death imposed on array)

      DALV= 0.
      IF (LVAGE(I1) .GT. SPAN .AND. REST .GT. 0. .AND. I1 .GE. 1) THEN
        DALV= LV(I1) - REST
        REST= 0.
        I1= I1 -1
      ENDIF

210   IF (I1 .GE. 1 .AND. LVAGE(I1) .GT. SPAN) THEN
        DALV= DALV + LV(I1)
        I1= I1 - 1
        GOTO 210
      ENDIF

      DALV= DALV/DELT

*     total death rate of leaves

      DRLV= DSLV + DALV

*     physiological ageing of leaves per time step
  
      FYSDEL= AMAX1(0., (TMPA - TBASE)/(35. - TBASE))

*     Leaf growth and leaf area

*     SLA depending on DVS, CO2 conc. and N shortage
      SLA    = AFGEN (ISLAN,SLAT,DVS)* 1.0E-4 * AFGEN(IFSLN,FSLATB,CO) 
     $   * (1. - 0.2 * (1. -REDN))
      GLAI= GRLV * SLA

*     growth not to exceed exponential growth curve during juvenile stage
*     (i.e. sink or source limited leaf growth)

      IF ((DVS .LT. 1.2) .AND. LAI .LT. 0.75) THEN      
      DTEFF= AMAX1(0.,TMPA - TBASE)
      DELT= 1.
      GLAI = AMIN1((LAI * (EXP (RGRL*DTEFF*DELT)-1.))/DELT,
     $        GLAI)
      ENDIF

*     adjustement of specific leaf area of youngest leaf class

      IF (GRLV .GT. 0.) SLA= GLAI/GRLV


*    leaf death is imposed on leaf array untill no more leaves have to die or all leaves are dead
 
      DSLVT= DSLV * DELT
      I1= ILVOLD
220   IF (DSLVT .GT. 0. .AND. I1 .GE. 1) THEN
        IF (DSLVT .GE. LV(I1)) THEN
          DSLVT= DSLVT - LV(I1)
          LV(I1)= 0.
          I1= I1 - 1
        ELSE
          LV(I1)= LV(I1) - DSLVT
          DSLVT= 0.
        ENDIF
      GOTO 220
      ENDIF

230   IF (LVAGE(I1) .GE. SPAN .AND. I1 .GE. 1) THEN
        LV(I1)= 0.
        I1= I1 - 1
      GOTO 230
      ENDIF

      ILVOLD= I1

*     shifting of array contents, integration of physiological age

      DO 240 I1= ILVOLD, 1, -1
        LV(I1+1)= LV(I1)
        SLAC(I1+1)= SLAC(I1)
        IF (DVS .GE. 1.2 .OR. LAI .GE. 0.75) SLAC(I1+1)= SLA
        LVAGE(I1+1)= LVAGE(I1) + FYSDEL*DELT
240   CONTINUE
      ILVOLD= ILVOLD + 1
        
*     new leaves in array class 1

      LV(1)= GRLV * DELT
      SLAC(1)= SLA
      LVAGE(1)= 0.

*--- death rates of roots and stems due to water stress, nitrogen shortage or ageing

      IF (DVS .GT. 1.0) THEN
        DRRT1= (1. - RED) * WRT * 0.03
        DRRT3= (1. - REDN) * WRT * 0.03
      ELSE
        DRRT1= 0.0
        DRRT3= 0.0      
      ENDIF

      RDR= AFGEN(IRRT,RDRTB,DVS)
      DRRT2 = RDR * WRT

      DRRT= AMAX1(DRRT1,DRRT2,DRRT3)
 
      IF (DVS .GT. 1.0) THEN
        DRST1= (1. - RED) * WST * 0.03
        DRST3= (1. - REDN) * WST * 0.03
      ELSE
        DRST1= 0.0
        DRST3= 0.0      
      ENDIF

      RDS= AFGEN(IRST,RDSTB,DVS)
      DRST2 = RDS * WST

      DRST= AMAX1(DRST1,DRST2,DRST3)

*---- rate of carbon translocation to tubers from the various 
*       organs (set at half of death rates)

      RCTRT = INSW(DVS - 1., 0., 0.5 * DRRT)
      RCTLV = INSW(DVS - 1., 0., 0.5 * DRLV)
      RCTST = INSW(DVS - 1., 0., 0.5 * DRST)
      RCTTU = RCTRT + RCTLV + RCTST

*---- nitrogen losses caused by death of vegetative material
*     in case of nitrogen demand
*     (half of the carbon amount assumed to be translocated)

      IF (NDEMT .GT. 0.) THEN
        RNLDLV = RNFLV*(DRLV-RCTLV)
        RNLDST = RNFST*(DRST-RCTST)
        RNLDRT = RNFRT*(DRRT-RCTRT)
      ELSE
*     in case of nitrogen in excess --> all nitrogen lost in dead 
*     plant mat. to prevent increasing N-concentration in living plant organs
        RNLDLV = NFLV*DRLV
        RNLDST = NFST*DRST
        RNLDRT = NFRT*DRRT
      ENDIF

*---- maximum nitrogen uptake rate by the crop as limited by rooting system
*     and actual demand
     

      MXUR   = PMXUPR * (1.-EXP(-0.5*(WLV+WST)/FC))
      NDEM   = AMIN1(NDEMT,MXUR)


*----- store old values for N demand of the various organs

      NDEMLO = NDEML
      NDEMSO = NDEMS
      NDEMRO = NDEMR
      NDEMTUO = NDEMTU
      NDEMTO = NDEMLO + NDEMSO + NDEMRO  

*---------------------------------------------------------------------------
*     output of simulation results 
*---------------------------------------------------------------------------

      


*---- finish condition of simulation                                    

      IF (DVS .GT. 2. .OR. (DVS .GT. 1.3 .AND. LAI .LE. 0.1) ) THEN

         TERMIN =.TRUE.

       
         WRITE (*,'(//2A)') ' Crop development completed '

      ENDIF

      IF (ILVOLD .GT. 365) THEN
        WRITE(*,'(A)') ' number of leaf classes exceeds 365 !'
        TERMIN= .TRUE.
      ENDIF


999   CONTINUE

      IF (TERMIN) THEN
      TWRT= WRT + WRD
      TWST= WST + WSD
      TWLV= WLV + WLD
      TWTU= WTU
      TBMR1= TBMR
      HI= WTU/TBMR
      IDHALT= IDAY
      IDTUBI1= IDTUBI
      IDPL1= IDPL
      IDEMERG1= IDEMERG
      LAIMAX1= LAIMAX
      NUPTMR1= NUPTMR
*-----Radiation absorbed in MJ m-2
      TPARINT1= TPARINT/10.**6
*------radiation use efficiency in g above-ground D.M./radiation abs. in MJ
      RUE= (TBMR*10.**5)/TPARINT
      ENDIF

      RETURN                                                   
      END






*-----------------------------------------------------------------------------------*
*  SUBROUTINE PHENO1                                                                 *
*  Author : Joost Wolf                                                              *
*           based on earlier version written by Rob Groot (nomember 1985)           *
*  Date   : July 1996                                                               *
*  Purpose: This subroutine calculates the development rate of the                  *
*	    crop based on average daily temperatures and daylength.                 *
*           References to literature :                                              *
*           P.L. Kooman, 1995. Yielding ability of potato crops as influenced       *
*             by temperature and daylength. Ph.D.-thesis.                           *
*           C.J.T. Spitters, 1990. Crop growth models: their usefulness and         *
*             limitations. Acta Horticulturae 267: 349-368                          *
*           C.J.T. Spitters, 1987. An analysis of variation in yield among          *
*           potato cultivars in terms of light absorption, light utilization        *
*             and dry matter partitioning. Acta Horticulturae 214: 71-84.           * 
*                                                                                   *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)                  *
*  name     meaning                                                   units  class  *
*  ----     -------                                                   -----  -----  *
*  IDAY     Julian day number                                           -       I,T *
*  IDEM     date of crop emergence                                      -       I,T *
*  TMPA     average daily air temperature                               C       I   *
*  TBASE    base temperature for temperature sums                       C       I   *
*  TSUMEM   required degree days for emergence                          C.day   I   *
*  TSUMTI   required TSUM from emergence till tuber initiat.            C.day   I   *
*  TSUMCS   required TSUM from tuber initiation until crop senescence   C.day   I   *
*  IDAYL    indicates day length effect (=1) or not (=0)                 -      I   *
*  DAYMAX   day length for maximum long day effect                       h      I   *
*  DAYMIN   day length with no long day effect                           h      I   *
*  TSUTE    temperature sum after emergence for tuber development       C.day   O   *
*  DAYLP    photosynthetically active daylength                          h      I   *
*  DVR1     pre-tuber initiation development rate                       day-1   O   *
*  DVR2     post-tuber initiation development rate                      day-1   O   *
*  INIT     logical, indicates initialization of simulation              -      C   *
*  TUB      logical, indicates tuber filling                             -      C   *
*  PL       logical, indicates planting as start of simulation           -      C   *
*  IDPL     date of planting                                             -      I   *
*  IDEMERG  date of crop emergence                                       -      O   *
*  IDTUBI   start date of tuber filling                                  -      O   *
*                                                                                   *
*  SUBROUTINES and FUNCTIONS called : none                                          *
*  FILE usage : none                                                                *
*-----------------------------------------------------------------------------------*

      SUBROUTINE PHENO1 (IDAY,IDEM,TMPA,TBASE,TSUMEM,TSUMTI,TSUMCS,
     $  IDAYL,DAYMAX,DAYMIN,TSUTE,
     $  DAYLP,DVR1,DVR2,INIT,TUB,PL,IDPL,IDEMERG,IDTUBI)

      IMPLICIT REAL (A-Z)
      INTEGER IDAYL,IDAY,IDTUBI,IDEMERG,IDPL,IDEM
      LOGICAL INIT,TUB,PL,EMERG
    

      IF (INIT)  THEN
        EMERG= .FALSE.
        TUB= .FALSE.
        TSULP= 0.
        TSULE= 0.
        TSUTE= 0.
        DTSULP= 0.
        DTSULE= 0.
        DTSUTE= 0.
      ENDIF

*------------------------------------------------------------------
*      INTEGRATION
*-------------------------------------------------------------------

*------ temperature sums after planting (P) and emergence (E) for development 
*       of foliage (TSUL*) and tubers (TSUT*)


      TSULP = INTGRL (DTSULP, TSULP)
      TSULE = INTGRL (DTSULE, TSULE)
      TSUTE = INTGRL (DTSUTE, TSUTE)

*----- Development rate before (=1) and after tuber initiation (=2)
 
      DVR1= INSW(TSUTE - TSUMTI, DTSUTE/TSUMTI,0.) 
      DVR2= INSW(TSUTE - TSUMTI, 0., DTSULE/TSUMCS)

*----- at tuber initiation
      IF (.NOT. TUB .AND. TSUTE .GE. TSUMTI) THEN
        DVR1= DTSUTE/TSUMTI
        DVR2= 0.
      ENDIF

*----------------------------------------------------------------------------------
*     RATE CALCULATIONS
*----------------------------------------------------------------------------------    

      DAY= REAL(IDAY)
      DAYPL= REAL(IDPL)

*---- date of planting 
      IF (PL) THEN
        PUSHPL= INSW(DAY - DAYPL, 0., 1.)
      ELSE
        PUSHPL= 0.
      ENDIF

      IF (PL .AND. (TSULP .GE. TSUMEM) .AND. (.NOT. EMERG) ) THEN
        IDEMERG= IDAY
        EMERG= .TRUE.
      ENDIF

*---- start date of tuber filling
      IF (.NOT. TUB .AND. TSUTE .GE. TSUMTI) THEN
         IDTUBI= IDAY
         TUB= .TRUE.
      ENDIF

*---- Reduction of development rate until tuber initiation by long day length
       FRDAY= (DAYLP - DAYMIN) / (DAYMAX - DAYMIN)

       IF (IDAYL .EQ. 1 .AND. (TSUTE - TSUMTI) .LT. 0.) THEN
          RDAYL= 1. - 0.5 * LIMIT(0.,1., FRDAY)
       ELSE
          RDAYL= 1.
       ENDIF

       IF (.NOT. PL) IDEMERG= IDEM


*----- Temperature sums after planting for development of foliage  (TSUL*) 
*      and tubers (TSUT*)    

       DAYEM= REAL(IDEMERG)

       PUSHEM= INSW (DAY - DAYEM, 0., 1.)
       DTSULL= LIMIT (0., 18., TMPA - TBASE)      
       DTSUL= MAX(0., TMPA - TBASE)
       TEFF= INSW(TMPA - 13., TMPA - TBASE, 29. - TMPA)
       DTSUT= LIMIT(0., 11., TEFF)

       DTSULP = DTSULL * PUSHPL
       DTSULE = DTSUL * PUSHEM * RDAYL
       DTSUTE = DTSUT * PUSHEM * RDAYL


      RETURN
      END
