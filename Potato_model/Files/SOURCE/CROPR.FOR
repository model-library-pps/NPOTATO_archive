      SUBROUTINE CROPR (ICROP,TDWI,WTUI,RGRL,RGRT,
     $           RD,TSUMEM,TSUMTI,TSUMCS,TBASE,IDAYL,DAYMAX,DAYMIN,
     $           MAINLV,MAINST,MAINRT,MAINTU,REFTMP,
     $           Q10RM,PMXUPR,RDMCR,LAICR,EFCPR,EFCCH,EFF,KDIF,
     $           FC,TCNT,RNFLV,RNFST,RNFRT,RNFTU,RGM,CHAMAX,
     $           DISTF,TMPFTB,TTUBTB,FRRTB,FRLTB,FRSTB,FRTTB,
     $           RDRTB,RDSTB,RDLTB,SPAN,
     $           SLAT,CROPHTB,TRDRTT,WRDRTT,
     $           NMAXST,NMAXLT,NMAXRT,NMAXTU,FAMAXTB,FSLATB,
     $           ITMPN,ITUBN,IFRN,IFLN,IFSN,IFTN,ISLAN,ICRPN,
     $           ITRDRT,IWRDRT,
     $           INMXSN,INMXLN,INMXRN,INMXTN, IFAMN,IFSLN,
     $           IRLT,IRST,IRRT)


      IMPLICIT REAL (A-Z)
      INTEGER ICROP,ITMPN,ITUBN,IFRN,IFLN,ISLAN,ICRPN,ITRDRT,IWRDRT
      INTEGER INMXSN,INMXLN,INMXRN,INMXTN,IFAMN,IFSLN,IFSN,IFTN
      INTEGER IRLT,IRST,IRRT

*------------------------ afgen array declarations -------------------------


      DIMENSION FRRTB(30),FRLTB(30),FRSTB(30),FRTTB(30)
      DIMENSION TMPFTB(30),TTUBTB(30),SLAT(30)
      DIMENSION TRDRTT(30),NMAXST(30),NMAXTU(30)
      DIMENSION NMAXRT(30),NMAXLT(30)
      DIMENSION WRDRTT(30),FAMAXTB(30), FSLATB(30),CROPHTB(30)
      DIMENSION RDRTB(30),RDSTB(30),RDLTB(30)

*---- open data file with crop data

      IF (ICROP .EQ. 1) CALL RDINIT (14,0,'CROPN1.DAT')
      IF (ICROP .EQ. 2) CALL RDINIT (14,0,'CROPN2.DAT')
      IF (ICROP .EQ. 3) CALL RDINIT (14,0,'CROPN3.DAT')
      IF (ICROP .EQ. 4) CALL RDINIT (14,0,'CROPN4.DAT')
      IF (ICROP .EQ. 5) CALL RDINIT (14,0,'CROPN5.DAT')
      IF (ICROP .EQ. 6) CALL RDINIT (14,0,'CROPN6.DAT')


*------ Reading crop data from file CROPN.DAT ------------------------------

*-------------------------- initial conditions -----------------------------

      CALL RDSREA ('TDWI',TDWI)
      CALL RDSREA ('RD',RD)
      CALL RDSREA ('WTUI',WTUI)

*------------------------- parameters --------------------------------------

*     maximum relative increase in LAI and tuber weight
 
      CALL RDSREA ('RGRL',RGRL)
      CALL RDSREA ('RGRT',RGRT)    

*     temperature sums for emergence, from emergence to tuber initiation and flowering, 
*     and from anthesis until end of crop growth, and the base temperature

      CALL RDSREA ('TSUMEM',TSUMEM)
      CALL RDSREA ('TSUMTI',TSUMTI)
      CALL RDSREA ('TSUMCS',TSUMCS)
      CALL RDSREA ('TBASE',TBASE)
*     maximum and minimum day length for photoperiod effects on phenology 
*     (until tuber initiation)

      CALL RDSINT ('IDAYL',IDAYL)
      CALL RDSREA ('DAYMAX',DAYMAX)
      CALL RDSREA ('DAYMIN',DAYMIN)

*     maintenance coefficients

      CALL RDSREA ('MAINLV', MAINLV)
      CALL RDSREA ('MAINST',MAINST)
      CALL RDSREA ('MAINRT',MAINRT)
      CALL RDSREA ('MAINTU',MAINTU)

*     maintenance reference temperature, and Q10

      CALL RDSREA ('REFTMP',REFTMP)
      CALL RDSREA ('Q10RM',Q10RM) 

*     potential maximum nitrogen uptake rate (kg(N)/ha/d), 
*     maximum effective rooting depth (cm) and critical leaf area index (m2 m-2) 

      CALL RDSREA ('PMXUPR',PMXUPR)
      CALL RDSREA ('RDMCR', RDMCR)
      CALL RDSREA ('LAICR', LAICR)

*     Efficiency coefficients for the conversion of assimilates into
*     proteins and into structural carbohydrates. 

      CALL RDSREA ('EFCPR',EFCPR)
      CALL RDSREA ('EFCCH',EFCCH)  

*     photosynthesis parameters

      CALL RDSREA ('EFF',EFF) 
      CALL RDSREA ('KDIF',KDIF) 


*     conversion factor from weight to relative root extension

      CALL RDSREA ('FC',FC)

*     time constant for nitrogen translocation

      CALL RDSREA ('TCNT',TCNT) 

*     Residual nitrogen fractions

      CALL RDSREA ('RNFLV',RNFLV)
      CALL RDSREA ('RNFST',RNFST) 
      CALL RDSREA ('RNFRT',RNFRT) 
      CALL RDSREA ('RNFTU',RNFTU) 

*     Potential root length growth rate

      CALL RDSREA ('RGM',RGM)

*     Variety specific change in maximum assimilation rate and initial light-use efficiency

      CALL RDSREA ('CHAMAX', CHAMAX)

*     shape factor for exponential root distibution function

      CALL RDSREA ('DISTF',DISTF) 

*      life span of leaves at 35 C (d)

       CALL RDSREA ('SPAN', SPAN)


*------------------------- afgen function values ---------------------------

*     Temperature response of maximum assimilation rate of a leaf  

      CALL RDAREA ('TMPFTB',TMPFTB,30,ITMPN)

*     Temperature response of tuber growth rate  

      CALL RDAREA ('TTUBTB',TTUBTB,30,ITUBN)

*     fraction of total growth allocated to roots 

      CALL RDAREA ('FRRTB',FRRTB,30,IFRN)

*     fraction of shoot growth allocated to leaves

      CALL RDAREA ('FRLTB',FRLTB,30,IFLN)

*     fraction of shoot growth allocated to stems

      CALL RDAREA ('FRSTB',FRSTB,30,IFSN)

*     fraction of shoot growth allocated to tubers

      CALL RDAREA ('FRTTB',FRTTB,30,IFTN)

*     specific leaf area as function of development stage          

      CALL RDAREA ('SLAT',SLAT,30,ISLAN)

*    crop height as a function of development stage

      CALL RDAREA ('CROPHTB',CROPHTB,30,ICRPN)

*     temperature reduction factor for root extension.

      CALL RDAREA ('TRDRTT',TRDRTT,30,ITRDRT)

*     moisture reduction factor for root extension 

      CALL RDAREA ('WRDRTT',WRDRTT,30,IWRDRT)

*     death rates in roots, leaves and stems
      
      CALL RDAREA ('RDRTB', RDRTB, 30, IRRT)
      CALL RDAREA ('RDLTB', RDLTB, 30, IRLT)
      CALL RDAREA ('RDSTB', RDSTB, 30, IRST)

*     Maximum nitrogen concentrations in leaves, stems and roots as function of
*     development stage

      CALL RDAREA ('NMAXST',NMAXST,30,INMXSN)
      CALL RDAREA ('NMAXLT',NMAXLT,30,INMXLN)
      CALL RDAREA ('NMAXRT',NMAXRT,30,INMXRN)
      CALL RDAREA ('NMAXTU',NMAXTU,30,INMXTN)


*     correction factors dependent on atmospheric CO2 concentration
      
      CALL RDAREA ('FAMAXTB',FAMAXTB,30,IFAMN)

      CALL RDAREA ('FSLATB',FSLATB,30,IFSLN)
          
      CLOSE (14, STATUS= 'DELETE')

      RETURN
      END
