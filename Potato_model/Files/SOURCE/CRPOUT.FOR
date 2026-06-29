*---------------------------------------------------------------------------*
*  SUBROUTINE CRPOUT                                                        *
*  Author : Joost Wolf, based on routine by Rob Groot (Jan. 1987)           *
*  Date   : June 1996                                                       *
*  Purpose: This subroutine writes crop status to the                       *
*	    output file CRPOUT.DAT                                          *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  IDAY     julian daynumber                             -         I,T      *
*  IDEM     date of plant emergence  (model input)       -         I,T      *
*  IDPL     date of planting                             -         I,T      *
*  IDEMERG  date of plant emergence  (calculated)        -         I,T      *                  
*  WLV      weight of leaves                           kg/ha       I        *
*  WLD      weight of dead leaves                      kg/ha       I        *
*  WST      weight of stems                            kg/ha       I        *
*  WSTOT    total stem weight (WST+WSD)                kg/ha       I        *
*  WRT      weight of roots                            kg/ha       I        *
*  WTU      weight of tubers                           kg/ha       I        *
*  LAI      leaf area index                            m2/m2       I        *
*  DVS      crop development stage                       -         I        *
*  NFLV     nitrogen fraction of leaves                  -         I        *
*  NFLD     nitrogen fraction of dead leaves             -         I        *
*  NFSTOT   nitrogen fraction of total stem weight       -         I        * 
*  NFTU     nitrogen fraction of tubers                  -         I        *
*  RD       rooting depth                                cm        I        *
*  NUPTMR   nitrogen uptake in total crop minus roots  kg/ha       I        *
*  NUPTT    nitrogen uptake in total crop              kg/ha       I        *
*  TBMR     total crop dry matter minus roots          kg/ha       I        *
*  TB       total crop dry matter                      kg/ha       I        *
*  TPARINT  total intercepted PAR                      MJ/m2       I        *
*  TPAR     total incoming PAR                         MJ/m2       I        *
*  REMARK   output remarks                               -         I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*  RED      reduction factor for photosynthesis          -         I        *
*	    as a result of water stress                  -         I        *
*  REDN     idem for nitrogen shortage                   -         I        *
*  TERMIN   logical, simulation is finished when .TRUE.  -         I,C      *         
*  STATR    name of weather station (CHAR*5)             -         I        *
*  ISOIL    number of soil type                          -         I        *
*  ICROP    number of crop species                       -         I        *
*  IORGM    number of soil organic matter data base      -         I        *
*  INSOIL   number of soil nitrogen data base            -         I        *
*  IRRI     0 = no irrigation; 1 = irrigation            -         I        *
*  ITUBS    assimil.distrib. fixed(=0) or dep. on tuber gr.(=1)    I        *
*  ILEAFC   1 = daily leaf classes included              -         I        *  
*  IPOT     1 = potential production(i.e. no N shortage) -         I        *
*  CO       atmospheric CO2 concentration               ppmv       I        * 
*  IOUT     frequency of output                          d         I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : crop status is written to output file                       *
*	    CRPOUT.DAT                                                      *
*---------------------------------------------------------------------------*

      SUBROUTINE CRPOUT (IDAY,IDEM,IDPL,IDEMERG,
     $                   WLV,WLD,WST,WSTOT,WRT,WTU,
     $                   LAI,DVS,NFLV,NFLD,NFSTOT,NFTU,RD,NUPTMR,NUPTT,
     $                   TBMR,TB,TPARINT,TPAR,REMARK,RUNNAME,
     $                   RED,REDN,TERMIN,STATR,ISOIL,
     $                   ICROP,IORGM,INSOIL,IRRI,ITUBS,ILEAFC,
     $                   IPOT,CO,IOUT)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IDEM,IDPL,IDEMERG,ISOIL,ICROP,IORGM,INSOIL,ISYR
      INTEGER ISYEAR,ISDAY,IRRI,IPOT,ITUBS,ILEAFC,IDEM1,IDPL1
      INTEGER ICOUNT,IOUT
      LOGICAL TERMIN, OPEN
      CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
      COMMON /ITIME/ ISYR, ISYEAR, ISDAY
      DATA OPEN /.TRUE./
      

      IF (OPEN) THEN

*---- open output file CRPOUT.DAT

      OUTFIL = RUNNAME//'CRP.OUT'
      OPEN(13,FILE=OUTFIL,STATUS='NEW')
      
      OPEN = .FALSE.
      ENDIF

      IF ((IDPL .LE. 0 .AND. IDAY .EQ. IDEM) .OR.
     $  (IDPL .GT. 0 .AND. IDAY .EQ. IDPL)) THEN

      IDEM1= IDEM
      IDPL1= IDPL
      IF (IDPL .LE. 0) IDPL1= 0
      IF (IDPL .GT. 0) IDEM1= 0 


*---- write filename and start date on top of output-file

      WRITE (13,'(//A,A)') 'DATA-FILE  ', OUTFIL
      WRITE (13,'(//A,A)') 'Name of run: ', RUNNAME 
      WRITE (13,'(//A,A,A,I3,A,I3,A,I3,A,I3,A,I3,A,I3,A,I3,
     $    A,I3,A,F4.0)') 
     $   'Station=  ',STATR,
     $   ', Soil=',ISOIL,', Crop=',ICROP,', Organic m.=',IORGM, 
     $   ', Nsoil=', INSOIL,', Tub.gr.=',ITUBS,', Leafcl.=',ILEAFC, 
     $   ', Irr.=',IRRI,', Pot.p.=',IPOT,', CO2=',CO 
      WRITE (13,'(/(A,I3,A,I4))')
     $      ' Planting on day ',IDPL1,', ',ISYEAR,
     $      ' Crop emergence on day ',IDEM1,', ',ISYEAR


*---- Write output remarks to file CRPOUT.DAT

      WRITE (13,'(1X,A)') REMARK


*---- writes header for to output file CRPOUT.DAT
      WRITE (13,'(/A)') ' CROP PRODUCTION'
      WRITE (13,'(A)')  ' ==============='


         WRITE (13,'(3A)')
     $  ' DAY    WLV    WLD    WST   WSTOT   WRT',
     $  '    WTU    TB-R    TB    LAI   DVS   RD   NPLV  NPLD  ',
     $  'NPSTOT NPTU NUP-R NUPTT  RED  REDN    PARINT    PAR'

                  
      ICOUNT = -1
      
      ENDIF



*---- Express N-contents as percentages
   
      NPLV   = NFLV*100.
      NPLD   = NFLD*100.
      NPSTOT = NFSTOT*100.            
      NPTU   = NFTU*100.

*-----express PAR in MJ/m2
      TPARINT1 = TPARINT/1000000.
      TPAR1   = TPAR/1000000.
 

      ICOUNT = ICOUNT + 1
            
      IF (MOD(ICOUNT,IOUT) .EQ. 0 .OR. (IDAY .EQ. IDEMERG) 
     $    .OR. TERMIN) THEN

      WRITE (13,'(1X,I4,8F7.0,2F6.2,F6.1,4F6.2,2F6.1,2F6.2,2F8.1))')
     $       IDAY,WLV,WLD,WST,WSTOT,WRT,
     $       WTU,TBMR,TB,LAI,DVS,RD,NPLV,NPLD,NPSTOT,NPTU,NUPTMR,
     $       NUPTT,RED,REDN,TPARINT1,TPAR1

      ENDIF
      RETURN
      END
