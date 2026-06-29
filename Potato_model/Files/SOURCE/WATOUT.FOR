*---------------------------------------------------------------------------*
*  SUBROUTINE WATOUT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine writes soil water status to the                 *
*	    output file WATOUT.DAT                                          *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  IDAY     julian daynumber                             =         I,T      *
*  WCACT    array, actual water content of each soil                        *
*  ACT      array, actual water content of each soil                         *
*	    compartment                                 cm/cm      I        *
*  TTRANS   total transpiration over simulated period    cm        I        *
*  TDRAIN   total drainage over simulated period         cm        I        *
*  TRAIN    total precipitation over sim. period         cm        I        *
*  TESOIL   total soil evaporation over sim. period      cm        I        *
*  TRUNOF   total water loss by runoff                   cm        I        *
*  TIRR     total water supply by irrigation             cm        I        *
*  RUNFR    fraction of rainfall lost by run off         -         I        *
*  IRRI     0 = no irrigation; 1 = irrigation                      I        *
*  REMARK   output remarks                               =         I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil waterstatus is written to output file                  *
*	    WATOUT.DAT                                                      *
*---------------------------------------------------------------------------*
    
	SUBROUTINE WATOUT (IDAY,WCACT,
     1         TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,RUNFR,IRRI,
     1         WTOT,WAVT,WAVR,REMARK,
     1         RUNNAME,TERMIN,IDEMERG,INIT,STATR,ISOIL,ICROP,IORGM,
     1         INSOIL,CO,IOUT)

	IMPLICIT REAL (A-Z)
	INTEGER IDAY,IRRI,ISOIL,ICROP,IORGM,INSOIL,ISYEAR,ISYR,ISDAY
        INTEGER ICOUNT,IDEMERG,IOUT 
	LOGICAL INIT,TERMIN,OPEN
	CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
	DIMENSION WCACT(10)
      
        COMMON /ITIME/ ISYR, ISYEAR, ISDAY
        
	DATA OPEN /.TRUE./

        IF (OPEN) THEN

*------	open output file #####WAT.OUT
        OPEN = .FALSE.
	OUTFIL = RUNNAME//'WAT.OUT'	
	OPEN (11,FILE=OUTFIL,STATUS='NEW')
        ENDIF

        IF (INIT) THEN

        WRITE (11,'(//A,A)') 'DATA-FILE  ', OUTFIL
        WRITE (11,'(//A,A)') 'Name of run: ',RUNNAME
        WRITE (11,'(//A,A,A,I3,A,I3,A,I3,A,I3,A,I3,A,F4.2,A,F4.0)') 
     $  'Station= ',STATR,
     $  ', Soil=',ISOIL,', Crop=',ICROP,', Organic m.=',IORGM, 
     $  ', Nsoil=',INSOIL,', Irr.=',IRRI,', Runof=',RUNFR,', CO2=',CO
	WRITE (11,'(/(A,I3,A,I4))')
     $      ' Simulation starts on day ',ISDAY,', ',ISYEAR

*------	write output remarks to file WATOUT.DAT

	WRITE (11,'(1X,A)') REMARK

*------ writes header for to output file 

	   WRITE (11,'(/A)') ' SOIL WATER STATUS'
	   WRITE (11,'(A)')  ' ================='
	   WRITE (11,'(3A)')
     1     '   DAY   WCACT1  WCACT2  WCACT3  WCACT4  WCACT5  WCACT6',
     2     '   WCACT7  WCACT8  WCACT9  WCACT10  TTRANS TDRAIN  TRAIN',
     3     '   TESOIL   TIRR  TRUNOF     WTOT    WAVT    WAVR'

        ICOUNT = -2       
	INIT = .FALSE.                                                  
        
        ENDIF

*------ write soil water status data to WATOUT.DAT during simulation 


      ICOUNT = ICOUNT + 1
      IF (MOD(ICOUNT,IOUT) .EQ. 0 .OR. (IDAY .EQ. IDEMERG + 1) 
     $   .OR. TERMIN) THEN

      WRITE (11,'(1X,I5,14F8.2,2F8.2,2X,3F8.2)') 
     1       IDAY,(WCACT(I),I=1,10),TTRANS,TDRAIN,TRAIN,TESOIL,
     1       TIRR,TRUNOF,WTOT,WAVT,WAVR

      ENDIF

      RETURN
      END
