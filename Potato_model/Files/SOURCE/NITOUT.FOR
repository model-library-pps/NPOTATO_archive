*---------------------------------------------------------------------------*
*  SUBROUTINE NITOUT                                                        *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine writes soil nitrogen status to the              *
*	    output file *****NIT.OUT                                        *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                       units     class   *
*  ----     -------                                       -----     -----   *
*  IDAY     julian daynumber                                =         I,T   *
*  ANLAY    array, amount of nitrogen in soil comptmnt    kg(N)/ha    I     *
*  NTOT     total amount of nitrogen in soil profile      kg(N)/ha    I     *
*  NDF      nitrogen uptake rate by diffusion             kg(N)/ha/d  I     *
*	    stream                 	                  kg(N)/ha/d  I     *
*  NUPTR    nitrogen uptake rate of the crop              kg(N)/ha/d  I     *
*  TNUPTC   total N-uptake by the crop                    kg(N)/ha    I     *
*  TMINOM   total N mineralization from humus             kg(N)/ha    I     *
*  TMINFM   total N mineralization from crop residues     kg(N)/ha    I     *
*  TBIOM   total N immobilization  in biomass             kg(N)/ha    I     *
*  TNIMM   total N lost from soil nitrate N by                              *
*	    immobilization                                kg(N)/ha    I     *
*  TLEACH   total N lost by leaching                      kg(N)/ha    I     *
*  FMTOT    total nitrogen in fresh organic matter        kg(N)/ha    I     *
*  REMARK   output remarks                                  =         I     *  
*  RUNNAME  first 5 characters for name of output files     -         I     *
*  TERMIN   logical, indicates end of simulation            -         I     *
*  IDEMERG  date of crop emergence                          -         I     *
*  INIT     logical, indicates start of simulation          -         I     *
*  TNRAIN   total nitrogen supply in rainfall              kg(N)/ha   I     *
*  TNGIFT   total supply of fertilizer nitrogen            kg(N)/ha   I     *
*  STATR    name of weather station (CHAR*5)                 -        I     *
*  ISOIL    number of soil type                              -        I     *
*  ICROP    number of crop species                           -        I     *
*  IORGM    number of soil organic matter data base          -        I     *
*  INSOIL   number of soil nitrogen data base                -        I     *
*  IRRI     0 = no irrigation; 1 = irrigation                -        I     *
*  IPOT     1 = potential production (no N shortage)         -        I     * 
*  CO       atmospheric CO2 concentration                   ppmv      I     *
*  IOUT     frequency of output                              d        I     *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil nitrogen status is written to output file              *
*	    NITOUT.DAT                                                      *
*---------------------------------------------------------------------------*


      SUBROUTINE NITOUT (IDAY,ANLAY,NTOT,NDF,NUPTR,TNUPTC,
     $                   TMINOM,TMINFM,TBIOM,TNIMM,TLEACH,FMTOT,
     $                   REMARK,RUNNAME,TERMIN,IDEMERG,INIT,
     $                   TNRAIN,TNGIFT,STATR,ISOIL,ICROP,IORGM,INSOIL,
     $                   IRRI,IPOT,CO,IOUT)                   


      IMPLICIT REAL (A-Z)
      INTEGER IDAY, ICROP, ISOIL, IORGM, INSOIL, ICOUNT
      INTEGER ISDAY, ISYEAR, ISYR, I,IRRI,IPOT,IOUT,IDEMERG
      LOGICAL INIT, TERMIN, OPEN
      CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
      DIMENSION ANLAY(10)

      COMMON /ITIME/ ISYR, ISYEAR, ISDAY

      DATA OPEN /.TRUE./

      IF (OPEN) THEN
      OPEN = .FALSE.                                                  
      
*---- open file #####NIT.OUT 

      OUTFIL = RUNNAME//'NIT.OUT'
      OPEN (4,FILE=OUTFIL,STATUS='NEW')
      ENDIF

      IF (INIT) THEN

      ICOUNT = -2
     
      WRITE (4,'(//A,A)') 'DATA-FILE  ', OUTFIL
      WRITE (4,'(//A,A)') 'Name of run= ', RUNNAME
      WRITE (4,'(//A,A,A,I3,A,I3,A,I3,A,I3,A,I3,A,I3,A,F4.0)') 
     $       'Station= ', STATR,
     $       ', Soil=', ISOIL, ', Crop=',ICROP, ', Organic m.=', 
     $       IORGM,', Nsoil=', INSOIL,', Irr.=',IRRI,
     $       ', Pot.p.=',IPOT,', CO2=',CO
      WRITE (4,'(/(A,I3,A,I4))')
     $      ' Simulation starts on day ',ISDAY,', ',ISYEAR

      
*---- write output remarks to file nitout.dat

      WRITE (4,'(1X,A)') REMARK


*---- write header file NITOUT.DAT

      WRITE (4,'(/A)') ' SOIL NITROGEN STATUS'
      WRITE (4,'(A)')  ' ===================='
      WRITE (4,'(A, A,/A,A)')
     $     ' DAY    ANL1 ANL2  ANL3  ANL4  ANL5  ANL6',
     $     '  ANL7  ANL8  NTOT    NDF    NUPTR TNUPT',
     $     ' MINOM MINFM NBIOM NIMMO NFM   NLEACH',
     $     ' NSAM1 NSAM2 NSAM3 NRAIN  NGIFT'

      INIT = .FALSE.
      ENDIF


*---- output data are rearranged to obtain output for comparison with
*     field measurements

      AN1 = ANLAY(1) + ANLAY(2) + ANLAY(3) + ANLAY(4)
      AN2 = ANLAY(5) + ANLAY(6) + ANLAY(7)
      AN3 = ANLAY(8) + ANLAY(9) + ANLAY(10)


c      WRITE (4,'(1X,I4,8F6.1,4F7.2,9F6.1)')


      ICOUNT = ICOUNT  + 1

      IF (MOD(ICOUNT, IOUT) .EQ. 0 .OR. (IDAY .EQ. IDEMERG + 1) 
     $   .OR. TERMIN) THEN

      WRITE (4,'(1X,I4,8F6.1,4F7.2,/,11F6.1)')
     $       IDAY,(ANLAY(I),I=1,8),NTOT,NDF,NUPTR,TNUPTC,TMINOM,
     $       TMINFM,TBIOM,TNIMM,FMTOT,TLEACH,AN1,AN2,AN3,TNRAIN,
     $       TNGIFT

       ENDIF

      RETURN
      END
