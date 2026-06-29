*-----------------------------------------------------------------------------*
*  SUBROUTINE NITDAT                                                          *
*  Author : Joost Wolf, based on routine developed by Rob Groot (sept. 1988)  *
*  Date   : june 1996                                                         *
*  Purpose: This subroutine reads soil nitrogen data from the data            *
*	    file NSOILN.DAT                                                   *
*                                                                             *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)           *
*  name     meaning                                       units  class       *
*  ----     -------                                       -----  -----       *
*  ILAYER   number of soil compartments                     =      I         *
*  NTOT     total nitrogen in soil profile              kg(N)/ha   O         *
*  ANLAY    array, amount of nitrogen in soil comp.     kg(N)/ha   O         *
*  IDAYN    array, dates of nitrogen application            =      O,T       *
*  ANAP     array, amounts of nitrogen in successive                         *
*	    nitrogen application                        kg(N)/ha   O         *
*  NCRAIN    nitrogen concentration in precipitation     kg(N)/cm   O        *
*  TCND     time constant for N diffusion                   day    O         *
*  ADC      adsorption constant                          ml/cm3    O         *
*  FMAX     max. N-influx per cm2 root area              mg/cm2    O         *
*  INUMAP   number of nitrogen applications                 =      O         * 
*                                                                            *
*  subroutines called : none                                                 *
*  FILE usage : soil nitrogen data are read from the                         *
*	    data file NSOIL.DAT                                              *
*----------------------------------------------------------------------------*


      SUBROUTINE NITDAT (INSOIL,ILAYER,NTOT,ANLAY,IDAYN,ANAP,NCRAIN,
     $                   TCND,ADC,FMAX,INUMAP)

      IMPLICIT REAL(A-Z)
      INTEGER INSOIL,INUMAP,IDAYN,ILAYER,IDN,IANN

      DIMENSION ANLAY(10),IDAYN(5),ANAP(5),DAYN(5)


*---- open nsoil data-file

      IF (INSOIL .EQ. 1) CALL RDINIT (22,0,'NSOILN1.DAT')
      IF (INSOIL .EQ. 2) CALL RDINIT (22,0,'NSOILN2.DAT')
      IF (INSOIL .EQ. 3) CALL RDINIT (22,0,'NSOILN3.DAT')
      IF (INSOIL .EQ. 4) CALL RDINIT (22,0,'NSOILN4.DAT')

*---- total nitrogen in soil profile (kg(N)/ha)

      CALL RDSREA ('NTOT',NTOT)

*---- initial amounts of nitrogen in soil compartments 
*---- in kg(N)/ha/compartment

      CALL RDAREA ('ANLAY',ANLAY,10,ILAYER)
      
*---- number of nitrogen applications
 
      CALL RDSINT ('INUMAP',INUMAP)

*---- timing of nitrogen applications, given as daynumbers

      CALL RDAREA ('DAYN',DAYN,5,IDN)
      IDAYN = INT (DAYN)

*---- amounts of nitrogen in applications (kg/ha)

      CALL RDAREA ('ANAP',ANAP,5,IANN)

*---- read nitrogen concentration in rainwater (kg/cm)

      CALL RDSREA ('NCRAIN',NCRAIN)

*---- read time constant for nitrogen diffusion towards roots (days)

      CALL RDSREA ('TCND',TCND)

*---- read adsorption constant for N (ml/cm3)

      CALL RDSREA ('ADC',ADC)

*---- read max. N-influx per cm2 root area (mg/cm2)

      CALL RDSREA ('FMAX',FMAX)
  
      CLOSE (22, STATUS= 'DELETE')

      RETURN
      END
