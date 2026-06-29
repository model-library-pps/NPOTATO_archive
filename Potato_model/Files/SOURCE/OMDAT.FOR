*---------------------------------------------------------------------------*
*  SUBROUTINE OMDAT                                                         *
*  Author : Rob Groot                                                       *
*  Date   : 15 sept 1988                                                    *
*  Purpose: This subroutine reads data on soil organic nitrogen dynamics    *
*           from the data file OMSOILN.DAT                                  *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                       units  class      *
*  ----     -------                                       -----  -----      *
*  PLAYER   depth of plough layer                          cm      O        *
*  PERCOM   percentage organic material of humus in                         *
*	    plough layer                                    =      O        *
*  RDROM    relative decomposition  rate old org.mat.      d-1     O        *
*  REFTMP   reference temperature for mineralization        C      O        *
*  FM       amount of fresh organic matter or crop                          *
*	    residues in plouging layer                    kg/ha    O        *
*  RDRFM    relative decomposition  rate fresh org.mat.    d-1     O        *
*  CCFM     carbon content of fresh organic matter          =      O        *
*  NCFM     nitrogen content of fresh organic matter        =      O        *
*  DISASS   dissimilation:assimilation ratio for growth of                  *
*	    microbial biomass                               =      O        *
*	    and dissimilation                               =      O        *
*  NCRB     N:C ratio of biomass                            =      O        *
*  NAPDAY   array, dates of nitrogen application            =      O,T      *
*  ANAP     array, amounts of nitrogen in successive                        *
*	    nitrogen application                        kg(N)/ha   O        *
*  NRAIN    nitrogen concentration in precipitation     kg(N)/cm   O        *
*  TCND     time constant for nitrogen diffusion           d-1     O        *
*                                                                           *
*  subroutines called : none                                                *
*  FILE usage : soil organic nitrogen data are read from the                *
*	    data file OMSOILN.DAT                                           *
*---------------------------------------------------------------------------*

      SUBROUTINE OMDAT(IORGM,PLAYER,PERCOM,RDROM,REFTMP,FM,RDRFM,CCFM,
     $               NCFM,DISASS,NCRB)


      IMPLICIT REAL(A-Z)
      INTEGER IORGM 


*---- soil organic data file OMSOILN is opened

      IF (IORGM .EQ. 1) CALL RDINIT (23,0,'OMSOILN1.DAT')
      IF (IORGM .EQ. 2) CALL RDINIT (23,0,'OMSOILN2.DAT')
      IF (IORGM .EQ. 3) CALL RDINIT (23,0,'OMSOILN3.DAT')
      IF (IORGM .EQ. 4) CALL RDINIT (23,0,'OMSOILN4.DAT')


*---- reading soil organic matter data from file

*---- read depth of ploughing layer (cm)

      CALL RDSREA ('PLAYER',PLAYER)

*---- read percentage organic material (humus) in ploughing layer

      CALL RDSREA ('PERCOM',PERCOM)

*---- read relative decomposition rate of old org.mat. (kg/kg/d)

      CALL RDSREA ('RDROM',RDROM)

*---- read reference temperature for mineralization (C)

      CALL RDSREA ('REFTMP',REFTMP)

*---- read amount of crop residues from previous crop

      CALL RDSREA ('FM',FM)

*---- read relative decomposition rate of fresh organic matter

      CALL RDSREA ('RDRFM',RDRFM)

*---- read carbon content of fresh organic material

      CALL RDSREA ('CCFM',CCFM)

*---- read nitrogen content of fresh organic material

      CALL RDSREA ('NCFM',NCFM)

*---- read dissimilation:assimilation ratio for growth of microbial biomass

      CALL RDSREA ('DISASS',DISASS)

*---- read N:C ratio biomass 

      CALL RDSREA ('NCRB',NCRB)


      CLOSE (23, STATUS= 'DELETE')


      RETURN
      END
