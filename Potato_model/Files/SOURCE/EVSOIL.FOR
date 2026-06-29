*---------------------------------------------------------------------------*
*  SUBROUTINE EVSOIL                                                        *
*  Author : Daniel van Kraalingen                                           *
*  Date   : december 1986                                                   *
*  Purpose: This subroutine calculates soil evaporation and                 *
*	    distributes evaporation over soil compartments with             *
*	    exponential decrease.                                           *
*                                                                           *
*           References to literature:                                       *
*           Keulen H. van, 1975. Simulation of water use and herbage        *
*	        growth in arid regions. Simulation Monographs Pudoc,        *
*               Wageningen, 184pp.                                          *
*           Keulen, H.van &  N.Seligman, 1987. Simulation of water use,     *
*	        nitrogen nutrition and growth of a spring wheat crop.       *
*               Pudoc, Wageningen, in press.                                *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                        units  class     *
*  ----     -------                                        -----  -----     *
*  LAI      leaf area index of crop                       m2 m-2    I       *
*  ES0      potential evaporation of a bare soil surface    cm      I       *
*  KDIF     extinction coefficient for diffuse visible light -      I       * 
*  ILAYER   number of soil compartments                      =      I       *
*  WCACT    array, actual water content of soil                             *
*	    compartments                                  cm cm-1   I       *
*  WCMAX    array, maximum water content of soil                            *
*	    compartments                                  cm cm-1   I       *
*  WCFLDC   array, water content of soil compartments                       *
*	    at field capacity                             cm cm-1   I       *
*  WCAIR    array, water content of soil compartments                       *
*	    when compartments are air dry                 cm cm-1   I       *
*  THCKN    array, thickness of soil compartments           cm      I       *
*  PROP     proportionality factor for evaporation                          * 
*	    distribution over soil compartments              =      I       *
*  DEPTH    array, depth of center of each soil                             *
*	    compartment measured from surface               cm      I       *
*  ESOIL    soil evaporation                              cm d-1    O       *
*  RESOIL   array, evaporation rate per soil depth    cm cm-1 d-1   O       *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE EVSOIL (LAI,ES0,KDIF,ILAYER,WCACT,WCFLDC,WCAIR,     
     $                   THCKN,PROP,DEPTH,ESOIL,RESOIL)
	

      IMPLICIT REAL (A-Z)
      INTEGER ILAYER, IDRY, I
      LOGICAL INIT

      DIMENSION WCACT(ILAYER) , THCKN(ILAYER), DEPTH(ILAYER),
     $          VAR(100),RESOIL(ILAYER), DRYLTB(14),WCFLDC(ILAYER),
     $          WCAIR(ILAYER)


*---- effect of relative water content in first soil compartment              
*     on ratio of actual and potential soil evaporation,
*     van Keulen & Seligman, 1987 for a surface layer of 2 cm.

      DATA DRYLTB/-0.001,0., 0.,0., 0.2,0.05, 0.22,0.275, 0.33,0.9,
     $             1.,1., 1.5,1./

*---  enhanced drought reduction on soil evaporation
*      DATA DRYLTB/-0.001,0., 0.,0., 0.2, 0.05, 0.35,0.6, 0.50, 0.9,
*     $             1.,1., 1.5,1./

      IDRY = 14

*---- calculation of soil evaporation                                   

      EVMAX  = ES0*EXP(-0.75*KDIF*LAI)

      RELWC = (WCACT(1)-WCAIR(1))/(WCFLDC(1)-WCAIR(1))
      ESOIL = EVMAX*AFGEN(IDRY,DRYLTB,RELWC)

      SUM = 0.                                                          

      DO 10,I=1,ILAYER

         VAR(I) = AMAX1(0.,(WCACT(I)-WCAIR(I)))
     $            * EXP(-PROP*0.01*DEPTH(I))
         SUM = SUM+VAR(I)*THCKN(I)

10    CONTINUE


*---- calculation of evaporation rate per soil compartment (RESOIL)           
*     with exponential extinction of evaporated moisture over
*     soil compartments

      DO 20,I=1,ILAYER

         IF (SUM.GT.0.) THEN
            RESOIL(I) = ESOIL*VAR(I)/SUM
         ELSE
            RESOIL(I)=0.
         END IF

20    CONTINUE


      RETURN
      END
