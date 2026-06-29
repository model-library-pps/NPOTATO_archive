*---------------------------------------------------------------------------*
*  SUBROUTINE BGLEAF                                                        *
*  Author : Joost Wolf                                                      *
*           based on an earlier version written by                          *
*           Sanderine Nonhebel and Kees Spitters and on an FAO expert       *
*           consultation (FAO, 1992)                                        *
*                                                                           *
*  Date   : 5-DEC-1994                                                      *
*  Purpose: This subroutine calculates the potential transpiration          *
*            of a closed crop canopy (ET0) using the bigleaf model          *
*            (Penman-Monteith equation; Monteith, 1965)                     *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  ATMTR   atmospheric transmission                      -      I           *
*  TMIN    minimum temperature during day                C      I           *
*  TMAX    maximum temperature during day                C      I           *
*  AVRAD   daily total global irradiation            J m-2 d-1  I           *
*  WIND    average windspeed                           m s-1    I           *
*  VAP     average vapour pressure                     mbar     I           *
*  ALTI    altitude of weather station                  m       I           *
*  CROPHT  height of crop                               m       I           *   
*  LAI     leaf area index                           m2 m-2     I           *
*  CO      atmospheric CO2 concentration               ppmv     I           *
*  ETC     potential evapo-transpiration of crop       cm d-1   O           *
*                                                                           *
*                                                                           *
*  SUBROUTINES called : PETPLA                                              *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE BGLEAF (ATMTR,TMIN,TMAX,AVRAD,WIND,VAP,ALTI,CROPHT,
     $   LAI,CO,ETC)

      IMPLICIT REAL (A-Z)
      INTEGER ISC

      DIMENSION SCTB(10)

*----- reduction function of stomatal conductivity of a single leaf 
*      as a function of atmospheric CO2 concentration (Morrison, 1993) 
*      but reduction is halved because of limited coupling with the atmosphere
       DATA SCTB/40.,1.2,355.,1.0,710.,0.8, 1000., 0.7,2000., 0.7/  
       DATA ISC/10/

*----- reflection coefficient of crop
       REFCFC = 0.25

*----- Stefan Boltzmann constant (J m-2 d-1 K-4); average daily 
*      stomatal resistance of a single leaf (s m-1) (FAO, 1992)
       STBC= 4.9E-3
       RL= 140.

*------ correction of stomatal resistance for CO2 concentration
       REDSC= AFGEN(ISC, SCTB, CO)
       RLC= RL/REDSC 

*----- Angstrom constants
       A= 0.20
       B= 0.56

*----- crop canopy resistance (s m-1) (FAO, 1992)
       
       RC= RLC/AMAX1(0.01,0.5*LAI)

*----- aerodynamic resistance (s m-1) (FAO, 1992)
      
       RAERO= LOG((2.-0.667*CROPHT)/(0.123*AMAX1(0.01,CROPHT))) *
     $     LOG((2.-0.667*CROPHT)/(0.0123*AMAX1(0.01,CROPHT)))/
     $     (0.41**2 * AMAX1(0.1,WIND)) 

*----- average daily temperature (C)
       TMPA= (TMIN + TMAX) / 2.

*----- the expression n/N (RELSSD) is estimated from the Angstrom formula:
*       RI= RA(A+B.n/N)
 
      RELSSD= LIMIT (0., 1., (ATMTR -A)/B)

*----- net absorbed radiation (RNET, J m-2 d-1)
      RLOSS = STBC * (TMPA + 273.)**4 * (0.1 + 0.9 * RELSSD) 
     $  * (0.34-0.14*SQRT(VAP/10.)) 
      RNET = (1. -REFCFC) * AVRAD - RLOSS

*----- potential transpiration (mm d-1)
      CALL PETPLA (TMPA,RNET,VAP,ALTI,RAERO,RC,ET0,ETR,ETD)

*----- conversion to cm d-1
      ETC = 0.1 * ET0

      RETURN
      END


*---------------------------------------------------------------------------*
*  SUBROUTINE PETPLA                                                        *
*  Author : Joost Wolf                                                      *
*           based on an earlier version written by: Sanderine Nonhebel      *
*  Date   : 2-DEC-1994                                                      *
*  Purpose: This subroutine calculates the potential transpiration          *
*            of a closed crop canopy (ET0) according to the                 *
*            Bigleaf model (Monteith, 1965)                                  *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  TMPA   average daily temperature                      C      I           *
*  RNET    net radiation                            J m-2 d-1   I           *
*  VAP    vapour pressure                              mbar     I           *
*  ALTI    altitude of weather station                  m       I           *
*  RAERO   aerodynamic resistance                      s m-1    I           *
*  RC      crop canopyresistance                       s m-1    I           *
*  ET0      potential total transpiration              mm d-1   O           *
*  ETR     transpiration due to radiation              mm d-1   O           *
*  ETD     transpiration due to drying power           mm d-1   O           *
*                                                                           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE PETPLA (TMPA,RNET,VAP,ALTI,RAERO,RC,ET0,ETR,ETD)

      IMPLICIT REAL (A-Z)
    

*---- Latent heat of evaporation of water (J kg-1= J mm-1 m-2), 
*     and psychrometric instrument constant (K-1) 

      LHVAP  = 2.45E6
      PSYCON = 0.000662

*---- Barometric pressure (mbar), Psychrometric constant (mbar K-1)

      PBAR = 1013.*EXP(-0.034*ALTI/(TMPA+273.))
      GAMMA = PSYCON*PBAR

*---- Saturated vapour pressure according to equation of Goudriaan (1977)

      SVAP = 6.11*EXP(17.4*TMPA/(TMPA+239.))

*---- Measured vapour pressure should not be greater than saturated
*     vapour pressure

      VAP  = AMIN1(SVAP,VAP)

*---- Derivative of SVAP with respect to temperature, i.e. slope of the 
*     SVAP-temperature curve (mbar K-1)

      DELTA = 239.*17.4*SVAP/(TMPA+239.)**2

*    volumetric heat capacity (J/M3/K)
      RHOCP= (GAMMA * 0.0622 * LHVAP * 3.486)/ 
     $        (1.01 * (TMPA + 273.)) 

*---- Radiation and aero-dynamic terms (mm d-1)

      APSCN = GAMMA * (RC + RAERO)/ (RAERO)
      ETR = (DELTA * RNET) / (LHVAP * (DELTA + APSCN)) 
      ETD = RHOCP * ((SVAP - VAP)/ (RAERO  * 
     &      LHVAP * (DELTA + APSCN))) * 86400.

*---- Penman-Monteith formula (1965) in mm/d

      ET0 = (ETR + ETD)

*---- Evapo(transpi)ration is limited in order to prevent negative
*     values

      ET0 = AMAX1(0.,ET0)

      RETURN
      END
