*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*

*---------------------------------------------------------------------------*
*  SUBROUTINE SOILR                                                         *
*  Author : Rob Groot                                                       *
*  Date   : january 1987                                                    *
*  Purpose: This subroutine reads physical soil data from the data          *
*	    file SOIL.DAT                                                   *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  ISOIL    number of soil type                          -         I        *
*  ISMTB1   number of points in pF-curve, text.class 1   =         O        *
*  ISMTB2   number of points in pF-curve, text.class 2   =         O        *
*  ISMTB3   number of points in pF-curve, text.class 3   =         O        *
*  ISMTB4   number of points in pF-curve, text.class 4   =         O        *
*  ISMTB5   number of points in pF-curve, text.class 5   =         O        *
*  SMTB1    array with pF values for texture class 1     =         O        *
*  SMTB2    array with pF values for texture class 2     =         O        *
*  SMTB3    array with pF values for texture class 3     =         O        *
*  SMTB4    array with pF values for texture class 4     =         O        *
*  SMTB5    array with pF values for texture class 5     =         O        *
*  THCKN    array, thickness of soil compartments       cm         O        *
*  ITEXCL   texture class of soil compartments           =         O        *
*  RHOD     bulk density of soil compartments          gr/cm3      O        *
*  ILAYER   number of soil compartments                  =         O        *
*  RDMSOL   maximum rooting depth as determined by soil cm         O        *
*  PROP     proportionality factor for evaporation       =         O        *
*  WCINI    array, initial water content of the soil                        *
*	    compartments                               cm3 cm-3    O        *
*  RUNFR    fraction of precipitation lost by runoff     -         O        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : soil data are read from the data file SOILN.DAT             *
*---------------------------------------------------------------------------*
    
      SUBROUTINE SOILR (ISOIL,ISMTB1,ISMTB2,ISMTB3,ISMTB4,
     $                  ISMTB5,SMTB1,SMTB2,SMTB3,SMTB4,
     $                  SMTB5,THCKN,RHOD,ITEXCL,ILAYER,RDMSOL,PROP,
     $                  WCINI,RUNFR)
	

      IMPLICIT REAL (A-Z)
      INTEGER ISOIL,ISMTB1,ISMTB2,ISMTB3,ISMTB4,ISMTB5, ITEXCL,ILAYER
      INTEGER ITEXT,ITHCKN,IRHOD,IWCINI,ITEXC

      DIMENSION SMTB1(30),SMTB2(30),SMTB3(30),SMTB4(30),SMTB5(30)
      DIMENSION WCINI(10),THCKN(10),ITEXCL(10),RHOD(10),TEXTCL(10)


*---- the soil data file is opened
      IF (ISOIL .EQ. 1) CALL RDINIT (21,0,'SOILN1.DAT')
      IF (ISOIL .EQ. 2) CALL RDINIT (21,0,'SOILN2.DAT')
      IF (ISOIL .EQ. 3) CALL RDINIT (21,0,'SOILN3.DAT')
      IF (ISOIL .EQ. 4) CALL RDINIT (21,0,'SOILN4.DAT')
      IF (ISOIL .EQ. 5) CALL RDINIT (21,0,'SOILN5.DAT')
      IF (ISOIL .EQ. 6) CALL RDINIT (21,0,'SOILN6.DAT')


*---- number of texture classes
       
      CALL RDSINT('ITEXT', ITEXT)

*---- in case there is only one texture class, the data are read        

      IF (ITEXT.GE.1) THEN
         CALL RDAREA ('SMTB1',SMTB1,30,ISMTB1)
      END IF

*---- in case there are two texture classes, the data are read


      IF (ITEXT.GE.2) THEN
         CALL RDAREA ('SMTB2',SMTB2,30,ISMTB2)
      END IF


*---- in case there are three texture classes, the data are read

      IF (ITEXT.GE.3) THEN
         CALL RDAREA ('SMTB3',SMTB3,30,ISMTB3)
      END IF

*---- in case there are four texture classes, the data are read

      IF (ITEXT.GE.4) THEN
         CALL RDAREA ('SMTB4',SMTB4,30,ISMTB4)
      END IF
 
*---- in case there are five texture classes, the data are read

      IF (ITEXT.GE.5) THEN
         CALL RDAREA ('SMTB5',SMTB5,30,ISMTB5)
      END IF

*---- number of soil compartments is read                                     

      CALL RDSINT ('ILAYER', ILAYER)

*---- thickness and bulk density of each compartment are read   

      CALL RDAREA ('THCKN', THCKN,10, ITHCKN)
      CALL RDAREA ('RHOD', RHOD, 10, IRHOD)


*---- maximum rooting depth of soil and proportionality factor are read 

      CALL RDSREA ('RDMSOL', RDMSOL)
      CALL RDSREA ('PROP', PROP)

*---- initial water content and the texture class to which a compartment
*     belongs are read              

      CALL RDAREA ('WCINI', WCINI,10, IWCINI)
      CALL RDAREA ('TEXTCL', TEXTCL,10, ITEXC)

      ITEXCL = INT (TEXTCL)

*---- fraction of precipitaion lost by runoff

      CALL RDSREA ('RUNFR',RUNFR)
     
      CLOSE (21, STATUS= 'DELETE')
      RETURN                                                            
      END
