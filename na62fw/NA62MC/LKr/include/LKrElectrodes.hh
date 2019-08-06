//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------------
// History:
//
// Created by 
//            
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
//  03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)      
//
//
// --------------------------------------------------------------------
#ifndef LKrElectrodes_H
#define LKrElectrodes_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"
#include "G4Region.hh"
#include "G4RegionStore.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class LKrElectrodeParameterisation;
class LKrVSpacerParameterisation;
class LKrIrregularElectrodeParameterisation;
class LKrIrregularVSpacerParameterisation;

class LKrElectrodes : public NA62VComponent {

  public:

    ~ LKrElectrodes();
    LKrElectrodes(G4Material*, G4LogicalVolume*);

    void ReadGeometryParameters();
    void CreateGeometry();
    void SetProperties();

  private:

    G4int ParameterisedNumberElectrodes;
    G4Trap* solidLINES;
    G4LogicalVolume* logicLINES;
    G4VPhysicalVolume* physiLINES;

    LKrElectrodeParameterisation * ElectrodeParameterisation;
    LKrVSpacerParameterisation * VSpacerParameterisation;
    G4VPhysicalVolume * physiParameterisedVSpacer;

    G4Trap* solidIrregularLINES;
    G4LogicalVolume* logicIrregularLINES;
    G4VPhysicalVolume* physiIrregularLINES;

    LKrIrregularElectrodeParameterisation * IrregularElectrodeParameterisation;

    G4Trap * solidParameterisedIrregularVSpacer;
    G4LogicalVolume* logicParameterisedIrregularVSpacer;
    LKrIrregularVSpacerParameterisation * IrregularVSpacerParameterisation;
    G4VPhysicalVolume * physiParameterisedIrregularVSpacer; 

    G4double fZtr;
    G4double fXbackReferenceCell;
    G4double fYbackReferenceCell;
    G4double fXfrontReferenceCell;
    G4double fYfrontReferenceCell;

    G4double fXbackProjectivityReferenceCell;
    G4double fYbackProjectivityReferenceCell;
    G4double fProjectivityPointPositionZ;
    G4double fProjectivityAxisProjectionZ;

    G4double fDistanceToNextElectrodeFrontX;
    G4double fDistanceToNextElectrodeBackX;
    G4double fDistanceToNextElectrodeFrontY;
    G4double fDistanceToNextElectrodeBackY;

    G4int fHalfNElectrodesX;
    G4int fHalfNElectrodesY;
    G4int fNRowsFullNumberElectrodes;

    G4double fRHoleX; 
    G4double fRHoleY; 
    G4double fHalfZWidth;
    G4double fHalfYWidthF;
    G4double fHalfXWidthF;
    G4double fHalfYWidthB;
    G4double fHalfXWidthB;
    G4double fBackWallPositionZ;				    
    G4double fHalfSizeCentralGap;
    G4double fHalfLengthOfFrontPlateOctagon;
    G4double fLongitudinalLengthInnerCryo;
    G4double fDistanceFrontPlateBackPlate;
    G4double fLongitudinalLengthLKrVolume;
    G4double fLengthOfColdWindowPlate;
    G4double fHalfIrregularCellSizeAtBackWallY;
    G4double fHalfIrregularCellSizeAtFrontWallY;
    G4double fHalfYWidthIrregularB;
    G4double fHalfYWidthIrregularF;
    G4double fPassiveLKrInsideOctagon;
    G4Region* caloRegion;
};

#endif
