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
// --------------------------------------------------------------------
#ifndef LKrCryostat_H
#define LKrCryostat_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class LKrElectrodeCell;
class G4Tubs;
class LKrLKrVolume;
class LKrFrontBackPlate;

class LKrCryostat : public NA62VComponent {

    public:

        ~ LKrCryostat();
        LKrCryostat( G4Material*,G4LogicalVolume*, G4Transform3D);

        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    private:

        G4Tubs*            solidInnerCryostat;     //pointer to inner cryo tube
        G4LogicalVolume*   logicInnerCryostat;     //pointer to logical inner cryo tube     
        G4VPhysicalVolume* physiInnerCryostat;     // pointer to the physical inner cryo tube

        G4Tubs*            solidOuterCryostat;     //pointer to outer cryo tube
        G4LogicalVolume*   logicOuterCryostat;     //pointer to logical outer cryo tube     
        G4VPhysicalVolume* physiOuterCryostat;     // pointer to the physical outer cryo tube

        G4Tubs*            solidElectronics;     //pointer to electronics
        G4LogicalVolume*   logicElectronics;     //pointer to electronics     
        G4VPhysicalVolume* physiElectronics;     // pointer to electronics

        G4Tubs*            solidElectronicsCu;     //pointer to electronics
        G4LogicalVolume*   logicElectronicsCu;     //pointer to electronics     
        G4VPhysicalVolume* physiElectronicsCu;     // pointer to electronics

        G4Tubs*            solidElectronicsTeflon;     //pointer to electronics
        G4LogicalVolume*   logicElectronicsTeflon;     //pointer to electronics     
        G4VPhysicalVolume* physiElectronicsTeflon;     // pointer to electronics

        G4Tubs*            solidElectronicsBrass;     //pointer to electronics
        G4LogicalVolume*   logicElectronicsBrass;     //pointer to electronics     
        G4VPhysicalVolume* physiElectronicsBrass;     // pointer to electronics

        LKrLKrVolume * fLKrVolume;
        LKrFrontBackPlate* fFrontPlate;
        G4Transform3D fCryostatTransform;
        G4double fInnerCryostatMinRadius; 
        G4double fThicknessOfInnerCryo;
        G4double fOuterCryostatMinRadius;
        G4double fThicknessOfOuterCryo;

        G4double  fLongitudinalLengthInnerCryo;
        G4double  fLongitudinalLengthOuterCryo;

        G4double  fInnerDistanceFrontBackWarmWindow ;
        G4double  fDistanceFrontWarmWindowLKr;

        G4double fLongitudinalLengthLKrVolume;
        G4double fLengthOfColdWindowPlate;

       G4double fBEATCHPositionFrontWarmWindowEdge;
       G4double fBEATCHPositionFrontEdgeOuterVessel;
       G4double fBEATCHPositionBackEdgeOuterVessel;
       G4double fBEATCHPositionBackWarmWindowEdge ;
       G4double fRadiusHoleSpacerPlate;

       G4double fElectronicsBackG10HalfLength;
       G4double fElectronicsBackCuHalfLength ;
       G4double fElectronicsBackBrassHalfLength;
       G4double fElectronicsBackTeflonHalfLength;
};

#endif
