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
// 2008-04-23 A.Sergi  Added 4 Chambers
//
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// Modified by Giuseppe Ruggiero 2011-03-03
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "G4FieldManager.hh"
#include "G4PropagatorInField.hh"
#include "G4MagIntegratorStepper.hh"
#include "G4ChordFinder.hh"
#include "G4MagneticField.hh"
#include "G4ElectroMagneticField.hh"
#include "G4Mag_EqRhs.hh"
#include "G4Mag_SpinEqRhs.hh"
#include "G4EqMagElectricField.hh"
#include "G4EqEMFieldWithSpin.hh"
#include "G4ClassicalRK4.hh"
#include "G4MagIntegratorDriver.hh"

#include "SpectrometerGeometryParameters.hh"
#include "SpectrometerMaterialParameters.hh"

#include "SpectrometerDetector.hh"
#include "SpectrometerSD.hh"
#include "G4SDManager.hh"

#include "Chamber.hh"
#include "SpectrometerMagnet.hh"

SpectrometerDetector::SpectrometerDetector(G4Material * Material, G4LogicalVolume * MotherVolume) :
  NA62VComponent(Material,MotherVolume), NA62VNamed("Spectrometer"),
  fChamberZLength(0),
  fMagnetZPosition(0)
{
  fFieldManager = 0;
  SpectrometerMaterialParameters::GetInstance();
}

SpectrometerDetector::~SpectrometerDetector() {
  if (fFieldManager) delete fFieldManager;
}

void SpectrometerDetector::ReadGeometryParameters() {
  SpectrometerGeometryParameters* GeoPars = SpectrometerGeometryParameters::GetInstance();

  // Parameters of the responsibility regions
  for (G4int iRR=0; iRR<4; iRR++) {
    fRR_ZofFrontFace[iRR] = GeoPars->GetResponsibilityRegionZofFrontFace(iRR);
    fRR_ZofBackFace[iRR]  = GeoPars->GetResponsibilityRegionZofBackFace(iRR);
    fRR_ZofCenter[iRR]    = 0.5*(fRR_ZofFrontFace[iRR]+fRR_ZofBackFace[iRR]);
    fRR_Length[iRR]       = fRR_ZofBackFace[iRR] - fRR_ZofFrontFace[iRR];
    fRR_Radius[iRR]       = GeoPars->GetResponsibilityRegionRadius(iRR);
  }

  // Parameters of the chambers
  fChamberZLength  = GeoPars->GetChamberZLength();
  fMagnetZPosition = GeoPars->GetMagnetZPosition();
  for (G4int i=0; i<4; i++) {
    fChamberZCenter[i]       = GeoPars->GetChamberZCenter(i);
    fChamberXDisplacement[i] = GeoPars->GetChamberXDisplacement(i);
  }
}

void SpectrometerDetector::CreateGeometry() {
  ReadGeometryParameters();

  /////////////////////////////////////////////////////////////////
  // Sensitive detector must be instantiated and registered here
  // because each straw will search for it to attach it to the gas

  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String SensitiveDetectorName = "/Spectrometer";
  G4String CollectionName= "SpectrometerCollection";
  SpectrometerSD* spectrometerSD =
    static_cast<SpectrometerSD*>(SDman->FindSensitiveDetector(SensitiveDetectorName));
  if (!spectrometerSD) {
    spectrometerSD = new SpectrometerSD(SensitiveDetectorName, CollectionName);
    SDman->AddNewDetector(spectrometerSD);
  }

  ////////////////////////////////////////////////////////////////////////
  // Create responsibility regions and place them inside the mother volume

  G4Tubs* rrSolid[4];
  G4LogicalVolume* rrLogicalVolumes[4];
  for (G4int iRR=0; iRR<4; iRR++) {
    G4String Name = Form("SpecRR%d", iRR);
    rrSolid[iRR] = new G4Tubs(Name, 0, fRR_Radius[iRR], 0.5*fRR_Length[iRR], 0*deg, 360*deg);
    rrLogicalVolumes[iRR] = new G4LogicalVolume(rrSolid[iRR], fMaterial, Name, 0, 0, 0);
    rrLogicalVolumes[iRR]->SetVisAttributes(G4VisAttributes::Invisible);
    new G4PVPlacement(0, G4ThreeVector(0.0, 0.0, fRR_ZofCenter[iRR]), rrLogicalVolumes[iRR],
                      Form("Spectrometer_RR%d", iRR), fMotherVolume, false, iRR);
  }

  /////////////////////////////////////////////////////////////////
  // Put the chambers into the corresponding responsibility regions

  for (G4int ich=0; ich<4; ich++) { // chamber index = responsibility region index
    new Chamber
      (fMaterial, rrLogicalVolumes[ich],
       G4ThreeVector(0.0, 0.0, fChamberZCenter[ich]-fRR_ZofCenter[ich]),
       fChamberXDisplacement[ich], ich);
  }

  ////////////////////////////////////////////////////////////////////////
  // Put the MNP33 magnet into responsibility region 1.
  // |UniformFieldScaleFactor| < 0.001 means uniform field is not simulated.
  // There is currently no possibility to revert the field polarity.

  G4double UniformFieldScaleFactor = 0.0;
  if (!MagneticField::GetInstance()->GetMNP33FieldMode()) { // uniform field required
    UniformFieldScaleFactor = MagneticField::GetInstance()->GetMNP33FieldScale();
  }
  new SpectrometerMagnet
    (fMaterial, rrLogicalVolumes[1],
     G4ThreeVector(0.0, 0.0, fMagnetZPosition-fRR_ZofCenter[1]), UniformFieldScaleFactor);

  //////////////////////////////////////////////////////////////////////////////
  // Simulate the detailed MNP33 + fringe field map in RR1 if required.
  // The fringe field of the MNP33 is present in Spectrometer RR0.
  // This field is also present in LAV RR2, i.e. downstream of Spectrometer RR1,
  // spanning into Spectrometer RR2.

  if (MagneticField::GetInstance()->GetMNP33FieldMode() && // detailed field map requested
      fabs(MagneticField::GetInstance()->GetMNP33FieldScale())>0.001) {

    G4Mag_SpinEqRhs *equation = new G4Mag_SpinEqRhs(MagneticField::GetInstance());
    G4MagIntegratorStepper *stepper = new G4ClassicalRK4(equation, 12); // 12: mag field
    G4MagInt_Driver* driver = new G4MagInt_Driver(1.0e-3*mm, stepper,
                                                  stepper->GetNumberOfVariables());
    G4ChordFinder* finder = new G4ChordFinder(driver);
    fFieldManager = new G4FieldManager(MagneticField::GetInstance());
    fFieldManager->SetDeltaOneStep(1e-10); // improved tracking precision
    fFieldManager->SetMinimumEpsilonStep(1e-10);
    fFieldManager->SetMaximumEpsilonStep(1e-10);
    fFieldManager->SetDetectorField(MagneticField::GetInstance());
    fFieldManager->SetChordFinder(finder);

    rrLogicalVolumes[0]->SetFieldManager(fFieldManager, true); // Fringe field
    rrLogicalVolumes[1]->SetFieldManager(fFieldManager, true); // MNP33 field
    rrLogicalVolumes[2]->SetFieldManager(fFieldManager, true); // Fringe field
  }
}
