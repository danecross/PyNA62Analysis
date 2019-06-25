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
// --------------------------------------------------------------
// History:
//
// 2016-05-06 E Goudzovski (eg@hep.ph.bham.ac.uk)
// - Run-dependent BEND6 field tuning
// 
// 2014-03-01 B.Velghe (bob.velghe@cern.ch)
// - Add TRIM5 and BEND magnets mechanical structure
//
// 2009-03-12 S.Bifani
// - Added the collimator before station 3
//
// 2009-03-03 S.Bifani
// - Removed the alluminum foil
// - Changed the geometrical configuration according to the 2008/11/24 BEATCH file
//   (x -> y deflection and 5th magnet)
//
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
// - Added 3 stations and 4 magnets
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//

#include "globals.hh"
#include "GigaTrackerGeometryParameters.hh"
#include "GigaTrackerMaterialParameters.hh"
#include "GigaTrackerDetector.hh"
#include "GigaTrackerSD.hh"
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4SDManager.hh"
#include "DatacardManager.hh"
#include "BeamPipe.hh"

GigaTrackerDetector::GigaTrackerDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
  NA62VComponent(Material,MotherVolume), NA62VNamed("GigaTracker"),
  fXLength(0.0), fYLength(0.0), fZLength(0.0), fZPosition(0.0),
  fStation1(nullptr), fStation2(nullptr), fStation3(nullptr),
  fMagnet1(nullptr), fMagnet2(nullptr), fMagnet3(nullptr), fMagnet4(nullptr), fMagnet5(nullptr),
  fScraper(nullptr), fCollimator(nullptr), fGeoPars(nullptr) {
  // Mandatory here to Find or Build the needed materials
  GigaTrackerMaterialParameters::GetInstance();
}

void GigaTrackerDetector::ReadGeometryParameters() {

  // Read all the geometrical parameters and copy them to private members
  fGeoPars = GigaTrackerGeometryParameters::GetInstance();

  fGigaTrackerSensitiveDetectorName = fGeoPars->GetGigaTrackerSensitiveDetectorName();
  fGigaTrackerCollectionName = fGeoPars->GetGigaTrackerCollectionName();

  fXLength = fGeoPars->GetGigaTrackerDetectorXLength();
  fYLength = fGeoPars->GetGigaTrackerDetectorYLength();
  fZLength = fGeoPars->GetGigaTrackerDetectorZLength();
  fZPosition = fGeoPars->GetGigaTrackerDetectorZPosition();

  fStation1Position = fGeoPars->GetGigaTrackerStationPosition(0);
  fStation2Position = fGeoPars->GetGigaTrackerStationPosition(1);
  fStation3Position = fGeoPars->GetGigaTrackerStationPosition(2);

  fMagnet1Position = fGeoPars->GetGigaTrackerMagnet1Position();
  fMagnet2Position = fGeoPars->GetGigaTrackerMagnet2Position();
  fMagnet3Position = fGeoPars->GetGigaTrackerMagnet3Position();
  fMagnet4Position = fGeoPars->GetGigaTrackerMagnet4Position();
  fMagnet5Position = fGeoPars->GetGigaTrackerMagnet5Position();
  fScraperPosition = fGeoPars->GetGigaTrackerScraperMagnetPosition();

  fCollimatorPosition = fGeoPars->GetGigaTrackerCollimatorPosition();
}

void GigaTrackerDetector::CreateGeometry() {
    ReadGeometryParameters();

    // Sensitive detector: it would be associated to smaller volume/s inside the global box/es

    G4SDManager* SDman = G4SDManager::GetSDMpointer();
    GigaTrackerSD * GTKSD = static_cast<GigaTrackerSD*>(SDman->FindSensitiveDetector(fGigaTrackerSensitiveDetectorName));
    if(!GTKSD) {
        GTKSD = new GigaTrackerSD(fGigaTrackerSensitiveDetectorName, fGigaTrackerCollectionName);
        SDman->AddNewDetector(GTKSD);
    }

    // G4 volumes
    G4double HalfXLength = 0.5 * fXLength;
    G4double HalfYLength = 0.5 * fYLength;
    G4double HalfZLength = 0.5 * fZLength;
    G4ThreeVector Position(0., 0., fZPosition);

    fSolidVolume= new G4Box("GigaTracker", HalfXLength, HalfYLength, HalfZLength);

    fLogicalVolume= new G4LogicalVolume(fSolidVolume,  // solid
            fMaterial,     // material
            "GigaTracker", // name
            0,             // field manager 
            0,             // sensitive detector
            0);            // user limits

    fPhysicalVolume = new G4PVPlacement(0,
            Position,
            fLogicalVolume, // its logical volume
            "GigaTracker",  // its name
            fMotherVolume,  // its mother  volume
            false,          // no boolean operations
            0);             // copy number

    //////////////////////
    // Create the stations

    G4Material *Galactic = G4Material::GetMaterial("G4_Galactic");
    const G4int nstation = fGeoPars->GetNStation();
    G4bool statin[nstation];
    for(int i = 0;i<nstation;i++){
      statin[i] = fGeoPars->GetStationIn(i); // 1.0 -> In / 0.0 -> Out
    }

    if (statin[0]) fStation1 = new GigaTrackerStation(Galactic, fLogicalVolume, fStation1Position, 0);
    if (statin[1]) fStation2 = new GigaTrackerStation(Galactic, fLogicalVolume, fStation2Position, 1);
    if (statin[2]) fStation3 = new GigaTrackerStation(Galactic, fLogicalVolume, fStation3Position, 2);

    G4double Bend6SF = DatacardManager::GetInstance()->GetBend6FieldScaleFactor();
    G4double Trim5SF = DatacardManager::GetInstance()->GetTrim5FieldScaleFactor();
    G4cout << "[GigaTrackerDetector] Bend6, Trim5 field scale factors = " <<
      Form("%6.4f", Bend6SF) << " " << Form("%6.4f", Trim5SF) << G4endl;

    fMagnet1 = new GigaTrackerMCBMagnet(Galactic, fLogicalVolume, fMagnet1Position, 0, 0, 1.0);
    fMagnet2 = new GigaTrackerMCBMagnet(Galactic, fLogicalVolume, fMagnet2Position, 1, 0, 1.0);
    fMagnet3 = new GigaTrackerMCBMagnet(Galactic, fLogicalVolume, fMagnet3Position, 2, 0, 1.0);
    fMagnet4 = new GigaTrackerMCBMagnet(Galactic, fLogicalVolume, fMagnet4Position, 3, 1, Bend6SF); // yoke up
    fMagnet5 = new GigaTrackerMDXMagnet(Galactic, fLogicalVolume, fMagnet5Position, 0, Trim5SF);
    fScraper = new GigaTrackerScraperMagnet(Galactic, fLogicalVolume, fScraperPosition);

    // Create the collimator
    fCollimator = new GigaTrackerCollimator(G4Material::GetMaterial("G4_Fe"), fLogicalVolume, fCollimatorPosition, 0);

    SetProperties();
}

void GigaTrackerDetector::SetProperties() {
  // Set visualization properties
  fVisAtt = new G4VisAttributes(G4Colour(1.0, 1.0, 1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
