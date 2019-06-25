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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4RotationMatrix.hh"

#include "G4SDManager.hh"
#include "CHANTIGeometryParameters.hh"
#include "CHANTIMaterialParameters.hh"
#include "CHANTIDetectorMessenger.hh"
#include "CHANTIDetector.hh"
#include "CHANTIStation.hh"
#include "CHANTIFrame.hh"
#include "CHANTIVessel.hh"
#include "CHANTIRing.hh"
#include "CHANTISD.hh"

CHANTIDetector::CHANTIDetector(G4Material * Material, G4LogicalVolume * MotherVolume) :
  NA62VComponent(Material,MotherVolume),fVesselStatus(0),fSiliconRingStatus(0) {
  // Connect to CHANTIDetectorMessenger to enable datacard configuration
  fCHANTIMessenger = new CHANTIDetectorMessenger(this);
  //Read Geometry
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  CHANTIMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

CHANTIDetector::~CHANTIDetector() {
  delete fCHANTIMessenger;
}

void CHANTIDetector::ReadGeometryParameters() {
  // Read all the geometrical parameters and copy them to private members
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();

  fXLength = GeoPars->GetCHANTIDetectorXLength();
  fYLength = GeoPars->GetCHANTIDetectorYLength();
  fZLength = GeoPars->GetCHANTIDetectorZLength();

  fZPosition = GeoPars->GetCHANTIDetectorZPosition();
  fWordZHalfLength = 0.5*GeoPars->GetCHANTIDetectorZLength();

  fCHANTISensitiveDetectorName = GeoPars->GetCHANTISensitiveDetectorName();
  fCHANTICollectionName = GeoPars->GetCHANTICollectionName();
}

void CHANTIDetector::CreateGeometry() {
  // Sensitive detector: it would be associated to smaller volume/s insid the global box/es
  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();

  fNStation = GeoPars->GetCHANTINumberOfStation();
  CHANTISD *Anti0SD =
    static_cast<CHANTISD*>(SDman->FindSensitiveDetector(fCHANTISensitiveDetectorName));
  if (!Anti0SD) {
    Anti0SD = new CHANTISD(fCHANTISensitiveDetectorName, fCHANTICollectionName);
    SDman->AddNewDetector(Anti0SD);
  }

  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region

  G4double HalfZLength = 0.5*fZLength;
  G4double HalfXLength = 0.5*fXLength;
  G4double HalfYLength = 0.5*fYLength;

  fSolidVolume   = new G4Box("CHANTI",
			     HalfXLength,
			     HalfYLength,
			     HalfZLength);

  fLogicalVolume = new G4LogicalVolume
    (fSolidVolume,    // solid
     fMaterial,       // material
     "CHANTI",        // name
     0,               // field manager 
     0,               // sensitive detector
     0);              // user limits

  fPhysicalVolume = new G4PVPlacement
    (0,
     G4ThreeVector(0.*mm,0.,fZPosition), // its position
     fLogicalVolume,                     // its logical volume
     "CHANTI",                           // its name
     fMotherVolume,                      // its mother  volume
     false,                              // no boolean operations
     0);                                 // copy number

  G4RotationMatrix* Rotation = new G4RotationMatrix();
  Rotation->rotateZ(90.*deg);

  // creation of six stations
  for(int station=0; station<fNStation; station++){
    new CHANTIStation(G4Material::GetMaterial("G4_Galactic"),
		      fLogicalVolume, station);
    new CHANTIFrame(G4Material::GetMaterial("G4_Galactic"),
		    fLogicalVolume, station, this);
  }

  if (GetVesselStatus()==1)
    new CHANTIVessel(G4Material::GetMaterial("G4_Galactic"), fLogicalVolume); 
}

void CHANTIDetector::SetProperties() {
  // Set visualization properties
  fVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
