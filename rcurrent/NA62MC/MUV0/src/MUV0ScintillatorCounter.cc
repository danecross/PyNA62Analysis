// --------------------------------------------------------------------
// History:
//
// 2016-05-09 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - The real scintillator dimensions (pads, not stripes)
//
// Created by Giuseppe Ruggiero 2012-09-04
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4Material.hh"
#include "G4SubtractionSolid.hh"

#include "MUV0GeometryParameters.hh"
#include "MUV0MaterialParameters.hh"
#include "MUV0ScintillatorCounter.hh"
#include "MUV0SD.hh"
#include "G4SDManager.hh"

MUV0ScintillatorCounter::MUV0ScintillatorCounter
(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Size, G4ThreeVector Position, G4int ID) :
  NA62VComponent(Material,MotherVolume),
  fID      (ID),
  fName    (Form("MUV0Counter%d", ID+1)),
  fSize    (Size),
  fPosition(Position)
{
  MUV0MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV0ScintillatorCounter::CreateGeometry() {

  if (fID != 7) { // regular counters
    fSolidVolume = new G4Box (fName, 0.5*fSize.x(), 0.5*fSize.y(), 0.5*fSize.z());
  }
  else { // the irregular corner counter #7
    G4Box *Box1 = new G4Box("Box1", 0.5*fSize.x(), 0.5*fSize.y(), 0.50*fSize.z());
    G4Box *Box2 = new G4Box("Box2", 0.5*fSize.x(), 0.5*fSize.y(), 0.55*fSize.z());
    fSolidVolume = new G4SubtractionSolid
      (fName, Box1, Box2, 0, G4ThreeVector(0.5*fSize.x(), -0.5*fSize.y(), 0));
  }

  fLogicalVolume  = new G4LogicalVolume(fSolidVolume, fMaterial, fName, 0, 0, 0);
  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, fName, fMotherVolume, false, fID);

  // Define scintillator as a sensitive volume
  G4SDManager* SDmanager = G4SDManager::GetSDMpointer();
  G4VSensitiveDetector* Muv0SD = SDmanager->FindSensitiveDetector("/MUV0");
  fLogicalVolume->SetSensitiveDetector(Muv0SD);
}

void MUV0ScintillatorCounter::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
