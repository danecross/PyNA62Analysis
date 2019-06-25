// ---------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (Evgueni.Goudzovski@cern.ch) 2016-02-16
// ---------------------------------------------------------------------

/// \class GigaTrackerScraperMagnet
/// \Brief
/// The beam scraper magnet upstream of GTK2
/// \EndBrief
/// \Detailed
/// The geometry is simulated following the "CERN Magnets Kit" by L.Gatignon (reference: CERN-OPEN-2004-03), Fig.2.
/// The geometry parameters are taken from NA62MC/Beam/datacard/halo_datacard_k12hika+.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"
#include "G4UnionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "GigaTrackerScraperMagnet.hh"

GigaTrackerScraperMagnet::GigaTrackerScraperMagnet
(G4Material* Material, G4LogicalVolume* MotherVolume, G4ThreeVector Position) :
  NA62VComponent(Material, MotherVolume),
  fPosition(Position)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

void GigaTrackerScraperMagnet::ReadGeometryParameters() {
  GigaTrackerGeometryParameters* GeoPars = GigaTrackerGeometryParameters::GetInstance();
  fZReference         = GeoPars->GetGigaTrackerDetectorZPosition();
  fZLength            = GeoPars->GetGigaTrackerScraperMagnetZLength();
  fApertureHalfWidth  = GeoPars->GetGigaTrackerScraperMagnetApertureHalfWidth();
  fApertureHalfHeight = GeoPars->GetGigaTrackerScraperMagnetApertureHalfHeight();
  fOverallHalfHeight  = GeoPars->GetGigaTrackerScraperMagnetOverallHalfHeight();
  fFieldStrength      = GeoPars->GetGigaTrackerScraperMagnetFieldStrength();
}

///////////////////////////////////////////////////////////////////////////////////////////
// The geometry follows the "CERN Magnets Kit" by L.Gatignon (reference: CERN-OPEN-2004-03)

void GigaTrackerScraperMagnet::CreateGeometry() {

  // Build the two sections of rectangular cross-section
  G4Box *SolidBox1 = new G4Box("GigaTrackerScraperBox1",     fApertureHalfWidth, fOverallHalfHeight,  0.50*fZLength);
  G4Box *SolidBox2 = new G4Box("GigaTrackerScraperBox2", 1.1*fApertureHalfWidth, fApertureHalfHeight, 0.51*fZLength);
  G4SubtractionSolid *SolidBox = new G4SubtractionSolid("GigaTrackerScraperBox", SolidBox1, SolidBox2);

  // Add the two "arches" on the sides
  G4Tubs *SolidTubeLeft = new G4Tubs
    ("GigaTrackerScraperTubeLeft",  fApertureHalfHeight, fOverallHalfHeight, 0.5*fZLength, +90*deg, 180*deg);
  G4Tubs *SolidTubeRight = new G4Tubs
    ("GigaTrackerScraperTubeRight", fApertureHalfHeight, fOverallHalfHeight, 0.5*fZLength, -90*deg, 180*deg);
  G4UnionSolid *solid1 = new G4UnionSolid
    ("solid1", SolidBox, SolidTubeLeft,  0, G4ThreeVector(-fApertureHalfWidth, 0.0, 0.0));
  G4UnionSolid *solid2 = new G4UnionSolid
    ("solid2", solid1,   SolidTubeRight, 0, G4ThreeVector(+fApertureHalfWidth, 0.0, 0.0));

  // Place the physical volume  
  fLogicalVolume = new G4LogicalVolume
    (solid2, G4Material::GetMaterial("G4_Fe"), "GigaTrackerScraper", 0, 0, 0);
  fPhysicalVolume = new G4PVPlacement
    (0, fPosition, fLogicalVolume, "GigaTrackerScraperIron", fMotherVolume, false, 0);

  // Add a toroidal magnetic field
  fMagField = new GigaTrackerScraperField
    (fZReference, fPosition, fZLength,
    fApertureHalfWidth, fApertureHalfHeight, fOverallHalfHeight, fFieldStrength);
  fFieldMgr = new G4FieldManager(fMagField);
  fFieldMgr->SetDetectorField(fMagField);
  fFieldMgr->CreateChordFinder(fMagField);
  fLogicalVolume->SetFieldManager(fFieldMgr, true);
}
