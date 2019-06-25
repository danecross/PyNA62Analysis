// --------------------------------------------------------------------
// History:
//
// 2016-05-09 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - The real scintillator geometry; added frame and covers
//
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Updated to be consistent with the NewCHOD simulation
//
// Created by Giuseppe Ruggiero 04-09-2012 
//
// --------------------------------------------------------------------

/// \class MUV3Detector
/// \Brief
/// Construction of objects within the MUV0 responsibility region
/// \EndBrief

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4SubtractionSolid.hh"
#include "G4SDManager.hh"
#include "MUV0Detector.hh"

MUV0Detector::MUV0Detector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
  NA62VComponent(Material,MotherVolume), NA62VNamed("MUV0"),
  fRespRegionXCentre(.0),
  fRespRegionZCentre(.0),
  fRespRegionXLength(.0),
  fRespRegionYLength(.0),
  fRespRegionZLength(.0),
  fNCounters(0),
  fZPosition(.0),
  fScintillatorThickness(.0),
  fFrameInnerSize(.0),
  fFrameThickness(.0),
  fCoverThickness(.0)
{
  MUV0MaterialParameters::GetInstance();
}

void MUV0Detector::ReadGeometryParameters() {

  MUV0GeometryParameters* GeoPars = MUV0GeometryParameters::GetInstance();

  fRespRegionXCentre     = GeoPars->GetRespRegionXCentre();
  fRespRegionZCentre     = GeoPars->GetRespRegionZCentre();
  fRespRegionXLength     = GeoPars->GetRespRegionXLength();
  fRespRegionYLength     = GeoPars->GetRespRegionYLength();
  fRespRegionZLength     = GeoPars->GetRespRegionZLength();
  fScintillatorThickness = GeoPars->GetScintillatorThickness();
  fZPosition             = GeoPars->GetDetectorZPosition() - fRespRegionZCentre;
  fNCounters             = GeoPars->GetNCounters();
  for (G4int i=0; i<fNCounters; i++) {
    fScintillatorSize[i]     = GeoPars->GetScintillatorSize(i);
    fScintillatorPosition[i] = GeoPars->GetScintillatorPosition(i);
  }
  fFrameInnerSize = GeoPars->GetFrameInnerSize();
  fFrameThickness = GeoPars->GetFrameThickness();
  fCoverThickness = GeoPars->GetCoverThickness();
}

void MUV0Detector::CreateGeometry() {
  ReadGeometryParameters();

  ////////////////////////////////
  // Define the sensitive detector

  G4SDManager* SDManager = G4SDManager::GetSDMpointer();
  G4String     SensitiveDetectorName = "/MUV0";
  G4String     CollectionName = "MUV0Collection";
  MUV0SD*      SD = static_cast<MUV0SD*>(SDManager->FindSensitiveDetector(SensitiveDetectorName));
  if (!SD) {
    SD = new MUV0SD(SensitiveDetectorName, CollectionName);
    SDManager->AddNewDetector(SD);
  }

  ////////////////////////////////////////
  // Create the MUV0 responsibility region

  fSolidVolume = new G4Box
    ("MUV0", 0.5*fRespRegionXLength, 0.5*fRespRegionYLength, 0.5*fRespRegionZLength);
  fLogicalVolume = new G4LogicalVolume (fSolidVolume, fMaterial, "MUV0RespReg", 0, 0, 0);
  fPhysicalVolume = new G4PVPlacement
    (0, G4ThreeVector(fRespRegionXCentre, 0.0, fRespRegionZCentre),
     fLogicalVolume, "MUV0RespReg", fMotherVolume, false, 0);

  ////////////////////////////
  // Create the detector frame

  G4Box *FrameOuterBox = new G4Box
    ("FrameOuterBox",
     0.5*fFrameInnerSize + fFrameThickness,
     0.5*fFrameInnerSize + fFrameThickness,
     0.5*fScintillatorThickness);
  G4Box *FrameInnerBox = new G4Box
    ("FrameInnerBox", 0.5*fFrameInnerSize, 0.5*fFrameInnerSize, 0.55*fScintillatorThickness);
  G4SubtractionSolid *WholeFrameSolid = new G4SubtractionSolid
    ("MUV0WholeFrame", FrameOuterBox, FrameInnerBox, 0, G4ThreeVector(0.0,0.0,0.0));
  G4SubtractionSolid *FrameSolid = new G4SubtractionSolid // remove the corner
    ("MUV0Frame", WholeFrameSolid, FrameInnerBox, 0,
     G4ThreeVector(6.0/7.0*fFrameInnerSize, -6.0/7.0*fFrameInnerSize, 0.0));

  G4LogicalVolume *FrameLogical = new G4LogicalVolume
    (FrameSolid, G4Material::GetMaterial("MUV0_StainlessSteel"), "MUV0Frame");
  new G4PVPlacement
    (0, G4ThreeVector(0.0, 0.0, fZPosition), FrameLogical, "MUV0Frame", fLogicalVolume, false, 0);

  //////////////////////////
  // Create the cover sheets

  G4Box *CoverBox1 = new G4Box
    ("CoverBox1",
     0.5*fFrameInnerSize + fFrameThickness,
     0.5*fFrameInnerSize + fFrameThickness,
     0.5*fCoverThickness);

  G4Box *CoverBox2 = new G4Box
    ("CoverBox2",
     0.5*fFrameInnerSize + fFrameThickness,
     0.5*fFrameInnerSize + fFrameThickness,
     0.55*fCoverThickness);

  G4SubtractionSolid *CoverSolid = new G4SubtractionSolid
    ("MUV0Cover", CoverBox1, CoverBox2, 0,
     G4ThreeVector(6.0/7.0*fFrameInnerSize+fFrameThickness, -6.0/7.0*fFrameInnerSize-fFrameThickness, 0));

  G4LogicalVolume *CoverLogical = new G4LogicalVolume
    (CoverSolid, G4Material::GetMaterial("G4_Al"), "MUV0Cover");

  new G4PVPlacement
    (0, G4ThreeVector(0.0, 0.0, fZPosition-0.5*fScintillatorThickness-0.5*fCoverThickness),
     CoverLogical, "MUV0Cover1", fLogicalVolume, false, 0);
  new G4PVPlacement
    (0, G4ThreeVector(0.0, 0.0, fZPosition+0.5*fScintillatorThickness+0.5*fCoverThickness),
     CoverLogical, "MUV0Cover2", fLogicalVolume, false, 0);

  //////////////////////////////
  // Create an array of counters
  
  for (G4int i=0; i<fNCounters; i++) {
    new MUV0ScintillatorCounter
      (G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"),
       fLogicalVolume, fScintillatorSize[i], fScintillatorPosition[i], i+1);
  }

  SetProperties();
}

void MUV0Detector::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
