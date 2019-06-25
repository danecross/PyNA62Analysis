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

// --------------------------------------------------------------------
// History:
//
// Created by   Antonino Sergi (Antonino.Sergi@cern.ch) 
//              Spasimir Balev (Spasimir.Balev@cern.ch)
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) March 2014
//
// --------------------------------------------------------------------

/// \class MUV3Detector
/// \Brief
/// Construction of objects within the MUV3 responsibility region
/// \EndBrief

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4Material.hh"

#include "MUV3GeometryParameters.hh"
#include "MUV3MaterialParameters.hh"
#include "MUV3Detector.hh"
#include "MUV3SD.hh"
#include "G4SDManager.hh"

MUV3Detector::MUV3Detector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
  NA62VComponent(Material,MotherVolume), NA62VNamed("MUV3"),
  fResponsibilityRegionXLength(0),
  fResponsibilityRegionYLength(0),
  fResponsibilityRegionZStart(0),
  fResponsibilityRegionZEnd(0),
  fFiscZStart(0),
  fFeWallZStart(0),
  fFrontPlateZStart(0),
  fScintillatorZStart(0),
  fBackPlateZStart(0),
  fFrontPlateThickness(0),
  fBackPlateThickness(0),
  fModuleZLength(0),
  fBeamPipeInnerRadius(0),
  fBeamPipeOuterRadius(0),
  fActiveInnerRadius(0),
  fPassiveInnerRadius(0),
  fFeWallThickness(0),
  fGapWidth(0),
  fNModulesX(0),
  fNModulesY(0),
  fLargeModuleSize(0),
  fSmallModuleSize(0),
  fIronWall(nullptr),
  fFrontPlate(nullptr),
  fBackPlate(nullptr)
{
  MUV3MaterialParameters::GetInstance();
}

void MUV3Detector::ReadGeometryParameters() {

  MUV3GeometryParameters* GeoPars = MUV3GeometryParameters::GetInstance();

  fResponsibilityRegionXLength = GeoPars->GetResponsibilityRegionXLength();
  fResponsibilityRegionYLength = GeoPars->GetResponsibilityRegionYLength();
  fResponsibilityRegionZStart  = GeoPars->GetResponsibilityRegionZStart();
  fResponsibilityRegionZEnd    = GeoPars->GetResponsibilityRegionZEnd();

  fFiscZStart          = GeoPars->GetFiscZStart();
  fFeWallZStart        = GeoPars->GetFeWallZStart();
  fFeWallThickness     = GeoPars->GetFeWallThickness();
  fFrontPlateZStart    = GeoPars->GetFrontPlateZStart();
  fBackPlateZStart     = GeoPars->GetBackPlateZStart();
  fScintillatorZStart  = GeoPars->GetScintillatorZStart();
  fModuleZLength       = GeoPars->GetModuleZLength();
  fFrontPlateThickness = GeoPars->GetFrontPlateThickness();
  fBackPlateThickness  = GeoPars->GetBackPlateThickness();
  fBeamPipeInnerRadius = GeoPars->GetBeamPipeInnerRadius(0);
  fBeamPipeOuterRadius = GeoPars->GetBeamPipeOuterRadius(0);
  fActiveInnerRadius   = GeoPars->GetActiveInnerRadius();
  fPassiveInnerRadius  = GeoPars->GetPassiveInnerRadius();
  fGapWidth            = GeoPars->GetGapWidth();
  fNModulesX           = GeoPars->GetNModulesX();
  fNModulesY           = GeoPars->GetNModulesY();
  fLargeModuleSize     = GeoPars->GetLargeModuleSize();
  fSmallModuleSize     = GeoPars->GetSmallModuleSize();
}

void MUV3Detector::CreateGeometry() {
  ReadGeometryParameters();

  ////////////////////////////////
  // Define the sensitive detector

  G4SDManager* SDManager = G4SDManager::GetSDMpointer();
  G4String     SensitiveDetectorName = "/MUV3";
  G4String     CollectionName = "MUV3Collection";
  MUV3SD*      SD = static_cast<MUV3SD*>(SDManager->FindSensitiveDetector(SensitiveDetectorName));
  if (!SD) {
    SD = new MUV3SD(SensitiveDetectorName, CollectionName);
    SDManager->AddNewDetector(SD);
  }

  ////////////////////////////////////////
  // Create the MUV3 responsibility region

  G4double ResponsibilityRegionZLength = fResponsibilityRegionZEnd - fResponsibilityRegionZStart;
  G4double ResponsibilityRegionZCentre = fResponsibilityRegionZStart + 0.5*ResponsibilityRegionZLength;

  fSolidVolume = new G4Box
    ("MUV3",
     0.5*fResponsibilityRegionXLength,
     0.5*fResponsibilityRegionYLength,
     0.5*ResponsibilityRegionZLength);

  fLogicalVolume = new G4LogicalVolume
    (fSolidVolume, fMaterial, "MUV3", 0, 0, 0);

  fPhysicalVolume = new G4PVPlacement
    (0, G4ThreeVector(0.0,0.0,ResponsibilityRegionZCentre),
     fLogicalVolume, "MUV3", fMotherVolume, false, 0);

  ////////////
  // Iron wall

  G4ThreeVector FeWallPosition = G4ThreeVector
    (0.0, 0.0, fFeWallZStart + 0.5*fFeWallThickness - ResponsibilityRegionZCentre);

  fIronWall = new MUV3IronWall
    (G4Material::GetMaterial("G4_Fe"), fLogicalVolume, FeWallPosition);

  ////////////////////////////
  // Beam pipe & FISC5 counter

  G4Tubs *BeamPipeSolid =
    new G4Tubs("BeamPipeSolid", fBeamPipeInnerRadius, fBeamPipeOuterRadius,
	       0.5*ResponsibilityRegionZLength, 0.0, 360*deg);
  G4LogicalVolume *BeamPipeLogical =
    new G4LogicalVolume(BeamPipeSolid, G4Material::GetMaterial("G4_Al"), "MUV3BeamPipe");
  new G4PVPlacement
    (0, G4ThreeVector(0,0,0), BeamPipeLogical, "MUV3BeamPipe", fLogicalVolume, false, 0);

  // The FISC is a vacuum cyliners of 10mm thickness.
  // It is required to generate a checkpoint for "true" beam parameter diagnostics.

  G4Tubs *FiscSolid = new G4Tubs("FiscSolid", 0.0, fBeamPipeInnerRadius, 5.0*mm, 0.0, 360*deg);
  G4LogicalVolume *FiscLogical =
    new G4LogicalVolume(FiscSolid, G4Material::GetMaterial("G4_Galactic"), "FISC5");
  G4ThreeVector FiscPosition = G4ThreeVector
    (0.0, 0.0, fFiscZStart + 5.0*mm - ResponsibilityRegionZCentre);
  new G4PVPlacement (0, FiscPosition, FiscLogical, "FISC5", fLogicalVolume, false, 0);

  //////////////////////
  // Front & back plates

  G4double FrontPlateZPosition = fFrontPlateZStart + 0.5*fFrontPlateThickness - ResponsibilityRegionZCentre;
  G4double BackPlateZPosition  = fBackPlateZStart  + 0.5*fBackPlateThickness  - ResponsibilityRegionZCentre;

  fFrontPlate = new MUV3Plate
    (G4Material::GetMaterial("G4_Al"), fLogicalVolume,
     FrontPlateZPosition, fFrontPlateThickness, "MUV3FrontPlate");
  fBackPlate = new MUV3Plate
    (G4Material::GetMaterial("G4_Fe"), fLogicalVolume,
     BackPlateZPosition, fBackPlateThickness, "MUV3BackPlate");

  //////////////////////////////////////
  // Semi-cylinders around the beam pipe

  G4Tubs *SemiCylinderSolid =
    new G4Tubs("SemiCylinderSolid", fPassiveInnerRadius, fActiveInnerRadius,
	       0.5*fModuleZLength, 90*deg, 180*deg);

  G4LogicalVolume *SemiCylinderLogical =
    new G4LogicalVolume(SemiCylinderSolid, G4Material::GetMaterial("G4_Al"), "MUV3SemiCylinder");

  G4ThreeVector SemiCylinderLeftPosition = G4ThreeVector
    (-0.5*fGapWidth, 0.0,
     fFrontPlateZStart + fFrontPlateThickness + 0.5*fModuleZLength - ResponsibilityRegionZCentre);

  G4ThreeVector SemiCylinderRightPosition = G4ThreeVector
    (+0.5*fGapWidth, 0.0,
     fFrontPlateZStart + fFrontPlateThickness + 0.5*fModuleZLength - ResponsibilityRegionZCentre);

  new G4PVPlacement
    (0, SemiCylinderLeftPosition, SemiCylinderLogical,
      "MUV3SemiCylinderLeft", fLogicalVolume, false, 0);

  G4RotationMatrix *RotationSemiCylinder = new G4RotationMatrix;
  RotationSemiCylinder->rotateZ(180*deg);

  new G4PVPlacement
    (RotationSemiCylinder, SemiCylinderRightPosition, SemiCylinderLogical,
      "MUV3SemiCylinderRight", fLogicalVolume, false, 0);

  /////////////////////////////////////////////////////////////////
  //
  // Place the detector modules
  // A module = coated scintillator layer followed by the light box
  //
  /////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////
  //
  // PAD NUMBERING SCHEME:
  //
  // The 140 large modules (except for the central 440mm x 440mm area)
  // are numbered from 0 to 143 (positions 65, 66, 77 and 78 are empty).
  // The numbering is sequential from negative X (Saleve) to positive X (Jura) within a row.
  // The rows are numbered sequentially from bottom to top.
  // Therefore the IDs of vertically adjecent large pads differ by 12.
  //
  // The 8 small central modules are numbered from 144 to 151,
  // sequentially from Saleve to Jura and from bottom to top.
  //
  /////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////////////////////
  // Put a 12x12 arrangement of large modules (module type 0, transverse size: 220mm x 220mm),
  // leaving the central 440x440 mm area empty.
  // This makes 12*12-4 = 140 modules in total.

  G4double zModule = fScintillatorZStart + 0.5*fModuleZLength - ResponsibilityRegionZCentre;

  for (G4int i=0; i<fNModulesY; i++) { // rows

    G4double yModule = (i-5.5)*fLargeModuleSize;

    for (G4int j=0; j<fNModulesX; j++) { // columns
      if ((i==5 || i==6) && (j==5 || j==6)) continue; // skip 4 central modules

      G4double xModule = (j-5.5)*fLargeModuleSize;
      if (j<6) xModule -= (0.5*fGapWidth);
      else     xModule += (0.5*fGapWidth);

      G4int ModuleID = j + i*fNModulesX;
      G4ThreeVector Position = G4ThreeVector(xModule, yModule, zModule);

      new MUV3Module(G4Material::GetMaterial("G4_AIR"), fLogicalVolume, Position, 0, ModuleID);
    }
  }

  //////////////////////////////////////////
  // Put the 4 small corner modules (type 1)

  G4double xCornerPosition[4] =
    {-fSmallModuleSize-0.5*fGapWidth,
     +fSmallModuleSize+0.5*fGapWidth,
     -fSmallModuleSize-0.5*fGapWidth,
     +fSmallModuleSize+0.5*fGapWidth};

  G4double yCornerPosition[4] =
    {-fSmallModuleSize,
     -fSmallModuleSize,
     +fSmallModuleSize,
     +fSmallModuleSize};

  G4int CornerPadID[4] = {144, 146, 149, 151};
  for (G4int i=0; i<4; i++) {
    G4ThreeVector Position = G4ThreeVector(xCornerPosition[i], yCornerPosition[i], zModule);
    new MUV3Module(G4Material::GetMaterial("G4_AIR"), fLogicalVolume, Position, 1, CornerPadID[i]);
  }

  ///////////////////////////////////////////////////////////////////////////
  // Put the 4 small side modules (left halves: type 2, right halves: type 3)

  G4double xSidePositionL[4] =
    {-0.5*fGapWidth-0.25*fSmallModuleSize,
     +0.5*fGapWidth+0.25*fSmallModuleSize,
     -0.5*fGapWidth-fSmallModuleSize,
     +0.5*fGapWidth+fSmallModuleSize};
  G4double xSidePositionR[4] =
    {+0.5*fGapWidth+0.25*fSmallModuleSize,
     -0.5*fGapWidth-0.25*fSmallModuleSize,
     -0.5*fGapWidth-fSmallModuleSize,
     +0.5*fGapWidth+fSmallModuleSize};
  G4double ySidePositionL[4] =
    {+fSmallModuleSize,
     -fSmallModuleSize,
     -0.25*fSmallModuleSize,
     +0.25*fSmallModuleSize};
  G4double ySidePositionR[4] =
    {+fSmallModuleSize,
     -fSmallModuleSize,
     +0.25*fSmallModuleSize,
     -0.25*fSmallModuleSize};

  G4int SidePadID[4] = {150, 145, 147, 148};
  for (G4int i=0; i<4; i++) {
    G4ThreeVector PositionL = G4ThreeVector(xSidePositionL[i],ySidePositionL[i],zModule);
    G4ThreeVector PositionR = G4ThreeVector(xSidePositionR[i],ySidePositionR[i],zModule);
    new MUV3Module(G4Material::GetMaterial("G4_AIR"),fLogicalVolume,PositionL,2,SidePadID[i]);
    new MUV3Module(G4Material::GetMaterial("G4_AIR"),fLogicalVolume,PositionR,3,SidePadID[i]);
  }

  SetProperties();
}

void MUV3Detector::SetProperties() {
  fVisAtt= new G4VisAttributes(G4Colour(G4Colour::Yellow()));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
