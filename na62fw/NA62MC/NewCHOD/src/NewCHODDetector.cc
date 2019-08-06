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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Polyhedra.hh"
#include "G4LogicalVolume.hh"
#include "G4SubtractionSolid.hh"
#include "G4PVPlacement.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"
#include "G4SDManager.hh"
#include "DatacardManager.hh"

#include "BeamPipe.hh"
#include "NewCHODGeometryParameters.hh"
#include "NewCHODMaterialParameters.hh"
#include "NewCHODDetector.hh"
#include "NewCHODScintillatorCounter.hh"
#include "NewCHODSD.hh"

NewCHODDetector::NewCHODDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume), NA62VNamed("NewCHOD") {
  NewCHODMaterialParameters::GetInstance();
}

void NewCHODDetector::ReadGeometryParameters() {

  NewCHODGeometryParameters* GeoPars = NewCHODGeometryParameters::GetInstance();

  fRespRegionZStart  = GeoPars->GetRespRegionZStart();
  fRespRegionZEnd    = GeoPars->GetRespRegionZEnd();
  fRespRegionZCentre = GeoPars->GetRespRegionZCentre();
  fRespRegionXLength = GeoPars->GetRespRegionXLength();
  fRespRegionYLength = GeoPars->GetRespRegionYLength();
  fRespRegionZLength = GeoPars->GetRespRegionZLength();

  fFiberglassThickness    = GeoPars->GetFiberglassThickness();
  fScintThickness         = GeoPars->GetScintThickness();
  fAlWindowThickness      = GeoPars->GetAlWindowThickness();
  fHoneycombThickness     = GeoPars->GetHoneycombThickness();
  fHoneycombSkinThickness = GeoPars->GetHoneycombSkinThickness();
  fHoneycombAlThickness   = GeoPars->GetHoneycombAlThickness();

  fInnerRadius           = GeoPars->GetInnerRadius(); // active area
  fOuterRadius           = GeoPars->GetOuterRadius(); // active area
  fHoneycombInnerRadius  = GeoPars->GetHoneycombInnerRadius();
  fHoneycombOuterRadius  = GeoPars->GetHoneycombOuterRadius();  // apothem of a regular octagon
  fFiberglassOuterRadius = GeoPars->GetFiberglassOuterRadius(); // apothem of a regular octagon

  fZPosition           = GeoPars->GetZPosition();
  fScintZPosition1     = GeoPars->GetScintZPosition1();
  fScintZPosition2     = GeoPars->GetScintZPosition2();
  fAlWindowZPosition1  = GeoPars->GetAlWindowZPosition1();
  fAlWindowZPosition2  = GeoPars->GetAlWindowZPosition2();
  fHoneycombZPosition  = GeoPars->GetHoneycombZPosition();

  fNCounters = GeoPars->GetNCounters();
  fScintSize = GeoPars->GetScintSize();
  for (G4int i=0; i<fNCounters; i++) {
    fScintPosition[i] = GeoPars->GetScintPosition(i);
  }
}

void NewCHODDetector::CreateGeometry() {
  ReadGeometryParameters();
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4String NewCHODSensitiveDetectorName = "/NewCHOD";
  G4String NewCHODCollectionName= "NewCHODCollection";
  NewCHODSD* sd = static_cast<NewCHODSD*>(SDman->FindSensitiveDetector(NewCHODSensitiveDetectorName));
  if (!sd) {
    sd = new NewCHODSD(NewCHODSensitiveDetectorName,NewCHODCollectionName);
    SDman->AddNewDetector(sd);
  }

  // Create the responsibility region
  fSolidVolume = new G4Box
    ("NewCHOD", 0.5*fRespRegionXLength, 0.5*fRespRegionYLength, 0.5*fRespRegionZLength);
  fLogicalVolume = new G4LogicalVolume(fSolidVolume, fMaterial, "NewCHODRespReg", 0, 0, 0);
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);
  new G4PVPlacement
    (0, G4ThreeVector(0.0, 0.0, fRespRegionZCentre), fLogicalVolume,
     "NewCHODRespReg", fMotherVolume, false, 0);

  // Beam pipe
  new BeamPipe(0, G4Material::GetMaterial("G4_Galactic"),
	       NewCHODGeometryParameters::GetInstance(), fLogicalVolume);

  ///////////////////////////////////////////////////////////////////////////////
  // The NewCHOD is NOT constructed for 2015 data: only the beam pipe is present.
  // It is however constructed in the default geometry (RunNumber=0).

  G4int RunNumber = DatacardManager::GetInstance()->GetRunNumber();
  if (RunNumber>=1560 && RunNumber<=4173) return;

  ///////////////////////////////////////////////////////////////
  // Fiberglass plate: a regular octagon with a specified apothem
  // and with a circular hole in the middle

  G4double rInner[8], rOuter1[8], rOuter2[8];
  for (G4int i=0; i<8; i++) {
    rInner[i]  = 0.0;
    rOuter1[i] = fFiberglassOuterRadius / cos(22.5*deg); // apothem --> circumradius conversion
    rOuter2[i] = fHoneycombOuterRadius  / cos(22.5*deg); // apothem --> circumradius conversion
  }

  G4double zPlanes[2] = {-0.5*fFiberglassThickness, +0.5*fFiberglassThickness};
  G4Polyhedra* FiberglassSolid1 = new G4Polyhedra
    ("FiberglassSolid1", 22.5*deg, 360*deg, 8, 2, zPlanes, rInner, rOuter1);
  G4Tubs* FiberglassSolid2 = new G4Tubs
    ("FiberglassSolid2", 0.0, fInnerRadius, 0.55*fFiberglassThickness, 0.0, 360*deg);
  G4SubtractionSolid *FiberglassSolid = new
    G4SubtractionSolid("FiberglassSolid2", FiberglassSolid1, FiberglassSolid2);

  G4LogicalVolume *FiberglassLogical = new G4LogicalVolume
    (FiberglassSolid, G4Material::GetMaterial("NemaG10"),
     "NewCHODFiberglass", 0, 0, 0);
  new G4PVPlacement
    (0, G4ThreeVector(0, 0, fZPosition),
     FiberglassLogical, "NewCHODFiberglass", fLogicalVolume, false, 0);

  ////////////////////////////////////////////////////
  // Aluminium windows covering the scintillator area:
  // their exact geometry needs to be clarified.

  G4Tubs *AlWindowSolid = new G4Tubs
    ("AlWindowSolid", fInnerRadius, fOuterRadius, 0.5*fAlWindowThickness, 0.0, 360*deg);
  G4LogicalVolume *AlWindowLogical = new G4LogicalVolume
    (AlWindowSolid, G4Material::GetMaterial("G4_Al"), "NewCHODAlWindow1", 0, 0, 0);
  new G4PVPlacement
    (0, G4ThreeVector(0, 0, fAlWindowZPosition1),
     AlWindowLogical, "NewCHODAlWindow1", fLogicalVolume, false, 0);
  new G4PVPlacement
    (0, G4ThreeVector(0, 0, fAlWindowZPosition2),
     AlWindowLogical, "NewCHODAlWindow2", fLogicalVolume, false, 1);

  ////////////////////////////////////////////////////////////////////////////
  // Honeycomb structure glued to the downstream side of the fiberglass plate:
  // fiberglass plates with alimunium in between.

  G4double zPlanesSkin[2] = {-0.5*fHoneycombSkinThickness, +0.5*fHoneycombSkinThickness};
  G4Polyhedra* HoneycombSkinSolid1 = new G4Polyhedra
    ("HoneycombSkinSolid1", 22.5*deg, 360*deg, 8, 2, zPlanesSkin, rInner, rOuter2);
  G4Tubs* HoneycombSkinSolid2 = new G4Tubs
    ("HoneycombSkinSolid2", 0.0, fHoneycombInnerRadius, 0.55*fHoneycombSkinThickness, 0.0, 360*deg);
  G4SubtractionSolid *HoneycombSkinSolid = new
    G4SubtractionSolid("HoneycombSkinSolid2", HoneycombSkinSolid1, HoneycombSkinSolid2);
  G4LogicalVolume *HoneycombSkinLogical = new G4LogicalVolume
    (HoneycombSkinSolid, G4Material::GetMaterial("NemaG10"), "HoneycombSkin", 0, 0, 0);
  new G4PVPlacement
    (0, G4ThreeVector(0, 0, fHoneycombZPosition-0.5*fHoneycombThickness+0.5*fHoneycombSkinThickness),
     HoneycombSkinLogical, "HoneycombSkin1", fLogicalVolume, false, 0);
  new G4PVPlacement
    (0, G4ThreeVector(0, 0, fHoneycombZPosition+0.5*fHoneycombThickness-0.5*fHoneycombSkinThickness),
     HoneycombSkinLogical, "HoneycombSkin2", fLogicalVolume, false, 1);

  G4double zPlanesAl[2] = {-0.5*fHoneycombAlThickness, +0.5*fHoneycombAlThickness};
  G4Polyhedra* HoneycombAlSolid1 = new G4Polyhedra
    ("HoneycombAlSolid1", 22.5*deg, 360*deg, 8, 2, zPlanesAl, rInner, rOuter2);
  G4Tubs* HoneycombAlSolid2 = new G4Tubs
    ("HoneycombAlSolid2", 0.0, fHoneycombInnerRadius, 0.55*fHoneycombAlThickness, 0.0, 360*deg);
  G4SubtractionSolid *HoneycombAlSolid = new
    G4SubtractionSolid("HoneycombAlSolid2", HoneycombAlSolid1, HoneycombAlSolid2);
  G4LogicalVolume *HoneycombAlLogical = new G4LogicalVolume
    (HoneycombAlSolid, G4Material::GetMaterial("G4_Al"), "HoneycombAluminium", 0, 0, 0);

  new G4PVPlacement
    (0, G4ThreeVector(0, 0, fHoneycombZPosition),
     HoneycombAlLogical, "HoneycombAluminium", fLogicalVolume, false, 0);

  /////////////////////////////////////////////////////////////////////
  // Place the counters. The quadrant numbering scheme is clockwise:
  // 1: Top-Jura (x>0, y>0);
  // 2: Bottom-Jura (x>0, y<0);
  // 3: Bottom-Saleve (x<0, y<0);
  // 4: Top-Saleve (x<0. y>0).
  // Geometric channel ID is 100*QuadrantID + 50*i + j,
  // where i=0,1 corresponds to the two channels per tile, and 1<=j<=38.

  for (G4int iCounter=0; iCounter<fNCounters; iCounter++) {

    G4ThreeVector Position1 = fScintPosition[iCounter];

    if (Position1.x()<1*mm) continue; // no counter with this ID

    G4ThreeVector Position2 = Position1;
    G4ThreeVector Position3 = Position1;
    G4ThreeVector Position4 = Position1;

    Position2.setY(-Position1.y());
    Position3.setX(-Position1.x());
    Position3.setY(-Position1.y());
    Position4.setX(-Position1.x());

    if (Position1.z()>fZPosition) {
      Position2.setZ(fScintZPosition1);
      Position3.setZ(fScintZPosition1);
    }
    else {
      Position2.setZ(fScintZPosition2);
      Position3.setZ(fScintZPosition2);
    }

    new NewCHODScintillatorCounter
      (G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"),
       fLogicalVolume, Position1, 100+iCounter,
       100+NewCHODGeometryParameters::GetInstance()->GetScintMap(iCounter));

    new NewCHODScintillatorCounter
      (G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"),
       fLogicalVolume, Position2, 200+iCounter,
       200+NewCHODGeometryParameters::GetInstance()->GetScintMap(iCounter));

    new NewCHODScintillatorCounter
      (G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"),
       fLogicalVolume, Position3, 300+iCounter,
       300+NewCHODGeometryParameters::GetInstance()->GetScintMap(iCounter));

    new NewCHODScintillatorCounter
      (G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"),
       fLogicalVolume, Position4, 400+iCounter,
       400+NewCHODGeometryParameters::GetInstance()->GetScintMap(iCounter));
  }
  SetProperties();
}

void NewCHODDetector::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
