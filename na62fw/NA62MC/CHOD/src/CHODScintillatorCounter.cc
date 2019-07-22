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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "CHODGeometryParameters.hh"
#include "CHODMaterialParameters.hh"

#include "CHODScintillatorCounter.hh"
#include "CHODSD.hh"
#include "G4SDManager.hh"

CHODScintillatorCounter::CHODScintillatorCounter
(G4Material* Material, G4LogicalVolume* MotherVolume,
 G4ThreeVector ScintillatorSize, G4ThreeVector ScintillatorPosition, G4ThreeVector BeamHolePosition,
 G4int Copy) :
  NA62VComponent(Material,MotherVolume),
  fCopy(Copy)
{
  ReadGeometryParameters();
  fScintillatorSize     = ScintillatorSize;
  fScintillatorPosition = ScintillatorPosition;
  fBeamHolePosition     = BeamHolePosition;

  // Mandatory here to find or build the needed materials
  CHODMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

CHODScintillatorCounter::~CHODScintillatorCounter(){}

void CHODScintillatorCounter::ReadGeometryParameters() {
  CHODGeometryParameters* GeoPars = CHODGeometryParameters::GetInstance();
  fInnerRadius = GeoPars->GetInnerRadius();
  fOuterRadius = GeoPars->GetOuterRadius();
}

void CHODScintillatorCounter::CreateGeometry() {

  G4String Name = Form("OldCHODCounter%03d", fCopy);

  // Define scintillator volume (a box)
  fSolidVolume = new G4Box
    ("CHODCounter",  fScintillatorSize.x(), fScintillatorSize.y(), fScintillatorSize.z());

  // Put the scintillator in its position
  G4VSolid *fDisplacedSolidVolume = new G4DisplacedSolid
    ("CHODDisplacedScintillator", fSolidVolume, 0, fScintillatorPosition);

  G4double surfaceTol = 1*mm;
  // Define Beam Pipe Hole volume (a cylinder); enlarge by 1.01 to see a clean hole in Vis
  G4Tubs *BeamPipeHole = new G4Tubs
    ("CHODBeamPipeHole", 0.0, fInnerRadius*1.01, fScintillatorSize.z() + surfaceTol, 0.0, 360*deg);

  // Put the Pipe Hole in its position
  G4VSolid *fDisplacedBeamPipeHole = new G4DisplacedSolid
    ("CHODDisplacedBeamPipeHole", BeamPipeHole, 0, fBeamHolePosition);

  // Subtract the Pipe Hole from the scintillator volume
  G4VSolid *ScintillatorWithHole = new G4SubtractionSolid
    ("CHODScintillatorWithHole", fDisplacedSolidVolume, fDisplacedBeamPipeHole);

  // Scintillator logical volume
   fLogicalVolume = new G4LogicalVolume
    (ScintillatorWithHole, fMaterial, Name, 0, 0, 0);

  // Scintillator physical volume
  fPhysicalVolume = new G4PVPlacement
    (0, G4ThreeVector(0,0,0), fLogicalVolume, Name, fMotherVolume, false, fCopy);

  G4SDManager* SDmanager = G4SDManager::GetSDMpointer();
  G4String CHODSDname = "/CHOD";
  G4VSensitiveDetector *ChodSD = SDmanager->FindSensitiveDetector(CHODSDname);
  fLogicalVolume->SetSensitiveDetector(ChodSD);
}

void CHODScintillatorCounter::SetProperties() {
  fVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt->SetVisibility(true);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
