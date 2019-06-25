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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"

#include "CHODGeometryParameters.hh"
#include "CHODMaterialParameters.hh"
#include "CHODPlane.hh"
#include "CHODQuadrant.hh"
#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"
#include "CHODSD.hh"
#include "G4SDManager.hh"

CHODPlane::CHODPlane(G4Material * Material, G4LogicalVolume * MotherVolume,G4Transform3D PlaneTransform,G4int iPlane) : 
NA62VComponent(Material,MotherVolume) {
  ReadGeometryParameters();

  fPlane = iPlane;
  fPlaneTransform = PlaneTransform;

  // Mandatory here to Find or Build the needed materials
  CHODMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void CHODPlane::ReadGeometryParameters() {
  CHODGeometryParameters* GeoPars = CHODGeometryParameters::GetInstance();
  fXLength    = GeoPars->GetTransverseSize();
  fYLength    = fXLength;
  fZLength    = 0.5*GeoPars->GetScintThickness();
  fNQuadrants = GeoPars->GetNQuadrants();
}

void CHODPlane::CreateGeometry() {
  G4double surfaceTol = 1*mm;
  G4String Name = Form("CHODPlane%03d", fPlane);

  fSolidVolume = new G4Box ("CHODPlane", fXLength, fYLength, fZLength);
//  fLogicalVolume = new G4LogicalVolume
//    (fSolidVolume, fMaterial, "CHODPlane", 0, 0, 0);

  CHODGeometryParameters* GeoPars = CHODGeometryParameters::GetInstance();

  // Define Beam Pipe Hole volume (a cylinder); enlarge by 1.005 to see a clean hole in Vis [the hole in the scintillatorCounter is bigger than this]
  G4Tubs *BeamPipeHole = new G4Tubs
    ("CHODBeamPipeHole", 0.0, GeoPars->GetInnerRadius(), fZLength + surfaceTol, 0.0, 360*deg);
  G4ThreeVector BeamPipeCentrePosition = G4ThreeVector(0.,0.,0.);

  // Put the Pipe Hole in its position
  G4VSolid *fDisplacedBeamPipeHole = new G4DisplacedSolid
    ("CHODDisplacedBeamPipeHole", BeamPipeHole, 0, BeamPipeCentrePosition);

  // Subtract the Pipe Hole from the scintillator volume
  G4VSolid *PlaneWithHole = new G4SubtractionSolid
    ("CHODPlaneWithHole", fSolidVolume, fDisplacedBeamPipeHole);

  // Plane logical volume
   fLogicalVolume = new G4LogicalVolume
    (PlaneWithHole, fMaterial, Name, 0, 0, 0);

  fPhysicalVolume = new G4PVPlacement
    (fPlaneTransform, fLogicalVolume, "CHODPlaneWithHole", fMotherVolume, false, 0);

  // First quadrant position
  G4ThreeVector QuadrantPosition = G4ThreeVector(-0.5*fXLength,0.5*fYLength,0.*cm);
  G4RotationMatrix QuadrantRotation;
  for (G4int iQuadrant = 0; iQuadrant < fNQuadrants; iQuadrant++) {
    G4Transform3D QuadrantTransform = G4Transform3D(QuadrantRotation, QuadrantPosition);
    new CHODQuadrant
      (fMaterial, fLogicalVolume, QuadrantTransform, iQuadrant + fPlane*fNQuadrants);
    QuadrantPosition.rotateZ(90.*deg);                    // rotate position for the next quadrant
    QuadrantRotation.rotateX(180.*deg*((iQuadrant+1)%2)); // rotate next quadrant alternate
    QuadrantRotation.rotateY(180.*deg*(iQuadrant%2));     // in X or Y
  }
}

void CHODPlane::SetProperties() {
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt-> SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
