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
#include "G4SDManager.hh"
#include "G4Tubs.hh"
#include "G4SubtractionSolid.hh"

#include "CHODGeometryParameters.hh"
#include "CHODMaterialParameters.hh"
#include "CHODQuadrant.hh"
#include "CHODScintillatorCounter.hh"
#include "CHODSD.hh"

CHODQuadrant::CHODQuadrant(G4Material * Material, G4LogicalVolume * MotherVolume, 
			   G4Transform3D QuadrantTransform, G4int Quadrant) :
  NA62VComponent(Material,MotherVolume),
  fQuadrant(Quadrant),
  fQuadrantTransform(QuadrantTransform)
{
  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  CHODMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void CHODQuadrant::ReadGeometryParameters() {

  CHODGeometryParameters* GeoPars = CHODGeometryParameters::GetInstance();

  // Quadrant half-dimensions
  fXLength = 0.5*GeoPars->GetTransverseSize();
  fYLength = fXLength;
  fZLength = 0.5*GeoPars->GetScintThickness();

  // Define the beam pipe hole position at the innermost corner of the quadrant
  fBeamHolePosition = G4ThreeVector(fXLength,-fYLength,0.0*cm);

  fNCounters = GeoPars->GetNCounters();
  for (G4int iCounter = 0; iCounter < fNCounters; iCounter++) {
    fScintillatorSize[iCounter]     = GeoPars->GetScintSize(iCounter);
    fScintillatorPosition[iCounter] = GeoPars->GetScintPosition(iCounter);
  }
}

void CHODQuadrant::CreateGeometry() {

  CHODGeometryParameters* GeoPars = CHODGeometryParameters::GetInstance();

  fSolidVolume = new G4Box
    ("CHODQuadrant", fXLength, fYLength, fZLength);
   
  G4double surfaceTol = 1*mm;
  // Define Beam Pipe Hole volume (a cylinder); enlarge by 1.005 to see a clean hole in Vis [the hole in the scintillatorCounter is bigger than this]
  G4Tubs *BeamPipeHole = new G4Tubs
    ("CHODBeamPipeHole", 0.0, GeoPars->GetInnerRadius(), fZLength + surfaceTol, 0.0, 360*deg);

  // Put the Pipe Hole in its position
  G4VSolid *fDisplacedBeamPipeHole = new G4DisplacedSolid
    ("CHODDisplacedBeamPipeHole", BeamPipeHole, 0, fBeamHolePosition);

  // Subtract the Pipe Hole from the scintillator volume
  G4VSolid *QuadrantWithHole = new G4SubtractionSolid
    ("CHODQuadrantWithHole", fSolidVolume, fDisplacedBeamPipeHole);

  fLogicalVolume = new G4LogicalVolume
    (QuadrantWithHole, fMaterial, "CHODQuadrant", 0, 0, 0);

  fPhysicalVolume = new G4PVPlacement
    (fQuadrantTransform, fLogicalVolume, "CHODQuadrant", fMotherVolume, false, 0);

  // loop over the 16 counters of each quadrant
  for (G4int iCounter = 0; iCounter < fNCounters; iCounter++) { 
    fScintillatorCounter = new CHODScintillatorCounter
      (G4Material::GetMaterial("G4_PLASTIC_SC_VINYLTOLUENE"),
       fLogicalVolume,
       fScintillatorSize[iCounter],
       fScintillatorPosition[iCounter],
       fBeamHolePosition,
       iCounter + fQuadrant*fNCounters); // "copy number" --> propagated to hits
  }
}

void CHODQuadrant::SetProperties() {
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt->SetVisibility(false);
  fLogicalVolume->SetVisAttributes(fVisAtt);
}
