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
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//            Evelina Marinova (Evelina.MArinova@cern.ch)
//
//            LKr-> MUV2 Mario Vormstein (mario.vormstein@cern.ch)
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV2GeometryParameters.hh"
#include "MUV2MaterialParameters.hh"

#include "MUV2BeamPipe.hh"
#include "G4SDManager.hh"

/// \class MUV2BeamPipe
/// \Brief
/// MUV2BeamPipe class. 
/// \EndBrief
/// 
/// \Detailed
/// This class stores and provides the information about the geometry and position of the beam pipe passing through the calorimeter.
/// \EndDetailed

MUV2BeamPipe::MUV2BeamPipe (G4Material * Material, G4LogicalVolume * MotherVolume, G4double LongitudinalLengthBeamPipe, 
		  G4Transform3D  BeamPipeTransform):
  NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  
  fBeamPipeTransform = BeamPipeTransform;
  fLongitudinalLengthBeamPipe =  LongitudinalLengthBeamPipe;

  // Mandatory here to Find or Build the needed materials
  MUV2MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

MUV2BeamPipe::~MUV2BeamPipe(){}


void MUV2BeamPipe::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();
  
  fInnerBeamPipeRadius = GeoPars->GetInnerBeamPipeRadius();
  fOuterBeamPipeRadius = GeoPars->GetOuterBeamPipeRadius();
  // fLongitudinalLengthBeamPipe =  GeoPars->GetLongitudinalLengthBeamPipe();
}

void MUV2BeamPipe::CreateGeometry()
{
  G4double  pRMin = fInnerBeamPipeRadius;
  G4double  pRMax = fOuterBeamPipeRadius; 
  G4double  pDz = fLongitudinalLengthBeamPipe /2;
  G4double  pSPhi = 0*deg;
  G4double  pDPhi = 360*deg;
  
  solidBeamPipe = new G4Tubs("solid_BeamPipe",                        
				  pRMin,
				  pRMax,
				  pDz,
				  pSPhi,
				  pDPhi);
  
  // The beam pipe is made of stainless steel. It has the length of the Outer Cryostat.
  logicBeamPipe = new G4LogicalVolume(solidBeamPipe , fMaterial, "beampipe",0,0,0);
  physiBeamPipe = new G4PVPlacement(fBeamPipeTransform,              // no rotation
				    logicBeamPipe,    // its logical volume
				    "beampipe",       // its name
				    fMotherVolume,      // its mother  volume
				    false,           // no boolean operations
				    0);              // copy number
}

void MUV2BeamPipe::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  logicBeamPipe ->SetVisAttributes(fVisAtt);
}
