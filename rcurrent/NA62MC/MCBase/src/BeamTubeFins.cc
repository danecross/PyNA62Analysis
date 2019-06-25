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
// Created by Francesca Bucci (francesca.bucci@cern.ch) 2011-03-11
//
// --------------------------------------------------------------
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "BeamTubeFins.hh"
#include "BeamPipeMaterialParameters.hh"
#include "G4LogicalSkinSurface.hh"

BeamTubeFins::BeamTubeFins(G4int SegmentIndex, G4Material * Material, NA62VGeometryParameters* GeoPars,G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  SetSegmentIndex(SegmentIndex);
  SetGeoPars(GeoPars);
  DefineOpticalSurface(); 
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
}

BeamTubeFins::~BeamTubeFins(){}

void BeamTubeFins::ReadGeometryParameters() {
  fZPosition = 0;
  fZLength = fGeoPars->GetBeamPipeFinZLength(fSegmentIndex);
  fInnerRadius = fGeoPars->GetBeamPipeOuterRadius(fSegmentIndex);
  fOuterRadius = fGeoPars->GetBeamPipeFinOuterRadius(fSegmentIndex);
  fSpacing = fGeoPars->GetBeamPipeFinSpacing(fSegmentIndex);
  fBeamPipeZLengthWFins = fGeoPars->GetBeamPipeZLengthWFins(fSegmentIndex);
}

void BeamTubeFins::CreateGeometry() {
  G4double HalfZLength = 0.5*fZLength;
  G4double startPhiAngle=0;
  G4double deltaPhiAngle=360*deg;
  G4int NFins=0;

  fSolidVolume= new G4Tubs("BeamPipeFins",
			   fInnerRadius,
			   fOuterRadius,
			   HalfZLength,
			   startPhiAngle,
			   deltaPhiAngle);

  fLogicalVolume = new G4LogicalVolume(fSolidVolume,         // solid
				       fMaterial,            // material
				       "BeamPipeFins",       // name
				       0,                    // field manager
				       0,                    // sensitive detector
				       0);                   // user limits

  NFins=fBeamPipeZLengthWFins/fSpacing;

  for(G4int iFin = 0; iFin < NFins; iFin++){
    
    fPhysicalVolume = new G4PVPlacement(0,                         // no rotation
 					G4ThreeVector(0,0,fZPosition - 0.5*(NFins-1)*fSpacing+iFin*fSpacing),           // at pos
                                        fLogicalVolume,             // its logical volume
					"BeamPipeFins",             // its name
					fMotherVolume,              // its mother  volume
					false,                     // no boolean operations
					0);
  }
 
  //------------------------------ 
  // Optical Surface of BeamTube
  //------------------------------ 
  new G4LogicalSkinSurface("Fin Surface",fLogicalVolume,GetOpticalSurface());
}

void BeamTubeFins::DefineOpticalSurface() {
  // BeamTubeFins reflective surface
  fOpticalSurface = new G4OpticalSurface("BeamTubeFins");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(BeamPipeMaterialParameters::GetInstance()
				 ->GetTubeOpticalSurfaceSigmaAlpha());
  fOpticalSurface -> SetMaterialPropertiesTable(BeamPipeMaterialParameters::GetInstance()
						->GetTubeOpticalSurfacePT());
}

void BeamTubeFins::SetProperties() {
  fVisAtt= new G4VisAttributes(G4Colour(0.5,0.5,0.5));
  fLogicalVolume ->SetVisAttributes(fVisAtt); 
}
