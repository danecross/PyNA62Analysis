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

#include "BeamTube.hh"
#include "BeamPipeMaterialParameters.hh"
#include "G4LogicalSkinSurface.hh"

BeamTube::BeamTube(G4int SegmentIndex, G4Material * Material, NA62VGeometryParameters* GeoPars, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{ 
 
  SetSegmentIndex(SegmentIndex);
  SetGeoPars(GeoPars);
  DefineOpticalSurface(); 
 
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();

}

BeamTube::~BeamTube(){}

void BeamTube::ReadGeometryParameters()
{

   fInnerRadius = fGeoPars->GetBeamPipeInnerRadius(fSegmentIndex);
   fOuterRadius = fGeoPars->GetBeamPipeOuterRadius(fSegmentIndex);
   fZLength = fGeoPars->GetBeamPipeZLength(fSegmentIndex);
}


void BeamTube::CreateGeometry()
{
    G4double HalfZLength = 0.5*fZLength;
    G4double startPhiAngle=0;
    G4double deltaPhiAngle=360*deg;


    fSolidVolume= new G4Tubs("BeamTube",
			     0,
                             fOuterRadius,
                             HalfZLength,
                             startPhiAngle,
                             deltaPhiAngle);

    fLogicalVolume = new G4LogicalVolume(fSolidVolume,        // solid
                                         fMaterial,             // material
                                         "BeamTube",           // name
                                         0,                    // field manager
                                         0,                    // sensitive detector
                                         0);                   // user limits


    fPhysicalVolume = new G4PVPlacement(0,                         // no rotation
                                        G4ThreeVector(0,0,0),           // at pos
                                        fLogicalVolume,             // its logical volume
                                        "BeamTube",                // its name
                                        fMotherVolume,              // its mother  volume
                                        false,                     // no boolean operations
                                        0);                        // copy number

    G4Tubs* SolidBeamTubeVacuum = new G4Tubs("BeamTubeVacuum",
 					      0,
 					      fInnerRadius,
 					      HalfZLength,
 					      startPhiAngle,
 					      deltaPhiAngle);

    fLogicalBeamTubeVacuum = new G4LogicalVolume(SolidBeamTubeVacuum,        // solid
 						 G4Material::GetMaterial("G4_Galactic"),             // material
 						 "BeamTubeVacuum",           // name
 						 0,                    // field manager
 						 0,                    // sensitive detector
 						 0);                   // user limits

    fPhysicalBeamTubeVacuum = new G4PVPlacement(0,               // no rotation
 					        G4ThreeVector(), // at (0,0,0)
 						fLogicalBeamTubeVacuum,      // its logical volume
 						"BeamTubeVacuum",         // its name
 						fLogicalVolume,        // its mother  volume
 						false,           // no boolean operations
						0);              // copy number 


  
    //------------------------------ 
    // Optical Surface of BeamTube
    //------------------------------ 
    new G4LogicalSkinSurface("Tube Surface",fLogicalVolume,fOpticalSurface);   

} 

  void BeamTube::DefineOpticalSurface()
  {
    // BeamTube reflective surface
    fOpticalSurface = new G4OpticalSurface("BeamTube");
    fOpticalSurface->SetType(dielectric_metal);
    fOpticalSurface->SetFinish(ground);
    fOpticalSurface->SetModel(unified);
    fOpticalSurface->SetSigmaAlpha(BeamPipeMaterialParameters::GetInstance()
  				  ->GetTubeOpticalSurfaceSigmaAlpha());
    fOpticalSurface -> SetMaterialPropertiesTable(BeamPipeMaterialParameters::GetInstance()
  						 ->GetTubeOpticalSurfacePT());
  } 


void BeamTube::SetProperties()
{
    fVisAtt= new G4VisAttributes(G4Colour(0.5,0.5,0.5));
    fLogicalVolume ->SetVisAttributes(fVisAtt); 
    G4VisAttributes* BeamTubeVacuumVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
    fLogicalBeamTubeVacuum ->SetVisAttributes(BeamTubeVacuumVisAtt);

}


