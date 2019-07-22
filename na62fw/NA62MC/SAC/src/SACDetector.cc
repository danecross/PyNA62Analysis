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
// Created by   Antonino Sergi (Antonino.Sergi@cern.ch) 
//              Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4PVReplica.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"

#include "SACGeometryParameters.hh"
#include "SACMaterialParameters.hh"

#include "SACDetector.hh"
#include "SACSegment.hh"
#include "BeamPipe.hh"
#include "SACSD.hh"
#include "G4SDManager.hh"

SACDetector::SACDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume), NA62VNamed("SAC") {
  // Mandatory here to Find or Build the needed materials
  SACMaterialParameters::GetInstance();
}

void SACDetector::ReadGeometryParameters() {
  // Read all the geometrical parameters and copy them to private members
  GeoPars = SACGeometryParameters::GetInstance();

  fSACSimulationMode = GeoPars->GetSACSimulationMode();

  fSACResponsibilityRegionXLength = GeoPars->GetSACResponsibilityRegionXLength();
  fSACResponsibilityRegionYLength = GeoPars->GetSACResponsibilityRegionYLength();
  fSACResponsibilityRegionZBeginning = GeoPars->GetSACResponsibilityRegionZBeginning();
  fSACResponsibilityRegionZEnd = GeoPars->GetSACResponsibilityRegionZEnd();
  
  fSACDetectorFrontZPosition = GeoPars->GetSACDetectorFrontZPosition();
  fSACDetectorXLength = GeoPars->GetSACDetectorXLength();
  fSACDetectorYLength = GeoPars->GetSACDetectorYLength();

  fSACDetectorYRotation = GeoPars->GetSACDetectorYRotation();

  fAbsorberLayerXLength = GeoPars->GetAbsorberLayerXLength();
  fAbsorberLayerYLength = GeoPars->GetAbsorberLayerYLength();
  fAbsorberLayerZLength = GeoPars->GetAbsorberLayerZLength();

  fScintillatorLayerXLength = GeoPars->GetScintillatorLayerXLength();
  fScintillatorLayerYLength = GeoPars->GetScintillatorLayerYLength();
  fScintillatorLayerZLength = GeoPars->GetScintillatorLayerZLength();

  fNLayers = GeoPars->GetNLayers();

  fAluminiumLayerXLength = GeoPars->GetAluminiumLayerXLength();
  fAluminiumLayerYLength = GeoPars->GetAluminiumLayerYLength();
  fAluminiumLayerZLength = GeoPars->GetAluminiumLayerZLength();

  fFiberDiameter = GeoPars->GetFiberDiameter();
  fFiberSpacing = GeoPars->GetFiberSpacing();
  fNFibers = GeoPars->GetNFibers();

  fSACFlangeZLength = GeoPars->GetSACFlangeZLength();
  fSACFlangeZPosition = GeoPars->GetSACFlangeZPosition();
  fSACFlangeInnerRadius = GeoPars->GetSACFlangeInnerRadius();
  fSACFlangeOuterRadius = GeoPars->GetSACFlangeOuterRadius();
  fSACFlangeDisplacement = GeoPars->GetSACFlangeDisplacement();


}

void SACDetector::CreateGeometry()
{
  ReadGeometryParameters();
  // Example of sensitive detector; in general it would be
  // associated to smaller volume/s inside the global box/es
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4String SACSensitiveDetectorName = "/SAC";
  G4String SACCollectionName= "SACCollection";
  SACSD * SacSD = static_cast<SACSD*>(SDman->FindSensitiveDetector(SACSensitiveDetectorName));
  if(!SacSD){
      SacSD = new SACSD(SACSensitiveDetectorName,SACCollectionName);
      SDman->AddNewDetector(SacSD);
  }

  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region
  
  // Define some more variables
  G4double SACResponsibilityRegionZLength     = fSACResponsibilityRegionZEnd - fSACResponsibilityRegionZBeginning;
  G4double SACResponsibilityRegionZCenter     = fSACResponsibilityRegionZBeginning + 0.5*SACResponsibilityRegionZLength;
  //  G4double SACDetectorFrontZRelativePosition  = fSACDetectorFrontZPosition - SACResponsibilityRegionZCenter;

  G4double LayerSpacing = fAbsorberLayerZLength+fScintillatorLayerZLength;
  G4double SACAbsorberScintillatorTotalZLength = fNLayers*LayerSpacing;
  G4double fSACDetectorZLength = SACAbsorberScintillatorTotalZLength + 2.*fAluminiumLayerZLength;

  ///////////////////////////////////////
  //
  //  RESPONSIBILITY REGION
  //
  ///////////////////////////////////////

  fSolidVolume= new G4Box("SAC",
			  0.5*fSACResponsibilityRegionXLength,
			  0.5*fSACResponsibilityRegionYLength,
			  0.5*SACResponsibilityRegionZLength);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,       // solid
				      fMaterial,          // material
				      "SAC",              // name
				      0,                  // field manager 
				      0,                  // sensitive detector
				      0);                 // user limits
  fPhysicalVolume = new G4PVPlacement(0,
				      G4ThreeVector(0.,0.,SACResponsibilityRegionZCenter),   // position
				      fLogicalVolume,                    // its logical volume
				      "SAC",                             // its name
				      fMotherVolume,                     // its mother  volume
				      false,                             // no boolean operations
				      0);                                // copy number

  ///////////////////////////////////////
  //
  //  SAC DETECTOR
  //
  ///////////////////////////////////////

  // Common
  SACSegment* Segment = new SACSegment(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume);
  G4Box* SACDetector = new
    G4Box("SACDetector", 0.5*fSACDetectorXLength, 0.5*fSACDetectorYLength, 0.5*fSACDetectorZLength);
  G4LogicalVolume* SACDetectorLogical = new
    G4LogicalVolume(SACDetector, G4Material::GetMaterial("G4_Galactic"), "SACDetector");

  // Beam pipe
  /*BeamPipe* BP0 = */new BeamPipe(0,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);

  // Flange
  //  G4double flangeZ = 255.522*m-SACResponsibilityRegionZCenter;
  G4Tubs *SACFlange = new G4Tubs("SACFlange",fSACFlangeInnerRadius,fSACFlangeOuterRadius,fSACFlangeZLength*0.5,0,360*deg);
  G4LogicalVolume* SACFlangeLogical = new G4LogicalVolume(SACFlange,G4Material::GetMaterial("G4_Fe"),"SACFlange",0,0,0);
  new G4PVPlacement(0,G4ThreeVector(fSACFlangeDisplacement,0.,fSACFlangeZPosition),SACFlangeLogical,"SACFlange",fLogicalVolume,false,0);
  G4VisAttributes *SACFlangeVisAtt = new G4VisAttributes(G4Colour(1.0,0.0,0.0));

  SACFlangeLogical->SetVisAttributes(SACFlangeVisAtt); 
  BeamPipe* BP1 = new BeamPipe(1,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);
  G4LogicalVolume* LogicalBeamTube1Vacuum = static_cast<BeamTube*>( BP1->GetBeamTube())->GetLogicalBeamTubeEnvelope();


  //
  // SAC SIMULATION MODE 1 [DEFAULT]
  //
  // 260x260 mm Absorber and Scintillator plates
  // with 784 fiber holes replicated 70 times in Z
  if(fSACSimulationMode==1) {
    // Place the segment inside SACDetector		
    G4ThreeVector SegmentPosition = G4ThreeVector(0.,0.,0.);
    new G4PVPlacement(0, SegmentPosition, Segment->GetLogicalVolume(), "Segment",
		      SACDetectorLogical, false, 0);
  }

  //
  // SAC SIMULATION MODE 2
  //
  // Segmentation of 9.5x9.5 mm for each Absorber and Scintillator layer
  // (+ fiber in the center) replicated along Z,X and Y axis
  if(fSACSimulationMode==2) {
    // Replicate the segment along X axis 28 times
    G4Box* HorizontalRowSolid = new
      G4Box("HorizontalRow", (G4double)fNFibers*0.5*fFiberSpacing,
	    0.5*fFiberSpacing, 0.5*fSACDetectorZLength);
    G4LogicalVolume* HorizontalRowLogical = new
      G4LogicalVolume(HorizontalRowSolid, G4Material::GetMaterial("G4_Galactic"), "HorizontalRow");
    new G4PVReplica("HorizontalRow", Segment->GetLogicalVolume(), HorizontalRowLogical,
		    kXAxis, fNFibers, fFiberSpacing, 0);

    // Replicate the horizontal row along Y axis 28 times to fill the SAC detector
    SACDetector = new
      G4Box("SACDetector", (G4double)fNFibers*0.5*fFiberSpacing,
	    (G4double)fNFibers*0.5*fFiberSpacing, 0.5*fSACDetectorZLength);
    SACDetectorLogical = new
      G4LogicalVolume(SACDetector, G4Material::GetMaterial("G4_Galactic"), "SACDetector");
    new G4PVReplica("FilledSAC", HorizontalRowLogical, SACDetectorLogical,
		    kYAxis, fNFibers, fFiberSpacing, 0);
  }

  // Position and rotation of SAC detector 
  G4double SACDetectorFrontZRelativePosition  = fSACDetectorFrontZPosition - SACResponsibilityRegionZCenter - GeoPars->GetBeamPipeZPosition(1);

  G4ThreeVector SACDetectorPosition = G4ThreeVector(-GeoPars->GetBeamPipeInputDisplacementWRTBeam(1),0.,SACDetectorFrontZRelativePosition + 0.5*fSACDetectorZLength);

  G4RotationMatrix SACDetectorRotation;
  SACDetectorRotation.rotateX(0);
  SACDetectorRotation.rotateY(fSACDetectorYRotation);
  SACDetectorRotation.rotateZ(0);
  G4Transform3D SACDetectorTransform3D = G4Transform3D(SACDetectorRotation, SACDetectorPosition);


  new G4PVPlacement(SACDetectorTransform3D,     // position
		    SACDetectorLogical,	        //its logical volume
		    "SACDetector",	        //its name
		    LogicalBeamTube1Vacuum,     //was fLogicalVolume,	        //its mother  volume
		    false,		        //no boolean operation
		    0);  		        //copy number

  SetProperties();

//  // Beam pipe
//  new BeamPipe(0,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);
//  new BeamPipe(1,G4Material::GetMaterial("G4_Galactic"),GeoPars,fLogicalVolume);
//
//  // Flange
//  G4double flangeZ = 255.522*m-SACResponsibilityRegionZCenter;
//  G4Tubs *SACFlange = new G4Tubs("SACFlange",162*mm,299*mm,5*mm,0,360*deg);
//  G4LogicalVolume* SACFlangeLogical = new G4LogicalVolume(SACFlange,G4Material::GetMaterial("G4_Fe"),"SACFlange",0,0,0);
//  new G4PVPlacement(0,G4ThreeVector(-40.*mm,0.,flangeZ),SACFlangeLogical,"SACFlange",fLogicalVolume,false,0);
//  G4VisAttributes *SACFlangeVisAtt = new G4VisAttributes(G4Colour(1.0,0.0,0.0));
//  SACFlangeLogical->SetVisAttributes(SACFlangeVisAtt); 
}

void SACDetector::SetProperties() {
  // Set visualization properties
  //fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt= new G4VisAttributes(G4Colour(G4Colour::Yellow()));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
