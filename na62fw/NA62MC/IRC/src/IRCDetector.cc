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
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// 2010-11-10 Spasimir Balev
//            -- Change geometry according to TDR
//            -- Add beam pipe
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"
#include "G4Transform3D.hh"

#include "IRCGeometryParameters.hh"
#include "IRCMaterialParameters.hh"

#include "IRCDetector.hh"
#include "IRCModule.hh"

#include "IRCBeamPipe.hh"

#include "IRCSD.hh"
#include "G4SDManager.hh"

IRCDetector::IRCDetector(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume), NA62VNamed("IRC")
{
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  IRCMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

IRCDetector::~IRCDetector(){}

void IRCDetector::ReadGeometryParameters()
{

}

void IRCDetector::CreateGeometry()
{
  // Example of sensitive detector; in general it would be
  // associated to smaller volume/s inside the global box/es
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4String IRCSensitiveDetectorName = "/IRC";
  G4String IRCCollectionName= "IRCCollection";
  IRCSD * IrcSD = static_cast<IRCSD*>(SDman->FindSensitiveDetector(IRCSensitiveDetectorName));
  if(!IrcSD){
      IrcSD = new IRCSD(IRCSensitiveDetectorName, IRCCollectionName);
      SDman->AddNewDetector(IrcSD);
  }

  IRCGeometryParameters* GeoPars = IRCGeometryParameters::GetInstance();

  // Define IRC responsibility region
  fSolidVolume= new G4Box("IRC",
			  GeoPars->GetIRCRespRegionXLength()/2.,
			  GeoPars->GetIRCRespRegionYLength()/2.,
			  GeoPars->GetIRCRespRegionZLength()/2.);

  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                        // solid
				      fMaterial,                           // material
				      "IRC",                               // name
				      0,                                   // field manager 
				      0,                                   // sensitive detector
				      0);                                  // user limits

  fPhysicalVolume = new G4PVPlacement(0,
				      G4ThreeVector(0.,0.,GeoPars->GetIRCRespRegionZCenter()),
				      fLogicalVolume,      // its logical volume
				      "IRC",               // its name
				      fMotherVolume,       // its mother  volume
				      false,               // no boolean operations
				      0);                  // copy number

  G4RotationMatrix Rotation;
  // Place Module 1
  G4ThreeVector Position1 = G4ThreeVector(0.,0.,GeoPars->GetIRCDetectorZFrontPosition() - 
					  GeoPars->GetIRCRespRegionZCenter() + 
					  (GeoPars->GetIRCStation1NLayers()*GeoPars->GetLayerSpacing() + 
					   2.*GeoPars->GetAluminumLayerZLength())/2.);
  G4Transform3D FisrtModule = G4Transform3D(Rotation, Position1);
  new IRCModule(G4Material::GetMaterial("G4_Galactic"), fLogicalVolume, FisrtModule,
  		GeoPars->GetIRCStation1InnerRadius(),
  		GeoPars->GetIRCStation1OuterRadius(),
  		GeoPars->GetIRCStation1NLayers(), 0);

  // Place Module 2
  Rotation.rotateZ(GeoPars->GetIRCModuleRotation());
  G4ThreeVector Position2 = G4ThreeVector(0.,0.,GeoPars->GetIRCDetectorZFrontPosition() + 
					  (GeoPars->GetIRCStation1NLayers()*GeoPars->GetLayerSpacing() + 
					   2.*GeoPars->GetAluminumLayerZLength()) +
					  GeoPars->GetIRCDistanceBetweenStations() +
					  (GeoPars->GetIRCStation2NLayers()*GeoPars->GetLayerSpacing() + 
					   GeoPars->GetAluminumLayerZLength() + GeoPars->GetAluminumCloseZLength() )/2 -
					  GeoPars->GetIRCRespRegionZCenter());
  //  G4cout << " IRC module2 position" << Position2 << G4endl;
  G4Transform3D SecondModule = G4Transform3D(Rotation, Position2);
  new IRCModule(G4Material::GetMaterial("G4_Galactic"), fLogicalVolume, SecondModule,
  		GeoPars->GetIRCStation2InnerRadius(),
  		GeoPars->GetIRCStation2OuterRadius(),
  		GeoPars->GetIRCStation2NLayers(), 1);

  // Place beam pipe pieces and flange
  for(G4int iPipe=0;iPipe<NUMBER_OF_BEAM_PIPE_PIECES;iPipe++) {
    G4ThreeVector BeamPipePiecePosition = G4ThreeVector(GeoPars->GetBeamPipePieceDisplacementX(iPipe),0.,
							(GeoPars->GetBeamPipePieceZEnd(iPipe)+
							 GeoPars->GetBeamPipePieceZStart(iPipe))*0.5-
							GeoPars->GetIRCRespRegionZCenter());
    G4Transform3D BeamPipePieceTransform3D = G4Transform3D(Rotation, BeamPipePiecePosition);
    new IRCBeamPipe(G4Material::GetMaterial(GeoPars->GetBeamPipePieceMaterialName(iPipe)),
		    fLogicalVolume,
		    BeamPipePieceTransform3D,
		    GeoPars->GetBeamPipePieceZEnd(iPipe)-GeoPars->GetBeamPipePieceZStart(iPipe),
		    GeoPars->GetBeamPipePieceInnerRadius(iPipe),
		    GeoPars->GetBeamPipePieceOuterRadius(iPipe),
		    GeoPars->GetBeamPipePieceInnerDisplacementX(iPipe),
		    iPipe);
  }
}

void IRCDetector::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
