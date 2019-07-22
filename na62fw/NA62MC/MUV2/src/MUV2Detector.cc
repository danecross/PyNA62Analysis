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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-10
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
// Modified (MUV -> MUV2) Rainer Wanke              2010-11-26
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
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

#include "MUV2SD.hh"
#include "MUV2GeometryParameters.hh"
#include "MUV2MaterialParameters.hh"
#include "MUV2Detector.hh"
#include "MUV2DetectorMessenger.hh"

#include "MUV2IronPlate.hh"
#include "MUV2IronTube.hh"
#include "MUV2BeamPipe.hh"
#include "MUV2ScintillatorLayer.hh"

MUV2Detector::MUV2Detector(G4Material * Material, G4LogicalVolume * MotherVolume) :
  NA62VComponent(Material,MotherVolume), NA62VNamed("MUV2")
{
  // Connect to MUV2DetectorMessenger
  fMUV2Messenger = new MUV2DetectorMessenger(this);

  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  MUV2MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

MUV2Detector::~MUV2Detector(){
	delete fMUV2Messenger;
}

void MUV2Detector::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  MUV2GeometryParameters* GeoPars = MUV2GeometryParameters::GetInstance();
  fMUV2ResponsibilityRegionZBeginning = GeoPars->GetMUV2ResponsibilityRegionZBeginning();
  fMUV2ResponsibilityRegionZEnd       = GeoPars->GetMUV2ResponsibilityRegionZEnd();
  fMUV2ResponsibilityRegionXLength    = GeoPars->GetMUV2ResponsibilityRegionXLength();
  fMUV2ResponsibilityRegionYLength    = GeoPars->GetMUV2ResponsibilityRegionYLength();
  fXLength   = 0.5 * GeoPars->GetMUV2Size();    // half transverse X size of all MUV2
  fYLength   = 0.5 * GeoPars->GetMUV2Size();    // half transverse Y size of all MUV2
  fZLength   = 0.5 * GeoPars->GetMUV2Length();   // half length of all MUV2

  fZPosition = GeoPars->GetMUV2DetectorFrontZPosition();     // MUV2 center position in z


  // thickness of each scintillator (zsci)
  fScintillatorThickness = GeoPars->GetScintillatorThickness();
  // thickness of each iron plate (zfe)
  fIronThickness         = GeoPars->GetIronThickness();
  fGapThickness			 = GeoPars->GetGapThickness();

  fSkinAluminumWidth     = GeoPars->GetSkinAluminumWidth();
  fSkinTapeWidth         = GeoPars->GetSkinTapeWidth();
  fAirGapWidth           = GeoPars->GetAirGapWidth();

  //Iron parameters
  fIronPlateSizeX		 = GeoPars->GetMUV2Size();
  fIronPlateSizeY		 = GeoPars->GetMUV2Size();

  // Define some more useful variables
  MUV2ResponsibilityRegionZLength      =  fMUV2ResponsibilityRegionZEnd - fMUV2ResponsibilityRegionZBeginning;
  MUV2ResponsibilityRegionZCenter      =  fMUV2ResponsibilityRegionZBeginning + MUV2ResponsibilityRegionZLength/2.;

}

void MUV2Detector::CreateGeometry()
{
  // Example of sensitive detector; in general it would be
  // associated to smaller volume/s inside the global boxes
  G4SDManager* SDman = G4SDManager::GetSDMpointer();

  G4String MUV2SensitiveDetectorName = "/MUV2";
  G4String MUV2CollectionName= "MUV2Collection";
  MUV2SD *Muv2SD = static_cast<MUV2SD*>( SDman->FindSensitiveDetector( MUV2SensitiveDetectorName ));
  if ( !Muv2SD ) {
    Muv2SD = new MUV2SD( MUV2SensitiveDetectorName, MUV2CollectionName );
    SDman->AddNewDetector( Muv2SD );
  }


  // Build one or more boxes that will contain all the 
  // detector sections, up to fill the responsibility region
  
  // Define MUV2 volume (a box)
  fSolidVolume= new G4Box("MUV2",
			  0.5*fMUV2ResponsibilityRegionXLength,
			  0.5*fMUV2ResponsibilityRegionXLength,
			  0.5*(fMUV2ResponsibilityRegionZEnd - fMUV2ResponsibilityRegionZBeginning));

  // MUV2 logical volume
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,       // solid
				      fMaterial,          // material
				      "MUV2",             // name
				      0,                  // field manager 
				      0,                  // sensitive detector
				      0);                 // user limits

  // MUV2 physical volume
  fPhysicalVolume = new G4PVPlacement(0,
				      G4ThreeVector(0.,0.,MUV2ResponsibilityRegionZCenter),   // position
				      fLogicalVolume,                    // its logical volume
				      "MUV2",                            // its name
				      fMotherVolume,                     // its mother  volume
				      false,                             // no boolean operations
				      0);                                // copy number



  // MUV2 scintillator layer rotation
  G4RotationMatrix* g4rot = new G4RotationMatrix();
  g4rot->rotateZ(90.*deg);
  *g4rot = g4rot->inverse();



//****************************************************
//*                                                  *
//*            Layers                                *
//*                                                  *
//****************************************************



  // Start positions of layers

  //Transparent tape is applied on the narrow sides of the scintillator but also partially on its larger sides.
  //A thin air gap is assumed between each neighbour surfaces of constituents (to avoid G4 round-off overlaps): scintillator, aluminum, tape.
  //The total thickness also counts the thin air gap between a scintillator volume and an iron plate (against the same G4 round-off overlaps),
  //which is the sum of distances (determined in MUV2Scintillator::CreateGeometry()):
  //	a) between the iron plate surface and the scintillator layer mother volume surface
  //	b) between the scintillator layer mother volume surface and the scintillator mother volume surface
  //	c) between the scintillator mother volume surface and the surface of the tape wrapper volume
  //
  //   3*fAirGapWidth + fSkinTapeWidth + fAirGapWidth + fSkinAluminumWidth + fAirGapWidth +  fScintillatorThickness + fAirGapWidth + fSkinAluminumWidth + fAirGapWidth + fSkinTapeWidth + 3*fAirGapWidth;
  //
  G4double ScintVolumeTotalThickness =  3*fAirGapWidth + 2*(fSkinTapeWidth + 2*fAirGapWidth + fSkinAluminumWidth) + fScintillatorThickness + 3*fAirGapWidth ;

  G4double IronPlatePosZ = fZPosition - MUV2ResponsibilityRegionZCenter + 0.5*fIronThickness;
  G4double ScintPosZ     = fZPosition - MUV2ResponsibilityRegionZCenter + fIronThickness + 0.5*ScintVolumeTotalThickness;
    
  G4ThreeVector IronPlateSize = G4ThreeVector(0.5*fIronPlateSizeX,0.5*fIronPlateSizeY,0.5*fIronThickness);
  G4ThreeVector IronPlatePosition = G4ThreeVector(0.*cm,0.*cm,IronPlatePosZ);

  //  G4cout<<"fZPosition - MUV2ResponsibilityRegionZCenter = "<< fZPosition - MUV2ResponsibilityRegionZCenter <<G4endl;
  //  G4cout<<"IronPlatePosZ = "<<IronPlatePosZ <<G4endl;
  //  G4cout<<"ScintVolumeTotalThickness = "<<ScintVolumeTotalThickness <<G4endl;
  //  G4cout<<"ScintPosZ = "<< ScintPosZ <<G4endl;


  MUV2ScintillatorLayer * Layer = NULL;

  // loop over all 12 layers
  for(int iScintLayer = 0; iScintLayer < 12; iScintLayer++){


	  // *** IRON ***

	  IronPlatePosition = G4ThreeVector(0,0,IronPlatePosZ);
	  fIronPlate = new MUV2IronPlate(G4Material::GetMaterial("G4_Fe"),
			  fLogicalVolume,
			  IronPlateSize,
			  IronPlatePosition,
			  -1);
	  IronPlatePosZ += (fScintillatorThickness + fIronThickness + fGapThickness);



	  // *** SCINTILLATOR *** HORIZONTAL ***

	  Layer = new MUV2ScintillatorLayer(G4Material::GetMaterial("G4_AIR"),fLogicalVolume,G4ThreeVector(0,0,ScintPosZ),iScintLayer,50);
	  Layer->GetPhysicalVolume()->SetRotation(g4rot);
	  ScintPosZ += (fScintillatorThickness + fIronThickness + fGapThickness);



	  // *** IRON ***

	  IronPlatePosition = G4ThreeVector(0,0,IronPlatePosZ);
	  fIronPlate = new MUV2IronPlate(G4Material::GetMaterial("G4_Fe"),
			  fLogicalVolume,
			  IronPlateSize,
			  IronPlatePosition,
			  0);
	  IronPlatePosZ += (fScintillatorThickness + fIronThickness + fGapThickness);



	  // *** SCINTILLATOR *** VERTICAL ***

	  Layer =  new MUV2ScintillatorLayer(G4Material::GetMaterial("G4_AIR"),fLogicalVolume,G4ThreeVector(0,0,ScintPosZ),iScintLayer,0);
	  ScintPosZ += (fScintillatorThickness + fIronThickness + fGapThickness);

  }


  //****************************************************
  //*                                                  *
  //*            iron tube                             *
  //*                                                  *
  //****************************************************

  G4double MUV2TotalThickness = 2*fZLength + 3*fAirGapWidth + 2*(fSkinTapeWidth + 2*fAirGapWidth + fSkinAluminumWidth) + 3*mm; //extra 3 mm for the thickness of the downstream end-cover metal plate of MUV2

  fIronTube = new MUV2IronTube(G4Material::GetMaterial("G4_Fe"),
		  	  	  	  	  	   fLogicalVolume,
							   MUV2TotalThickness,
							   G4ThreeVector(0.,0., fZPosition - MUV2ResponsibilityRegionZCenter + 0.5*MUV2TotalThickness),
							   0);



  //****************************************************
  //*                                                  *
  //*            beam pipe                             *
  //*                                                  *
  //****************************************************

  G4RotationMatrix Ra;
  G4ThreeVector Ta = G4ThreeVector(0,0,0);
  
  
  fBeamPipe = new MUV2BeamPipe(G4Material::GetMaterial("G4_Al"),
			       fLogicalVolume,
			       MUV2ResponsibilityRegionZLength,			      
			       G4Transform3D(Ra,Ta));
}



void MUV2Detector::SetProperties()
{
  // Set visualization properties
  fVisAtt = new G4VisAttributes(G4Colour(0.0,1.0,0.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}


 //****************************************************
  //*                                                  *
  //*            SCINTILLATORS                         *
  //*                                                  *
  //****************************************************

 // Creating scintillator planes from left to right
 // Copy numbers of scintillators
 // 01 - 43  1st layer vertical
 // 101 - 143  2nd layer vertical
 // ...
 // 1201 - 1243  12th layer vertical



 // 2  4  6  ...  24  26  ...  42  44
 //                Hole
 // 1  3  5  ...  23  25  ...  41  43


 // 51 - 94  1st layer horizontal
 // 1251 - 1294  12th layer horizontal
 //
 //	94     93
 //     92     91
 //     ..     ..
 //     76     75
 //        Hole
 //     74     73
 //     ..     ..
 //     52     51
