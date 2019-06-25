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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"
#include "G4RotationMatrix.hh"
#include "G4SDManager.hh"

#include "CHANTIGeometryParameters.hh"
#include "CHANTIMaterialParameters.hh"
#include "CHANTIFrame.hh"
#include "CHANTIStation.hh"
#include "CHANTIRing.hh"
#include "CHANTIDetector.hh"
#include "CHANTISD.hh"
#include "G4SubtractionSolid.hh"
#include "G4UnionSolid.hh"
#include "CHANTISDSiPM.hh"

CHANTIFrame::CHANTIFrame(G4Material * Material, G4LogicalVolume * MotherVolume,int index, CHANTIDetector * Detector) :
NA62VComponent(Material,MotherVolume) {
  fIndex = index;
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  CHANTIMaterialParameters::GetInstance();
  fDetector = Detector;
  CreateGeometry();
  SetProperties();
}

CHANTIFrame::~CHANTIFrame(){}

void CHANTIFrame::ReadGeometryParameters() {
  // Read all the geometrical parameters and copy them to private members
  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();

  fZPosRings = GeoPars->GetZPos_Ring(fIndex);
  fRingThickness = GeoPars->GetCHANTIRingThickness();
  fXInnerHalfLength = GeoPars->GetCHANTIXInnerHoleLength()/2.;
  fYInnerHalfLength = GeoPars->GetCHANTIYInnerHoleLength()/2.;
  fSquareLength = GeoPars->GetCHANTISquareLength()/2.;
  fXFrameLength = GeoPars->GetCHANTIXFrameLength();
  fYFrameLength = GeoPars->GetCHANTIYFrameLength();
  fZFrameLength = GeoPars->GetCHANTIZFrameLength();
  fFrameThickness = GeoPars->GetCHANTIFrameThickness()/2.;
  //fFrameThickness = 10*mm;
  fXFrame = GeoPars->GetCHANTIXFrame()/2.;
  fYFrame = GeoPars->GetCHANTIYFrame()/2.;
  fDistXHole = GeoPars->GetCHANTIDistXHole()/2.;
  fDistYHole = GeoPars->GetCHANTIDistYHole()/2.;

  fSupportAlaX = GeoPars->GetCHANTISupportAlaX()/2.; 
  fSupportAlaY = GeoPars->GetCHANTISupportAlaY()/2.;
  fSupportAlaZ = GeoPars->GetCHANTISupportAlaZ()/2.;
  fFrameMaterial = G4Material::GetMaterial("G4_Al");

  fSilThickness = GeoPars->GetCHANTISilThickness()/2;
  fSilRingMaterial = G4Material::GetMaterial("G4_Si");
}

void CHANTIFrame::CreateGeometry() {

  //Front Support: half-sizes are (150.5 x 5 x 0.5) mm^3
  G4VSolid* FrameSupport1 = new
    G4Box("CHANTIFrameSupport1",                         
	  fSquareLength+fFrameThickness,
	  fYFrame,
	  fFrameThickness);
  //Wing: half-sizes are (52 x 0.5 x 5.25) mm^3
  /*
  G4VSolid* FrameSupportAla = new
    G4Box("CHANTIFrameAla",
	  fSupportAlaX,
	  fSupportAlaY,				       
	  fSupportAlaZ);
  */
  //Back Support
  G4VSolid* FrameSupport2 = new G4Box
    ("CHANTIFrameSupport2",
     fYFrame,
     fSquareLength+2.0*fYFrame,
     fFrameThickness);
  G4VSolid* FrameVerticalBar  = new G4Box
    ("CHANTIFrame",
     fXFrame+fFrameThickness,
     fSquareLength+2.0*fFrameThickness+2.0*fYFrame,
     fFrameThickness);
  G4VSolid* FrameOrizontalBar1 = new G4Box
    ("CHANTIFrame",
     fSquareLength-2.0*fXFrame,
     fYFrame+fFrameThickness,
     fFrameThickness);
  G4VSolid* FrameOrizontalBar2 = new G4Box
    ("CHANTIFrame",     
     fSquareLength+fFrameThickness,
     fYFrame+fFrameThickness,
     fFrameThickness);
  G4VSolid* FrameBaseBar = new G4Box
    ("CHANTIFrame",
     fSquareLength,
     fFrameThickness,
     fRingThickness);
  G4VSolid* FrameLateralBar = new G4Box
    ("CHANTIFrame",
     fFrameThickness,
     fSquareLength,
     fRingThickness);
  G4LogicalVolume * FrameVerticalBarLogic = new
    G4LogicalVolume(FrameVerticalBar,
		    fFrameMaterial,
		    "CHANTIFrameVerticalBar",
		    0,0,0);
  G4LogicalVolume * FrameOrizontalBar1Logic = new
    G4LogicalVolume(FrameOrizontalBar1,
		    fFrameMaterial,
		    "CHANTIFrameOrizontalBar1",
		    0,0,0);
  G4LogicalVolume * FrameOrizontalBar2Logic = new
    G4LogicalVolume(FrameOrizontalBar2,
		    fFrameMaterial,
		    "CHANTIFrameOrizontalBar2",
		    0,0,0);
  G4LogicalVolume * FrameBaseBarLogic = new
    G4LogicalVolume(FrameBaseBar,
		    fFrameMaterial,
		    "CHANTIFrameBaseBar",
		    0,0,0);
  G4LogicalVolume * FrameLateralBarLogic = new
    G4LogicalVolume(FrameLateralBar,
		    fFrameMaterial,
		    "CHANTIFrameVerticalBar",
		    0,0,0);

  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - fFrameThickness)+
		    (fSquareLength + fFrameThickness - fXFrame)*G4ThreeVector(-1,0,0),
		    FrameVerticalBarLogic,
		    "CHANTIFramePhysics1",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - fFrameThickness)+
		    (fSquareLength + fFrameThickness - fXFrame)*G4ThreeVector(1,0,0),
		    FrameVerticalBarLogic,
		    "CHANTIFramePhysics2",
		    fMotherVolume,
		    false,
		    fIndex);

  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - fFrameThickness)+
		    (fSquareLength + fFrameThickness + fYFrame)*G4ThreeVector(0,1,0),
		    FrameOrizontalBar1Logic,
		    "CHANTIFramePhysics3",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - fFrameThickness)+
		    (fSquareLength + fFrameThickness + fYFrame)*G4ThreeVector(0,-1,0),
		    FrameOrizontalBar1Logic,
		    "CHANTIFramePhysics4",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + 2.0*fRingThickness + fFrameThickness)+
		    (fSquareLength + fFrameThickness + fYFrame)*G4ThreeVector(0,1,0),
		    FrameOrizontalBar2Logic,
		    "CHANTIFramePhysics5",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + 2.0*fRingThickness + fFrameThickness)+
		    (fSquareLength + fFrameThickness + fYFrame)*G4ThreeVector(0,-1,0),
		    FrameOrizontalBar2Logic,
		    "CHANTIFramePhysics6",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)+
		    (fSquareLength + fFrameThickness)*G4ThreeVector(0,1,0),
		    FrameBaseBarLogic,
		    "CHANTIFramePhysics7",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)+
		    (fSquareLength + fFrameThickness)*G4ThreeVector(0,-1,0),
		    FrameBaseBarLogic,
		    "CHANTIFramePhysics8",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)+
		    (fSquareLength + fFrameThickness)*G4ThreeVector(1,0,0),
		    FrameLateralBarLogic,
		    "CHANTIFramePhysics9",
		    fMotherVolume,
		    false,
		    fIndex);
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)+
		    (fSquareLength + fFrameThickness)*G4ThreeVector(-1,0,0),
		    FrameLateralBarLogic,
		    "CHANTIFramePhysics10",
		    fMotherVolume,
		    false,
		    fIndex);

  G4LogicalVolume * FrameSupAnt1Logic = new
    G4LogicalVolume(FrameSupport1,
		    fFrameMaterial,
		    "CHANTIFrameSupAnt1Logic",
		    0,0,0);
  /*
  G4LogicalVolume * FrameSupAnt2Logic = new
    G4LogicalVolume(FrameSupportAla,
		    fFrameMaterial,
		    "CHANTIFrameSupAnt1Logic",
		    0,0,0);
  */

  G4LogicalVolume * FrameSupPost1Logic = new
    G4LogicalVolume(FrameSupport2,
		    fFrameMaterial,
		    "CHANTIFrameSupAnt2Logic",
		    0,0,0);

  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - 3.0*fFrameThickness) +
		    fDistYHole*G4ThreeVector(0,-1,0),
		    FrameSupAnt1Logic,
		    "CHANTIFramePhysics11",
		    fMotherVolume,
		    false,
		    fIndex);

  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - 3.0*fFrameThickness) +
		    (fSquareLength-5*mm)*G4ThreeVector(0,1,0),
		    FrameSupAnt1Logic,
		    "CHANTIFramePhysics12",
		    fMotherVolume,
		    false,
		    fIndex);

  // (x, y) = (0, 33.5); z = (-1016, -958, -843, -613, -153, 767)
  // Volume half-sizes: (150.5 x 5 x 0.5)
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - 3.0*fFrameThickness) +
		    (fYInnerHalfLength+fSupportAlaY+fFrameThickness)*G4ThreeVector(0,1,0),
		    FrameSupAnt1Logic,
		    "CHANTIFramePhysics13",
		    fMotherVolume,
		    false,
		    fIndex);

  // (x, y) = (0, 33); z = (-1010.25, -952.25, -837.25, -607.25, -147.25, 772.75)
  // Volume half-sizes: (52 x 0.5 x 5.25)
  // New Z position: -3.0*fFrameThickness replaced with -2.0*fFrameThickness to avoid conflict.
  // Eventually this small element was removed as it overlaps with several volumes.
  // E Goudzovski, 22/10/2018
  /*
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings - 2.0*fFrameThickness) +
		    (fYInnerHalfLength+fFrameThickness)*G4ThreeVector(0,1,0)+
		    fSupportAlaZ*G4ThreeVector(0,0,1),
		    FrameSupAnt2Logic,
		    "CHANTIFramePhysics14",
		    fMotherVolume,
		    false,
		    fIndex);
  */
  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + 2.0*fRingThickness + 3.0*fFrameThickness) +
		    fDistXHole*G4ThreeVector(-1,0,0),
		    FrameSupPost1Logic,
		    "CHANTIFramePhysics15",
		    fMotherVolume,
		    false,
		    fIndex);

  new G4PVPlacement(0,
		    G4ThreeVector(0,0,1)*(fZPosRings + 2.0*fRingThickness + 3.0*fFrameThickness) +
		    fDistXHole*G4ThreeVector(1,0,0),
		    FrameSupPost1Logic,
		    "CHANTIFramePhysics16",
		    fMotherVolume,
		    false,
		    fIndex);
  //
  //
  //Frame Silicio => SiPM
  //
  //
  if(fDetector->GetActiveSilRing()==1) //Se nel .mac viene indicata la corona di silicio
    {
      G4VSolid* SilOrizontalBar = new G4Box("CHANTISilOrizontalBar",                         
				   fSquareLength+2.0*fFrameThickness+2.0*fSilThickness,
				   fSilThickness,
				   fRingThickness);
      G4VSolid* AlOrizontalBar = new G4Box("CHANTISilOrizontalBar",                         
				   fSquareLength+2.0*fFrameThickness+2.0*fSilThickness,
				   fSilThickness,
				   fFrameThickness);
      G4VSolid* SilVerticalBar = new G4Box("CHANTISilVerticalBar",                         
					   fSilThickness,
					   fSquareLength+2.0*fFrameThickness,
					   fRingThickness);
      G4VSolid* AlVerticalBar = new G4Box("CHANTIAlVerticalBar",                         
					   fSilThickness,
					   fSquareLength+2.0*fFrameThickness+10,
					   fFrameThickness);

      G4LogicalVolume * SilOrizontalLogic = new G4LogicalVolume(SilOrizontalBar,
							   fSilRingMaterial,
							   "CHANTISilOrizontalLogic",
							   0,0,0);
      G4LogicalVolume * AlOrizontalLogic = new G4LogicalVolume(AlOrizontalBar,
							   fFrameMaterial,
							   "CHANTISilOrizontalLogic",
							   0,0,0);
      G4LogicalVolume * SilVerticalLogic = new G4LogicalVolume(SilVerticalBar,
							   fSilRingMaterial,
							   "CHANTISilVerticalLogic",
							   0,0,0);
      G4LogicalVolume * AlVerticalLogic = new G4LogicalVolume(AlVerticalBar,
							   fFrameMaterial,
							   "CHANTISilVerticalLogic",
							   0,0,0);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)
			+ G4ThreeVector(0,1,0)*(fSquareLength+2.0*fFrameThickness+fSilThickness),
			SilOrizontalLogic,
			"CHANTIFSilRingPhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)
			+ G4ThreeVector(0,-1,0)*(fSquareLength+2.0*fFrameThickness+fSilThickness),
			SilOrizontalLogic,
			"CHANTIFSilRingPhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings-fFrameThickness)
			+ G4ThreeVector(0,1,0)*(10. + fSquareLength+2.0*fFrameThickness+fSilThickness),
			AlOrizontalLogic,
			"CHANTIFramePhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings-fFrameThickness)
			+ G4ThreeVector(0,-1,0)*(10. + fSquareLength+2.0*fFrameThickness+fSilThickness),
			AlOrizontalLogic,
			"CHANTIFramePhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)
			+ G4ThreeVector(1,0,0)*(fSquareLength+2.0*fFrameThickness+fSilThickness),
			SilVerticalLogic,
			"CHANTIFSilRingPhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings + fRingThickness)
			+ G4ThreeVector(-1,0,0)*(fSquareLength+2.0*fFrameThickness+fSilThickness),
			SilVerticalLogic,
			"CHANTIFSilRingPhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings-fFrameThickness)
			+ G4ThreeVector(1,0,0)*(fSquareLength+2.0*fFrameThickness+fSilThickness),
			AlVerticalLogic,
			"CHANTIFramePhysics",
			fMotherVolume,
			false,
			fIndex);
      new G4PVPlacement(0,
			G4ThreeVector(0,0,1)*(fZPosRings-fFrameThickness)
			+ G4ThreeVector(-1,0,0)*(fSquareLength+2.0*fFrameThickness+fSilThickness),
			AlVerticalLogic,
			"CHANTIFramePhysics",
			fMotherVolume,
			false,
			fIndex);

      G4SDManager* SDManSil = G4SDManager::GetSDMpointer();
      G4String SDNameSil("SilicioSilRing");
      CHANTISDSiPM * SiPMSD = static_cast<CHANTISDSiPM*>(SDManSil->FindSensitiveDetector(SDNameSil));
      if(!SiPMSD){
	SiPMSD = new CHANTISDSiPM(SDNameSil, G4String());
	SDManSil->AddNewDetector(SiPMSD);
      }
      SilOrizontalLogic->SetSensitiveDetector(SiPMSD);
      SilOrizontalLogic ->SetVisAttributes(new G4VisAttributes(G4Colour(1.0,0.0,1.0)));
      SilVerticalLogic->SetSensitiveDetector(SiPMSD);
      SilVerticalLogic ->SetVisAttributes(new G4VisAttributes(G4Colour(1.0,0.0,1.0)));
    }
}

void CHANTIFrame::SetProperties() {
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  //fLogicalVolume ->SetVisAttributes(fVisAtt);
}
