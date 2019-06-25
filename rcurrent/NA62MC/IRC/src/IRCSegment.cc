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
//
// --------------------------------------------------------------------
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "IRCGeometryParameters.hh"
#include "IRCMaterialParameters.hh"

#include "IRCSegment.hh"
#include "IRCAluminumLayer.hh"
#include "IRCAbsorberLayer.hh"
#include "IRCScintillatorLayer.hh"

IRCSegment::IRCSegment(G4Material * Material, G4LogicalVolume * MotherVolume, 
		       G4Transform3D Transform3D,
		       G4double InnerRadius, G4double OuterRadius, G4int NLayers, G4int iCopy) : 
  NA62VComponent(Material,MotherVolume),
  fiCopy(iCopy),
  fTransform3D(Transform3D),
  fInnerRadius(InnerRadius),
  fOuterRadius(OuterRadius),
  fNLayers(NLayers)
{
  ReadGeometryParameters();
  // Mandatory here to Find or Build the needed materials
  IRCMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

IRCSegment::~IRCSegment(){}

void IRCSegment::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  IRCGeometryParameters* GeoPars = IRCGeometryParameters::GetInstance();
  fNSegments = GeoPars->GetNSegments();
  fPhiAngle = GeoPars->GetSegmentPhiAngle();
  fAbsorberLayerZLength = GeoPars->GetAbsorberLayerZLength();
  fScintillatorLayerZLength = GeoPars->GetScintillatorLayerZLength();
  fAluminumLayerZLength = GeoPars->GetAluminumLayerZLength();
  fLayerSpacing = GeoPars->GetLayerSpacing();
  G4int ModuleIndex = fiCopy/10;
  if( ModuleIndex == 1  ) {
    fZLength = fNLayers*fLayerSpacing + fAluminumLayerZLength + GeoPars->GetAluminumCloseZLength(); 
  } else {
    fZLength = fNLayers*fLayerSpacing + 2*fAluminumLayerZLength;
  }
  fXdisplacement = GeoPars->GetIRCInnerHoleDisplacementX();
}

void IRCSegment::CreateGeometry()
{
  G4double HalfZLength = 0.5*fZLength;
  G4double deltaPhiAngle = fPhiAngle;
  G4double startPhiAngle = (fiCopy%10)*deltaPhiAngle;

  G4int ModuleIndex = fiCopy/10;
  IRCGeometryParameters* GeoPars = IRCGeometryParameters::GetInstance();

  G4Tubs* plate = new G4Tubs("Segment",
			     0.,
			     fOuterRadius,
			     HalfZLength,
			     startPhiAngle,
			     deltaPhiAngle);   
  G4Tubs* hole = new G4Tubs("Hole",
			    0.,fInnerRadius,
			    HalfZLength*1.01,
			    0,360*deg);


  G4RotationMatrix* RotationHole = new G4RotationMatrix;
  G4ThreeVector Displacement = G4ThreeVector(fXdisplacement,0.,0.);
  fSolidVolume = new G4SubtractionSolid("IRCSegment",plate,hole,RotationHole,Displacement);

  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                 // solid
				      fMaterial,                    // material
				      "IRCegment",                  // name
				      0,                            // field manager 
				      0,                            // sensitive detector
				      0);                           // user limits
  fPhysicalVolume = new G4PVPlacement(fTransform3D,
				      fLogicalVolume,             // its logical volume
				      "IRCSegment",               // its name
				      fMotherVolume,              // its mother  volume
				      false,                      // no boolean operations
				      fiCopy);                    // copy number
  // Place aluminium supports
  G4RotationMatrix Rotation;
  G4ThreeVector PositionAluminumFront = G4ThreeVector(0.,0., -fZLength*0.5 + fAluminumLayerZLength*0.5);
  G4Transform3D AluminumFrontTransform3D  = G4Transform3D(Rotation, PositionAluminumFront);
  new IRCAluminumLayer(G4Material::GetMaterial("G4_Al"), fLogicalVolume, AluminumFrontTransform3D, fInnerRadius, fOuterRadius, GeoPars->GetAluminumLayerZLength(), fiCopy*100+0);
  G4double Thickness;
  if(ModuleIndex == 1) {
    Thickness =  GeoPars->GetAluminumCloseZLength();
  } else {
    Thickness =  GeoPars->GetAluminumLayerZLength();
  }
  G4ThreeVector PositionAluminumBack = G4ThreeVector(0.,0., fZLength*0.5 - Thickness*0.5);
  G4Transform3D AluminumBackTransform3D  = G4Transform3D(Rotation, PositionAluminumBack);
  new IRCAluminumLayer(G4Material::GetMaterial("G4_Al"), fLogicalVolume, AluminumBackTransform3D, fInnerRadius, fOuterRadius, Thickness, fiCopy*100+1);
  // Fill the segment with absorber and scintillator layers
  for(G4int iLayer = 0; iLayer<fNLayers;iLayer++) {
    G4ThreeVector Position = G4ThreeVector(0.,0., -fZLength*0.5 + fAluminumLayerZLength + iLayer*fLayerSpacing + 0.5*fAbsorberLayerZLength);
    G4Transform3D AbsorberTransform3D = G4Transform3D(Rotation,Position);
    new IRCAbsorberLayer(G4Material::GetMaterial("IRC_PbSnAlloy"), fLogicalVolume, AbsorberTransform3D, fInnerRadius, fOuterRadius, fiCopy*100 + iLayer);
    Position += G4ThreeVector(0.,0.,0.5*fLayerSpacing);
    G4Transform3D ScintillatorTransform3D = G4Transform3D(Rotation,Position);
    new IRCScintillatorLayer(G4Material::GetMaterial("G4_POLYSTYRENE"), fLogicalVolume, ScintillatorTransform3D, fInnerRadius, fOuterRadius, fiCopy*100 + iLayer);    
  }

}

void IRCSegment::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
