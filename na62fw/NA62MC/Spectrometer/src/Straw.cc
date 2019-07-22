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
// Modified by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-01-07
// - Removed outer copper layer, golden layer moved inside the straw
//
// Created by Giuseppe Ruggiero (Giuseppe.Ruggiero@cern.ch) 2008-04-18
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "SpectrometerGeometryParameters.hh"
#include "SpectrometerMaterialParameters.hh"

#include "Straw.hh"

#include "SpectrometerSD.hh"
#include "G4SDManager.hh"

#include "G4RotationMatrix.hh"

Straw::Straw(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4int iCopy) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fPosition = Position;  

  fiCopy = iCopy;

  // Mandatory here to Find or Build the needed materials
  SpectrometerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

Straw::~Straw(){}

void Straw::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  SpectrometerGeometryParameters* GeoPars = SpectrometerGeometryParameters::GetInstance();
  fStrawInnerRadius = GeoPars->GetStrawInnerRadius(); 
  fCopperThickness  = GeoPars->GetCopperThickness(); 
  fMylarThickness   = GeoPars->GetMylarThickness(); 
  fGoldThickness    = GeoPars->GetGoldThickness(); 
  fStrawDiameter    = GeoPars->GetStrawDiameter(); 
  fStrawLength      = GeoPars->GetStrawLength(); 
  fWireRadius       = GeoPars->GetWireRadius(); 
  fStrawRadius      = GeoPars->GetStrawRadius(); 
}

void Straw::CreateGeometry()
{
  // Mylar
  G4double Rin = 0;
  G4double Rout = fStrawRadius;
  G4RotationMatrix Mrot;
  Mrot.rotateX(90.*deg);
  
  solidAbsorber = new G4Tubs("Absorber",Rin,Rout,fStrawLength/2,0.,360.*deg); 
  logicAbsorber = new G4LogicalVolume(solidAbsorber,G4Material::GetMaterial("G4_MYLAR"),"Absorber");
  physiAbsorber = new G4PVPlacement(G4Transform3D(Mrot,fPosition),logicAbsorber,"Absorber",fMotherVolume,false,fiCopy);

  // Incopper
  Rout = Rout-fMylarThickness;
  solidIncopper = new G4Tubs("Incopper",Rin,Rout,fStrawLength/2,0.,360.*deg); 
  logicIncopper = new G4LogicalVolume(solidIncopper,G4Material::GetMaterial("G4_Cu"),"Incopper");
  physiIncopper = new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),logicIncopper,"Incopper",logicAbsorber,false,fiCopy);

  // Ingold
  Rout = Rout-fCopperThickness;
  solidIngold = new G4Tubs("Ingold",Rin,Rout,fStrawLength/2,0.,360.*deg); 
  logicIngold = new G4LogicalVolume(solidIngold,G4Material::GetMaterial("G4_Au"),"Ingold");
  physiIngold = new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),logicIngold,"Ingold",logicIncopper,false,fiCopy);

  // Gas (sensitive)
  Rout = Rout-fGoldThickness; 
  solidStrawGas = new G4Tubs("StrawGas",Rin,Rout,fStrawLength/2,0.,360.*deg); 
  logicStrawGas = new G4LogicalVolume(solidStrawGas,G4Material::GetMaterial("GasStraw"),"StrawGas");
  physiStrawGas = new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),logicStrawGas,"StrawGas",logicIngold,false,fiCopy);

  // Wire
  Rout = fWireRadius;  
  solidWire = new G4Tubs("Wire",Rin,Rout,fStrawLength/2,0.,360.*deg); 
  logicWire = new G4LogicalVolume(solidWire,G4Material::GetMaterial("Luma861"),"Wire");       
  physiWire = new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),logicWire,"Wire",logicStrawGas,false,fiCopy); 

  fSolidVolume = solidStrawGas;
  fLogicalVolume = logicStrawGas;
  fPhysicalVolume = physiStrawGas;

  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String SpectrometerSDname = "/Spectrometer";
  G4VSensitiveDetector * spectrometerSD = SDman->FindSensitiveDetector(SpectrometerSDname);
  fLogicalVolume->SetSensitiveDetector(spectrometerSD);  
}

void Straw::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);

  G4VisAttributes* wireVisAtt = new G4VisAttributes(G4Colour::Red());
//  wireVisAtt -> SetVisibility(false);
  logicWire->SetVisAttributes(wireVisAtt);  

  G4VisAttributes* strawgasVisAtt = new G4VisAttributes(G4Colour(1.0, 1.0, 0.0, 0.1));
  strawgasVisAtt -> SetVisibility(false);
  logicStrawGas->SetVisAttributes(strawgasVisAtt);

  G4VisAttributes* ingoldVisAtt = new G4VisAttributes(G4Colour::Yellow());
  ingoldVisAtt -> SetVisibility(false);
  logicIngold->SetVisAttributes(ingoldVisAtt);

  G4VisAttributes* incopperVisAtt = new G4VisAttributes(G4Colour::Red());
  incopperVisAtt -> SetVisibility(false);
  logicIncopper->SetVisAttributes(incopperVisAtt);

  G4VisAttributes* AbsorberVisAtt = new G4VisAttributes(G4Colour(0.,1.0,1.0,0.3));
  logicAbsorber->SetVisAttributes(AbsorberVisAtt);
}
