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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
// Modified by Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 2010-03-03
// Modified by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-01-15
// --------------------------------------------------------------
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "G4Tubs.hh"

#include "SpectrometerGeometryParameters.hh"
#include "SpectrometerMaterialParameters.hh"

#include "Chamber.hh"
#include "View.hh"

#include "G4RotationMatrix.hh"

Chamber::Chamber(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4double HoleCenter, G4int iCopy) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fPosition = Position;
  fHoleCenter = HoleCenter;
  fiCopy = iCopy;

  // Mandatory here to Find or Build the needed materials
  SpectrometerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

Chamber::~Chamber(){}

void Chamber::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  SpectrometerGeometryParameters* GeoPars = SpectrometerGeometryParameters::GetInstance();
  fRadius               = GeoPars->GetChamberRadius();  
  fZLength              = GeoPars->GetChamberZLength();  
  fViewSpacing[0]       = GeoPars->GetViewSpacing1(); 
  fViewSpacing[1]       = GeoPars->GetViewSpacing2(); 
  fViewZLength          = GeoPars->GetViewZLength();
}

void Chamber::CreateGeometry()
{
  fSolidVolume    = new G4Tubs("Chamber",0,fRadius,0.5*fZLength,0.,360.*deg);
  fLogicalVolume  = new G4LogicalVolume(fSolidVolume,fMaterial,"Chamber",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,fPosition,fLogicalVolume,"Chamber",fMotherVolume,false,fiCopy);
  
  // Views: UVXY
  G4double nViews = 4.;
  G4double zPosition = 0.;
  for (G4int iView = 0; iView < nViews; iView++){
    if (iView==0) zPosition = -(fViewSpacing[1]-fViewZLength)/2.-fViewSpacing[0]-fViewZLength/2.;
    if (iView==1) zPosition = -(fViewSpacing[1]-fViewZLength)/2.-fViewZLength/2.;
    if (iView==2) zPosition = +(fViewSpacing[1]-fViewZLength)/2.+fViewZLength/2.;
    if (iView==3) zPosition = +(fViewSpacing[1]-fViewZLength)/2.+fViewSpacing[0]+fViewZLength/2.;
    G4RotationMatrix Mrot;
    Mrot.rotateZ((iView >= 2 ? iView-2 : (iView-0.5))*90*deg);
    new View(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4Transform3D(Mrot,G4ThreeVector(0.,0.,zPosition)),fHoleCenter,iView);
  }
}

void Chamber::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,0.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
