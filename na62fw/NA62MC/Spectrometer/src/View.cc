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
// Created by Giuseppe Ruggiero (Giuseppe.Ruggiero@cern.ch) 2008-04-18
//            Antonino Sergi (Antonino.Sergi@cern.ch)
// Modified by Giuseppe Ruggiero 2010-03-03
// Modified by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-01-15
// Modified by Zuzana Kucerova 2019-04-24 - fixed geometry conflicts
// --------------------------------------------------------------------
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "G4Box.hh"

#include "SpectrometerGeometryParameters.hh"
#include "SpectrometerMaterialParameters.hh"

#include "View.hh"
#include "HalfView.hh"

#include "SpectrometerSD.hh"
#include "G4SDManager.hh"

#include "G4DisplacedSolid.hh"
#include "G4RotationMatrix.hh"

View::View(G4Material * Material, G4LogicalVolume * MotherVolume, G4Transform3D Transform3D, G4double HoleCenter, G4int iCopy) : NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  fTransform3D = Transform3D;  
  fHoleCenter = HoleCenter;
  fiCopy = iCopy;

  // Mandatory here to Find or Build the needed materials
  SpectrometerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

View::~View(){}

void View::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  SpectrometerGeometryParameters* GeoPars = SpectrometerGeometryParameters::GetInstance();
  fZLength = GeoPars->GetViewZLength();  
  fXLength = GeoPars->GetViewXLength();  
  fYLength = GeoPars->GetViewYLength();  
  fHalfViewSpacing      = GeoPars->GetHalfViewSpacing(); 
  fHalfViewDisplacement = GeoPars->GetHalfViewDisplacement();
}

void View::CreateGeometry()
{
  fSolidVolume    = new G4Box("View",0.5*fXLength,0.5*fYLength,0.5*fZLength);
  fLogicalVolume  = new G4LogicalVolume(fSolidVolume,fMaterial,"View",0,0,0);
  fPhysicalVolume = new G4PVPlacement(fTransform3D,fLogicalVolume,"View",fMotherVolume,false,fiCopy);
  G4double rotationAngle = fTransform3D.getRotation().phiX();
  G4double HoleCenter    = fHoleCenter*cos(rotationAngle);
////  G4cout << fiCopy;
//in views V and Y the halfviews are exchanged in comparison with views U and X (also the planes, but that is encountered for in HalfView class)
  if (fiCopy==1 || fiCopy==3){
    new HalfView(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(-fHalfViewDisplacement/2.,0.,-fHalfViewSpacing/2.),HoleCenter,0);
    new HalfView(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(+fHalfViewDisplacement/2.,0.,+fHalfViewSpacing/2.),HoleCenter,1); 
 } else {
    new HalfView(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(+fHalfViewDisplacement/2.,0.,-fHalfViewSpacing/2.),HoleCenter,0); // warning sign of Displacement
    new HalfView(G4Material::GetMaterial("G4_Galactic"),fLogicalVolume,G4ThreeVector(-fHalfViewDisplacement/2.,0.,+fHalfViewSpacing/2.),HoleCenter,1);
  }
}
void View::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
