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

#include "HalfView.hh"
#include "Straw.hh"

#include "SpectrometerSD.hh"
#include "G4SDManager.hh"

#include "G4DisplacedSolid.hh"
#include "G4RotationMatrix.hh"

HalfView::HalfView(G4Material * Material, G4LogicalVolume * MotherVolume, G4ThreeVector Position, G4double HoleCenter, G4int iCopy) :
  NA62VComponent(Material,MotherVolume),
  fPosition(Position),
  fHoleCenter(HoleCenter),
  fiCopy(iCopy)
{
  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  SpectrometerMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

HalfView::~HalfView(){}

void HalfView::ReadGeometryParameters()
{
  SpectrometerGeometryParameters* GeoPars = SpectrometerGeometryParameters::GetInstance();

  // Read all the geometrical parameters and copy them to private members
  fZLength              = GeoPars->GetHalfViewZLength();  
  fXLength              = GeoPars->GetHalfViewXLength();  
  fYLength              = GeoPars->GetHalfViewYLength();
  fViewSize             = GeoPars->GetViewSize(); 
  fStrawSpacing         = GeoPars->GetStrawSpacing(); 
  fLayerSpacing         = GeoPars->GetLayerSpacing(); 
  fLayerDisplacement    = GeoPars->GetLayerDisplacement(); 
  fNStraws              = GeoPars->GetNStraws();

  // Hole chamber definition
 G4double delta = 0.0001;

  // y view (all the chambers)
  if (fabs(fHoleCenter)<delta) //HoleCenter for Y view is 0mm 
  { 
    fRmin = GeoPars->GetChambernXRminY();
    fRmax = GeoPars->GetChambernXRmaxY();
    fORmin = GeoPars->GetChambernXORminY();
    fORmax = GeoPars->GetChambernXORmaxY();
  }
 
  // Check which chamber and which view is being constructed
  for(G4int iChamber = 0; iChamber < 4; iChamber++){
      if(fabs(fabs(fHoleCenter)-fabs(GeoPars->GetChamberXDisplacement(iChamber))/sqrt(2.))<delta){ 
          fRmin = GeoPars->GetChamberXRminU(iChamber); 
          fRmax = GeoPars->GetChamberXRmaxU(iChamber);
	  fORmin = GeoPars->GetChamberXORminU(iChamber);
	  fORmax = GeoPars->GetChamberXORmaxU(iChamber);
      }
      if(fabs(fabs(fHoleCenter)-fabs(GeoPars->GetChamberXDisplacement(iChamber)))<delta){ 
          fRmin = GeoPars->GetChamberXRminX(iChamber); 
          fRmax = GeoPars->GetChamberXRmaxX(iChamber);
	  fORmin = GeoPars->GetChamberXORminX(iChamber);
	  fORmax = GeoPars->GetChamberXORmaxX(iChamber);
      }
  } 
}

void HalfView::CreateGeometry()
{
  fSolidVolume= new G4Box("HalfView",0.5*fXLength,0.5*fYLength,0.5*fZLength);
  fLogicalVolume= new G4LogicalVolume(fSolidVolume,fMaterial,"HalfView",0,0,0);
  fPhysicalVolume = new G4PVPlacement(0,fPosition,fLogicalVolume,"HalfView",fMotherVolume,false,fiCopy);
 
  // Build the straw tubes
  G4double pos;
  G4int iLayer1 = 0;
  G4int iLayer2 = 1000;

  for (G4int iStraw=0; iStraw<fNStraws; iStraw++){      
      if ((fabs(fHoleCenter)<0.0001)){ //Check for view Y
	pos=((G4double)iStraw - ((G4double)fNStraws/2.))*fStrawSpacing - fStrawSpacing/2; 
      } else {
	pos = ((G4double)iStraw-((G4double)fNStraws/2.)+1)*fStrawSpacing - fStrawSpacing/2.; 
      }
      // Layer 1
      if(fiCopy==0){ //halfview 1
	pos += fLayerDisplacement/2.;
      } else { //halfview 0
	pos -= fLayerDisplacement/2.;
      }      
      if (((pos+fPosition.x()<=fRmin) && (pos+fPosition.x()>=fORmin)) || ((pos+fPosition.x()>=fRmax-0.0001) && (pos+fPosition.x()<=fORmax))){ 
	new Straw(0,fLogicalVolume,G4ThreeVector(pos,0.,-fLayerSpacing/2.),iLayer1++);      
      } else {
	iLayer1++;
      }
      
      if ((fabs(fHoleCenter)<0.0001)){ //Check for view Y
	pos=((G4double)iStraw - ((G4double)fNStraws/2.))*fStrawSpacing - fStrawSpacing/2.; 
      } else {
	pos = ((G4double)iStraw-((G4double)fNStraws/2.)+1)*fStrawSpacing - fStrawSpacing/2.;
      }
      // Layer 2
      if(fiCopy==0){ //halfview 1
	pos -= fLayerDisplacement/2.;
      } else { //halfview 0
	pos += fLayerDisplacement/2.;
      }      
      if (((pos+fPosition.x()<=fRmin) && (pos+fPosition.x()>=fORmin)) || ((pos+fPosition.x()>=fRmax-0.0001) && (pos+fPosition.x()<=fORmax))){ 
	new Straw(0,fLogicalVolume,G4ThreeVector(pos,0.,fLayerSpacing/2.),iLayer2++);
      } else {
        iLayer2++;
      }  
  }
}

void HalfView::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,1.0,1.0));
  fVisAtt -> SetVisibility(false);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
