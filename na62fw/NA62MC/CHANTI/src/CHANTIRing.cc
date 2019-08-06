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
// Created by Vito Palladino
//
// --------------------------------------------------------------

#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "CHANTIGeometryParameters.hh"
#include "CHANTIMaterialParameters.hh"

#include "CHANTIRing.hh"
#include "CHANTIStrip.hh"

#include "CHANTISD.hh"
#include "G4SDManager.hh"

#include "fstream"

using namespace std;

CHANTIRing::CHANTIRing(G4Material * Material,
                     G4LogicalVolume * MotherVolume,
                     G4double ZPosition,
		     G4RotationMatrix* RingRotation,
		     G4double XInnerHalfLength,
		     G4double YInnerHalfLength,
                     G4int NCopies) :
  NA62VComponent(Material,MotherVolume),
  fZRingPos(ZPosition),
  fNCopies(NCopies),
  fRingRotation(RingRotation),
  fXInnerHalfLength(XInnerHalfLength),
  fYInnerHalfLength(YInnerHalfLength)
{
  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  CHANTIMaterialParameters::GetInstance();

  CreateGeometry();
  SetProperties();

}

void CHANTIRing::ReadGeometryParameters() {
  // Read all the geometrical parameters and copy them to private members

  CHANTIGeometryParameters* GeoPars = CHANTIGeometryParameters::GetInstance();
  fRingThickness = GeoPars->GetCHANTIRingThickness();
  fTriangleBase = GeoPars->GetCHANTITriangleBase();
  fHalfStripLength = GeoPars->GetCHANTIStripLength()/2.;
  fHalfSquareLength = GeoPars->GetCHANTISquareLength()/2.;
}

void CHANTIRing::CreateGeometry() {
  // Build the cilinder that will contain the single sub ring

  G4double HalfTriangleBase = fTriangleBase/2.;
  G4double HalfThickness = fRingThickness/2.;

  fSolidVolume = new G4Box("Ring",                // name
			   fHalfSquareLength,    // X
			   fHalfSquareLength,   // Y
			   HalfThickness       // Z
			   );

  fLogicalVolume= new G4LogicalVolume(fSolidVolume,                     // solid
				      fMaterial,                       // material
				      "Ring",                         // name
				      0,                             // field manager
				      0,                            // sensitive detector
				      0);                          // user limitsPosition


  fPhysicalVolume = new G4PVPlacement(fRingRotation,                     // rotation
				      G4ThreeVector(0.,0.,fZRingPos),   // its position
				      fLogicalVolume,                  // its logical volume
				      "Ring",                         // its name
				      fMotherVolume,                 // its mother  volume
				      false,                        // no boolean operations
				      fNCopies,                    // copy number
				      false);

  G4double heightOut;
  G4double heightIn;

  G4double baseIn = fYInnerHalfLength;
  G4double baseOut = fHalfSquareLength;

  G4double X   =  0.*mm;
  G4double Y   =  0.*mm;
  G4ThreeVector Position(X, Y);

  G4double HalfStripLength;

  //G4int NofStripsFLPerRing=0; // FL = frontal layer
  //G4int NofStripsBLPerRing=0; // BL = backward layer

  HalfStripLength = (baseOut - fYInnerHalfLength)/2.;
  Y = fYInnerHalfLength + HalfStripLength;

  // upstream layer
  G4int SubLayerID = 1; // define the id of a bar
  G4int IdPosition = 1; // define the position of each bar
  // the first two Strip are placed on the TOP and on the bottom of center of the hole!
  Position.setY(Y);  // first strip X=0
  Int_t id ;
  Int_t StationID ;
  if(fNCopies%2 == 0) {
    StationID = int(fNCopies/2 + 1)*100000 ;
    //G4cout<<"# copies = "<<fNCopies<<endl;
  }
  else {
    StationID = int(fNCopies/2 + 1)*100000 + 10000 ;
    //G4cout<<"# copies = "<<fNCopies<<endl;
  }
  id = -(X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
  if(id<0)  id = StationID + 1000 + abs(id);
  else id = StationID + abs(id);
  //G4cout<<"X1 = "<<X<<", Y = "<<Y<<", id = "<<id<<G4endl;
  fStripsID.push_back(id);
  new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		 fLogicalVolume,
		 Position,
		 0,
		 HalfStripLength,
		 id);

  // the first two Strip are placed on the top and on the BOTTOM of center of the hole!
  Position.setY(-Y); // X=0
  id = (X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ;
  if(id<0)  id = StationID + 1000 + abs(id);
  else id = StationID + abs(id);
  //G4cout<<"X2 = "<<X<<", Y = "<<-Y<<", id = "<<id<<G4endl;
  fStripsID.push_back(id);
  new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		 fLogicalVolume,
		 Position,
		 0,
		 HalfStripLength,
		 id); // -Y is labelled with a +-100 since it is +-X
  //NofStripsFLPerRing++;


  do{
    //then next layer
    X = HalfTriangleBase*(IdPosition)*2;
    Position.setX(X);

    heightOut = HalfTriangleBase*( 2*IdPosition+1 );
    heightIn  = HalfTriangleBase*( 2*IdPosition-1 );

    if( heightIn<fXInnerHalfLength ){
      //NofStripsFLPerRing += 2;
      HalfStripLength = (baseOut - baseIn)/2.;

      Y =  HalfStripLength + baseIn;

      Position.setY(Y);
      id = -(X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID + 1000 + abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X3 = "<<X<<", Y = "<<Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     0,
		     HalfStripLength,
		     id);

      Position.setY(-Y);
      id = (X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X4 = "<<X<<", Y = "<<-Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     0,
		     HalfStripLength,
		     id);

      Position.setX(-X);
      Position.setY(Y);
      id = -(-X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X5 = "<<-X<<", Y = "<<Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     0,
		     HalfStripLength,
		     id);

      Position.setX(-X);
      Position.setY(-Y);
      id = (-X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X6 = "<<-X<<", Y = "<<-Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     0,
		     HalfStripLength,
		     id);


    }

    else if( baseOut>0 ){

      HalfStripLength = baseOut;

      Y = 0.*mm;
      Position.setY(Y);
      id = (X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000+abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X7 = "<<X<<", Y = "<<Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     0,
		     HalfStripLength,
		     id);


      Position.setX(-X);
      id = (-X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000+abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X8 = "<<-X<<", Y = "<<Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     0,
		     HalfStripLength,
		     id);


    }

    SubLayerID++;
    IdPosition++;

  }while( (heightOut+2*HalfTriangleBase)<fHalfSquareLength );


  // placing the seconf layer; rotation strip 180° "dwstream"
  G4RotationMatrix* StripRotation = new G4RotationMatrix();
  StripRotation->rotateX(180.*deg);

  SubLayerID=1; // !!! No StripID=0
  IdPosition=0;

  do{

    X = HalfTriangleBase*( 1+IdPosition*2 );
    Position.setX(X);

    heightIn  = HalfTriangleBase*2*IdPosition;
    heightOut = HalfTriangleBase*2*(IdPosition+1);

    if( heightIn<fXInnerHalfLength ){
      //NofStripsBLPerRing+=2;

      HalfStripLength = (baseOut - baseIn)/2.;

      Y =  HalfStripLength + baseIn;

      Position.setY(Y);
      id = -(X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X9 = "<<X<<", Y = "<<Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength,
		     id);

      Position.setY(-Y);
      id = (X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000+abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X10 = "<<X<<", Y = "<<-Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength,
		     id);

      Position.setX(-X);
      Position.setY(Y);
      id = -(-X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X11 = "<<-X<<", Y = "<<Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength,
		     id);

      Position.setX(-X);
      Position.setY(-Y);
      id = (-X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      if(id<0)  id = StationID  + 1000+abs(id);
      else id = StationID + abs(id);
      //G4cout<<"X12 = "<<-X<<", Y = "<<-Y<<", id = "<<id<<G4endl;
      fStripsID.push_back(id);
      new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength,
		     id);


    }
    else if( baseOut>0 ){
      HalfStripLength = baseOut;

      Y = 0.*mm;
      Position.setY(Y);
      id = (X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      int idTemp = id;
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);
      if (idTemp/10==11 || idTemp/10==-11){
         //G4cout<<"X14 = "<<X<<", Y = "<<fHalfSquareLength/2<<", id = "<<id<<G4endl;
         Position.setY(-fHalfSquareLength/2);
         fStripsID.push_back(id);
         new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength/2,
		     id);
         Position.setY(fHalfSquareLength/2);
         //G4cout<<"X14 = "<<X<<", Y = "<<-fHalfSquareLength/2<<", id = "<<id+1000<<G4endl;
         fStripsID.push_back(id+1000);
         new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength/2,
		     id+1000);

      } else {
fStripsID.push_back(id);
         new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength,
		     id);
      }

      Position.setX(-X);
      id = (-X + fHalfSquareLength - HalfTriangleBase)/HalfTriangleBase*10 ; // P.M.
      idTemp=id;
      if(id<0)  id = StationID  + 1000 + abs(id);
      else id = StationID + abs(id);

      if (idTemp/10 == 5){
         //G4cout<<"X15 = "<<-X<<", Y = "<<fHalfSquareLength/2<<", id = "<<id<<G4endl;
         Position.setY(-fHalfSquareLength/2);
         fStripsID.push_back(id);
         new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength/2,
		     id);
         //G4cout<<"X15 = "<<-X<<", Y = "<<-fHalfSquareLength/2<<", id = "<<id+1000<<G4endl;
         Position.setY(fHalfSquareLength/2);
         fStripsID.push_back(id+1000);
         new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength/2,
		     id+1000);

      } else {
  fStripsID.push_back(id);
         new CHANTIStrip(G4Material::GetMaterial("G4_POLYSTYRENE"),
		     fLogicalVolume,
		     Position,
		     StripRotation,
		     HalfStripLength,
		     id);
      }
    }

    SubLayerID++;
    IdPosition++;

  }while( (heightOut+2*HalfTriangleBase)<fHalfSquareLength);
}


void CHANTIRing::SetProperties()
{

  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(1.0,.0,.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}

