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
//
// --------------------------------------------------------------------
#ifndef Straw_H
#define Straw_H 1

#include "NA62VComponent.hh"
#include "SpectrometerGeometryParameters.hh"
#include "globals.hh"
#include "G4Tubs.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class Straw : public NA62VComponent
{

public:
  
  ~Straw();
  Straw(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:
  G4double  GetStrawInnerRadius(){return fStrawInnerRadius;}
  void SetStrawInnerRadius(G4double  value){fStrawInnerRadius=value;}
  G4double  GetCopperThickness(){return fCopperThickness;}
  void SetCopperThickness(G4double  value){fCopperThickness=value;}
  G4double  GetMylarThickness(){return fMylarThickness;}
  void SetMylarThickness(G4double  value){fMylarThickness=value;}
  G4double  GetGoldThickness(){return fGoldThickness;}
  void SetGoldThickness(G4double  value){fGoldThickness=value;}
  G4double  GetStrawRadius(){return fStrawRadius;}
  void SetStrawRadius(G4double  value){fStrawRadius=value;}
  G4double  GetStrawDiameter(){return fStrawDiameter;}
  void SetStrawDiameter(G4double  value){fStrawDiameter=value;}
  G4double  GetStrawLength(){return fStrawLength;}
  void SetStrawLength(G4double  value){fStrawLength=value;}
  G4double  GetWireRadius(){return fWireRadius;}
  void SetWireRadius(G4double  value){fWireRadius=value;}
  G4ThreeVector  GetPosition(){return fPosition;}
  void SetPosition(G4ThreeVector  value){fPosition=value;}

private:
  G4int fiCopy;
  G4double  fStrawInnerRadius;
  G4double  fCopperThickness;
  G4double  fMylarThickness;
  G4double  fGoldThickness;
  G4double  fStrawRadius;
  G4double  fStrawDiameter; 
  G4double  fStrawLength;
  G4double  fWireRadius;
  G4ThreeVector fPosition;

  G4VSolid*          solidAbsorber;
  G4LogicalVolume*   logicAbsorber;
  G4VPhysicalVolume* physiAbsorber;
  G4Tubs*            solidIncopper;
  G4LogicalVolume*   logicIncopper;
  G4VPhysicalVolume* physiIncopper;
  G4Tubs*            solidIngold;
  G4LogicalVolume*   logicIngold;
  G4VPhysicalVolume* physiIngold;
  G4Tubs*            solidStrawGas;
  G4LogicalVolume*   logicStrawGas;
  G4VPhysicalVolume* physiStrawGas;
  G4Tubs*            solidWire;
  G4LogicalVolume*   logicWire;
  G4VPhysicalVolume* physiWire;
};

#endif
