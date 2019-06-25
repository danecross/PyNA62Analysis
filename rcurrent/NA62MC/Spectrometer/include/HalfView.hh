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
#ifndef HalfView_H
#define HalfView_H 1

#include "NA62VComponent.hh"
#include "SpectrometerGeometryParameters.hh"
#include "globals.hh"
#include "G4Tubs.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class HalfView : public NA62VComponent
{

public:
  
  ~HalfView();
  HalfView(G4Material*, G4LogicalVolume*, G4ThreeVector, G4double, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

private:
  G4ThreeVector fPosition;
  G4double fHoleCenter;  
  G4int fiCopy;
  G4int    fNStraws;
  G4double fViewSize; 
  G4double fStrawSpacing;
  G4double fLayerSpacing;
  G4double fLayerDisplacement;
  G4double fZLength; 
  G4double fXLength;
  G4double fYLength;
  G4double fHalfViewDisplacement;
  G4double fRmin;
  G4double fRmax;
G4double fORmin;
G4double fORmax;

};

#endif
