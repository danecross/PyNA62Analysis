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
// Created by Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#ifndef HACModule_H
#define HACModule_H 1

#include "NA62VComponent.hh"
#include "HACGeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;

class HACModule : public NA62VComponent
{

public:
  
  ~HACModule();
  HACModule(G4Material*, G4LogicalVolume*, G4Transform3D, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4LogicalVolume*     GetLogicalVolume() {return fLogicalVolume;};

private:

  G4int fiCopy;
  G4Transform3D fTransform3D;

  G4double fHACModuleXLength;
  G4double fHACModuleYLength;
  G4double fHACModuleZLength;

  G4int    fNLayers;

  G4double fAbsorberLayerXLength;
  G4double fAbsorberLayerYLength;
  G4double fAbsorberLayerZLength;
  
  G4double fScintillatorLayerXLength;
  G4double fScintillatorLayerYLength;
  G4double fScintillatorLayerZLength; 

};

#endif
