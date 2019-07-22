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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) March 2014
//
// --------------------------------------------------------------------
#ifndef MUV3Module_H
#define MUV3Module_H 1

#include "NA62VComponent.hh"
#include "MUV3GeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV3Module : public NA62VComponent {

public:

  MUV3Module(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int, G4int);
  ~MUV3Module() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  G4VSolid *ModuleShape(G4String, G4double);

public:

  G4int GetModuleType() {return fModuleType; }
  G4int GetModuleID()   {return fModuleID;   }

private:

  G4int    fModuleType;
  G4int    fModuleID;
  G4String fName;
  G4String fScintillatorName;

  G4double fScintillatorZLength;
  G4double fXPMT1;
  G4double fYPMT1;
  G4double fXPMT2;
  G4double fYPMT2;
  G4double fZLength;
  G4double fInnerRadius;
  G4double fSize;

  G4ThreeVector fPosition;
};

#endif
