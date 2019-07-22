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
// --------------------------------------------------------------------
#ifndef MUV3PMT_H
#define MUV3PMT_H 1

#include "NA62VComponent.hh"
#include "MUV3GeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV3PMT : public NA62VComponent {

public:

  MUV3PMT(G4Material*, G4LogicalVolume*, G4double, G4double, G4int);
  ~MUV3PMT() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties() {}

  G4LogicalVolume* GetLogicalVolume() { return fLogicalVolume; }

private:

  G4int    fID;
  G4double fX;
  G4double fY;
  G4String fName;
  G4double fModuleZStart;
  G4double fModuleZLength;
  G4double fPMTZStart;
  G4double fPMTWindowRadius;
  G4double fPMTWindowThickness;
};

#endif