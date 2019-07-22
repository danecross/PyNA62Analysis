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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------
#ifndef NewCHODScintillatorCounter_H
#define NewCHODScintillatorCounter_H 1

#include "NA62VComponent.hh"
#include "NewCHODGeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class NewCHODScintillatorCounter : public NA62VComponent {

public:

  NewCHODScintillatorCounter(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int, G4int);
  ~NewCHODScintillatorCounter() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4int GetBrickID()          { return fBrickID; }
  void  SetBrickID(G4int val) { fBrickID = val;  }
  G4int GetID()               { return fID;      }
  void  SetID(G4int val)      { fID = val;       }

private:

  G4int    fBrickID; ///< Unique ID of the brick
  G4int    fID;      ///< Geometric ID (can be shared by several bricks) mapped into readout ID
  G4double fInnerRadius;
  G4double fOuterRadius;
  G4ThreeVector fScintillatorSize;
  G4ThreeVector fScintillatorPosition;
};

#endif
