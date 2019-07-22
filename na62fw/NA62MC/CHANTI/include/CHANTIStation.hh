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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef CHANTIStation_H
#define CHANTIStation_H 1

#include "NA62VComponent.hh"
#include "CHANTIGeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4Material;
class G4VisAttributes;

class CHANTIStation : public NA62VComponent
{

public:
  
  ~CHANTIStation();
  CHANTIStation(G4Material*, G4LogicalVolume*,int index);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4double fZPosRings;
  G4double fRingThickness;
  G4double fXInnerHalfLength;
  G4double fYInnerHalfLength;
  G4LogicalVolume *fLogicalVolume;
  int fIndex;
  G4double fSquareLength;
};

#endif
