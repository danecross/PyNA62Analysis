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
//---------------------------------------------------------------------
//
//  Created by Gia Khoriauli (gia.khoriauli@cern.ch)
//
//---------------------------------------------------------------------

#ifndef MUV1TransportationTube_H
#define MUV1TransportationTube_H 1

#include "NA62VComponent.hh"
#include "globals.hh"

class G4LogicalVolume;

class MUV1TransportationTube : public NA62VComponent
{

public:
  
  MUV1TransportationTube(G4Material*, G4LogicalVolume*, G4double, G4ThreeVector, G4int);
  ~MUV1TransportationTube() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4int         fiCopy;

  G4double      fTubeLength;
  G4double      fTubeInnerRadius;
  G4double      fTubeOuterRadius;
  G4double      fAirGapWidht;
  G4ThreeVector fTubePosition;


};

#endif