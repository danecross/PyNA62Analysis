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
#ifndef SACSegment_H
#define SACSegment_H 1

#include "NA62VComponent.hh"
#include "SACGeometryParameters.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class SACSegment : public NA62VComponent
{

public:
  
  ~SACSegment();
  SACSegment(G4Material*, G4LogicalVolume*);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4LogicalVolume*     GetLogicalVolume() {return fLogicalVolume;};

private:

  G4int fSACSimulationMode;

  G4double fSACDetectorXLength;
  G4double fSACDetectorYLength;

  G4double fAbsorberLayerZLength;
  G4double fScintillatorLayerZLength;

  G4double fAluminiumLayerXLength;
  G4double fAluminiumLayerYLength;
  G4double fAluminiumLayerZLength;

  G4double fFiberSpacing;
  G4int    fNLayers;

  G4double fLayerSpacing;
  G4double fSACAbsorberScintillatorTotalZLength;
};

#endif
