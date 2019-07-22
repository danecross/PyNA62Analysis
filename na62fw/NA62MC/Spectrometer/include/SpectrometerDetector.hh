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
#ifndef SpectrometerDetector_H
#define SpectrometerDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "SpectrometerGeometryParameters.hh"
#include "globals.hh"

#include "G4FieldManager.hh"
#include "MagneticField.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class SpectrometerDetector : public NA62VComponent, public NA62VNamed {

public:

  SpectrometerDetector(G4Material*, G4LogicalVolume*);
  ~SpectrometerDetector();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties() {}

private:

  G4double fChamberZLength;
  G4double fChamberXDisplacement[4];
  G4double fChamberZCenter[4];
  G4double fMagnetZPosition;

  G4double fRR_ZofFrontFace[4];
  G4double fRR_ZofBackFace[4];
  G4double fRR_ZofCenter[4];
  G4double fRR_Length[4];
  G4double fRR_Radius[4];

  G4FieldManager* fFieldManager;
};

#endif
