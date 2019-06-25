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
#ifndef NewCHODDetector_H
#define NewCHODDetector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "NewCHODGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class NewCHODDetector : public NA62VComponent, public NA62VNamed {

public:

  NewCHODDetector(G4Material*, G4LogicalVolume*);
  ~NewCHODDetector() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4double fRespRegionZStart;
  G4double fRespRegionZEnd;
  G4double fRespRegionZCentre;
  G4double fRespRegionXLength;
  G4double fRespRegionYLength;
  G4double fRespRegionZLength;

  G4double fFiberglassThickness;
  G4double fScintThickness;
  G4double fAlWindowThickness;
  G4double fHoneycombThickness;
  G4double fHoneycombSkinThickness;
  G4double fHoneycombAlThickness;

  G4double fInnerRadius;
  G4double fOuterRadius;
  G4double fHoneycombInnerRadius;
  G4double fHoneycombOuterRadius;
  G4double fFiberglassOuterRadius; ///< apothem of a regular octagon

  G4double fZPosition;
  G4double fScintZPosition1;
  G4double fScintZPosition2;
  G4double fAlWindowZPosition1;
  G4double fAlWindowZPosition2;
  G4double fHoneycombZPosition;

  G4int         fNCounters;
  G4ThreeVector fScintSize;
  G4ThreeVector fScintPosition[100];
};

#endif
