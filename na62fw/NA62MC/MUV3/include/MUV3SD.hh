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

#ifndef MUV3SD_H
#define MUV3SD_H 1

#include "G4SystemOfUnits.hh"
#include "G4VSensitiveDetector.hh"
#include "MUV3Hit.hh"

class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class MUV3SD : public G4VSensitiveDetector {

public:
  MUV3SD(G4String name, G4String colName);
  ~MUV3SD() {}
  
  void Initialize   (G4HCofThisEvent*);
  G4bool ProcessHits(G4Step*, G4TouchableHistory*);
  void EndOfEvent   (G4HCofThisEvent*) {}

private:

  MUV3HitsCollection *Collection;
  MUV3Hit *HitMapScintillator[360], *HitMapPMTWindow[360];
  G4int nHits;
  G4int HCID;
};

#endif
