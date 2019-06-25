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
#ifndef SACSD_h
#define SACSD_h 1

#include "G4VSensitiveDetector.hh"
#include "SACHit.hh"
class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class SACSD : public G4VSensitiveDetector
{

  public:
  SACSD(G4String name, G4String colName);
  ~SACSD();

  void ReadGeometryParameters();
  void Initialize(G4HCofThisEvent*HCE);
  G4bool ProcessHits(G4Step*aStep,G4TouchableHistory*);
  void EndOfEvent(G4HCofThisEvent*HCE);
  void clear();
  void DrawAll();
  void PrintAll();
  
  private:
  G4int fHitArraySize;
  SACHit **hitArray;

  G4int fSACSimulationMode;

  G4double fSACDetectorFrontZPosition;
  G4double fSACDetectorXLength;
  G4double fSACDetectorYLength;
  G4double fSACDetectorZLength;
  G4double fFiberSpacing;
  G4int fNFibers;

  G4int fSDnSegmentsX;
  G4int fSDnSegmentsY;
  G4int fSDnSegmentsZ;

  SACHitsCollection *Collection;
  
  G4int nHits;
  int HCID;
};




#endif

