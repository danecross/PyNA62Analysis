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
//
// $Id: MCTruthConfig.hh,v 1.1.1.1 2008/04/07 18:27:35 sergiant Exp $
// GEANT4 tag $Name:  $
//
//
// --------------------------------------------------------------
//      GEANT 4 - MCTruthConfig class
// --------------------------------------------------------------
//
// Author: Witold POKORSKI (Witold.Pokorski@cern.ch)
// Date  : 2006-03-06
//
// --------------------------------------------------------------
#ifndef MCTruthConfig_H 
#define MCTruthConfig_H 1

#include<vector>
#include<iostream>

#include "globals.hh"

class MCTruthConfig  {

public:

  MCTruthConfig();
  virtual ~MCTruthConfig() {}

  void   SetSaveTracksForRegeneration(G4bool val) { fSaveTracksForRegeneration = val;  }
  G4bool GetSaveTracksForRegeneration() const     { return fSaveTracksForRegeneration; }

  void   SetSaveAllTracks(G4bool val) { fSaveAllTracks = val;  }
  G4bool GetSaveAllTracks() const     { return fSaveAllTracks; }
  void   SetSaveAllSteps(G4bool val)  { fSaveAllSteps = val;   }
  G4bool GetSaveAllSteps() const      { return fSaveAllSteps;  }

  void SetBeamParticleEndProcessToReject(std::vector<G4String>& types) {fBeamParticleEndProcessToReject = types;}
  void AddBeamParticleEndProcessToReject(G4String type) {fBeamParticleEndProcessToReject.push_back(type);}
  std::vector<G4String>& GetBeamParticleEndProcessToReject() {return fBeamParticleEndProcessToReject;}

  void     SetZMin(double val) { fZMin = val;  }
  G4double GetZMin() const     { return fZMin; }
  void     SetZMax(double val) { fZMax = val;  }
  G4double GetZMax() const     { return fZMax; }
  void     SetEMin(double val) { fEMin = val;  }
  G4double GetEMin() const     { return fEMin; }

  void   SetMaxInteractionLevel(G4int val) { fMaxInteractionLevel = val;  }
  G4int  GetMaxInteractionLevel() const    { return fMaxInteractionLevel; }
  void   SetSaveTrackVerbose(G4bool val)   { fSaveTrackVerbose = val;     }
  G4bool GetSaveTrackVerbose() const       { return fSaveTrackVerbose;    }

  void SetParticleTypeToSave(std::vector<G4String>& types) {fParticleTypeToSave = types;}
  void AddParticleTypeToSave(G4String type) {fParticleTypeToSave.push_back(type);}
  std::vector<G4String>& GetParticleTypeToSave() {return fParticleTypeToSave;}

  void SetParticleTypeToReject(std::vector<G4String>& types) {fParticleTypeToReject = types;}
  void AddParticleTypeToReject(G4String type) {fParticleTypeToReject.push_back(type);}
  std::vector<G4String>& GetParticleTypeToReject() {return fParticleTypeToReject;}

  void SetParticleCreatorProcessToSave(std::vector<G4String>& types) {fParticleCreatorProcessToSave = types;}
  void AddParticleCreatorProcessToSave(G4String type) {fParticleCreatorProcessToSave.push_back(type);}
  std::vector<G4String>& GetParticleCreatorProcessToSave() {return fParticleCreatorProcessToSave;}

  void SetParticleCreatorProcessToReject(std::vector<G4String>& types) {fParticleCreatorProcessToReject = types;}
  void AddParticleCreatorProcessToReject(G4String type) {fParticleCreatorProcessToReject.push_back(type);}
  std::vector<G4String>& GetParticleCreatorProcessToReject() {return fParticleCreatorProcessToReject;}

  void SetParticleEndProcessToSave(std::vector<G4String>& types) {fParticleEndProcessToSave = types;}
  void AddParticleEndProcessToSave(G4String type) {fParticleEndProcessToSave.push_back(type);}
  std::vector<G4String>& GetParticleEndProcessToSave() {return fParticleEndProcessToSave;}

  void SetParticleEndProcessToReject(std::vector<G4String>& types) {fParticleEndProcessToReject = types;}
  void AddParticleEndProcessToReject(G4String type) {fParticleEndProcessToReject.push_back(type);}
  std::vector<G4String>& GetParticleEndProcessToReject() {return fParticleEndProcessToReject;}

private:

  G4bool   fSaveTracksForRegeneration;
  G4bool   fSaveAllTracks;
  G4bool   fSaveAllSteps;
  std::vector<G4String> fBeamParticleEndProcessToReject;
  G4double fZMin; ///< Minimum Z of beam particle endpoint to save KineParts
  G4double fZMax; ///< Maximum Z of beam particle endpoint to save KineParts
  G4double fEMin;
  G4int    fMaxInteractionLevel;
  G4bool   fSaveTrackVerbose;
  std::vector<G4String> fParticleTypeToSave;
  std::vector<G4String> fParticleTypeToReject;
  std::vector<G4String> fParticleCreatorProcessToSave;
  std::vector<G4String> fParticleCreatorProcessToReject;
  std::vector<G4String> fParticleEndProcessToSave;
  std::vector<G4String> fParticleEndProcessToReject;
};

#endif // MCTruthConfig_H
