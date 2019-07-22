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
// $Id: MCTruthTrackInformation.hh,v 1.1.1.1 2008/04/07 18:27:35 sergiant Exp $
// GEANT4 tag $Name:  $
//
//
// --------------------------------------------------------------
//      GEANT 4 - MCTruthTrackInformation class
// --------------------------------------------------------------
//
// Author: Witold POKORSKI (Witold.Pokorski@cern.ch)
// Date  : 2006-03-06
//
// --------------------------------------------------------------

#ifndef MCTruthTrackInformation_h
#define MCTruthTrackInformation_h 1

#include "globals.hh"
#include "G4ThreeVector.hh"
#include "G4ParticleDefinition.hh"
#include "G4Track.hh"
#include "G4Allocator.hh"
#include "G4VUserTrackInformation.hh"
#include "TVector3.h"
#include "TLorentzVector.h"

class MCTruthTrackInformation : public G4VUserTrackInformation  {

public:

  MCTruthTrackInformation();
  explicit MCTruthTrackInformation(const G4Track* aTrack);
  explicit MCTruthTrackInformation(const MCTruthTrackInformation* aTrackInfo);
  virtual ~MCTruthTrackInformation();
   
  inline void *operator new(size_t);
  inline void operator delete(void *aTrackInfo);
  inline int operator ==(const MCTruthTrackInformation& right) const {return (this==&right);}

  void   Print() const;
  G4int  GetDepthLevel()               { return depthLevel;      }
  void   SetDepthLevel(G4int value)    { depthLevel = value;     }
  G4bool GetDirectParent()             { return directParent;    }
  void   SetDirectParent(G4bool value) { directParent = value;   }
  G4int  GetSavedParentID()            { return fSavedParentID;  }
  void   SetSavedParentID(G4int value) { fSavedParentID = value; }

  void           SetPosAtCheckPoint(G4int i, TVector3 v)       { fPos[i] = v;    }
  void           SetMomAtCheckPoint(G4int i, TLorentzVector v) { fMom[i] = v;    }
  TVector3       GetPosAtCheckPoint(G4int i)                   { return fPos[i]; }
  TLorentzVector GetMomAtCheckPoint(G4int i)                   { return fMom[i]; }

private:

  G4int                 originalTrackID;
  G4ParticleDefinition* particleDefinition;
  G4ThreeVector         originalPosition;
  G4ThreeVector         originalMomentum;
  G4double              originalEnergy;
  G4double              originalTime;
  G4int                 depthLevel;
  G4bool                directParent;
  G4int                 fSavedParentID;

  // Positions and momenta at checkpoints
  TVector3       fPos[20];
  TLorentzVector fMom[20];

public:

  inline G4int GetOriginalTrackID() const {return originalTrackID;}
  inline G4ParticleDefinition* GetOriginalParticle() const {return particleDefinition;}
  inline G4ThreeVector GetOriginalPosition() const {return originalPosition;}
  inline G4ThreeVector GetOriginalMomentum() const {return originalMomentum;}
  inline G4double GetOriginalEnergy() const {return originalEnergy;}
  inline G4double GetOriginalTime() const {return originalTime;}
};

extern G4Allocator<MCTruthTrackInformation> aTrackInformationAllocator;

inline void* MCTruthTrackInformation::operator new(size_t) {
  void* aTrackInfo;
  aTrackInfo = static_cast<void*>(aTrackInformationAllocator.MallocSingle());
  return aTrackInfo;
}

inline void MCTruthTrackInformation::operator delete(void *aTrackInfo)
{ aTrackInformationAllocator.FreeSingle(static_cast<MCTruthTrackInformation*>(aTrackInfo)); }

#endif
