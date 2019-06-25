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

// --------------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------

#ifndef MUV3Hit_h
#define MUV3Hit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"

class MUV3Hit : public G4VHit {

public:

  MUV3Hit();
  explicit MUV3Hit(G4LogicalVolume* logVol);
  ~MUV3Hit() {}
  MUV3Hit(const MUV3Hit &right);
  MUV3Hit& operator=(const MUV3Hit &right);
  G4int operator==(const MUV3Hit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print() {}

public:

  inline G4int         GetChannelID()                 { return fChannelID; }
  inline void          SetChannelID(G4int val)        { fChannelID = val;  }
  inline G4int         GetTrackID()                   { return fTrackID;   }
  inline void          SetTrackID(G4int val)          { fTrackID = val;    }
  inline G4double      GetTime()                      { return fTime;      }
  inline void          SetTime(G4double val)          { fTime = val;       }
  inline G4double      GetEnergy()                    { return fEnergy;    }
  inline void          SetEnergy(G4double val)        { fEnergy = val;     }
  inline void          AddEnergy(G4double val)        { fEnergy += val;    }
  inline G4ThreeVector GetPosition()                  { return fPosition;  }
  inline void          SetPosition(G4ThreeVector val) { fPosition = val;   }
  inline G4bool        GetMuonHit()                   { return fMuonHit;   }
  inline void          SetMuonHit(G4bool val)         { fMuonHit = val;    }

private:

  G4int         fChannelID;
  G4int         fTrackID;
  G4double      fTime;
  G4double      fEnergy;
  G4ThreeVector fPosition;
  G4bool        fMuonHit;
};

typedef G4THitsCollection<MUV3Hit> MUV3HitsCollection;

extern G4Allocator<MUV3Hit> MUV3HitAllocator;

inline void* MUV3Hit::operator new(size_t) {
  void *aHit = static_cast<void *>(MUV3HitAllocator.MallocSingle());
  return aHit;
}

inline void MUV3Hit::operator delete(void *aHit) {
  MUV3HitAllocator.FreeSingle(static_cast<MUV3Hit*>(aHit));
}

#endif
