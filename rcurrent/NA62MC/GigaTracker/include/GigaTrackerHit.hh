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
// 2008-04-22 S.Bifani (Simone.Bifani@cern.ch)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// --------------------------------------------------------------
//
#ifndef GigaTrackerHit_h
#define GigaTrackerHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"

class GigaTrackerHit : public G4VHit
{
public:

  GigaTrackerHit();
  explicit GigaTrackerHit(G4LogicalVolume* logVol);
  ~GigaTrackerHit();
  GigaTrackerHit(const GigaTrackerHit &right);
  GigaTrackerHit& operator=(const GigaTrackerHit &right);
  G4int operator==(const GigaTrackerHit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print();

public:

  inline G4int             GetStationNo()                                 { return fStationNo;                    };
  inline void              SetStationNo(G4int value)                      { fStationNo = value;                   };
  inline G4int             GetPixelID()                                   { return fPixelID;                      };
  inline void              SetPixelID(G4int value)                        { fPixelID = value;                     };
 
  inline G4double          GetTime()                                      { return fTime;                         };
  inline void              SetTime(G4double value)                        { fTime = value;                        };
  inline G4double          GetEnergy()                                    { return fEnergy;                       };
  inline void              SetEnergy(G4double value)                      { fEnergy = value;                      };
 
  inline G4int             GetTrackID()                                   { return fTrackID;                      };
  inline void              SetTrackID(G4int value)                        { fTrackID = value;                     };

  inline G4ThreeVector     GetPosition()                                  { return fPosition;                     };
  inline void              SetPosition(G4ThreeVector value)               { fPosition = value;                    };

private:

  G4int fStationNo;
  G4int fPixelID;

  G4double fTime;
  G4double fEnergy;

  G4int fTrackID;

  G4ThreeVector fPosition;

};

typedef G4THitsCollection<GigaTrackerHit> GigaTrackerHitsCollection;

extern G4Allocator<GigaTrackerHit> GigaTrackerHitAllocator;

inline void* GigaTrackerHit::operator new(size_t)
{

  void *aHit;
  aHit = static_cast<void *>(GigaTrackerHitAllocator.MallocSingle());
  return aHit;

}

inline void GigaTrackerHit::operator delete(void *aHit)
{

  GigaTrackerHitAllocator.FreeSingle(static_cast<GigaTrackerHit*>(aHit));

}

#endif
