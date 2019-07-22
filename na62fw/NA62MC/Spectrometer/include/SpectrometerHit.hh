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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#ifndef SpectrometerHit_h
#define SpectrometerHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"

class SpectrometerHit : public G4VHit
{
public:

  SpectrometerHit();
  explicit SpectrometerHit(G4LogicalVolume* logVol);
  ~SpectrometerHit();
  SpectrometerHit(const SpectrometerHit &right);
  SpectrometerHit& operator=(const SpectrometerHit &right);
  G4int operator==(const SpectrometerHit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print();

public:

  inline G4int         GetStrawID()                                       { return fStrawID;                      };
  inline void          SetStrawID(G4int value)                            { fStrawID = value;                     };
  inline G4int         GetHalfViewID()                                    { return fHalfViewID;                   };
  inline void          SetHalfViewID(G4int value)                         { fHalfViewID = value;                  };
  inline G4int         GetViewID()                                        { return fViewID;                       };
  inline void          SetViewID(G4int value)                             { fViewID = value;                      };
  inline G4int         GetChamberID()                                     { return fChamberID;                    };
  inline void          SetChamberID(G4int value)                          { fChamberID = value;                   };
  inline G4int         GetTrackID()                                       { return fTrackID;                      };
  inline void          SetTrackID(G4int value)                            { fTrackID = value;                     };

  inline G4double      GetWireDistance()                                  { return fWireDistance;                 };
  inline void          SetWireDistance(G4double value)                    { fWireDistance = value;                };
  inline G4double      GetTime()                                          { return fTime;                         };
  inline void          SetTime(G4double value)                            { fTime = value;                        };
  inline G4double      GetEnergy()                                        { return fEnergy;                       };
  inline void          SetEnergy(G4double value)                          { fEnergy = value;                      };

  inline G4ThreeVector GetPosition()                                      { return fPosition;                     };
  inline void          SetPosition(G4ThreeVector value)                   { fPosition = value;                    };
  inline G4ThreeVector GetLocalPosition()                                 { return fLocalPosition;                };
  inline void          SetLocalPosition(G4ThreeVector value)              { fLocalPosition = value;               };

  inline G4ThreeVector GetDirection()                                     { return fDirection;                    };
  inline void          SetDirection(G4ThreeVector value)                  { fDirection = value;                   };

private:

  G4int fStrawID;
  G4int fHalfViewID;
  G4int fViewID;
  G4int fChamberID;
  G4int fTrackID;

  G4double fWireDistance;
  G4double fTime;
  G4double fEnergy;

  G4ThreeVector fPosition;
  G4ThreeVector fLocalPosition;
  G4ThreeVector fDirection;
};

typedef G4THitsCollection<SpectrometerHit> SpectrometerHitsCollection;

extern G4Allocator<SpectrometerHit> SpectrometerHitAllocator;

inline void* SpectrometerHit::operator new(size_t)
{
  void *aHit;
  aHit = static_cast<void *>(SpectrometerHitAllocator.MallocSingle());
  return aHit;
}

inline void SpectrometerHit::operator delete(void *aHit)
{
  SpectrometerHitAllocator.FreeSingle(static_cast<SpectrometerHit*>(aHit));
}

#endif


