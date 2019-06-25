// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#ifndef CedarHit_h
#define CedarHit_h 1

#include "G4SystemOfUnits.hh"
#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"

class CedarHit : public G4VHit {

public:

  CedarHit();
  ~CedarHit();
  CedarHit(const CedarHit &right);
  CedarHit& operator=(const CedarHit &right);
  G4int operator==(const CedarHit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print();

  inline G4ThreeVector GetPosition()                  { return fPosition;                    }
  inline void          SetPosition(G4ThreeVector val) { fPosition = val;                     }
  inline G4double      GetTime()                      { return fTime;                        }
  inline void          SetTime(G4double val)          { fTime = val;                         }
  inline G4double      GetEnergy()                    { return fEnergy;                      }
  inline void          SetEnergy(G4double val)        { fEnergy = val;                       }
  inline G4double      GetWaveLength()                { return 1.986446e-25*joule*m/fEnergy; }
  inline void          SetWaveLength(G4double val)    { fEnergy = 1.986446e-25*joule*m/val;  }
  inline G4int         GetTrackID()                   { return fTrackID;                     }
  inline void          SetTrackID(G4int val)          { fTrackID = val;                      }
  inline G4int         GetPositionID()                { return fPositionID;                  }
  inline void          SetPositionID(G4int val)       { fPositionID = val;                   }
  inline G4int         GetPMType()                    { return iPMType;                      }
  inline void          SetPMType(G4int Type)          { iPMType = Type;                      }

private:

  G4ThreeVector fPosition;
  G4double      fTime;
  G4double      fEnergy;
  G4int         fTrackID;
  G4int         fPositionID;
  G4int         iPMType;
};

typedef G4THitsCollection<CedarHit> CedarHitsCollection;

extern G4Allocator<CedarHit> CedarHitAllocator;

inline void* CedarHit::operator new(size_t) {
  void *aHit;
  aHit = static_cast<void*>(CedarHitAllocator.MallocSingle());
  return aHit;
}

inline void CedarHit::operator delete(void *aHit) {
  CedarHitAllocator.FreeSingle(static_cast<CedarHit*>(aHit));
}

#endif
