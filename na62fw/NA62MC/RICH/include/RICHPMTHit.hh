
#ifndef RICHPMTHit_h
#define RICHPMTHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"

class RICHPMTHit : public G4VHit
{
public:

  RICHPMTHit();
  ~RICHPMTHit();
  RICHPMTHit(const RICHPMTHit &right);
  RICHPMTHit& operator=(const RICHPMTHit &right);
  G4int operator==(const RICHPMTHit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print();

private:

  G4ThreeVector fPosition;
  G4double fTime;
  G4double fEnergy;
  G4int fTrackID;
  G4int fPositionID;
public:

  inline G4ThreeVector        GetPosition()                                { return fPosition;                     };
  inline void                 SetPosition(G4ThreeVector value)             { fPosition = value;                    };
  inline G4double             GetTime()                                    { return fTime;                         };
  inline void                 SetTime(G4double value)                      { fTime = value;                        };
  inline G4double             GetEnergy()                                  { return fEnergy;                       };
  inline void                 SetEnergy(G4double value)                    { fEnergy = value;                      };
  inline G4int                GetTrackID()                                 { return fTrackID;                      };
  inline void                 SetTrackID(G4int value)                      { fTrackID = value;                     };
  inline G4int                GetPositionID()                              { return fPositionID;                   };
  inline void                 SetPositionID(G4int value)                   { fPositionID = value;                  };
};

typedef G4THitsCollection<RICHPMTHit> RICHPMTHitsCollection;

extern G4Allocator<RICHPMTHit> RICHPMTHitAllocator;

inline void* RICHPMTHit::operator new(size_t)
{
  void *aHit;
  aHit = static_cast<void *>(RICHPMTHitAllocator.MallocSingle());
  return aHit;
}

inline void RICHPMTHit::operator delete(void *aHit)
{
  RICHPMTHitAllocator.FreeSingle(static_cast<RICHPMTHit*>(aHit));
}

#endif


