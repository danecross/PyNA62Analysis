#ifndef HACHit_h
#define HACHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"

class HACHit : public G4VHit
{
public:

  HACHit();
  explicit HACHit(G4LogicalVolume* logVol);
  ~HACHit();
  HACHit(const HACHit &right);
  HACHit& operator=(const HACHit &right);
  G4int operator==(const HACHit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print();

public:

  inline G4int         GetChannelID()                                { return fChannelID;               };
  inline void          SetChannelID(G4int value)                     { fChannelID = value;              };
  inline G4int         GetTrackID()                                       { return fTrackID;                      };
  inline void          SetTrackID(G4int value)                            { fTrackID = value;                     };

  inline G4double      GetTime()                                          { return fTime;                         };
  inline void          SetTime(G4double value)                            { fTime = value;                        };
  inline G4double      GetEnergy()                                        { return fEnergy;                       };
  inline void          SetEnergy(G4double value)                          { fEnergy = value;                      };

  inline void          AddEnergy(G4double value)                          { fEnergy += value;                     };

  inline G4ThreeVector GetPosition()                                      { return fPosition;                     };
  inline void          SetPosition(G4ThreeVector value)                   { fPosition = value;                    };

private:

  G4int fChannelID;
  G4int fTrackID;

  G4double fTime;
  G4double fEnergy;

  G4ThreeVector fPosition;
};

typedef G4THitsCollection<HACHit> HACHitsCollection;

extern G4Allocator<HACHit> HACHitAllocator;

inline void* HACHit::operator new(size_t)
{
  void *aHit;
  aHit = static_cast<void *>(HACHitAllocator.MallocSingle());
  return aHit;
}

inline void HACHit::operator delete(void *aHit)
{
  HACHitAllocator.FreeSingle(static_cast<HACHit*>(aHit));
}

#endif


