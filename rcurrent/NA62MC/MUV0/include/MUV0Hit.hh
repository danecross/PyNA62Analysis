#ifndef MUV0Hit_h
#define MUV0Hit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"

class MUV0Hit : public G4VHit {

public:

  MUV0Hit();
  explicit MUV0Hit(G4LogicalVolume* logVol);
  ~MUV0Hit() {}
  MUV0Hit(const MUV0Hit &right);
  MUV0Hit& operator=(const MUV0Hit &right);
  G4int operator==(const MUV0Hit &right) const;

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

private:

  G4int         fChannelID;
  G4int         fTrackID;
  G4double      fTime;
  G4double      fEnergy;
  G4ThreeVector fPosition;
};

typedef G4THitsCollection<MUV0Hit> MUV0HitsCollection;

extern G4Allocator<MUV0Hit> MUV0HitAllocator;

inline void* MUV0Hit::operator new(size_t) {
  void *aHit = static_cast<void *>(MUV0HitAllocator.MallocSingle());
  return aHit;
}

inline void MUV0Hit::operator delete(void *aHit) {
  MUV0HitAllocator.FreeSingle(static_cast<MUV0Hit*>(aHit));
}

#endif
