// LAVHit.hh
// --------------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch)
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of LAV hit structure
// 2010-03-15 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Optical Photons on photocatode
// 2011-01-24 - Domenico Di Filippo (difilippo@na.infn.it)
//   - Switch to std::vector to store optical photons data
//
// --------------------------------------------------------------------
#ifndef LAVHit_h
#define LAVHit_h 1

#include "G4VHit.hh"
#include "G4THitsCollection.hh"
#include "G4Allocator.hh"
#include "G4ThreeVector.hh"
#include "G4LogicalVolume.hh"
#include "G4Transform3D.hh"
#include "G4RotationMatrix.hh"
#include "LAVChannelID.hh"

#include <vector>

class LAVHit : public G4VHit
{
public:

  LAVHit();
  explicit LAVHit(G4LogicalVolume* logVol);
  ~LAVHit();
  LAVHit(const LAVHit &right);
  LAVHit& operator=(const LAVHit &right);
  G4int operator==(const LAVHit &right) const;

  inline void *operator new(size_t);
  inline void operator delete(void *aHit);

  void Draw();
  void Print();

public:

  inline G4int GetChannelID() { return fChannelID; };
  inline void SetChannelID(G4int value) { fChannelID = value; };

  inline G4int GetTrackID() { return fTrackID; };
  inline void SetTrackID(G4int value) { fTrackID = value; };

  inline G4double GetTime() { return fTime; };
  inline void SetTime(G4double value) { fTime = value; };

  inline G4double GetEnergy() { return fEnergy; };
  inline void SetEnergy(G4double value) { fEnergy = value; };

  inline G4ThreeVector GetPosition() { return fPosition; };
  inline void SetPosition(G4ThreeVector value) { fPosition = value; };

  inline G4ThreeVector GetLocalPosition() { return fLocalPosition; };
  inline void SetLocalPosition(G4ThreeVector value) { fLocalPosition = value; };

  inline G4ThreeVector GetLocalDirection() { return fLocalDirection; };
  inline void SetLocalDirection(G4ThreeVector value) { fLocalDirection = value; };

  inline G4double GetBeta() { return fBeta; };
  inline void SetBeta(G4double value) { fBeta = value; };

  inline G4double GetStepLength() { return fStepLength; };
  inline void SetStepLength(G4double value) { fStepLength = value; };

  std::vector<G4double>* GetPhotonsEnergy() { return &fPhotonsEnergy; };
  std::vector<G4double>* GetPhotonsTime() { return &fPhotonsTime; };

private:

  G4int fChannelID; // Channel id
  G4int fTrackID;   // Id of track generating hit

  G4double fTime;   // Global time of hit
  G4double fEnergy; // Energy of hit

  G4ThreeVector fPosition; // Global position of hit (used for visualization)

  // Quantities used to compute Cerenkov yeld during digitization
  G4ThreeVector fLocalPosition; // Local (within PBgL block) position of hit
  G4ThreeVector fLocalDirection; // Local direction of particle generating hit
  G4double fBeta;   // Beta of particle generating hit
  G4double fStepLength; // Length of step

  // Vectors of photons arriving on photocathode
  std::vector<G4double> fPhotonsEnergy;
  std::vector<G4double> fPhotonsTime;

};

typedef G4THitsCollection<LAVHit> LAVHitsCollection;

extern G4Allocator<LAVHit> LAVHitAllocator;

inline void* LAVHit::operator new(size_t)
{
  void *aHit;
  aHit = (void *) LAVHitAllocator.MallocSingle();
  return aHit;
}

inline void LAVHit::operator delete(void *aHit)
{
  LAVHitAllocator.FreeSingle(static_cast<LAVHit*>(aHit));
}

#endif


