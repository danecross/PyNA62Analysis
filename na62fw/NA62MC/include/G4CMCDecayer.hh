#ifndef G4_CMC_DECAYER_H
#define G4_CMC_DECAYER_H

#include "G4VExtDecayer.hh"
#include "CMC.hh"
#include "globals.hh"
#include "G4SystemOfUnits.hh"

class CMCParticle;
class G4Track;
class G4DecayProducts;

class G4CMCDecayer : public G4VExtDecayer {
public:
  G4CMCDecayer();
  virtual ~G4CMCDecayer();
  virtual G4DecayProducts* ImportDecayProducts(const G4Track& track);
  // void ForceDecayType(G4int);
  G4int Generate(G4double, G4double, G4double, G4double);
  ParticleVector *GetParticles() { return fDecayProductsArray; }

  Int_t GetNTrials()          { return fNTrials; }
  void  SetNTrials(G4int val) { fNTrials = val;  }

private:
  G4ParticleDefinition* GetParticleDefinition(const CMCParticle* p) const;
  G4DynamicParticle* CreateDynamicParticle(const CMCParticle* p) const;
  G4ThreeVector GetParticleMomentum(const CMCParticle* particle) const; 
  void Decay(G4int, const CLHEP::HepLorentzVector& p);
  ParticleVector* fDecayProductsArray;
  G4int fDecayType;
  G4int fNTrials; ///< Number of trials for biased MC (needed to compute event weight)
};

#endif
