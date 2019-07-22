// --------------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) and
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), October 2015
//
// --------------------------------------------------------------------

#ifndef ExoticParticlePhysics_h
#define ExoticParticlePhysics_h 1

#include "G4VPhysicsConstructor.hh"
#include "G4Decay.hh"
#include "G4CMCDecayer.hh"
#include "globals.hh"

class ExoticParticlePhysics : public G4VPhysicsConstructor {
public:
  explicit ExoticParticlePhysics(const G4String& name = "beam");
  virtual ~ExoticParticlePhysics();
  virtual void ConstructParticle();
  virtual void ConstructProcess();
  
private:
  ExoticParticlePhysics & operator=(const ExoticParticlePhysics &right);
  ExoticParticlePhysics(const ExoticParticlePhysics&);
};

#endif
