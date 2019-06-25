// --------------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) and
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), October 2015
//
// Updated by Viacheslav Duk (Viacheslav.Duk@cern.ch), August 2017
//
// --------------------------------------------------------------------

#ifndef ExoticParticle_h
#define ExoticParticle_h 1

#include "G4ParticleDefinition.hh"
#include "globals.hh"
#include "G4SystemOfUnits.hh"

class ExoticParticle : public G4ParticleDefinition {
public:
  static ExoticParticle* theExoticParticle[200];
  ExoticParticle() {}
  ~ExoticParticle() {}

public:
  static ExoticParticle* Definition(G4int);

private:

};

#endif
