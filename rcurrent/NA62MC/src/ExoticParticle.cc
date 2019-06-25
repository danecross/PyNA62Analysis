// --------------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) and
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), October 2015
//
// Updated by Viacheslav Duk (Viacheslav.Duk@cern.ch), August 2017
//
// --------------------------------------------------------------------

#include "ExoticParticle.hh"
#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh"
#include "G4ParticleTable.hh"
#include "G4PhaseSpaceDecayChannel.hh"
#include "G4DecayTable.hh"
#include "DatacardManager.hh"

/// \class ExoticParticle
/// \Brief
/// Definition of an exotic particle: can be heavy neutral lepton, dark photon, axion, etc
/// \EndBrief

ExoticParticle* ExoticParticle::theExoticParticle[200] = {0};

ExoticParticle* ExoticParticle::Definition(G4int iParticle) {

  if (theExoticParticle[iParticle]) return theExoticParticle[iParticle];

  G4ParticleTable* pTable = G4ParticleTable::GetParticleTable();
  G4ParticleDefinition* anInstance = pTable->FindParticle(Form("Exotic%d",iParticle));

  if (!anInstance) {

    anInstance = new G4ParticleDefinition
      (Form("Exotic%d",iParticle),       // Name
       0.0*GeV,                          // Mass; may be overwritten by macro via /Simulation/ExoticParticle/Mass
       1.0*eV,                           // Width
       0.0, 0, 0,                        // Charge, spin, parity
       0, 0, 0, 0,                       // Conjugation(?), isospin, isospinZ, gParity
       "other", 0, 0,                    // pType(?), lepton, baryon, 
       9000+iParticle,                   // PDG code (9000 is the NA62 custom code for the first exotic particle)
       kFALSE,                           // kFALSE=not stable; may be overwritten by macro via /Simulation/ExoticParticle/DecayMode
       1.0*nanosecond,                   // Lifetime; may be overwritten by macro via /Simulation/ExoticParticle/Lifetime
       NULL);                            // Decay table
  }
  theExoticParticle[iParticle] = reinterpret_cast<ExoticParticle*>(anInstance);
  return theExoticParticle[iParticle];
}
