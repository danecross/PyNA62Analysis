// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-11-18
//
// ---------------------------------------------------------------

/// \class MCInfo
/// \Brief
/// Container of settings specified in NA62MC macro file
/// \EndBrief

#include "MCInfo.hh"

#include <iostream>
#include <stdlib.h>
#include "TemplateFunctions.hh"

ClassImp(MCInfo)

MCInfo::MCInfo() {
  Clear();
}

void MCInfo::Clear(Option_t*) {
  fRevision = "";
  fBeamType = -1;
  fFastSimulationMode = 0;
  fBrPie2 = 1.23e-4;
  fForcedDecay = false;
  fForcedMuonDecay = false;
  fForcedPionDecay = false;
  fDecayType = fRadCor = fPiZeroDecay = -1;
  fZDecayMin = fZDecayMax = 0.0;
  fExoticParticleMass = 0.0;
  fExoticParticleDecayMode = -1;
  fExoticParticleLifetime = 0.0;

  fFileName.clear();
  fRunNumber.clear();
  fRandomSeed.clear();
  fNEvents.clear();
}

void MCInfo::Print(Option_t*) const {}

void MCInfo::UpdateUniqueAttributes(MCInfo& s) {
  fRevision = s.fRevision;
  fBeamType = s.fBeamType;
  fFastSimulationMode = s.fFastSimulationMode;
  fBrPie2 = s.fBrPie2;
  fForcedDecay = s.fForcedDecay;
  fForcedMuonDecay = s.fForcedMuonDecay;
  fForcedPionDecay = s.fForcedPionDecay;
  fDecayType = s.fDecayType;
  fRadCor = s.fRadCor;
  fPiZeroDecay = s.fPiZeroDecay;
  fZDecayMin = s.fZDecayMin;
  fZDecayMax = s.fZDecayMax;
  fExoticParticleMass = s.fExoticParticleMass;
  fExoticParticleDecayMode = s.fExoticParticleDecayMode;
  fExoticParticleLifetime = s.fExoticParticleLifetime;
}

void MCInfo::MergeJobAttributes(MCInfo& s) {
  if(!all_equal(fFileName.size(), fRunNumber.size(), fRandomSeed.size(), fNEvents.size())){
    std::cout << "[MCInfo] Warning: merging attributes with inconsistent numbers of entries" << std::endl;
  }
  fFileName.insert(fFileName.end(), s.fFileName.begin(), s.fFileName.end());
  fRunNumber.insert(fRunNumber.end(), s.fRunNumber.begin(), s.fRunNumber.end());
  fRandomSeed.insert(fRandomSeed.end(), s.fRandomSeed.begin(), s.fRandomSeed.end());
  fNEvents.insert(fNEvents.end(), s.fNEvents.begin(), s.fNEvents.end());
}

void MCInfo::UpdateAndMergeAttributes(MCInfo& s) {
  UpdateUniqueAttributes(s);
  MergeJobAttributes(s);
}
