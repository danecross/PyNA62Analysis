#ifndef CHPARTICLECANDIDATEBUILDER_HH
#define CHPARTICLECANDIDATEBUILDER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "GeometricAcceptance.hh"
#include "TwoLinesCDA.hh"
#include <TLegend.h>
#include <TCanvas.h>
#include "ChParticleCandidate.hh"
#include "NeParticleCandidate.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "EventCandidate.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class ParticleCandidateBuilder : public NA62Analysis::Analyzer {

  public:
    explicit ParticleCandidateBuilder(NA62Analysis::Core::BaseAnalysis *ba);
    ~ParticleCandidateBuilder();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void Process(int iEvent);
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void PostProcess();
    void DrawPlot();

protected:
  std::string parametro;
  std::string filetemp;

  Double_t LKrElectronProbability;
  Double_t LKrMuonProbability;
  Double_t LKrPionProbability;
  Double_t RiCHElectronProbability;
  Double_t RiCHMuonProbability;
  Double_t RiCHPionProbability;

  Double_t ElectronProbabilityProd;
  Double_t MuonProbabilityProd;
  Double_t PionProbabilityProd;

  TwoLinesCDA *fCDAcomp;

  LAVMatching *fLAVMatching;
  SAVMatching *fSAVMatching;

  std::vector<NeParticleCandidate*> fNeParticle;
  std::vector<ChParticleCandidate*> fChParticle;
  std::vector<int> fPionsIndex;
  std::vector<int> fElectronsIndex;
  std::vector<int> fMuonsIndex;
  std::vector<EventCandidate*> fParticlesEvents;

private:
};
 #endif
