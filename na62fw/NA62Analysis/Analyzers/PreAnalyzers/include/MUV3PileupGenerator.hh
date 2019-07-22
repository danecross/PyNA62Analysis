#ifndef MUV3PILEUPGENERATOR_HH
#define MUV3PILEUPGENERATOR_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include <stdlib.h>
#include "Analyzer.hh"
#include "MUV3Geometry.hh"
#include <TRandom2.h>

class TH1I;
class TH2F;
class TTree;

class MUV3PileupGenerator : public NA62Analysis::Analyzer {

public:
  explicit MUV3PileupGenerator(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV3PileupGenerator();
  void InitHist();
  void InitOutput();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser() {}
  void StartOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  TString fTriggerName;
  MUV3Geometry *fGeo;
  TRandom2 *fRandom;
  UInt_t   fTriggerID;
  Double_t fTriggerTime;
  Double_t fCutHitTriggerTimeDiff;
  Double_t fEventTimeWindowLowEdge;
  Double_t fEventTimeWindowHighEdge;
  Double_t fScaleFactor; ///< A scale factor to be applied to the number of generated pileup MUV3 candidates
  UInt_t   fNAccidentalCandidates;
  TFile*   fHitMapFile;
  TH1F*    fhHitMap;
  TH1F*    fHistoProfile[200]; ///< Distribution of numbers of accidentals in bins of intensity
  TH2F*    fhnAccidentalCandsVsBeamIntensity;
  Double_t fBeamIntensity;

  void generateHitMapFromData();
  void generateAccidentalsInMC();
  UInt_t getNaccidentals(Double_t);
};
#endif
