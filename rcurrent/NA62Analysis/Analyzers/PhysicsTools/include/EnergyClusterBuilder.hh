// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#ifndef ENERGYCLUSTERBUILDER_HH
#define ENERGYCLUSTERBUILDER_HH

#include "Analyzer.hh"
#include "EnergyCluster.hh"
#include "TRecoLKrEvent.hh"

class EnergyClusterBuilder : public NA62Analysis::Analyzer {

public:
  explicit EnergyClusterBuilder(NA62Analysis::Core::BaseAnalysis *ba);
  ~EnergyClusterBuilder();
  void InitOutput();
  void InitHist();
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

protected:
  std::vector<EnergyCluster> fContainer;

private:
  Bool_t IsClusterElectromagnetic(TRecoLKrCandidate* LKrCand);
  Bool_t IsClusterIsolated(TRecoLKrCandidate* LKrCand, int ID);
  Bool_t IsInLKrAcceptance(TRecoLKrCandidate* LKrCand);

  TRecoLKrEvent *fLKrEvent;

  // parameters of isolation
  double fCutMinDist;
  double fCutMaxTimeDiff;

  // parameters of 2D EM cut
  double fCutK1;
  double fCutQ1;
  double fCutK2;
  double fCutQ2;
  double fCutSR1;
  double fCutSR2;
};

#endif
