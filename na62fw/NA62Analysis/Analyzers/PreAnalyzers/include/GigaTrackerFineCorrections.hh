#ifndef GIGATRACKERFINECORRECTIONS_HH
#define GIGATRACKERFINECORRECTIONS_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include "Analyzer.hh"

class GigaTrackerFineCorrections : public NA62Analysis::Analyzer {
public:
  explicit GigaTrackerFineCorrections(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerFineCorrections(){}
  void InitHist(){}
  void InitOutput();
  void DefineMCSimple(){}
  void ProcessSpecialTriggerUser(int , unsigned int ){}
  void Process(int iEvent);
  void PostProcess(){}
  void StartOfBurstUser(){}
  void EndOfBurstUser(){}
  void StartOfRunUser();
  void EndOfRunUser(){}
  void EndOfJobUser(){}
  void DrawPlot(){} 
protected:
  Double_t fA, fB, fC, fD; ///< Units: 10^{-6}
  Double_t fGTKMomentumScale;
};

#endif
