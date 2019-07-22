//------------------------------------------------------------
//History:
//
//Created by Zuzana Kucerova (zukucero@cern.ch) 2016-04-27
//
//-----------------------------------------------------------

#ifndef SPECTROMETERMCTESTER_HH
#define SPECTROMETERMCTESTER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1F;
class TH2F;
class TH3D;
class TGraph;
class TTree;

class SpectrometerMCTester : public NA62Analysis::Analyzer {
public:
  explicit SpectrometerMCTester(NA62Analysis::Core::BaseAnalysis *ba);
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void Process(Int_t);
  void StartOfBurstUser() {}
  void EndOfBurstUser() {}
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:
  void Publish() {}
  double fHVminZ[32];
  double fHVmaxZ[32];

protected:

  //Standard
  TH2F* fChActivity;
  TH2F* fChActivityCoord;
  TH1F* fHitProf;
  TH2F* fPlActivity;  
  TH1F* fNHits;
  TH1F* fPlID;
  TH2F* fChamberActivity;

  //Geantino
};

#endif

  
