#ifndef LAVBURSTCHECKER_HH
#define LAVBURSTCHECKER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class LAVBurstChecker : public NA62Analysis::Analyzer
{

public:

  explicit LAVBurstChecker(NA62Analysis::Core::BaseAnalysis *ba);
  ~LAVBurstChecker();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSOBEvent();
  void ProcessEOBEvent();
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void PostProcess();
  void DrawPlot();

private:

  Bool_t   fReadingData;

protected:

  std::ofstream fBurstFile;
  std::ofstream fChannelFile;

  Double_t fArgonionCounts;                                   //Argonion counts
  UInt_t fBurstTime;                                          //Burst identifier (time)
  UInt_t fBurstID;                                            //Burst identifier (ID)
  UInt_t fRunID;                                              //Run identifier (ID)

  Bool_t fEOBProcessed;

};
#endif
