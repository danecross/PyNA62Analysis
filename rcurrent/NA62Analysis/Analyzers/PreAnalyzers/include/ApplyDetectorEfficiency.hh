#ifndef APPLYDETECTOREFFICIENCY_HH
#define APPLYDETECTOREFFICIENCY_HH

// This analyzer can only be run as a pre-analyzer
#pragma pre-analyzer

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class ApplyDetectorEfficiency : public NA62Analysis::Analyzer {
public:
  explicit ApplyDetectorEfficiency(NA62Analysis::Core::BaseAnalysis *ba);
  ~ApplyDetectorEfficiency();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();

protected:
  TFile* fFile;
  TEfficiency* hMUV3Efficiency; 
  TEfficiency* hNewCEfficiency; 

  TFile* fSTRAWFileData;
  TH2F*  fSTRAWDataPosNum;
  TH2F*  fSTRAWDataPosDen;
  TH2F*  fSTRAWDataNegNum;
  TH2F*  fSTRAWDataNegDen;

  TFile* fSTRAWFileMC;
  TH2F*  fSTRAWMCPosNum;
  TH2F*  fSTRAWMCPosDen;
  TH2F*  fSTRAWMCNegNum;
  TH2F*  fSTRAWMCNegDen;

  Double_t fIntensity;
};
#endif
