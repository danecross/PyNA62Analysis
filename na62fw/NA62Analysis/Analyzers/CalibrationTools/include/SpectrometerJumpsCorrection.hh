// ---------------------------------------------------------------
// History:
//
// Created by Dmitry Madigozhin (madigo@mail.cern.ch) 2016-11-15
//
// ---------------------------------------------------------------

#ifndef SPECTROMETERJUMPSCORRECTION_HH
#define SPECTROMETERJUMPSCORRECTION_HH

#include <stdlib.h>
#include <vector>
#include <iostream>
#include <fstream>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "TGraphErrors.h"

class TH1I;
class TH2F;
class TGraph;
class TTree;

using namespace std;
// using namespace NA62Analysis;

#include <math.h>
#include "TH2.h"

using namespace std;

#define NRING 50

class DRing{
 private:
  bool full, nemp;
  Int_t l;
  Double_t val[NRING], err[NRING];

 public:

  DRing();
  ~DRing();

  void Reset();
  void Put(Double_t x, Double_t e);    // Put a point
  void Get(Double_t *x, Double_t *e);  // Get sliding average
  bool IsNotEmpty(){return nemp;};

};

#define UN 24.951059536
#define REBIN 25
#define MINSIGNIF 12.0 // minimum significance of the peak
#define DT0 0.7*UN     // minimum t0 shift to be regarded as a possible 25-ns jump
#define SIGNI 5.0      // minimum significance for the jump
#define BUFSIZE 256

class SpectrometerJumpsCorrection : public NA62Analysis::Analyzer
{

private:

  ifstream fInputListStream;
 // TH2F *HDigiTimeRaw;
  TH1D *HAccumulated[512][2];
  TGraphErrors *GPeakTimeVsTimeStamp[512];
  TGraph *GBadBurstVsTimeStamp;
  TGraph *GRunVsTimeStamp;
  string fListOfInputFiles;
  Int_t fPreTimestamp, fTimestamp, fPreRun, fRun, fToggle[512];
  Int_t fRunBeginTimestamp, fPreRunBeginTimestamp;

  Int_t BadBurst(TH2F *h);

public:
  TH2F *HDigiTimeRaw;
  explicit SpectrometerJumpsCorrection(NA62Analysis::Core::BaseAnalysis *ba);
  ~SpectrometerJumpsCorrection();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(Int_t iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfRunJumps(Int_t cov);
  void EndOfJobUser();
  void DrawPlot();
protected:

};
#endif
