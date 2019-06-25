#ifndef GIGATRACKERTIMERESOLUTION_HH
#define GIGATRACKERTIMERESOLUTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "ThreePiAssociationAlgo.hh"
#include "TRecoCedarEvent.hh"
#include "BlueTubeTracker.hh"
#include "DownstreamTrack.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TRecoSpectrometerEvent;
class TRecoCHODEvent;
class TRecoGigaTrackerEvent;
class TRecoCHANTIEvent;

class GigaTrackerTimeResolution : public NA62Analysis::Analyzer {
public:
  explicit GigaTrackerTimeResolution(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerTimeResolution();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(int iEvent);
  void PostProcess();
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void DrawPlot();
  void Standalone(FILE*, TString);
  void Simultaneous();

protected:

  std::vector<DownstreamTrack*> fDSTs;
  ThreePiAssociationAlgo *fThreePiAlgo;
  TRecoSpectrometerEvent* fSpectrometerEvt;
  TRecoCHODEvent* fCHODEvt;
  TRecoCedarEvent* fCedarEvt;
  TRecoGigaTrackerEvent* fGigaTrackerEvt;
  TRecoCHANTIEvent* fCHANTIEvt;
  double fT0;
  double fT1[20];
  double fX1[20];
  double fY1[20];
  int fN1;
  double fT2[20];
  double fX2[20];
  double fY2[20];
  int fN2;
  double fT3[20];
  double fX3[20];
  double fY3[20];
  int fN3;
  bool fReadingData;

  TH1D* fHDtCedarRICH;
  TH2D* fHDtKTAGN[4];
  TH2D* fHDtKTAGToT[3];
  TH1D* fHDtKTAG[7];

  TH2D* fHDtRICHN[4];
  TH2D* fHDtRICHToT[3];
  TH1D* fHDtRICH[7];

  TH3D* fHDtNN;
  TGraph* fGNMKTAG[3];
  TGraph* fGNMRICH[3];

  TCanvas* fCv;

};
#endif
