#ifndef GIGATRACKERTIMECORRECTIONS_HH
#define GIGATRACKERTIMECORRECTIONS_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"

class TH1I;
class TH2F;
class TH3F;
class TGraph;
class TTree;
class TRecoGigaTrackerEvent;
class TRecoCedarEvent;

class GigaTrackerPixelT0ChipTW : public NA62Analysis::Analyzer {
public:
  explicit GigaTrackerPixelT0ChipTW(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerPixelT0ChipTW();
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
  int GetUID(int ix, int iy, int x0, int y0);
  void CheckPixels(TH3F* h, int s, int c, bool mask[40][45]);
  int GetTZero(TH2D* h, double& t0, double tot, std::string repo = "" );
  void ApplyTW(TH2D* h, float* tw);
  bool isGoodSlice(TH1D* h , int min = 1000);
  double FitSlice(TH1D* h, TF1* f1,int nSlice);
  bool ProcessSlice(TH2D* h, int& start,int& stop, float* tw, TF1* f1, int& nSlice, int dir , int min = 1000);
  void GetTW(TH2D* h, float* tw,  int min= 1000 ,std::string repo = "" , bool svH = 0);
  void SmoothT0(float vt0[40][45], bool masks[40][45], std::string repo1 = "", std::string repo2 = "");

  TRecoGigaTrackerEvent* fGigaTrackerEvt;
  TRecoCedarEvent* fCedarEvt;
  int fChip;
  int fStation;
  Bool_t fSavePixHisto;
  std::string fDirName;
  TH1F * fHdtKTAG;
  TH3F * fHdtKTAG_ToT_Pix[3][10];
};
#endif
