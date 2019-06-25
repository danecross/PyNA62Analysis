#ifndef KINEMATICTAILS_HH
#define KINEMATICTAILS_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "BlueTubeTracker.hh"
#include <TCanvas.h>

class TRecoGigaTrackerEvent;
class TRecoSpectrometerEvent;

class KinematicTails : public NA62Analysis::Analyzer
{
public:
  KinematicTails(NA62Analysis::Core::BaseAnalysis *ba);
  ~KinematicTails();
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
  void PrepareOutputs();
  void ValidateOutputs();

private:
  void Kmu2();
  void K2pi();
  void FillHistoRegions(TString, std::string, std::string, double, double, double, double, int);
  int WhichRegion(double, double, double, double, int);
  BlueTubeTracker *ftracker;

  int fWhichDecay;
  int fTrackID;
  int fKaonID;
  double fMissM2;
  double fMissM2beam;
  double fMissM2rich;
  TVector3 fVertex;
  TLorentzVector fKaon;
  TLorentzVector fTrack;
  bool fReadingData;

  //parameters
  int fWhichTrigger;
  bool verb;
  bool UseGTK;
  bool fFillHistoCheck;

  double fSR1_low;
  double fSR1_up;
  double fSR2_low;
  double fSR2_up;
  double fCR1_low;
  double fCR1_up;
  double fCR2_low;
  double fCR2_up;
  double fPiPi_low;
  double fPiPi_up;
  double fCRKmu2_up;
  double fKmu2_low;
  double fK3pi_low;
  double fSR1richlowp_low;
  double fSR1richlowp_up;
  double fSR1richmiddlep_low;
  double fSR1richmiddlep_up;
  double fSR1richhighp_low;
  double fSR1richhighp_up;
  double fSR2rich_low;
  double fSR2rich_up;
  double fPiPirich_low;
  double fPiPirich_up;
  double fSR1beamlowp_low;
  double fSR1beamlowp_up;
  double fSR1beammiddlep_low;
  double fSR1beammiddlep_up;
  double fSR1beamhighp_low;
  double fSR1beamhighp_up;
  double fSR2beam_low;
  double fSR2beam_up;

  TH2D* hKP;
  TH2D* hKM;
  TH1D* hKP1;
  TH1D* hKM1;
  TString fOutPDFfileName;
  TString fOutTableName;

  TString k2pi;
  TString kmu2;
  TString gamma;
  TString nogamma;
  TString all;
  TString reg[9] = {"_no", "_kmu2", "_CRkmu2", "_R1", "_CR1k2pi", "_k2pi", "_CR2k2pi", "_R2", "_k3pi"};
  TLorentzVector fKaonNom;
  TRecoGigaTrackerEvent *fGTKEvent;
  TRecoSpectrometerEvent *fSTRAWEvent;
};
#endif
