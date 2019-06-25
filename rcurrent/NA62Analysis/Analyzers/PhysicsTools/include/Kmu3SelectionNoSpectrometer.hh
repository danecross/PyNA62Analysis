#ifndef KMU3SELECTIONNOSPECTROMETER_HH
#define KMU3SELECTIONNOSPECTROMETER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TriggerConditions.hh"
#include "TwoLinesCDA.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class Kmu3SelectionNoSpectrometer : public NA62Analysis::Analyzer
{
public:
  explicit Kmu3SelectionNoSpectrometer(NA62Analysis::Core::BaseAnalysis *ba);
  ~Kmu3SelectionNoSpectrometer();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void PostProcess();
  void DrawPlot();
  TVector3 GetMuonMomAM(double, TVector2);
  TVector3 GetMomBM(TVector3);
  double GetMomFromR(double, double);
  TVector3 GetPositionAtZ(TVector3, TVector3, double);
  TVector3 MomAfterKick(TVector3, double);
  bool GetIsCloseInPosMUV3(TVector3, TVector3, double);

private:
  TriggerConditions *fTriggerConditions;

  //parameters
  double Lf;
  double refInd;
  double fMNP33kick;
  double fCutTimeDiffRICH;
  double fCutTimeDiffMUV3;
  double fCutTimeDiffMUV1Pi0;
  double fCutMinEnergyOfMUV1Hit;
  double fCutTimeDiffMUV2Pi0;
  double fCutMinEnergyOfMUV2Hit;
  int fCutMaxNHitsMUV12;
  double fTimeWindowIRC;
  double fTimeWindowSAC;
  double fCutMissM2;

  //output
  bool fEventSelected;
  TLorentzVector fTrackFourMomentum;
  TVector3 fTrackPosition;

  //variables
  double ftime;

};
#endif
