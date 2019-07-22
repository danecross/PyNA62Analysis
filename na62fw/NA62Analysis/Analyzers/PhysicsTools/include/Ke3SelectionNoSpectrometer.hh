#ifndef KE3SELECTIONNOSPECTROMETER_HH
#define KE3SELECTIONNOSPECTROMETER_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "SpectrometerRICHAssociationOutput.hh"
#include "SpectrometerLKrAssociationOutput.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "TwoLinesCDA.hh"
#include "TriggerConditions.hh"
#include <TCanvas.h>

class TH1I;
class TH2F;
class TGraph;
class TTree;

class Ke3SelectionNoSpectrometer : public NA62Analysis::Analyzer {
public:
  explicit Ke3SelectionNoSpectrometer(NA62Analysis::Core::BaseAnalysis *ba);
  ~Ke3SelectionNoSpectrometer();
  void InitHist();
  void InitOutput();
  void DefineMCSimple();
  void Process(int iEvent);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser();
  void EndOfJobUser();
  void PostProcess();
  void DrawPlot();
  TVector3 GetFermionMomLKr(TVector3, TVector3, double, double);
  double GetRFromMom(double, double);
  bool GetHasCloseRingR(double, double, double);
  TVector2 GetRingCenterRICH(TVector3);
  TVector3 GetMomBM(TVector3);
  TVector3 GetMomAM(TVector3);
  double GetMomFromR(double, double);

  TriggerConditions *fTriggerConditions;

private:
  Bool_t  fReadingData; ///< Reading data or my own output? 
  bool fTriggerFlag;
  int fTriggerFlagID;
  double fTimeWindowIRC;
  double fTimeWindowSAC;
  double fCutTimeDiffLKr;
  double fCutTimeDiffRICHTrigger;
  double fCutTimeDiffRICHPi0;
  double fCutTimeDiffRICH;
  double fCutTimeDiffMUV1_min;
  double fCutTimeDiffMUV1_max;
  double fCutTimeDiffMUV3;
  double fCutCloseRingR;
  double fCutRingCenterDifferenceLKrRICHMaxRadius;
  double fCutMissM2;
  double fLKrZ;
  double Lf;
  double refInd;

  bool fEventSelected;
  TLorentzVector fTrackFourMomentum;
  TVector3 fTrackPosition;
  double ftime;

  //Spectrometer+MUV3
  std::vector<SpectrometerMUV3AssociationOutput> fSpecMUV3;
  OutputState fSpecMUV3ParticleID_Output_state;
  TwoLinesCDA fTwoLinesCDA;

  //Spectrometer+LKr
  std::vector<SpectrometerLKrAssociationOutput> fSpecLKr;
  OutputState fSpecLKrParticleID_Output_state;
  double EoP;

  //Spectrometer+RICH
  std::vector<SpectrometerRICHAssociationOutput> fSpecRICH;
  OutputState fSpecRICHParticleID_Output_state;

};
#endif
