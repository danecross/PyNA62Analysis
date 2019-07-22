// ---------------------------------------------------------------
//
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-08-08
//
// ---------------------------------------------------------------

#ifndef L0TRIGGERRESPONSE_HH
#define L0TRIGGERRESPONSE_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "TEfficiency.h"
#include "TGraphAsymmErrors.h"
#include "TLegend.h"
#include "TText.h"
#include "TStyle.h"

#include "SpectrometerMUV3AssociationOutput.hh"
#include "NewCHODGeometry.hh"
#include "NewCHODDataQualityPlotter.hh"
#include "DownstreamTrack.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TSpecialTriggerEvent;

struct LAVPrimitive{
  Double_t Time;
  Double_t Energy;
  Int_t NHit;
  Int_t iDet;
  Int_t iVeto;
  Int_t NClus;
  Double_t clusterPositionX[20];
  Double_t clusterPositionY[20];
  Double_t clusterEnergy[20];
};

struct PrimAndTimes{
  UInt_t PrimitiveID_N0;
  UInt_t PrimitiveID_N1;
  UInt_t PrimitiveID_N2;
  UInt_t PrimitiveID_N3;
  Double_t Time_N0_ref;
  Double_t Time_N1_ref;
  Double_t Time_N2_ref;
  Int_t T_N0;
  Int_t T_N1;
  Int_t T_N2;
};

struct K3piSelResult{
  bool success ;
  bool q1 ;
  bool q2 ;
  bool qx ;
  bool utmc ;
  int nhits;
};

class L0TriggerResponse : public NA62Analysis::Analyzer {

public:
  explicit L0TriggerResponse(NA62Analysis::Core::BaseAnalysis *ba);
  ~L0TriggerResponse() {}
  void InitHist();
  void InitOutput() {}
  void DefineMCSimple() {}
  void ProcessSpecialTriggerUser(Int_t, unsigned int);
  void ProcessEOBEvent();
  void Process(Int_t);
  void StartOfBurstUser();
  void EndOfBurstUser();
  void StartOfRunUser();
  void EndOfRunUser() {}
  void EndOfJobUser();
  void PostProcess() {}
  void DrawPlot() {}

private:

  Bool_t ControlData ;
  Bool_t PhysicsData ;
  Bool_t fReadingData;
  Int_t  fNRICHHits;
  Int_t  fNRICHSuperCells;
  Double_t fLKrEnergy;
  Int_t fCount ;
  UInt_t fRunID ;
  UInt_t fBurstID ;
  Int_t fPlotResolution ;
  Double_t fTrackMom ;
  Double_t fZposition;
  Int_t fOccupancy[7];
  Int_t fNCOccTS;
  Int_t fNCOcc100;
  Int_t fRef;
  Double_t fRefTime;
  Double_t fBadTriggerThreshold;
  std::vector<double> fBadTriggerThresholds;
  Double_t fArgonionCount;
  NewCHODGeometry* fGeo ;
  Int_t fNL0;
  Int_t fUTMCHits;
  Int_t fNMasks;
  Int_t fFineTimeBit;

  std::vector<double> MaskDownscales;
  std::vector<UInt_t> nPrimitives;
  Bool_t fHasSpecialTrigger;

  std::map< UInt_t, std::vector<TString> > BadTriggers;
  std::ofstream fTriggerBadBursts;

  TString MUV3TriggerBitNames[16];
  TString NCTriggerBitNames1[16];
  TString NCTriggerBitNames2[16];
  TString NCTriggerBitNames3[16];
  TString RICHTriggerBitNames[16];
  TString LAV12TriggerBitNames[16];

  std::vector<PrimAndTimes> PrimInfo;

  std::vector<TString> events;
  std::vector<TString> primitives;
  std::vector<TH1*> h_temp;
  std::vector<TH1*> h_info;
  std::vector<TH1*> h_eobdata;
  std::vector<TH1*> h_muv3eob;
  std::vector<TH1*> h_newchodeob;
  std::vector<TH1*> h_1Dcorr;
  std::vector<TH1*> h_2Dcorr;
  std::vector<TH1*> h_RICH ;
  std::vector<TH1*> h_DigiRaw ;
  std::vector<TH1*> h_HitCorr2D ;
  std::vector<TH1*> h_EpB ;
  std::vector<TH1*> h_EpP ;
  std::vector<TH1*> h_EpZ ;
  std::vector<TH1*> h_EpR ;
  std::vector<TH1*> h_EpO ;
  std::vector<TH1*> h_E1D ;
  std::vector<TH1*> h_ots ;
  std::vector<TH1*> h_primbits ;
  std::vector<TH1*> h_Occupancy ;
  std::vector<TH1*> h_nprimitives ;
  std::vector<TH2F*> h_muv3quadrants ;
  std::vector<TEfficiency*> fEfficiency;
  std::vector<TString> nPerBurst;

  std::vector<LAVPrimitive> fPrimitiveCollection;

  UInt_t Q2bit;
  UInt_t QXbit;
  UInt_t UTMCbit;
  UInt_t E10bit;
  UInt_t E20bit;

  TH1F* fhRunNumber;
  TH1F* fhBurstTime;

  // T0 reading
  std::map<Int_t,Double_t> fNewCHODT0Map;

  void K3piSelection();
  K3piSelResult MyK3piSelection();
  void Kmu2Selection();
  void CHODEfficiencySelection();
  void OnePionSelection();
  void K2piSelection();
  void LavVetoSelection();
  Bool_t MyLavVetoSelection();
  Int_t LAVPrimitives();
  void SetPrimitive(Double_t finetime, Int_t nhits, Int_t det);

  void GetHardcodedFTB();

  void ComputeEfficiency(TString histname, UInt_t L0Detector);
  void ComputeDimuonEfficiency (SpectrometerMUV3AssociationOutput& SpecMUV3,
				TString histname, UInt_t L0Detector);

  void PageOne(TCanvas* Canvas, TString histname, UInt_t L0Detector, TString plotname);
  void PageTwo(TCanvas* Canvas, TEfficiency* gEff, UInt_t L0Detector);
  TObject* GetAxisHisto(bool mode, UInt_t L0Detector);

  void BuildPDFReport();
  void BuildPDFReport2();
  void CorrelationPlots(Int_t ref, Int_t t0, Int_t t1, Int_t t2, TString histname );
  double GetLKrTotEnergy();
  void RichMultPlot(TString histname, UInt_t L0Detector);
  void FillLKrEnergy2D();
  void FillSpasimirPlot(UInt_t L0Detector);
  void FillL0CaloPlots();

  void PrimitiveEOB(TSpecialTriggerEvent* STE, int ChannelGap, TString DetName);
  void PlotPrimitiveEOBInfo(TCanvas* Canvas, int mode, std::vector<TH1*> h_eob) ;
  void DigiTimeRaw(TString detector, Bool_t mode) ;
  void HitCorrelations(TString det1, TString det2) ;
  void MissingMasses(int Mask) ;
  Double_t GetM2(TRecoSpectrometerCandidate* fTrack,
		 TLorentzVector& P_k,
		 Double_t mass_hyp) ;
  void ResponsePerBurst(TCanvas* can, TH1F* num, TH1F* den);
  void ResponsePerPorZ(TCanvas* can, TH1F* num, TH1F* den);
  void L0CaloResponsePerE(TCanvas* can, TH1F* num, TH1F* den, bool rescale);
  void FillEfficiencies(TString var, UInt_t L0Detector, UInt_t bit);
  void FillL0CaloEfficiency(TString var, UInt_t L0Detector, UInt_t bit);
  void GetPrimitiveAndTimes(UInt_t L0Detector, Bool_t mode);
  void PrimCorrelationPlots(TString histname, UInt_t L0Detector);
  bool OneHitInTime(DownstreamTrack& Track);

  void GetPrimitiveBits(Bool_t mode);
  void GetPrimitiveTSBits(UInt_t);
  void GetPrimitiveFTBits(UInt_t, TString);

  void GetDetOccupancies();
  void FillOccupancyPlots(Int_t Type);
};
#endif
