// ---------------------------------------------------------------
//
// History:    
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 01.2019
//
// ---------------------------------------------------------------
/// \class Ke3SelectionNoSpectrometer
/// \Brief
/// Ke3 decay selection without STRAW spectrometer
/// \EndBrief
/// \Detailed
/// The analyzer is used by SpectrometerEfficiency analyzer. 
/// It has three outputs: EventSelected, TrackFourMomentum and TrackPosition. 
/// 
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// \EndDetailed 

#include <stdlib.h>
#include <iostream>
#include <bitset>
#include <TChain.h>
#include "Ke3SelectionNoSpectrometer.hh"
#include "MCSimple.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "Pi0Selection.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"
#include "DownstreamTrack.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

Ke3SelectionNoSpectrometer::Ke3SelectionNoSpectrometer(Core::BaseAnalysis *ba) : Analyzer(ba, "Ke3SelectionNoSpectrometer")
{
  RequestTree("Cedar", new TRecoCedarEvent, "Reco");
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");
  RequestTree("RICH", new TRecoRICHEvent, "Reco");
  RequestTree("CHOD", new TRecoCHODEvent, "Reco");
  RequestTree("LAV", new TRecoLAVEvent, "Reco");
  RequestTree("IRC", new TRecoIRCEvent, "Reco");
  RequestTree("SAC", new TRecoSACEvent, "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("MUV1", new TRecoMUV1Event, "Reco");

  RequestL0Data();

  fReadingData = kTRUE;

  AddParam("TimeWindowIRC", "double", &fTimeWindowIRC, 10.);
  AddParam("TimeWindowSAC", "double", &fTimeWindowSAC, 10.);
  AddParam("CutTimeDiffLKr", "double", &fCutTimeDiffLKr, 10.);
  AddParam("CutTimeDiffRICHTrigger", "double", &fCutTimeDiffRICHTrigger, 10.);
  AddParam("CutTimeDiffRICHPi0", "double", &fCutTimeDiffRICHPi0, 2.);
  AddParam("CutTimeDiffRICH", "double", &fCutTimeDiffRICH, 2.);
  AddParam("CutTimeDiffMUV1_min", "double", &fCutTimeDiffMUV1_min, -20.);
  AddParam("CutTimeDiffMUV1_max", "double", &fCutTimeDiffMUV1_max, 20.);
  AddParam("CutTimeDiffMUV3", "double", &fCutTimeDiffMUV3, 10.);
  AddParam("CutCloseRingR", "double", &fCutCloseRingR, 4.);
  AddParam("CutRingCenterDifferenceLKrRICHMaxRadius", "double", &fCutRingCenterDifferenceLKrRICHMaxRadius, 400.);
  AddParam("CutMissM2", "double", &fCutMissM2, 2000.);
  fLKrZ = GeometricAcceptance::GetInstance()->GetZLKr();
  fTriggerConditions = TriggerConditions::GetInstance();
  Lf = 17020.;
  refInd = 1.000062;
}

void Ke3SelectionNoSpectrometer::InitOutput(){
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("Ke3PositronFourMomentum", &fTrackFourMomentum);
  RegisterOutput("Ke3PositronPosition", &fTrackPosition);
}

void Ke3SelectionNoSpectrometer::InitHist(){
  fReadingData = GetIsTree();

  BookHisto(new TH1I("NEvents", "N events;N events", 1, 1, 2));
  BookHisto(new TH2I("hWhichTrigger", "Physics VS Control; Control trigger; Physics trigger", 2, 0, 2, 2, 0, 2));
  BookHisto(new TH1F("hVertexZ", "Vertex Z;Z_{vertex} [mm]", 1000, 90000., 190000.));
  BookHisto(new TH1F("hTimePi0", "Time of the #pi^{0}; t_{#pi^{0}} [ns]", 200, -100., 100.));
  BookHisto(new TH1F("hTimeDiffPI0_MUV3", "#Delta T (PI0-MUV3); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNInTimeMUV3", "N MUV3 candidates in time with #pi^{0};N candidates", 10, 0, 10));
  BookHisto(new TH1F("hTimeDiffPI0_LKr", "#Delta T (PI0-LKr); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNInTimeLKr", "N LKr candidates in time with #pi^{0};N candidates", 10, 0, 10));
  BookHisto(new TH1I("hNRICHRingCandidates", "N RICH ring candidates;N candidates", 10, 0, 10));
  BookHisto(new TH1D("hTimeDiffRICH_trigger", "#Delta T (RICH-trigger); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1D("hTimeDiffPI0_RICH", "#Delta T (RICH-PI0); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNRICHInTimeWithTriggerPi0", "N RICH candidates in time with trigger and #pi^{0};N candidates", 10, 0, 10));
  BookHisto(new TH1F("hLKrClusterEnergy", "Energy of the additional LKr cluster; E [MeV]", 1000, 0., 100000.));
  BookHisto(new TH2D("hLKrClusterPosition", "Position of the additional LKr cluster; X [mm]; Y [mm]", 600, -1500., 1500., 600, -1500., 1500.));
  BookHisto(new TH1F("hPositronMom", "Momentum in positron hypothesis; P_{e^{+}} [MeV]", 1000, 0., 100000.));
  BookHisto(new TH1F("hPionMom", "Momentum in pion hypothesis; P_{#pi^{+}} [MeV]", 1000, 0., 100000.));
  BookHisto(new TH1D("hRICHRingRPositron", "Radius of RICH ring (positron hypothesis); R [mm]", 1000, 189., 190.));
  BookHisto(new TH2D("hRICHRingRVSMomPositron", "RICH ring radius VS momentum (positron hypothesis);P_{e^{+}} [MeV];R [mm]", 500, 0., 100000., 1000, 189., 190.));
  BookHisto(new TH1D("hRICHRingRPion", "Radius of RICH ring (pion hypothesis); R [mm]", 900, 0., 300.));
  BookHisto(new TH2D("hRICHRingRVSMomPion", "RICH ring radius VS momentum (pion hypothesis);P_{#pi^{+}} [MeV];R [mm]", 500, 0., 100000., 900, 0., 300.));
  BookHisto(new TH1D("hRICHRingRDiffPionPositron", "RICH ring R difference (positron - pion); #Delta R [mm]", 1000, -1000., 1000.));
  BookHisto(new TH2F("hExpectedPositronRingCenter_LKr", "Expected ring center (LKr, positron hypothesis); X [mm]; Y [mm]", 2000, -10000., 10000., 4000, -20000., 20000.));
  BookHisto(new TH2I("hCloseRingRPionVSPositron", "Which radius is close to RICH ring radius;#pi^{+};e^{+}", 2, 0, 2, 2, 0, 2));
  BookHisto(new TH1F("hRingRDiffPion", "Ring R difference (pion hypothesis); #Delta R [mm]", 600, 0., 300.));
  BookHisto(new TH1F("hRingRDiffPion_", "Ring R difference (pion hypothesis); #Delta R [mm]", 600, -300., 300.));
  BookHisto(new TH1F("hRingRDiffPositron", "Ring R difference (positron hypothesis); #Delta R [mm]", 600, 0., 300.));
  BookHisto(new TH1F("hRingRDiffPositron_", "Ring R difference (positron hypothesis); #Delta R [mm]", 600, -300., 300.));
  BookHisto(new TH2D("hRingRDiffPionVSPositron", "Ring R difference; #Delta R_{e^{+}} [mm]; #Delta R_{#pi^{+}} [mm]", 600, -300., 300., 600, -300., 300.));
  BookHisto(new TH1I("hWhichRingRIsCloser", "Which hypothesis fits better (based on the ring R comparison); e^{+}, equal, #pi^{+}", 3, 1, 4));
  BookHisto(new TH1F("hRICHRingR_InTimeTriggerPI0", "R of RICH ring in time with trigger and #pi^{0}; R [mm]", 300, 0., 300.));
  BookHisto(new TH2F("hPositronRingCenter_RICH", "Ring center (RICH, positron hypothesis); X [mm]; Y [mm]", 500, -1000., 1000., 500, -1000., 1000.));
  BookHisto(new TH1F("hRCXDiff_LKr_RICH", "#Delta X of ring center (LKr-RICH) (positron hypothesis); #Delta X [mm]", 1000, -500., 500.));
  BookHisto(new TH1F("hRCYDiff_LKr_RICH", "#Delta Y of ring center (LKr-RICH) (positron hypothesis); #Delta Y [mm]", 1000, -500., 500.));
  BookHisto(new TH2F("hRCYvsXDiff_LKr_RICH", "Ring center difference (LKr-RICH) (positron hypothesis); #Delta X [mm]; #Delta Y [mm]", 1000, -500., 500., 1000, -500., 500.));
  BookHisto(new TH1I("hNMUV1Candidates", "N MUV1 candidates; N candidates", 10, 0, 10));
  BookHisto(new TH1D("hTimeDiffPI0_MUV1", "#Delta T (PI0-MUV1 candidate); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNMUV1CandidatesInTime", "N MUV1 candidates in time with #pi^{0}; N candidates", 10, 0, 10));
  BookHisto(new TH1I("hNMUV1Hits", "N MUV1 hits; N hits", 30, 0, 30));
  BookHisto(new TH1D("hTimeDiffPI0_MUV1hit", "#Delta T (PI0-MUV1 hit); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNMUV1HitsInTime", "N MUV1 hits in time with #pi^{0}; N hits", 10, 0, 10));
  BookHisto(new TH1I("hLAVHasTimeMatching", "LAV has time matching; ;N events", 2, 0, 2));
  BookHisto(new TH1I("hSAVHasTimeMatching", "SAV has time matching; ;N events", 2, 0, 2));
  BookHisto(new TH1I("hNGTKCandidates", "Number of GTK candidates", 50, 0, 50));
  BookHisto(new TH1F("hKaonMom", "Kaon momentum; P_{K} [MeV]", 1000, 0., 100000.));
  BookHisto(new TH1F("hMissM2", "Squared neutrino missing mass; M_{miss}^{2} (#nu) [MeV^{2}]", 200, -10000., 10000.));
  BookHisto(new TH1F("hMissM2_AS", "Squared neutrino missing mass (after full selection); M_{miss}^{2}(#nu) [MeV^{2}]", 60, -3000., 3000.));
}

void Ke3SelectionNoSpectrometer::DefineMCSimple(){}

void Ke3SelectionNoSpectrometer::StartOfRunUser(){}

void Ke3SelectionNoSpectrometer::StartOfBurstUser(){}

void Ke3SelectionNoSpectrometer::Process(int iEvent){
  FillHisto("NEvents", 1);

  fEventSelected = false;
  SetOutputState("EventSelected", kOValid); 
  fTrackFourMomentum.SetXYZT(0., 0., 0., 0.);
  SetOutputState("Ke3PositronFourMomentum", kOValid);
  fTrackPosition.SetXYZ(0., 0., 0.);
  SetOutputState("Ke3PositronPosition", kOValid);

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  // Prepare trigger variables
  Bool_t physicsTrigger = fTriggerConditions->IsPhysicsTrigger(GetL0Data());
  Bool_t controlTrigger = fTriggerConditions->IsControlTrigger(GetL0Data());
  Double_t fineTime = GetEventHeader()->GetFineTime();
  if(!GetWithMC()){ // data
    FillHisto("hWhichTrigger", (int)controlTrigger, (int)physicsTrigger);
    if(controlTrigger){ // control
      fineTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();
    }else if(physicsTrigger){ // physics
      fineTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
    }else{
      cout<<user()<<"Data event "<<iEvent <<" is not Physics nor Control"<<endl;
    };
  };
  Double_t triggerTime = fineTime*TdcCalib;
  if(!controlTrigger) return;

  //no K2pi
  OutputState state;
  bool k2piSelected = *(bool*)GetOutput("K2piSelection.EventSelected", state);
  if(state!=kOValid) return;
  if(k2piSelected) return;

  //pion
  TVector3 vertex;
  int clus1;
  int clus2;
  bool selected = *(bool*)GetOutput("Pi0Selection.EventSelected");
  if(!selected) return;
  std::vector<Pi0SelectionOutput> pi0output = *(std::vector<Pi0SelectionOutput>*)GetOutput("Pi0Selection.SelectedPi0");
  if(pi0output.size()!=1) return;

  TLorentzVector Pion;
  Pion = pi0output[0].fMomentum;
  ftime = pi0output[0].fTime;
  vertex = pi0output[0].fPosition;
  std::pair<int, int> clustersID = pi0output[0].fClustersID;
  clus1 = clustersID.first;
  clus2 = clustersID.second;
  FillHisto("hVertexZ", vertex.Z());
  FillHisto("hTimePi0", ftime);

  // MUV3 veto - no muon in MUV3 in time with pi0 (do not want Kmu2 nor Kmu3)
  TRecoMUV3Event *MUV3Event = static_cast<TRecoMUV3Event*>(GetEvent("MUV3"));
  int MUV3nc = MUV3Event->GetNCandidates();
  int countMUV3InTime = 0;
  for(int i=0; i<MUV3nc; i++){
    TRecoMUV3Candidate *MUV3cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(i));
    double time = MUV3cand->GetTime();
    FillHisto("hTimeDiffPI0_MUV3", time - ftime);
    if(fabs(time - ftime)<fCutTimeDiffMUV3) countMUV3InTime++;
  };
  FillHisto("hNInTimeMUV3", countMUV3InTime);
  if(countMUV3InTime>0) return;

  //extra LKr candidate in time with pi0
  TRecoLKrEvent *LKrEvent = static_cast<TRecoLKrEvent*>(GetEvent("LKr"));
  int countLKrInTime = 0;
  int inTimeID_LKr = -1;
  for(int i=0; i<LKrEvent->GetNCandidates(); i++){
    if((i==clus1) || (i==clus2)) continue;
    TRecoLKrCandidate *LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
    double time = LKrCand->GetTime();
    FillHisto("hTimeDiffPI0_LKr", time - ftime);
    if(fabs(time - ftime)<fCutTimeDiffLKr){
      countLKrInTime++;
      inTimeID_LKr = i;
    };
  };
  FillHisto("hNInTimeLKr", countLKrInTime);

  //one RICH in time with trigger and pi0
  TRecoRICHEvent *RICHEvent = static_cast<TRecoRICHEvent*>(GetEvent("RICH"));
  int nRICHInTimeWithTriggerPi0 = 0;
  int inTimeID_TriggerPi0RICH = -1;
  FillHisto("hNRICHRingCandidates", RICHEvent->GetNRingCandidates());
  for(int i=0; i<RICHEvent->GetNRingCandidates(); i++){
    TRecoRICHCandidate *RICHCand = static_cast<TRecoRICHCandidate*>(RICHEvent->GetRingCandidate(i));
    double t = RICHCand->GetRingTime();
    FillHisto("hTimeDiffRICH_trigger", t-triggerTime);
    FillHisto("hTimeDiffPI0_RICH", t - ftime);
    if(fabs(t-triggerTime)<fCutTimeDiffRICHTrigger && fabs(t - ftime)<fCutTimeDiffRICHPi0){
      nRICHInTimeWithTriggerPi0++;
      inTimeID_TriggerPi0RICH = i;
    };
  };
  FillHisto("hNRICHInTimeWithTriggerPi0", nRICHInTimeWithTriggerPi0);

  if(countLKrInTime!=1 || nRICHInTimeWithTriggerPi0!=1) return;
  TRecoRICHCandidate *RICHCand = static_cast<TRecoRICHCandidate*>(RICHEvent->GetRingCandidate(inTimeID_TriggerPi0RICH));

  //positron in RICH
  bool isPositronFromRICH = false;
  TVector2 RCpos_RICH(0., 0.);
  FillHisto("hRICHRingR_InTimeTriggerPI0", RICHCand->GetRingRadius());
  if(RICHCand->GetRingRadius()>185.){ //positron
    RCpos_RICH = RICHCand->GetRingCenter();
    FillHisto("hPositronRingCenter_RICH", RCpos_RICH.X(), RCpos_RICH.Y());
    isPositronFromRICH = true;
  };

  //positron in LKr and RICH
  bool isPositronFromLKrRICH = false;
  TVector3 positronMom_LKr(0., 0., 0.);
  TVector3 pionMom_LKr(0., 0., 0.);
  TVector2 RCpos_LKr(0., 0.);
  TRecoLKrCandidate *LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(inTimeID_LKr));
  double E = LKrCand->GetClusterEnergy();
  FillHisto("hLKrClusterEnergy", E);
  TVector3 clusPos(LKrCand->GetClusterX(), LKrCand->GetClusterY(), fLKrZ);
  FillHisto("hLKrClusterPosition", clusPos.X(), clusPos.Y());
  positronMom_LKr = GetFermionMomLKr(vertex, clusPos, E, MEL);
  FillHisto("hPositronMom", positronMom_LKr.Mag());
  pionMom_LKr = GetFermionMomLKr(vertex, clusPos, E, MPI);
  FillHisto("hPionMom", pionMom_LKr.Mag());
  double Rpos = GetRFromMom(positronMom_LKr.Mag(), MEL);
  FillHisto("hRICHRingRPositron", Rpos);
  FillHisto("hRICHRingRVSMomPositron", positronMom_LKr.Mag(), Rpos);
  double Rpi = GetRFromMom(pionMom_LKr.Mag(), MPI);
  FillHisto("hRICHRingRPion", Rpi);
  FillHisto("hRICHRingRVSMomPion", pionMom_LKr.Mag(), Rpi);
  FillHisto("hRICHRingRDiffPionPositron", Rpi-Rpos);
  RCpos_LKr = GetRingCenterRICH(GetMomAM(positronMom_LKr));
  FillHisto("hExpectedPositronRingCenter_LKr", RCpos_LKr.X(), RCpos_LKr.Y());

  bool hasCloseRingRpos = GetHasCloseRingR(RICHCand->GetRingRadius(), Rpos, fCutCloseRingR);
  bool hasCloseRingRpi = GetHasCloseRingR(RICHCand->GetRingRadius(), Rpi, fCutCloseRingR);
  FillHisto("hCloseRingRPionVSPositron", (int)hasCloseRingRpos, (int)hasCloseRingRpi);
  FillHisto("hRingRDiffPion", fabs(RICHCand->GetRingRadius()-Rpi));
  FillHisto("hRingRDiffPion_", RICHCand->GetRingRadius()-Rpi);
  FillHisto("hRingRDiffPositron", fabs(RICHCand->GetRingRadius()-Rpos));
  FillHisto("hRingRDiffPositron_", RICHCand->GetRingRadius()-Rpos);
  FillHisto("hRingRDiffPionVSPositron", RICHCand->GetRingRadius()-Rpos, RICHCand->GetRingRadius()-Rpi);
  bool notPositron = false;  
  if(fabs(RICHCand->GetRingRadius()-Rpos)<fabs(RICHCand->GetRingRadius()-Rpi)){
    FillHisto("hWhichRingRIsCloser", 1);
  }else if(fabs(RICHCand->GetRingRadius()-Rpos)==fabs(RICHCand->GetRingRadius()-Rpi)){
    FillHisto("hWhichRingRIsCloser", 2);
  }else{
    FillHisto("hWhichRingRIsCloser", 3);
    notPositron = true;
  }; 
  if(!notPositron){
    if(hasCloseRingRpos) isPositronFromLKrRICH = true;
  };

  //compare positron from LKr&&RICH and from RICH
  if(isPositronFromLKrRICH && isPositronFromRICH){
    FillHisto("hRCXDiff_LKr_RICH", RCpos_LKr.X() - RCpos_RICH.X());
    FillHisto("hRCYDiff_LKr_RICH", RCpos_LKr.Y() - RCpos_RICH.Y());
    FillHisto("hRCYvsXDiff_LKr_RICH", RCpos_LKr.X() - RCpos_RICH.X(),  RCpos_LKr.Y() - RCpos_RICH.Y());
    if(pow(RCpos_LKr.X() - RCpos_RICH.X(), 2) + pow(RCpos_LKr.Y() - RCpos_RICH.Y(), 2)>fCutRingCenterDifferenceLKrRICHMaxRadius) return;
  }else{
    return;
  };

  //MUV1 pion veto (candidates)
  TRecoMUV1Event *MUV1Event = static_cast<TRecoMUV1Event*>(GetEvent("MUV1"));
  FillHisto("hNMUV1Candidates", MUV1Event->GetNCandidates());
  int nCandInTimeMUV1 = 0;
  for(int i=0; i<MUV1Event->GetNCandidates(); i++){
    TRecoMUV1Candidate *MUV1Cand = static_cast<TRecoMUV1Candidate*>(MUV1Event->GetCandidate(i));
    double t = MUV1Cand->GetTime();
    FillHisto("hTimeDiffPI0_MUV1", t-ftime);
    if(((t-ftime)>fCutTimeDiffMUV1_min) && ((t-ftime)<fCutTimeDiffMUV1_max)) nCandInTimeMUV1++;
  };
  FillHisto("hNMUV1CandidatesInTime", nCandInTimeMUV1);
  if(nCandInTimeMUV1>0) return;

  //MUV1 pion veto (hits)
  FillHisto("hNMUV1Hits", MUV1Event->GetNHits());
  int nHitsInTimeMUV1 = 0;
  for(int i=0; i<MUV1Event->GetNHits(); i++){
    TRecoMUV1Hit *MUV1Hit = static_cast<TRecoMUV1Hit*>(MUV1Event->GetHit(i));
    double t = MUV1Hit->GetTime();
    FillHisto("hTimeDiffPI0_MUV1hit", t-ftime);
    if(((t-ftime)>fCutTimeDiffMUV1_min) && ((t-ftime)<fCutTimeDiffMUV1_max)) nHitsInTimeMUV1++;
  };
  FillHisto("hNMUV1HitsInTime", nHitsInTimeMUV1);
  if(nHitsInTimeMUV1>0) return;

  TLorentzVector Positron;
  Positron.SetVectM(positronMom_LKr, MEL);

  //LAV veto (with timing)
  TRecoLAVEvent *LAVEvent = static_cast<TRecoLAVEvent*>(GetEvent("LAV"));
  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(ftime);
  FillHisto("hLAVHasTimeMatching", pLAVMatching->LAVHasTimeMatching(LAVEvent));
  if(pLAVMatching->LAVHasTimeMatching(LAVEvent)) return;

  // IRC and SAC veto (with timing)
  TRecoSACEvent *SACEvent = static_cast<TRecoSACEvent*>(GetEvent("SAC"));
  TRecoIRCEvent *IRCEvent = static_cast<TRecoIRCEvent*>(GetEvent("IRC"));
  SAVMatching* pSAVMatching = *(SAVMatching**)GetOutput("PhotonVetoHandler.SAVMatching");
  pSAVMatching->SetReferenceTime(ftime);
  pSAVMatching->SetIRCTimeCuts(fTimeWindowIRC, fTimeWindowIRC); // half time window; default = 5ns
  pSAVMatching->SetSACTimeCuts(fTimeWindowSAC, fTimeWindowSAC); // half time window; default = 5ns
  Bool_t SAVmatched = pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent);
  FillHisto("hSAVHasTimeMatching", SAVmatched);
  if(SAVmatched) return;

  //kaon
  TVector3 kaonThreeMomentum;
  TRecoGigaTrackerEvent *GTKEvent = static_cast<TRecoGigaTrackerEvent*>(GetEvent("GigaTracker"));
  int gtkID = pi0output[0].fGTKID; 
  if(gtkID >= 0){
    if(gtkID<GTKEvent->GetNCandidates()){
      FillHisto("hNGTKCandidates", GTKEvent->GetNCandidates());
      TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(gtkID));
      kaonThreeMomentum = GTKCand->GetMomentum();
    }else{
      cout << user_normal() << "WARNING: Requested GTK ID out of range (too large)!" << endl;
      FillHisto("hNGTKCandidates", -1);
    };
  } else {                      // take average kaon momentum
    kaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  };
  FillHisto("hKaonMom", kaonThreeMomentum.Mag());

  TLorentzVector Kaon;
  Kaon.SetVectM(kaonThreeMomentum, MKCH);

  double missM2= (Kaon-Positron-Pion).M2();
  FillHisto("hMissM2", missM2);
  if(fabs(missM2)>fCutMissM2) return;
  FillHisto("hMissM2_AS", missM2);

  fTrackFourMomentum = Positron;
  fTrackPosition = vertex; 
  fEventSelected = true;
}

void Ke3SelectionNoSpectrometer::PostProcess(){}

void Ke3SelectionNoSpectrometer::EndOfBurstUser(){}

void Ke3SelectionNoSpectrometer::EndOfRunUser(){}

void Ke3SelectionNoSpectrometer::EndOfJobUser(){
  SaveAllPlots();
}

void Ke3SelectionNoSpectrometer::DrawPlot(){}

Ke3SelectionNoSpectrometer::~Ke3SelectionNoSpectrometer(){}

TVector3 Ke3SelectionNoSpectrometer::GetFermionMomLKr(TVector3 vertex, TVector3 position, double E, double M){
  //momentum before magnet
  double denom = sqrt((position.Y() - vertex.Y())*(position.Y() - vertex.Y()) + (position.Z() - vertex.Z())*(position.Z() - vertex.Z()));
  double kick = 270.;
  double magnetZ = 197600.;
  double p = sqrt(E*E - M*M);
  double px = kick*(position.Z()-magnetZ)/(position.Z()-vertex.Z()) + p*(position.X()-vertex.X())/denom;
  double py = p*(position.Y()-vertex.Y())/denom;
  double pz = p*(position.Z()-vertex.Z())/denom;

  TVector3 momentum(px, py, pz);
  return momentum;
}

double Ke3SelectionNoSpectrometer::GetRFromMom(double P, double M){
  double R = 0.;
  if(P>M/sqrt(refInd*refInd - 1.)){
    double beta = P/sqrt(M*M + P*P);
    double theta = acos(1./(beta*refInd));
    R = Lf*theta;
  };
  return R;
}

bool Ke3SelectionNoSpectrometer::GetHasCloseRingR(double candR, double trackR, double cut){
  bool isClose = false; 
  if(fabs(candR-trackR)<cut) isClose = true;

  return isClose;
}

TVector2 Ke3SelectionNoSpectrometer::GetRingCenterRICH(TVector3 momAM){
  double Rx = Lf*momAM.X()/momAM.Z();
  double Ry = Lf*momAM.Y()/momAM.Z();
  TVector2 RC(Rx, Ry);

  return RC;
}

TVector3 Ke3SelectionNoSpectrometer::GetMomBM(TVector3 momAM){
  double kick = 270.;
  double Px = momAM.X();
  double Pz = momAM.Z();
  double PxBM = Px + kick;
  TVector2 momT(Px, Pz);
  double pT = momT.Mod();
  double beta = acos(PxBM/pT);
  double theta = acos(Px/pT);
  double alpha = theta - beta;
  TVector3 mom = momAM;
  mom.RotateY(alpha);
  TVector3 momBM = mom;

  return momBM;
}

TVector3 Ke3SelectionNoSpectrometer::GetMomAM(TVector3 momBM){
  double kick = 270.;
  double Px = momBM.X();
  double Pz = momBM.Z();
  double PxAM = Px - kick;
  TVector2 momT(Px, Pz);
  double pT = momT.Mod();
  double beta = acos(Px/pT);
  double theta = acos(PxAM/pT);
  double alpha = theta - beta;
  TVector3 mom = momBM;
  mom.RotateY(-alpha);
  TVector3 momAM = mom;

  return momAM;
}

double Ke3SelectionNoSpectrometer::GetMomFromR(double R, double M){
  double theta = R/Lf;
  double beta = 1./(cos(theta)*refInd);
  double P = fabs((beta*M)/sqrt(1.-beta*beta));

  return P;
}
