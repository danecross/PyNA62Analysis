// ---------------------------------------------------------------
//
// History:
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2018-11-12
//
// ---------------------------------------------------------------

/// \class K2piSelectionNoSpectrometer
/// \Brief
/// K2pi decay selection without STRAW spectrometer
/// \EndBrief
/// \Detailed
/// The analyzer uses Pi0Selection analyzer as an input to the selection.
/// Exactly one reconstructed pi0 is required to be present in the event.
/// If STRAW information is not used, kaon candidate 
/// (GTK or nominal, controlled by parameter fUseGTK(=false by default))
/// is found and pi+ 4-momentum is computed from K+ and pi0 4-momenta.
/// The analyzer has the following outputs:
/// EventSelected, K2piTime, K2piTrackID, K2piPionFourMomentum, K2piVertexPosition.
/// The L0 trigger mask to be used can be passed from the command line (it is 0x1FF by default).
/// The bit corresponding to the mask 0x100 is for the control trigger,
/// bits 0xFF are for the physics trigger.
/// For example, to select events with L0 trigger bit 5 up, one can call
/// \code
/// ./MyApplication ... -p "K2piSelectionNoSpectrometer:TriggerMask=0x20"
/// \endcode
/// or equivalently, using decimal notation,
/// \code
/// ./MyApplication ... -p "K2piSelectionNoSpectrometer:TriggerMask=32"
/// \endcode
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <TChain.h>
#include "Pi0Selection.hh"
#include "K2piSelectionNoSpectrometer.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "MCSimple.hh"
#include "functions.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

K2piSelectionNoSpectrometer::K2piSelectionNoSpectrometer(Core::BaseAnalysis *ba) : Analyzer(ba, "K2piSelectionNoSpectrometer") {
  RequestTree("LKr",    new TRecoLKrEvent,    "Reco");
  RequestTree("LAV",    new TRecoLAVEvent,    "Reco");
  RequestTree("IRC",    new TRecoIRCEvent,    "Reco");
  RequestTree("SAC",    new TRecoSACEvent,    "Reco");
  RequestTree("MUV1",   new TRecoMUV1Event,   "Reco");
  RequestTree("MUV2",   new TRecoMUV2Event,   "Reco");
  RequestTree("MUV3",   new TRecoMUV3Event,   "Reco");
  RequestTree("Cedar",  new TRecoCedarEvent,  "Reco");
  RequestTree("GigaTracker",  new TRecoGigaTrackerEvent, "Reco");

  RequestL0Data();

  fReadingData = kTRUE;
  fTriggerConditions = TriggerConditions::GetInstance();
  fDownscalingCtrl = 0;

  // Parameters
  AddParam("TriggerMask"                      , &fTriggerMask          , 0x1FF);
  AddParam("MaxNBursts"                       , &fMaxNBursts           , 5000);
  AddParam("UseGTK"               , "bool"  , &fUseGTK               , false);
  AddParam("CutTimeDiffMuonPi0"   , "double", &fCutTimeDiffMuonPi0   , 5.);
  AddParam("CutTimeDiffMUV1Pi0"   , "double", &fCutTimeDiffMUV1Pi0   , 5.);
  AddParam("CutMinEnergyOfMUV1Hit", "double", &fCutMinEnergyOfMUV1Hit, 100.);
  AddParam("CutTimeDiffMUV2Pi0"   , "double", &fCutTimeDiffMUV2Pi0   , 5.);
  AddParam("CutMinEnergyOfMUV2Hit", "double", &fCutMinEnergyOfMUV2Hit, 100.);
  AddParam("CutMinNHitsMUV12"     , "int"   , &fCutMinNHitsMUV12     , 10);
  AddParam("CutTimeDiffLKr"       , "double", &fCutTimeDiffLKr       , 5.);
  AddParam("TimeWindowIRC"        , "double", &fTimeWindowIRC        , 10.);
  AddParam("TimeWindowSAC"        , "double", &fTimeWindowSAC        , 10.);
  AddParam("CutMinPionMass"       , "double", &fCutMinPionMass       , 120.);
  AddParam("CutMaxPionMass"       , "double", &fCutMaxPionMass       , 160.);

  // Outputs
  fEventSelected = false;
  fK2piTime = 0.0;
  fK2piTrackID = -1;
  fK2piPionFourMomentum = TLorentzVector();
  fK2piVertexPosition = TVector3();
}

K2piSelectionNoSpectrometer::~K2piSelectionNoSpectrometer() {}

void K2piSelectionNoSpectrometer::StartOfRunUser() {
  if (!fReadingData) return; // no action if reading own output in --histo mode

  // DS of the control trigger:
  // -999 means downscaling is not found the database;
  // -1 means downscaling is variable for this run
  fDownscalingCtrl = fTriggerConditions->GetControlTriggerDownscaling(GetRunID());
}

void K2piSelectionNoSpectrometer::InitHist() {
  fReadingData = GetIsTree();

  if (fReadingData) {
    BookHisto("hPhysicsEventsPerBurst", new TH1F("PhysicsEventsPerBurst", "Physics events per burst;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));

    BookHisto("hNEventsAfterPi0Selection", new TH1I("NEventsAfterPi0Selection", "N Events After Pi0Selection", 2, 0, 2));
    BookHisto("hK2piTime", new TH1F("K2piTime","K2pi event time;K2pi time [ns]", 500, -50., 50.));
    BookHisto("hTimeDiffPI0_MUV3", new TH1F("TimeDiffPI0_MUV3", "#Delta T (PI0-MUV3); #Delta T [ns]", 200, -100., 100.));
    BookHisto("hTimeDiffPI0_MUV1", new TH1F("TimeDiffPI0_MUV1", "#Delta T (PI0-MUV1); #Delta T [ns]", 200, -100., 100.));
    BookHisto("hEofHit_MUV1", new TH1F("EofHit_MUV1", "MUV1 hit energy; E_{hit} [MeV]", 200, 0., 40000.));
    BookHisto("hHitEvsTimeDiff_MUV1", new TH2F("HitEvsTimeDiff_MUV1", "MUV1 hit energy VS #Delta T (PI0-MUV1); #Delta T (PI0-MUV1) [ns];  E_{hit} [MeV]", 200, -100., 100., 200, 0., 40000.));
    BookHisto("hTimeDiffPI0_MUV2", new TH1F("TimeDiffPI0_MUV2", "#Delta T (PI0-MUV2); #Delta T [ns]", 200, -100., 100.));
    BookHisto("hEofHit_MUV2", new TH1F("EofHit_MUV2", "MUV2 hit energy; E_{hit} [MeV]", 200, 0., 40000.));
    BookHisto("hHitEvsTimeDiff_MUV2", new TH2F("HitEvsTimeDiff_MUV2", "MUV2 hit energy VS #Delta T (PI0-MUV2); #Delta T (PI0-MUV2) [ns];  E_{hit} [MeV]", 200, -100., 100., 200, 0., 40000.));
    BookHisto("hTimeDiffPI0_LKr", new TH1F("hTimeDiffPI0_LKr", "#Delta T (PI0-LKr); #Delta T [ns]", 200, -100., 100.));
    BookHisto("hNInTimeLKr", new TH1I("hNInTimeLKr", "N LKr candidates in time with #pi^{0}; N candidates", 10, 0, 10));
    BookHisto("hLAVHasTimeMatching", new TH1I("LAVHasTimeMatching", "LAV Time Matching", 2, 0, 2));
    BookHisto("hSAVHasTimeMatching", new TH1I("SAVHasTimeMatching", "SAV Time Matching", 2, 0, 2));
    BookHisto("hNGTKCandidates", new TH1I("NGTKCandidates", "Number of GTK candidates", 50, 0, 50));
    BookHisto("hPionM", new TH1F("PionM", "Pion missing mass: (P(K^{+}) - P(#pi^{0})).M(); M(#pi^{+}) [MeV]", 500, -550., 450.));
    BookHisto("hPionP", new TH1F("PionP", "Pion momentum: (P(K^{+}) - P(#pi^{0})).Rho(); P(#pi^{+}) [MeV]", 200, 0., 80000.));
    BookHisto("hPionM2vsVtxZ", new TH2F("PionM2vsVtxZ", "Pion squared missing mass vs Z_{vertex}; Z [mm]; M^{2} (#pi^{+}) [MeV^{2}]", 95, 95000., 190000., 400, -200000., 200000.));
    BookHisto("hMissM2vsMomPi", new TH2F("MissM2vsMomPi", "Pion squared missing mass vs pion momentum; P (#pi^{+}) [MeV]; M^{2}_{miss} [MeV^{2}]", 80, 0., 80000., 400, -200000., 200000.));
    BookHisto("hMissM2vsMomPi0", new TH2F("MissM2vsMomPi0", "Pion squared missing mass vs Neutr. pion momentum; P(#pi^{0}); M^{2}_{miss} [MeV^{2}]", 80, 0., 80000., 400, -200000., 200000.));
    BookHisto("hPionP_k2piSelected", new TH1F("PionP_k2piSelected", "Pion momentum (selected K2pi events); P(#pi^{+}) [MeV]", 200, 0., 80000.));

    BookHisto("hK2piEventsPerBurst", new TH1F("K2piEventsPerBurst", "K2pi candidates per burst;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hK2piEventsPerBurstControlTrigger", new TH1F("K2piEventsPerBurstControlTrigger", "K2pi candidates per burst (control trigger)*DS;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hK2piEventsPerBurstControlTriggerQM0", new TH1F("K2piEventsPerBurstControlTriggerQM0", "K2pi candidates per burst (control trigger, quality mask = 0)*DS;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
  } else {
    cout << user_normal() << "Reading my own output" << endl;
  }
}

void K2piSelectionNoSpectrometer::InitOutput() {
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("K2piTime",      &fK2piTime);
  RegisterOutput("K2piTrackID",   &fK2piTrackID);
  RegisterOutput("K2piPionFourMomentum", &fK2piPionFourMomentum);
  RegisterOutput("K2piVertexPosition",   &fK2piVertexPosition);
}

void K2piSelectionNoSpectrometer::Process(Int_t) {
  SetOutputState("EventSelected", kOValid);
  SetOutputState("K2piTime",      kOInvalid);
  SetOutputState("K2piTrackID",   kOInvalid);
  SetOutputState("K2piPionFourMomentum", kOInvalid);
  SetOutputState("K2piVertexPosition", kOInvalid);
  fEventSelected = false;
  fK2piTime = 0.;
  fK2piTrackID = -1;
  fK2piPionFourMomentum.SetXYZM(0., 0., 0., 0.);
  fK2piVertexPosition.SetXYZ(0., 0., 0.);

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  Bool_t physicsTrig = fTriggerConditions->IsPhysicsTrigger(GetL0Data());
  Bool_t controlTrig = fTriggerConditions->IsControlTrigger(GetL0Data());
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t TriggerOK =
    (physicsTrig && (L0TriggerWord & fTriggerMask)) || (controlTrig && (0x100 & fTriggerMask));

  Int_t BurstID   = GetBurstID();

  if (TriggerOK) FillHisto("hPhysicsEventsPerBurst", BurstID);

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoLAVEvent* LAVEvent = GetEvent<TRecoLAVEvent>();
  TRecoIRCEvent* IRCEvent = GetEvent<TRecoIRCEvent>();
  TRecoSACEvent* SACEvent = GetEvent<TRecoSACEvent>();
  TRecoMUV1Event* MUV1Event = GetEvent<TRecoMUV1Event>();
  TRecoMUV2Event* MUV2Event = GetEvent<TRecoMUV2Event>();
  TRecoMUV3Event* MUV3Event = GetEvent<TRecoMUV3Event>();
  TRecoGigaTrackerEvent* GTKEvent  = GetEvent<TRecoGigaTrackerEvent>();

  // read outputs
  auto Clusters = *GetOutput<vector<EnergyCluster>>("EnergyClusterBuilder.Output");
  auto pi0Selected = *GetOutput<std::vector<Pi0SelectionOutput>>("Pi0Selection.SelectedPi0");

  FillHisto("hNEventsAfterPi0Selection", (pi0Selected.size() == 1));
  if (pi0Selected.size() != 1) return;
  Pi0SelectionOutput pi0 = pi0Selected.at(0);

  fK2piTime = pi0.fTime;
  SetOutputState("K2piTime", kOValid);
  FillHisto("hK2piTime", fK2piTime);

  fK2piVertexPosition = pi0.fPosition;
  SetOutputState("K2piVertexPosition", kOValid);

  // MUV3 veto - no muon in MUV3 in time with pi0 (do not want Kmu2)
  for (Int_t icand = 0; icand < MUV3Event->GetNCandidates(); icand++) {
    auto MUV3cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(icand));
    Double_t time = MUV3cand->GetTime();
    FillHisto("hTimeDiffPI0_MUV3", time - fK2piTime);
    if (fabs(time - fK2piTime) < fCutTimeDiffMuonPi0) return;
  }

  int countMUV1 = 0;
  for (Int_t ihit = 0; ihit < MUV1Event->GetNHits(); ihit++) {
    auto MUV1hit = static_cast<TRecoMUV1Hit*>(MUV1Event->GetHit(ihit));
    Double_t time = MUV1hit->GetTime();
    FillHisto("hTimeDiffPI0_MUV1", time - fK2piTime);
    FillHisto("hEofHit_MUV1", MUV1hit->GetEnergy());
    FillHisto("hHitEvsTimeDiff_MUV1", time - fK2piTime, MUV1hit->GetEnergy());
    if ((fabs(time - fK2piTime) < fCutTimeDiffMUV1Pi0) && (MUV1hit->GetEnergy() > fCutMinEnergyOfMUV1Hit)) countMUV1++;
  }
  int countMUV2 = 0;
  for (Int_t ihit = 0; ihit < MUV2Event->GetNHits(); ihit++) {
    auto MUV2hit = static_cast<TRecoMUV2Hit*>(MUV2Event->GetHit(ihit));
    Double_t time = MUV2hit->GetTime();
    FillHisto("hTimeDiffPI0_MUV2", time - fK2piTime);
    FillHisto("hEofHit_MUV2", MUV2hit->GetEnergy());
    FillHisto("hHitEvsTimeDiff_MUV2", time - fK2piTime, MUV2hit->GetEnergy());
    if ((fabs(time - fK2piTime) < fCutTimeDiffMUV2Pi0) && (MUV2hit->GetEnergy() > fCutMinEnergyOfMUV2Hit)) countMUV2++;
  }
  if ((countMUV1 + countMUV2) < fCutMinNHitsMUV12) return;
  
  //LKr
  std::pair<int, int> clustersID = pi0.fClustersID;
  int clus1 = clustersID.first;
  int clus2 = clustersID.second;
  int countLKrInTime = 0;
  for (Int_t i = 0; i < LKrEvent->GetNCandidates(); i++) {
    if ((i == clus1) || (i == clus2)) continue;
    auto *LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
    Double_t time = LKrCand->GetTime();
    FillHisto("hTimeDiffPI0_LKr", time - fK2piTime);
    if (fabs(time - fK2piTime) < fCutTimeDiffLKr) {
      countLKrInTime++;
    }
  }
  FillHisto("hNInTimeLKr", countLKrInTime);
  if (countLKrInTime != 1) return;

  //LAV veto (with timing)
  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(fK2piTime);
  FillHisto("hLAVHasTimeMatching", pLAVMatching->LAVHasTimeMatching(LAVEvent));
  if (pLAVMatching->LAVHasTimeMatching(LAVEvent)) return;

  // IRC and SAC veto (with timing)
  SAVMatching* pSAVMatching = *(SAVMatching**)GetOutput("PhotonVetoHandler.SAVMatching");
  pSAVMatching->SetReferenceTime(fK2piTime);
  pSAVMatching->SetIRCTimeCuts(fTimeWindowIRC, fTimeWindowIRC); // half time window; default = 5ns
  pSAVMatching->SetSACTimeCuts(fTimeWindowSAC, fTimeWindowSAC); // half time window; default = 5ns
  Bool_t SAVmatched = pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent);
  FillHisto("hSAVHasTimeMatching", SAVmatched);
  if (SAVmatched) return;

  TLorentzVector Kaon;
  TVector3 KaonThreeMomentum;
  int gtkID = pi0.fGTKID;
  if(fUseGTK && (gtkID>=0)){ 
    if(gtkID<GTKEvent->GetNCandidates()){
      FillHisto("hNGTKCandidates", GTKEvent->GetNCandidates());
      TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(gtkID));
      KaonThreeMomentum = GTKCand->GetMomentum();  
    }else{
      cout << user_normal() << "WARNING: Requested GTK ID out of range (too large)!" << endl;
      FillHisto("hNGTKCandidates", -1);
    };
  } else {                      // take average kaon momentum
    KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  }
  Kaon.SetVectM(KaonThreeMomentum, MKCH);
  
  TLorentzVector Pion;
  Pion = Kaon - pi0.fMomentum;
  FillHisto("hPionM", Pion.M());
  FillHisto("hPionP", Pion.P());
  FillHisto("hPionM2vsVtxZ", pi0.fPosition.Z(), Pion.M2());
  FillHisto("hMissM2vsMomPi", Pion.P(), Pion.M2());
  FillHisto("hMissM2vsMomPi0", pi0.fMomentum.P(), Pion.M2());
  if (Pion.M() < fCutMinPionMass || Pion.M() > fCutMaxPionMass) return;
  FillHisto("hPionP_k2piSelected", Pion.P());

  fK2piPionFourMomentum = Pion;
  SetOutputState("K2piPionFourMomentum", kOValid);
  
  if (TriggerOK) FillHisto("hK2piEventsPerBurst", BurstID);
  
  // K2pi yields per trigger: control
  // Runs with variable or unknown control trigger downscaling cannot be processed: request DS>=0.
  if (controlTrig && fDownscalingCtrl>0) {
    FillHisto("hK2piEventsPerBurstControlTrigger", BurstID, 1.0*fDownscalingCtrl);
    if (!GetEventHeader()->GetEventQualityMask() ||
        (GetEventHeader()->GetRunID()<6278 &&
         GetEventHeader()->GetEventQualityMask() == (0x1<<kGigaTracker))) {
      FillHisto("hK2piEventsPerBurstControlTriggerQM0", BurstID, 1.0*fDownscalingCtrl);
    }
  }
  fEventSelected = true;
}

void K2piSelectionNoSpectrometer::EndOfJobUser() {
  if (fReadingData) SaveAllPlots();
}
