// ---------------------------------------------------------------
//
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2019-02-05
//  - Removed noSTRAW option (it is covered by new analyzer K2piSelectionNoSpectrometer)
// Updated by Michal Koval (michal.koval@cern.ch) 2018-02-12
//  - Update to the new Pi0Selection output and other improvements
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2017-10-17
//  - Added K2pi selection without using STRAW spectrometer
// Updated by Karim Massri (karim.massri@cern.ch) 2017-05-11
//  - Generalisation for NTracks>=1 (a good track in time with Pi0 is considered)
// Updated by Karim Massri (karim.massri@cern.ch) 2016-11-15
//  - Pi0Selection changed and moved to dedicated analyser
// Updated by Artur Shaikhiev (shaykhiev@inr.ru) 2016-10-26
//  - added Pi0 selection
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-08-08
//
// ---------------------------------------------------------------

/// \class K2piSelection
/// \Brief
/// K2pi decay selection
/// \EndBrief
/// \Detailed
/// The analyzer uses Pi0Selection analyzer as an input to the selection.
/// Exactly one reconstructed pi0 is required to be present in the event.
/// K2pi decay is recognized as pi+ track in STRAW and two
/// photons (pi0) in LKr. Outputs the basic monitoring plots: missing mass,
/// momentum, vertex position, etc.. The use of GTK candidate momentum is controlled
/// by parameter UseGTK (false by default).
/// The analyzer has the following outputs:
/// EventSelected, K2piTime, K2piTrackID, K2piPionFourMomentum, K2piVertexPosition.
/// The L0 trigger mask to be used can be passed from the command line (it is 0x1FF by default).
/// The bit corresponding to the mask 0x100 is for the control trigger,
/// bits 0xFF are for the physics trigger.
/// For example, to select events with L0 trigger bit 5 up, one can call
/// \code
/// ./MyApplication ... -p "K2piSelection:TriggerMask=0x20"
/// \endcode
/// or equivalently, using decimal notation,
/// \code
/// ./MyApplication ... -p "K2piSelection:TriggerMask=32"
/// \endcode
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include "K2piSelection.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

K2piSelection::K2piSelection(Core::BaseAnalysis *ba) : Analyzer(ba, "K2piSelection") {
  RequestTree("LKr",    new TRecoLKrEvent,    "Reco");
  RequestTree("LAV",    new TRecoLAVEvent,    "Reco");
  RequestTree("IRC",    new TRecoIRCEvent,    "Reco");
  RequestTree("SAC",    new TRecoSACEvent,    "Reco");
  RequestTree("Cedar",  new TRecoCedarEvent,  "Reco");
  RequestTree("GigaTracker",  new TRecoGigaTrackerEvent, "Reco");

  RequestL0Data();

  fTriggerConditions = TriggerConditions::GetInstance();
  fReadingData = kTRUE;

  // Parameters
  AddParam("TriggerMask"                      , &fTriggerMask          , 0x1FF);
  AddParam("MaxNBursts"                       , &fMaxNBursts           , 5000);
  AddParam("UseGTK"               , "bool"  , &fUseGTK               , false);
  AddParam("TimeWindowIRC"        , "double", &fTimeWindowIRC        , 10.);
  AddParam("TimeWindowSAC"        , "double", &fTimeWindowSAC        , 10.);
  AddParam("CutMinPionMass"       , "double", &fCutMinPionMass       , 120.);
  AddParam("CutMaxPionMass"       , "double", &fCutMaxPionMass       , 160.);

  // Histograms
  fHPhysicsEventsPerBurst = nullptr;
  fHK2piEventsPerBurst = nullptr;
  fHK2piEventsPerBurstControlTrigger = nullptr;
  fHK2piEventsPerBurstControlTriggerQM0 = nullptr;
  fHMass = nullptr;
  fHEOP = nullptr;
  fHZvertex = nullptr;

  //////////////////////////////////////////////////////////
  // Initialize parameters for kaon flux and POT computation

  fZFiducialMin     =  105000; // [mm]
  fZFiducialMax     =  180000; // [mm]
  fKaonMeanPath     =  563857; // [mm] at 75 GeV/c
  fPionMeanPath     = 4193851; // [mm] at 75 GeV/c
  fDecayProb        = 1.0 - exp((fZFiducialMin-fZFiducialMax)/fKaonMeanPath); // ~0.125
  fAcceptance       = 0.1430;  // Acceptance in Z range (105-180)m, evaluated with MC v1.0.1
  fBRK2pi           = 0.2067;  // PDG 2018
  fDownscalingCtrl  = 0;
  fStartOfRunTime   = 0;

  // Conversion of protons-on-target to number of kaons entering the decay volume.
  // Source: the NA62 detector paper [JINST 12 (2017) P05025, arXiv:1703.08501], Table 2.
  fPOT_to_Kaon = 1.1e12 / 45.0e6;

  // Outputs
  fEventSelected = false;
  fK2piTime = 0.0;
  fK2piTrackID = -1;
  fK2piPionFourMomentum = TLorentzVector();
  fK2piKaonFourMomentum = TLorentzVector();
  fK2piVertexPosition = TVector3();
  fPi0SelectionOutput.fMomentum = TLorentzVector();
  fPi0SelectionOutput.fTime = 0.0 ;
  fPi0SelectionOutput.fPosition = TVector3();
  fPi0SelectionOutput.fGTKID = -1;
  fPi0SelectionOutput.fGammaMomenta.first = TLorentzVector();
  fPi0SelectionOutput.fGammaMomenta.second = TLorentzVector();
  fPi0SelectionOutput.fClustersID.first = -1;
  fPi0SelectionOutput.fClustersID.second = -1;
}

K2piSelection::~K2piSelection() {}

void K2piSelection::StartOfRunUser() {

  // DS of the control trigger:
  // -999 means downscaling is not found the database;
  // -1 means downscaling is variable for this run
  fDownscalingCtrl = fTriggerConditions->GetControlTriggerDownscaling(GetRunID());

  fStartOfRunTime = GetBurstTime();
}

void K2piSelection::InitHist() {
  fReadingData = GetIsTree();

  if (fReadingData) {
    BookHisto("hNEventsAfterPi0Selection", new TH1I("NEventsAfterPi0Selection", "N Events After Pi0Selection", 2, 0, 2));
    BookHisto("hNLKrCandidates", new TH1I("NLKrCandidates", "N LKr Candidates", 30, 0, 30));
    BookHisto("hNTracks", new TH1F("NTracks", "Number of tracks", 11, -0.5, 10.5));
    BookHisto("hNGoodTracks", new TH1F("NGoodTracks", "Number of good tracks", 11, -0.5, 10.5));
    BookHisto("hEOP", new TH1F("EOP", "Track E/p; E/p", 150, 0.0, 1.5));
    BookHisto("hZvtx", new TH1F("Zvtx", "Z of track-beam axis vertex;Vertex z [m]", 200, 50, 250));
    BookHisto("hCDA", new TH1F("CDA", "CDA of the track-beam axis vertex;CDA [mm]", 200, 0, 200));
    BookHisto("hZvtxCDA", new TH2F("ZvtxCDA", "CDA vs Z of the track-beam axis vertex;Vertex z [m];CDA [mm]", 75, 50, 200, 100, 0, 200));
    BookHisto("hMMiss2Pi", new TH1F("MMiss2Pi", "Squared missing mass in pion hypothesis;M_{miss}^{2}(#pi) [GeV^{2}/c^{4}]", 300, -0.15, 0.15));
    BookHisto("hMMiss2Pi_2", new TH1F("MMiss2Pi_2", "Squared missing mass in pion hypothesis;M_{miss}^{2}(#pi) - M_{#pi_{0}}^{2} [GeV^{2}/c^{4}]", 300, -0.15, 0.15));
    BookHisto("hPMMiss2Pi", new TH2F("PMMiss2Pi", "Squared missing mass in pion hypothesis vs momentum; Track momentum [GeV/c];M_{miss}^{2}(#pi) [GeV^{2}/c^{4}]", 160, 0, 80, 100, -0.15, 0.15));
    BookHisto("hPTheta", new TH2F("PTheta", "Track opening angle wrt beam axis vs momentum;Track momentum [GeV/c];#theta", 160, 0, 80, 100, 0.0, 0.02));
    BookHisto("hKaonMass", new TH1F("hKaonMass","Reconstructed kaon mass in K2pi hypothesis [MeV]; M_{K} [MeV/c^{2}]", 200, 200, 600));
    BookHisto("MPI0MK", new TH2F("MPI0MK","Pi0 mass vs Kaon mass; M_{K} [MeV/c^{2}];M_{#pi0} [MeV/c^{2}]", 200, 200, 600, 200, 0, 400));
    BookHisto("hDiffEnergy", new TH1F("hDiffEnergy", "Total energy difference (initial - final) in K2pi hypothesis [GeV]; #DeltaE [GeV];Entries/0.25GeV", 160, -20, 20));
    BookHisto("hK2piTime", new TH1F("K2piTime","K2pi event time;K2pi time [ns]", 500, -50., 50.));
    BookHisto("hLAVHasTimeMatching", new TH1I("LAVHasTimeMatching", "LAV Time Matching", 2, 0, 2));
    BookHisto("hSAVHasTimeMatching", new TH1I("SAVHasTimeMatching", "SAV Time Matching", 2, 0, 2));
    BookHisto("hPhysicsEventsPerBurst", new TH1F("PhysicsEventsPerBurst", "Physics events per burst;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hK2piEventsPerBurst", new TH1F("K2piEventsPerBurst", "K2pi candidates per burst;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hK2piEventsPerBurstControlTrigger", new TH1F("K2piEventsPerBurstControlTrigger", "K2pi candidates per burst (control trigger)*DS;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hK2piEventsPerBurstControlTriggerQM0", new TH1F("K2piEventsPerBurstControlTriggerQM0", "K2pi candidates per burst (control trigger, quality mask = 0)*DS;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));

    // Histograms of MC true quantities
    BookHisto("mctrue/hZvtx",       new TH1F("Zvertex_true", "True Zvertex; z [m]", 300, 0, 300));

    ////////////////////////////
    // Set up the online monitor
    CreateCanvas("K2piCanvas");
    PlacePlotOnCanvas("hNTracks",      "K2piCanvas");
    PlacePlotOnCanvas("hEOP",          "K2piCanvas");
    PlacePlotOnCanvas("hZvtx",         "K2piCanvas");
    PlacePlotOnCanvas("hCDA",          "K2piCanvas");
    PlacePlotOnCanvas("hMMiss2Pi",     "K2piCanvas");
    SetUpdateInterval(50000);

  } else {
    cout << user_normal() << "Reading my own output" << endl;
    fHPhysicsEventsPerBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PhysicsEventsPerBurst", true));
    fHK2piEventsPerBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "K2piEventsPerBurst", true));
    fHK2piEventsPerBurstControlTrigger =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "K2piEventsPerBurstControlTrigger", true));
    fHK2piEventsPerBurstControlTriggerQM0 =
      static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "K2piEventsPerBurstControlTriggerQM0", true));
    fHMass = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MMiss2Pi", true));
    fHEOP = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "EOP", true));
    fHZvertex = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "Zvtx", true));
  }
}

void K2piSelection::InitOutput() {
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("K2piTime",      &fK2piTime);
  RegisterOutput("K2piTrackID",   &fK2piTrackID);
  RegisterOutput("K2piPionFourMomentum", &fK2piPionFourMomentum);
  RegisterOutput("K2piKaonFourMomentum", &fK2piKaonFourMomentum);
  RegisterOutput("K2piVertexPosition",   &fK2piVertexPosition);
  RegisterOutput("K2piPi0SelectionOutput",   &fPi0SelectionOutput);
}

void K2piSelection::Process(Int_t) {
  SetOutputState("EventSelected", kOValid);
  SetOutputState("K2piTime",      kOInvalid);
  SetOutputState("K2piTrackID",   kOInvalid);
  SetOutputState("K2piPionFourMomentum", kOInvalid);
  SetOutputState("K2piVertexPosition", kOInvalid);
  SetOutputState("K2piPi0SelectionOutput", kOInvalid);
  fEventSelected = false;
  fK2piTime = 0.;
  fK2piTrackID = -1;
  fK2piPionFourMomentum.SetXYZM(0., 0., 0., 0.);
  fK2piKaonFourMomentum.SetXYZM(0., 0., 0., 0.);
  fK2piVertexPosition.SetXYZ(0., 0., 0.);
  fPi0SelectionOutput.fMomentum.SetXYZM(0., 0., 0., 0.);
  fPi0SelectionOutput.fTime = 0.0 ;
  fPi0SelectionOutput.fPosition.SetXYZ(0., 0., 0.);
  fPi0SelectionOutput.fGTKID = -1;
  fPi0SelectionOutput.fGammaMomenta.first.SetXYZM(0., 0., 0., 0.);
  fPi0SelectionOutput.fGammaMomenta.second.SetXYZM(0., 0., 0., 0.);
  fPi0SelectionOutput.fClustersID.first= -1;
  fPi0SelectionOutput.fClustersID.second = -1;

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  Bool_t physicsTrig = fTriggerConditions->IsPhysicsTrigger(GetL0Data());
  Bool_t controlTrig = fTriggerConditions->IsControlTrigger(GetL0Data());
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t TriggerOK =
    (physicsTrig && (L0TriggerWord & fTriggerMask)) || (controlTrig && (0x100 & fTriggerMask));

  Int_t BurstID   = GetBurstID();

  if (TriggerOK) FillHisto("hPhysicsEventsPerBurst", BurstID);

  //////////////////////////////////////////////////////
  // True beam properties at the GTK3 plane (z=102400mm)

  if (GetWithMC()) {
    Event *evt = GetMCEvent();
    if (evt->GetNKineParts()) {
      FillHisto("mctrue/hZvtx", 0.001*evt->GetKinePart(0)->GetEndPos().Z()); // [m]
    }
  }

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoLAVEvent* LAVEvent = GetEvent<TRecoLAVEvent>();
  TRecoIRCEvent* IRCEvent = GetEvent<TRecoIRCEvent>();
  TRecoSACEvent* SACEvent = GetEvent<TRecoSACEvent>();
  TRecoGigaTrackerEvent* GTKEvent  = GetEvent<TRecoGigaTrackerEvent>();

  // read outputs
  auto Tracks = *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");
  auto Clusters = *GetOutput<vector<EnergyCluster>>("EnergyClusterBuilder.Output");
  auto pi0Selected = *GetOutput<std::vector<Pi0SelectionOutput>>("Pi0Selection.SelectedPi0");

  FillHisto("hNEventsAfterPi0Selection", (pi0Selected.size() == 1));

  if (TriggerOK) {
      FillHisto("hNLKrCandidates", LKrEvent->GetNCandidates());
      FillHisto("hNTracks", Tracks.size());
  }
  // Require at least 1 track and 3 LKr clusters: pi+ and two photons
  if (LKrEvent->GetNCandidates() < 3) return;
  if (Tracks.size() == 0) return;

  Int_t goodTrackID = -1;
  Int_t NGoodTracks = 0;
  for (UInt_t iTrack = 0; iTrack < Tracks.size(); iTrack++){
    Double_t ttime = -999.;   // track time
    if (Tracks[iTrack].CHODTimeExists()) {
      ttime = Tracks[iTrack].GetCHODTime();
    } else if (Tracks[iTrack].NewCHODTimeExists()) {
      ttime = Tracks[iTrack].GetNewCHODTime();
    } else {
      ttime = Tracks[iTrack].GetSpectrometerCandidate()->GetTime();
    }
    // check for closest pi0
    Double_t MinPi0TrackTime = 99999.;
    for(Int_t iPi0=0;iPi0<(Int_t)pi0Selected.size();iPi0++){
      if(fabs(ttime-pi0Selected[iPi0].fTime)<MinPi0TrackTime){
        MinPi0TrackTime = fabs(ttime-pi0Selected[iPi0].fTime);
      }
    }
    if (MinPi0TrackTime > 2.) continue; // skip tracks not matched to pi0 time
    Int_t    Q            = Tracks[iTrack].GetCharge();
    Double_t PtrackA      = Tracks[iTrack].GetMomentum(); // spectrometer calibration included
    Double_t Ptrackbefore = Tracks[iTrack].GetMomentumBeforeFit();
    Double_t cda          = Tracks[iTrack].GetBeamAxisCDA();
    Double_t Zvtx         = Tracks[iTrack].GetBeamAxisVertex().Z();
    if (Q != 1) continue;
    if (Tracks[iTrack].GetIsFake()) continue;
    if (fabs(PtrackA - Ptrackbefore) > 20000.0) continue; // 20 GeV
    if (PtrackA < 5000 || PtrackA > 70000) continue;
    if (Zvtx < fZFiducialMin || Zvtx > fZFiducialMax) continue;
    if (cda > 30.) continue;
    auto geomAcc = GeometricAcceptance::GetInstance();
    if (!geomAcc->InAcceptance(&Tracks[iTrack], kNewCHOD))continue;
    if (!geomAcc->InAcceptance(&Tracks[iTrack], kSpectrometer, 0)) continue;
    if (!geomAcc->InAcceptance(&Tracks[iTrack], kSpectrometer, 1)) continue;
    if (!geomAcc->InAcceptance(&Tracks[iTrack], kSpectrometer, 2)) continue;
    if (!geomAcc->InAcceptance(&Tracks[iTrack], kSpectrometer, 3)) continue;
    if (!geomAcc->InAcceptance(&Tracks[iTrack], kLKr)) continue;
    goodTrackID = iTrack;
    NGoodTracks++;
  }
  if (TriggerOK) FillHisto("hNGoodTracks", NGoodTracks);
  if (NGoodTracks != 1) return;
  fK2piTrackID = goodTrackID;
  SetOutputState("K2piTrackID", kOValid);

  // check for closest pi0
  Double_t trackTime = -999.;   // track time
  if (Tracks[fK2piTrackID].CHODTimeExists()) {
    trackTime = Tracks[fK2piTrackID].GetCHODTime();
  } else if (Tracks[fK2piTrackID].NewCHODTimeExists()) {
    trackTime = Tracks[fK2piTrackID].GetNewCHODTime();
  } else {
    trackTime = Tracks[fK2piTrackID].GetSpectrometerCandidate()->GetTime();
  }
  Int_t GoodPi0ID = -1;
  Double_t MinPi0TrackTime = 99999.;
  for(Int_t iPi0=0;iPi0<(Int_t)pi0Selected.size();iPi0++){
    if(fabs(trackTime-pi0Selected[iPi0].fTime)<MinPi0TrackTime){
      MinPi0TrackTime = fabs(trackTime-pi0Selected[iPi0].fTime);
      GoodPi0ID = iPi0;
    }
  }
  Pi0SelectionOutput pi0 = pi0Selected.at(GoodPi0ID);

  fK2piTime = pi0.fTime;
  SetOutputState("K2piTime", kOValid);
  FillHisto("hK2piTime", fK2piTime);

  // Zvertex & CDA: track wrt the beam axis
  Double_t cda    = Tracks[goodTrackID].GetBeamAxisCDA();
  Double_t Zvtx   = Tracks[goodTrackID].GetBeamAxisVertex().Z();
  fK2piVertexPosition = Tracks[goodTrackID].GetBeamAxisVertex();
  SetOutputState("K2piVertexPosition", kOValid);

  if (TriggerOK) {
    FillHisto("hZvtx", 0.001*Zvtx);
    FillHisto("hCDA", cda);
    FillHisto("hZvtxCDA", 0.001*Zvtx, cda);
  }
  // MUV3 veto: no track association
  if (Tracks[goodTrackID].MUV3AssociationExists()) return;

  // LKr selection
  if (!Tracks[goodTrackID].LKrAssociationExists()) return;
  // Pion ID with E/p
  Double_t eop = Tracks[goodTrackID].GetLKrEoP();
  if (TriggerOK) FillHisto("hEOP", eop);
  // if (eop < 0.05 || eop > 0.8) return;
  if (eop > 0.9) return;

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
  if (fUseGTK && (pi0.fGTKID >= 0)) {
    auto GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(pi0.fGTKID));
    KaonThreeMomentum = GTKCand->GetMomentum();
  } else {                      // take average kaon momentum
    KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  }
  Kaon.SetVectM(KaonThreeMomentum, MKCH);

  TLorentzVector Pion;
  Pion.SetVectM(Tracks[goodTrackID].GetMomentumBeforeMagnet(), MPI);
  Double_t Mmiss2Pi = (Kaon - Pion).M2();
  Double_t Theta    = Kaon.Angle(Tracks[goodTrackID].GetMomentumBeforeMagnet());

  Double_t kaon_mass = sqrt((pi0.fMomentum + Pion).M2());
  Double_t diff_energy = Kaon.E() - (pi0.fMomentum + Pion).E();

  auto c1 = Clusters.at(pi0.fClustersID.first);
  auto c2 = Clusters.at(pi0.fClustersID.second);
  Double_t pi0_mass = Pi0Selection::ComputeDiPhotonMass(c1, c2, fK2piVertexPosition);

  Double_t pi0mm2 = (MPI0/1000.)*(MPI0/1000.) ;
  Double_t mm2 = (Mmiss2Pi*1e-6) - pi0mm2 ;

  if (TriggerOK) {
    FillHisto("hKaonMass", kaon_mass); // [MeV]
    FillHisto("MPI0MK", kaon_mass, pi0_mass);
  }

  if (kaon_mass < 460. || kaon_mass > 520.) return;

  Double_t Ptrack = Tracks[goodTrackID].GetMomentum(); // spectrometer calibration included
  if (TriggerOK) {
    FillHisto("hDiffEnergy", 0.001*diff_energy); // [GeV]
    FillHisto("hMMiss2Pi",  Mmiss2Pi*1e-6); // [GeV^2]
    FillHisto("hMMiss2Pi_2",  mm2); // [GeV^2]
    FillHisto("hPMMiss2Pi", 0.001*Ptrack, Mmiss2Pi*1e-6);
    FillHisto("hPTheta",    0.001*Ptrack, Theta);
  }
  if (fabs(mm2) > 0.015) return;

  fK2piPionFourMomentum = Pion;
  SetOutputState("K2piPionFourMomentum", kOValid);
  fK2piKaonFourMomentum = Kaon;
  SetOutputState("K2piKaonFourMomentum", kOValid);

  if (TriggerOK) FillHisto("hK2piEventsPerBurst", BurstID);

  // K2pi yields per trigger: control
  // Runs with variable or unknown control trigger downscaling cannot be processed: request DS>=0.
  if (controlTrig && fDownscalingCtrl>0) {
    FillHisto("hK2piEventsPerBurstControlTrigger", BurstID, 1.0*fDownscalingCtrl);
    if (!GetEventHeader()->GetEventQualityMask() ||
        (GetEventHeader()->GetRunID()<6278 &&
         GetEventHeader()->GetEventQualityMask()==(0x1<<kGigaTracker))) {
      FillHisto("hK2piEventsPerBurstControlTriggerQM0", BurstID, 1.0*fDownscalingCtrl);
    }
  }
  fEventSelected = true;

  fPi0SelectionOutput = pi0;
  SetOutputState("K2piPi0SelectionOutput", kOValid);
}

void K2piSelection::EndOfJobUser() {
  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }
  if (!fHPhysicsEventsPerBurst) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
    return;
  }

  // K flux with control trigger
  Double_t CTL_Ndec = fHK2piEventsPerBurstControlTrigger->Integral() / fBRK2pi / fAcceptance;
  Double_t CTL_NK   = CTL_Ndec / fDecayProb;
  Double_t CTL_POT  = CTL_NK * fPOT_to_Kaon;

  // K flux with control trigger, quality mask = 0
  Double_t CTL_QM0_Ndec = fHK2piEventsPerBurstControlTriggerQM0->Integral() / fBRK2pi / fAcceptance;
  Double_t CTL_QM0_NK   = CTL_QM0_Ndec / fDecayProb;
  Double_t CTL_QM0_POT  = CTL_QM0_NK * fPOT_to_Kaon;

  cout << user_normal() << "N(CTL_decays_in_FV) NK= " << CTL_Ndec <<
    " N0= " << CTL_NK << " POT= " << CTL_POT << endl;
  cout << user_normal() << "N(CTL_decays_in_FV_QualityMaskOK) NK= " << CTL_QM0_Ndec <<
    " N0= " << CTL_QM0_NK << " POT= " << CTL_QM0_POT << endl;

  // Write all the info in a .dat file
  ofstream K2piInfoFile;
  TString K2piInfoFileName = "K2piInfo.AllBursts.dat";
  if(!Configuration::ConfigSettings::CLI::fNoSkipBadBurst) K2piInfoFileName = "K2piInfo.NoBadBursts.dat";  // skipping bad bursts
  K2piInfoFile.open(K2piInfoFileName);
  K2piInfoFile << "# Format:" << endl;
  K2piInfoFile << "# Line 1: CTL_QM0  RunID StartOfRunTime NK2pi(CTL_QM0) NK(CTL_decays_in_FV_QM0) N0(CTL_decays_in_FV_QM0) POT(CTL_decays_in_FV_QM0)" << endl;
  K2piInfoFile << "# Line 2: CTL_all  RunID StartOfRunTime NK2pi(CTL_all) NK(CTL_decays_in_FV_all) N0(CTL_decays_in_FV_all) POT(CTL_decays_in_FV_all)" << endl;
  K2piInfoFile << Form("CTL_QM0  %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHK2piEventsPerBurstControlTriggerQM0->Integral(),CTL_QM0_Ndec,CTL_QM0_NK,CTL_QM0_POT) << endl;
  K2piInfoFile << Form("CTL_all  %06d %d %.5e %.5e %.5e %.5e",GetRunID(),fStartOfRunTime,fHK2piEventsPerBurstControlTrigger->Integral(),CTL_Ndec,CTL_NK,CTL_POT) << endl;
  K2piInfoFile.close();

  // Produce the PDF report
  BuildPDFReport();
}

void K2piSelection::BuildPDFReport() {

  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(11);

  TCanvas *Canvas0 = new TCanvas("K2piCanvas0");
  Canvas0->Print(Form(OutputPDFFileName + "["), "pdf"); // open file

  Canvas0->Divide(2,2);
  for (Int_t i=1; i<=4; i++) {
    Canvas0->GetPad(i)->SetLeftMargin(0.04);
    Canvas0->GetPad(i)->SetRightMargin(0.01);
    Canvas0->GetPad(i)->SetTopMargin(0.06);
    Canvas0->GetPad(i)->SetBottomMargin(0.10);
  }

  fHMass->SetLineColor(kBlue);
  fHMass->SetFillColor(kYellow);
  fHEOP->SetLineColor(kBlue);
  fHEOP->SetFillColor(kYellow);
  fHZvertex->SetLineColor(kBlue);
  fHZvertex->SetFillColor(kYellow);

  Canvas0->cd(1);
  fHZvertex->Draw();
  Canvas0->cd(2); gPad->SetLogy();
  fHEOP->Draw();
  Canvas0->cd(3);
  fHMass->Draw();
  Canvas0->Print(OutputPDFFileName, "pdf");

  TCanvas *Canvas1 = new TCanvas("K2piCanvas1");
  Canvas1->Divide(2,2);
  for (Int_t i=1; i<=4; i++) {
    Canvas1->GetPad(i)->SetLeftMargin(0.04);
    Canvas1->GetPad(i)->SetRightMargin(0.01);
    Canvas1->GetPad(i)->SetTopMargin(0.06);
    Canvas1->GetPad(i)->SetBottomMargin(0.10);
  }

  fHZvertex->SetFillColor(kYellow);
  Canvas1->cd(1);
  fHZvertex->Draw();
  Canvas1->Print(OutputPDFFileName, "pdf");
  Canvas1->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  gErrorIgnoreLevel = -1; // restore the default

  delete Canvas0;
  delete Canvas1;
  // PrintStatisticsPerBurst();
}

void K2piSelection::PrintStatisticsPerBurst() {
  for (Int_t i=1; i<=fHPhysicsEventsPerBurst->GetNbinsX(); i++) {
    Double_t N = fHPhysicsEventsPerBurst->GetBinContent(i);
    if (!N) continue;
    Double_t n = fHK2piEventsPerBurst->GetBinContent(i);
    Double_t e = n/N;
    Double_t de = sqrt(e*(1.0-e)/N);
    cout << user_standard() << "@@K2pi "<<i-1<<" "<<n <<" "<<N<<" "<<e<<" "<<de<<endl;
  }
}
