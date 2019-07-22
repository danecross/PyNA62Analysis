// ---------------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-01-25
// Updated by Nicolas Lurkin (nicolas.lurkin@cern.ch) 2018-11-27
//
// ---------------------------------------------------------------

/// \class Ke3Selection
/// \Brief
/// Ke3 decay selection without PID
/// \EndBrief
/// \Detailed
/// The Ke3 decay selection: e+ in STRAW and two photons in LKr.
/// NB: no E/p cut is used: the selection relies on the kinematic criteria only.
/// Outputs the basic monitoring plots: missing mass, momentum, vertex position, etc.
/// Only "physics" and control L0 data type is considered when plotting.
/// The L0 trigger mask to be used can be passed from the command line (it is 0xFF by default).
/// For example, to select events with the L0 trigger bit 5 up, one can call
/// \code
/// ./MyApplication ... -p "Ke3Selection:TriggerMask=0x20"
/// \endcode
/// or equivalently, using the decimal notation,
/// \code
/// ./MyApplication ... -p "Ke3Selection:TriggerMask=32"
/// \endcode
/// The positron identification study (with LKr and RICH) involves additional selection criteria
/// to achieve higher purity. The loss of acceptance is 17%,
/// while the K2pi contamination drops from about 0.8% to 0.12%.
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TCanvas.h>
#include <TLegend.h>
#include <TStyle.h>
#include "SpectrometerTrackVertex.hh"
#include "Ke3Selection.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "DownstreamTrack.hh"
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "Pi0Selection.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

Ke3Selection::Ke3Selection(Core::BaseAnalysis *ba) : Analyzer(ba, "Ke3Selection") {
  RequestTree("LKr",        new TRecoLKrEvent,        "Reco");
  RequestTree("LAV",        new TRecoLAVEvent,        "Reco");
  RequestTree("IRC",        new TRecoIRCEvent,        "Reco");
  RequestTree("SAC",        new TRecoSACEvent,        "Reco");
  RequestTree("GigaTracker",new TRecoGigaTrackerEvent,"Reco");
  RequestL0Data();

  fReadingData = kTRUE;

  AddParam("TriggerMask", &fTriggerMask, 0xFF);
  AddParam("MaxNBursts",  &fMaxNBursts,  5000); // max number of bins in histograms
  AddParam("GTKEnabled",  &fGTKEnabled,  false); // Do we enable additional selection with GTK

  fHPhysicsEventsPerBurst = nullptr;
  fHKe3EventsPerBurst = nullptr;
  fHMass = nullptr;
  fHEOP = nullptr;
  fHZvertex = nullptr;
  fHPtot = nullptr;
  fHPttot = nullptr;
  fHEOP_Selected = nullptr;
  fEventSelected = false;
  fKe3Time = 0.0;
  fKe3TrackID = -1;
}

Ke3Selection::~Ke3Selection() {}

void Ke3Selection::InitHist() {
  fReadingData = GetIsTree();

  if (fReadingData) {

    // LKr-related quantities
    BookHisto("LKr/hNLKrCells", new TH1F("NLKrCells", "Number of LKr cells with signal;Number of cells", 125, -0.5, 249.5));
    BookHisto("LKr/hNLKrClusters", new TH1F("NLKrClusters", "Number of LKr clusters;Number of clusters", 10, -0.5, 9.5));
    BookHisto("LKr/hLKrNAssociatedClusters", new TH1F("LKrNAssociatedClusters", "Number of LKr clusters associated to a track", 4, -0.5, 3.5));
    BookHisto("LKr/hLKrDDeadCell", new TH1F("LKrDDeadCell", "Track distance to nearest dead cell;Distance to deal cell [mm]", 200, 0, 4000));
    BookHisto("LKr/hLKrClusterEnergy", new TH1F("LKrClusterEnergy", "LKr cluster energy;Energy [GeV]", 200, 0, 100));
    BookHisto("LKr/hLKrClusterTime", new TH1F("LKrClusterTime", "LKr cluster time wrt trigger;Time [ns]", 200, -50, 50));
    BookHisto("LKr/hLKrCellTotalEnergy", new TH1F("LKrCellTotalEnergy", "LKr total cell (E>40MeV) energy;Total cell energy [GeV]", 70, 0, 70));
    BookHisto("LKr/hLKrCellClusterTotalEnergy", new TH2F
	      ("LKrCellClusterTotalEnergy", "LKr total cluster energy vs cell energy;Total cell (>40MeV) energy [GeV];Total cluster energy [GeV]", 100, 0, 100, 100, 0, 100));

    // Positron E/p studies
    BookHisto("LKr/hLKrEoP", new TH1F("LKrEoP", "Positron E/p;Track E/p", 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum", new TH2F("LKrEoPVsMomentum", "Positron E/p vs momentum;Track momentum [GeV/c];Track E/p", 60, 0, 60, 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_RICH_pion", new TH2F("LKrEoPVsMomentum_RICH_pion", "Positron E/p vs momentum;Track momentum [GeV/c];Track E/p", 60, 0, 60, 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_RICH_electron", new TH2F("LKrEoPVsMomentum_RICH_electron", "Positron E/p vs momentum;Track momentum [GeV/c];Track E/p", 60, 0, 60, 150, 0.0, 1.5));
    BookHisto("LKr/hRichHypothesisVsLKrEoP", new TH2F("RichHypothesisVsLKrEoP", "Positron RICH hypothesis vs E/p;Track E/p;RICH hypothesis", 150, 0.0, 1.5, 6, -1.5, 4.5));
    BookHisto("LKr/hLKrEoP_DistDeadCell", new TH1F("LKrEoP_DistDeadCell", "Positron E/p;Positron E/p", 150, 0.0, 1.5));
    BookHisto("LKr/hLKrEoPVsMomentum_DistDeadCell", new TH2F
	      ("LKrEoPVsMomentum_DistDeadCell", "Positron E/p vs momentum [D(DeadCell)>20mm];Positron momentum [GeV/c];Positron E/p", 40, 0, 80, 150, 0.0, 1.5));

    BookHisto("hNTracks", new TH1F("hNTracks", "Number of tracks", 11, -0.5, 10.5));
    BookHisto("hNGoodTracks", new TH1F("NGoodTracks", "Number of good tracks", 11, -0.5, 10.5));
    BookHisto("hEOP",     new TH1F("hEOP", "Track E/p; E/p", 150, 0.0, 1.5));
    BookHisto("hZvtx",    new TH1F("hZvtx", "Z of track-beam axis vertex;Vertex z [m]", 200, 50, 250));
    BookHisto("hCDA",     new TH1F("hCDA", "CDA of the track-beam axis vertex;CDA [mm]", 200, 0, 200));
    BookHisto("hZvtxCDA", new TH2F("hZvtxCDA", "CDA vs Z of the track-beam axis vertex;Vertex z [m];CDA [mm]", 75, 50, 200, 100, 0, 200));
    BookHisto("hPtot",    new TH1F("hPtot", "Total momentum;P_{tot} [MeV/c]", 1000, 0., 100000.));
    BookHisto("hPttot",   new TH1F("hPttot","Total transverse momentum;P_{T}^{tot} [MeV/c]", 1000, 0., 1000.));
    BookHisto("hMMiss2_Ke3", new TH1F
	      ("hMMiss2_Ke3","Squared missing mass in Ke3 hypothesis;M_{miss}^{2}(K_{e3}) [GeV^{2}/c^{4}]", 600, -0.15, 0.15));
    BookHisto("hPMMiss2_Ke3", new TH2F("hPMMiss2_Ke3",
          "Squared missing mass in Ke3 hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(K_{e3}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 600, -0.15, 0.15));
    BookHisto("hMMiss2_K2pi", new TH1F("hMMiss2_K2pi", "Squared missing mass in K2pi hypothesis;M_{miss}^{2}(K_{2#pi}) [GeV^{2}/c^{4}]",
          600, -0.15, 0.15));
    BookHisto("hPMMiss2_K2pi", new TH2F("hPMMiss2_K2pi",
          "Squared missing mass in K2pi hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(K_{2#pi}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 600, -0.15, 0.15));
    BookHisto("hMMiss2_El", new TH1F("hMMiss2_El", "Squared missing mass in e^{+} hypothesis;M_{miss}^{2}(e^{+}) [GeV^{2}/c^{4}]",
          6000, -0.15, 0.15));
    BookHisto("hPMMiss2_El", new TH2F("hPMMiss2_El",
          "Squared missing mass in e^{+} hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(e^{+}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 6000, -0.15, 0.15));
    BookHisto("hMMiss2_Pi", new TH1F("hMMiss2_Pi", "Squared missing mass in #pi^{+} hypothesis;M_{miss}^{2}(#pi^{+}) [GeV^{2}/c^{4}]",
          6000, -0.15, 0.15));
    BookHisto("hPMMiss2_Pi", new TH2F("hPMMiss2_Pi",
          "Squared missing mass in #pi^{+} hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(#pi^{+}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 6000, -0.15, 0.15));
    BookHisto("hMMass2_Ke3", new TH1F("hMMass2_Ke3","Squared invariant mass in Ke3 hypothesis;M^{2}(#pi^{0} e^{+}) [GeV^{2}/c^{4}]",
          6000, -0.15, 0.35));
    BookHisto("hMMass2_K2pi", new TH1F("hMMass2_K2pi","Squared invariant mass in K2pi hypothesis;M^{2}(#pi^{0} #pi^{+}) [GeV^{2}/c^{4}]",
          6000, -0.15, 0.35));
    BookHisto("hPTheta", new TH2F("hPTheta", "Track opening angle wrt beam axis vs momentum;Track momentum [GeV/c];#theta",
          160, 0, 80, 100, 0.0, 0.02));
    BookHisto("hZvtx_Selected",    new TH1F("hZvtx_Selected", "Z of track-beam axis vertex;Vertex z [m]", 200, 50, 250));
    BookHisto("hCDA_Selected",     new TH1F("hCDA_Selected", "CDA of the track-beam axis vertex;CDA [mm]", 200, 0, 200));
    BookHisto("hZvtxCDA_Selected", new TH2F("hZvtxCDA_Selected", "CDA vs Z of the track-beam axis vertex;Vertex z [m];CDA [mm]", 75, 50, 200, 100, 0, 200));
    BookHisto("hEOP_Selected", new TH1F("hEOP_Selected", "Track E/p; E/p", 150, 0.0, 1.5));
    BookHisto("hMMiss2_Ke3_Selected", new TH1F("hMMiss2_Ke3_Selected",
          "Squared missing mass in Ke3 hypothesis;M_{miss}^{2}(K_{e3}) [GeV^{2}/c^{4}]", 600, -0.15, 0.15));
    BookHisto("hPMMiss2_Ke3_Selected", new TH2F("hPMMiss2_Ke3_Selected",
          "Squared missing mass in Ke3 hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(K_{e3}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 600, -0.15, 0.15));
    BookHisto("hMMiss2_K2pi_Selected", new TH1F("hMMiss2_K2pi_Selected",
          "Squared missing mass in K2pi hypothesis;M_{miss}^{2}(K_{2#pi}) [GeV^{2}/c^{4}]", 600, -0.15, 0.15));
    BookHisto("hPMMiss2_K2pi_Selected", new TH2F("hPMMiss2_K2pi_Selected",
          "Squared missing mass in K2pi hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(K_{2#pi}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 600, -0.15, 0.15));
    BookHisto("hMMiss2_El_Selected", new TH1F("hMMiss2_El_Selected",
          "Squared missing mass in e^{+} hypothesis;M_{miss}^{2}(e^{+}) [GeV^{2}/c^{4}]", 6000, -0.15, 0.15));
    BookHisto("hPMMiss2_El_Selected", new TH2F("hPMMiss2_El_Selected",
          "Squared missing mass in e^{+} hypothesis vs momentum;Track momentum [MeV/c];M_{miss}^{2}(e^{+}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 6000, -0.15, 0.15));
    BookHisto("hMMiss2_Pi_Selected", new TH1F("hMMiss2_Pi_Selected",
          "Squared missing mass in #pi^{+} hypothesis;M_{miss}^{2}(#pi^{+}) [GeV^{2}/c^{4}]", 6000, -0.15, 0.15));
    BookHisto("hPMMiss2_Pi_Selected", new TH2F("hPMMiss2_Pi_Selected",
          "Squared missing mass in #pi^{+} hypothesis vs momentum; Track momentum [MeV/c];M_{miss}^{2}(#pi^{+}) [GeV^{2}/c^{4}]",
          160, 0, 80000., 6000, -0.15, 0.15));
    BookHisto("hMMass2_Ke3_Selected", new TH1F
	      ("hMMass2_Ke3_Selected",
	       "Squared invariant mass in Ke3 hypothesis;M^{2}(#pi^{0} e^{+}) [GeV^{2}/c^{4}]", 6000, -0.15, 0.35));
    BookHisto("hMMass2_K2pi_Selected", new TH1F
	      ("hMMass2_K2pi_Selected","Squared invariant mass in K2pi hypothesis;M^{2}(#pi^{0} #pi^{+}) [GeV^{2}/c^{4}]",
	       6000, -0.15, 0.35));
    BookHisto("hPTheta_Selected", new TH2F
	      ("hPTheta_Selected", "Track opening angle wrt beam axis vs momentum;Track momentum [GeV/c];#theta",
	       160, 0, 80, 100, 0.0, 0.02));
    BookHisto("hPhysicsEventsPerBurst", new TH1F("hPhysicsEventsPerBurst", "Physics events per burst;Burst ID",
          fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hKe3EventsPerBurst", new TH1F("hKe3EventsPerBurst", "Ke3 candidates per burst;Burst ID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hKe3Time", new TH1F("hKe3Time","Ke3 event time;Ke3 time [ns]", 800, -200., 200.));

    ////////////////////////////
    // Set up the online monitor

    CreateCanvas("Ke3Canvas");
    PlacePlotOnCanvas("hNTracks",      "Ke3Canvas");
    PlacePlotOnCanvas("hEOP",          "Ke3Canvas");
    PlacePlotOnCanvas("hZvtx",         "Ke3Canvas");
    PlacePlotOnCanvas("hPttot",        "Ke3Canvas");
    PlacePlotOnCanvas("hCDA",          "Ke3Canvas");
    PlacePlotOnCanvas("hMMiss2_Ke3",   "Ke3Canvas");
    SetUpdateInterval(50000);
  }

  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHPhysicsEventsPerBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hPhysicsEventsPerBurst", true));
    fHKe3EventsPerBurst =     static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hKe3EventsPerBurst", true));
    fHMass =                  static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hMMiss2_Ke3", true));
    fHEOP =                   static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hEOP", true));
    fHZvertex =               static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hZvtx", true));
    fHPtot =                  static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hPtot", true));
    fHPttot =                 static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hPttot", true));
    fHEOP_Selected =          static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hEOP_Selected", true));
  }
}

void Ke3Selection::InitOutput() {
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("Ke3Time",       &fKe3Time);
  RegisterOutput("Ke3TrackID",    &fKe3TrackID);
}

void Ke3Selection::Process(Int_t) {

  SetOutputState("EventSelected", kOValid);
  SetOutputState("Ke3Time",       kOInvalid);
  SetOutputState("Ke3TrackID",    kOInvalid);
  fEventSelected = false;
  fKe3Time= 0.;
  fKe3TrackID= -1;

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData   = L0DataType & 0x1;
  Bool_t CTRLTrigger   = L0DataType & 0x10;
  Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&fTriggerMask)) || CTRLTrigger;

  Int_t BurstID   = GetBurstID();

  if (TriggerOK) FillHisto("hPhysicsEventsPerBurst", BurstID);

  TRecoLKrEvent*    LKrEvent    = GetEvent<TRecoLKrEvent>();;
  TRecoLAVEvent*    LAVEvent    = GetEvent<TRecoLAVEvent>();;
  TRecoIRCEvent*    IRCEvent    = GetEvent<TRecoIRCEvent>();;
  TRecoSACEvent*    SACEvent    = GetEvent<TRecoSACEvent>();;

  /////////////////////////////////////////////////
  // Require at least 3 clusters: e+ and two photons

  Int_t NLKrCand = LKrEvent->GetNCandidates();
  if (NLKrCand < 3) return;

  /////////////////////////////////////////////////
  // Require exactly one good track with a good pi0

  std::vector<DownstreamTrack> Tracks =
    *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");
  if (TriggerOK) FillHisto("hNTracks", Tracks.size());
  std::vector<Pi0SelectionOutput> pi0Selected =
    *GetOutput<std::vector<Pi0SelectionOutput>>("Pi0Selection.SelectedPi0");
  vector<EnergyCluster> clusters = *GetOutput<vector<EnergyCluster>>("EnergyClusterBuilder.Output");

  Int_t GoodTrackID = -1;
  Int_t NGoodTracks = 0;
  for(UInt_t iTrack=0; iTrack<Tracks.size();iTrack++){
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
    Double_t Ptrack       = Tracks[iTrack].GetMomentum(); // spectrometer calibration included
    Double_t Ptrackbefore = Tracks[iTrack].GetMomentumBeforeFit();
    Double_t Chi2track    = Tracks[iTrack].GetChi2();
    Double_t cda          = Tracks[iTrack].GetBeamAxisCDA();
    Double_t Zvtx         = Tracks[iTrack].GetBeamAxisVertex().Z();

    if (Q!=1) continue;
    if (Chi2track>20.0) continue;
    if (fabs(Ptrack-Ptrackbefore)>20000.0) continue; // 20 GeV
    if (Ptrack<3000 || Ptrack>70000) continue;
    if (Zvtx<110000 || Zvtx>180000) continue;
    if (cda>25) continue;

    GoodTrackID = iTrack;
    NGoodTracks++;
  }
  if (TriggerOK) FillHisto("hNGoodTracks", NGoodTracks);
  if(NGoodTracks!=1) return;

  fKe3TrackID = GoodTrackID;
  SetOutputState("Ke3TrackID", kOValid);
  TRecoSpectrometerCandidate* Scand = Tracks[GoodTrackID].GetSpectrometerCandidate();
  Double_t Ptrack       = Tracks[GoodTrackID].GetMomentum(); // spectrometer calibration included
  Double_t cda          = Tracks[GoodTrackID].GetBeamAxisCDA();
  Double_t Zvtx         = Tracks[GoodTrackID].GetBeamAxisVertex().Z();

  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD))         return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kLKr))             return;

  ///////////////////////////////////
  // MUV3 veto: no track association

  if (Tracks[GoodTrackID].MUV3AssociationExists()) return;

  ////////////////////////
  // LKr selection
  ////////////////////////

  if (!Tracks[GoodTrackID].LKrAssociationExists()) return;

  Double_t trackTime = -999.;   // track time
  if (Tracks[GoodTrackID].CHODTimeExists()) {
    trackTime = Tracks[GoodTrackID].GetCHODTime();
  } else if (Tracks[GoodTrackID].NewCHODTimeExists()) {
    trackTime = Tracks[GoodTrackID].GetNewCHODTime();
  } else {
    trackTime = Tracks[GoodTrackID].GetSpectrometerCandidate()->GetTime();
  }

  // check for closest pi0
  Int_t GoodPi0ID = -1;
  Double_t MinPi0TrackTime = 99999.;
  for(Int_t iPi0=0;iPi0<(Int_t)pi0Selected.size();iPi0++){
    if(fabs(trackTime-pi0Selected[iPi0].fTime)<MinPi0TrackTime){
      MinPi0TrackTime = fabs(trackTime-pi0Selected[iPi0].fTime);
      GoodPi0ID = iPi0;
    }
  }
  Pi0SelectionOutput pi0 = pi0Selected.at(GoodPi0ID);

  fKe3Time = 0.5*(trackTime+pi0.fTime);
  SetOutputState("Ke3Time", kOValid);
  FillHisto("hKe3Time", fKe3Time);

  TVector3 KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  TLorentzVector Kaon;
  Kaon.SetVectM(KaonThreeMomentum, MKCH);
  TLorentzVector Electron;
  Electron.SetVectM(Scand->GetThreeMomentumBeforeMagnet(), MEL);
  TLorentzVector Pion;
  Pion.SetVectM(Scand->GetThreeMomentumBeforeMagnet(), MPI);
  Double_t Theta = Kaon.Angle(Scand->GetThreeMomentumBeforeMagnet());

  /////////////////////////
  // LAV veto (with timing)

  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(fKe3Time);
  if (pLAVMatching->LAVHasTimeMatching(LAVEvent)) return;

  /////////////////////////////////
  // IRC and SAC veto (with timing)

  SAVMatching* pSAVMatching = *(SAVMatching**)GetOutput("PhotonVetoHandler.SAVMatching");
  pSAVMatching->SetReferenceTime(fKe3Time);
  Bool_t SAVmatched = pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent);
  if (SAVmatched) return;

  /////////////////////////////////

  Double_t eop         = Tracks[GoodTrackID].GetLKrEoP();
  Double_t Ptot        = (Electron+pi0.fMomentum).P();
  Double_t Pttot       = (Electron+pi0.fMomentum).Perp(Kaon.Vect());
  Double_t MMiss2_Ke3  = (Kaon-Electron-pi0.fMomentum).M2();
  Double_t MMiss2_K2pi = (Kaon-Pion-pi0.fMomentum).M2();
  Double_t MMiss2_El   = (Kaon-Electron).M2();
  Double_t MMiss2_Pi   = (Kaon-Pion).M2();
  Double_t MMass2_Ke3  = (Electron+pi0.fMomentum).M2();
  Double_t MMass2_K2pi = (Pion+pi0.fMomentum).M2();

  if (TriggerOK) {
    FillHisto("hZvtx", 0.001*Zvtx);
    FillHisto("hCDA", cda);
    FillHisto("hZvtxCDA", 0.001*Zvtx, cda);
    FillHisto("hEOP", eop);
    FillHisto("hPtot",         Ptot);        // [MeV]
    FillHisto("hPttot",        Pttot);       // [MeV]
    FillHisto("hMMiss2_Ke3",   1.e-6*MMiss2_Ke3);  // [GeV^2]
    FillHisto("hPMMiss2_Ke3",  Ptrack, 1.e-6*MMiss2_Ke3);
    FillHisto("hMMiss2_K2pi",  1.e-6*MMiss2_K2pi); // [GeV^2]
    FillHisto("hPMMiss2_K2pi", Ptrack, 1.e-6*MMiss2_K2pi);
    FillHisto("hMMiss2_El",    1.e-6*MMiss2_El);   // [GeV^2]
    FillHisto("hPMMiss2_El",   Ptrack, 1.e-6*MMiss2_El);
    FillHisto("hMMiss2_Pi",    1.e-6*MMiss2_Pi);   // [GeV^2]
    FillHisto("hPMMiss2_Pi",   Ptrack, 1.e-6*MMiss2_Pi);
    FillHisto("hMMass2_Ke3",   1.e-6*MMass2_Ke3);  // [GeV]
    FillHisto("hMMass2_K2pi",  1.e-6*MMass2_K2pi); // [GeV]
    FillHisto("hPTheta",       0.001*Ptrack, Theta);
  }

  if(Ptot>72000.) return; // [MeV]
  if(Pttot<40.)   return; // [MeV]

  if (TriggerOK) {
    FillHisto("hZvtx_Selected", 0.001*Zvtx);
    FillHisto("hCDA_Selected", cda);
    FillHisto("hZvtxCDA_Selected", 0.001*Zvtx, cda);
    FillHisto("hEOP_Selected", eop);
    FillHisto("hMMiss2_Ke3_Selected",   1.e-6*MMiss2_Ke3); // [GeV^2]
    FillHisto("hPMMiss2_Ke3_Selected",  Ptrack, 1.e-6*MMiss2_Ke3);
    FillHisto("hMMiss2_K2pi_Selected",  1.e-6*MMiss2_K2pi); // [GeV^2]
    FillHisto("hPMMiss2_K2pi_Selected", Ptrack, 1.e-6*MMiss2_K2pi);
    FillHisto("hMMiss2_El_Selected",    1.e-6*MMiss2_El); // [GeV^2]
    FillHisto("hPMMiss2_El_Selected",   Ptrack, 1.e-6*MMiss2_El);
    FillHisto("hMMiss2_Pi_Selected",    1.e-6*MMiss2_Pi); // [GeV^2]
    FillHisto("hPMMiss2_Pi_Selected",   Ptrack, 1.e-6*MMiss2_Pi);
    FillHisto("hMMass2_Ke3_Selected",   1.e-6*MMass2_Ke3);  // [GeV^2]
    FillHisto("hMMass2_K2pi_Selected",  1.e-6*MMass2_K2pi); // [GeV^2]
    FillHisto("hPTheta_Selected",       0.001*Ptrack, Theta);

    if(abs(MMiss2_Ke3)<10000.) { //0.01 GeV^2
      FillHisto("hKe3EventsPerBurst", BurstID);
    }
  }
  if (abs(MMiss2_Ke3)>10000.) return; //0.01 GeV^2
  fEventSelected = !fGTKEnabled;

  ///////////////////////////////////////
  // Start of the higher purity selection

  TRecoGigaTrackerEvent *gtkEvt = GetEvent<TRecoGigaTrackerEvent>();
  // ##### Find GTK best matching candidate
  TVector3 vtx, p_fit, pbeam_fit;
  int gtk_sel = -1;
  double chi2_min = 100;

  for (Int_t iGtk=0; iGtk<gtkEvt->GetNCandidates(); ++iGtk) {
    double dt, chi2;
    TRecoGigaTrackerCandidate* gtkCand = static_cast<TRecoGigaTrackerCandidate*>(gtkEvt->GetCandidate(iGtk));
    dt = gtkCand->GetTime() - Scand->GetTime();
    fVertexLSF.Reset();
    fVertexLSF.AddTrack(Scand);
    fVertexLSF.AddGTKTrack(gtkCand);
    fVertexLSF.FitVertex(true, 3);

    chi2 = fVertexLSF.GetChi2();
    vtx = fVertexLSF.GetVertexPosition();
    p_fit = fVertexLSF.GetTrackThreeMomentum(0);
    pbeam_fit = fVertexLSF.GetTrackThreeMomentum(1);

    if(fabs(dt)>15) continue; // Must be in time with the track
    if(chi2<chi2_min){        // Select the one with min chi2
      chi2_min = chi2;
      if(chi2<4)              // But chi2 must be good anyway
        gtk_sel = iGtk;
    }
  }
  if (gtk_sel==-1) return;

  TLorentzVector positron_fit;
  positron_fit.SetVectM(p_fit, NA62Constants::MEL);
  TLorentzVector k_gtk = positron_fit + pi0.fMomentum;
  Double_t Pttot_With_GTK = k_gtk.Vect().Perp(pbeam_fit);
  Double_t Mass_Ke3_With_GTK = k_gtk.M();

  if (Mass_Ke3_With_GTK>340 && Pttot_With_GTK<75) return;
  fEventSelected = true;

  Int_t nCells = LKrEvent->GetNHits();
  Int_t nClusters = LKrEvent->GetNCandidates();
  FillHisto("LKr/hNLKrCells", nCells);
  FillHisto("LKr/hNLKrClusters", nClusters);

  Double_t totalCellEnergy = 0.;
  for (Int_t i=0; i<nCells; i++) {
    TRecoLKrHit *hit = static_cast<TRecoLKrHit*>(LKrEvent->GetHit(i));
    Double_t energy = hit->GetEnergy();
    if (energy > 40.0) totalCellEnergy += energy;
  }
  FillHisto("LKr/hLKrCellTotalEnergy", 0.001*totalCellEnergy);
  FillHisto("LKr/hLKrCellClusterTotalEnergy", 0.001*totalCellEnergy, 0.001*LKrEvent->GetEnergyTotal());

  Double_t refTime = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data()) ?
    GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime() :
    GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
  refTime *= TdcCalib;
  for (Int_t i=0; i<nClusters; i++) {
    TRecoLKrCandidate* Lcand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
    FillHisto("LKr/hLKrClusterEnergy", 0.001 * Lcand->GetClusterEnergy());
    FillHisto("LKr/hLKrClusterTime", Lcand->GetTime() - refTime);
  }

  // Positron PID studies: E/p spectrum and RICH response.
  // RICH hypotheses:
  // kRICHHypothesisBackground 0
  // kRICHHypothesisElectron   1
  // kRICHHypothesisMuon       2
  // kRICHHypothesisPion       3
  // kRICHHypothesisKaon       4
  // kRICHHypothesisMultiple  99

  Double_t zlkr = GeometricAcceptance::GetInstance()->GetZLKr();
  Double_t xlkr = Tracks[GoodTrackID].xAtAfterMagnet(zlkr);
  Double_t ylkr = Tracks[GoodTrackID].yAtAfterMagnet(zlkr);
  FillHisto("LKr/hLKrNAssociatedClusters", Tracks[GoodTrackID].GetLKrNAssociatedClusters());
  FillHisto("LKr/hLKrDDeadCell", Tracks[GoodTrackID].GetLKrClusterDDeadCell());

  EnergyCluster clus1 = clusters[pi0.fClustersID.first];
  EnergyCluster clus2 = clusters[pi0.fClustersID.second];
  Double_t Dist01 = sqrt(pow(xlkr-clus1.GetClusterX(), 2) + pow(ylkr-clus1.GetClusterY(), 2));
  Double_t Dist02 = sqrt(pow(xlkr-clus2.GetClusterX(), 2) + pow(ylkr-clus2.GetClusterY(), 2));

  if (Dist01>300.0 && Dist02>300.0) {
    if (Tracks[GoodTrackID].GetChi2()<40.0 && !Tracks[GoodTrackID].MUV3AssociationExists()) {
      Double_t p = 0.001 * Tracks[GoodTrackID].GetMomentum(); // [GeV/c]
      Int_t hyp = Tracks[GoodTrackID].GetRICHMostLikelyHypothesis();
      if (hyp==kRICHHypothesisMultiple) hyp = -1; // redefine (99 --> -1) for plotting
      if (Tracks[GoodTrackID].GetLKrNAssociatedClusters()>1) eop = 1.495;
      FillHisto("LKr/hLKrEoP", eop);
      FillHisto("LKr/hLKrEoPVsMomentum", p, eop);
      if (hyp==kRICHHypothesisPion) {
        FillHisto("LKr/hLKrEoPVsMomentum_RICH_pion", p, eop);
      }
      else if (hyp==kRICHHypothesisElectron) {
        FillHisto("LKr/hLKrEoPVsMomentum_RICH_electron", p, eop);
      }
      FillHisto("LKr/hRichHypothesisVsLKrEoP", eop, hyp);
      if (Tracks[GoodTrackID].GetLKrClusterDDeadCell()>20.0) {
        FillHisto("LKr/hLKrEoP_DistDeadCell", eop);
        FillHisto("LKr/hLKrEoPVsMomentum_DistDeadCell", p, eop);
      }
    }
  }
}

void Ke3Selection::EndOfJobUser() {
  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }
  if (!fHPhysicsEventsPerBurst) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
    return;
  }
  BuildPDFReport();
}

void Ke3Selection::BuildPDFReport() {

  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(11);

  TCanvas *Canvas0 = new TCanvas("Ke3Canvas0");
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
  fHEOP_Selected->SetLineColor(kRed);
  fHEOP_Selected->SetFillColor(kOrange-3);
  fHZvertex->SetLineColor(kBlue);
  fHZvertex->SetFillColor(kYellow);

  Canvas0->cd(1);
  fHZvertex->Draw();
  Canvas0->cd(2); gPad->SetLogy();
  fHEOP->Draw();
  fHEOP_Selected->Draw("same");
  Canvas0->cd(3);
  fHMass->Draw();
  Canvas0->cd(4);
  fHKe3EventsPerBurst->Draw();
  Canvas0->Print(OutputPDFFileName, "pdf");

  Canvas0->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  gErrorIgnoreLevel = -1; // restore the default

  // PrintStatisticsPerBurst();
}

void Ke3Selection::PrintStatisticsPerBurst() {
  for (Int_t i=1; i<=fHPhysicsEventsPerBurst->GetNbinsX(); i++) {
    Double_t N = fHPhysicsEventsPerBurst->GetBinContent(i);
    if (!N) continue;
    Double_t n = fHKe3EventsPerBurst->GetBinContent(i);
    Double_t e = n/N;
    Double_t de = sqrt(e*(1.0-e)/N);
    cout << user_standard() << "@@Ke3 "<<i-1<<" "<<n <<" "<<N<<" "<<e<<" "<<de<<endl;
  }
}
