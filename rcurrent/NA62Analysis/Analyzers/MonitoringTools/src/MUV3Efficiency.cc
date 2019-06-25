// ---------------------------------------------------------------
//
// History:
//
// Created by Lubos Bician (lubos.bician@cern.ch) 2016-02-16
// Refurbished by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), 2016-12-15
// Modified by Michal Zamkovsky (michal.zamkovsky@cern.ch), 2018-05-12
// Updated by Lubos Bician (lubos.bician@cern.ch) 2018-09-11
// Updated by Lubos Bician (lubos.bician@cern.ch) 2018-09-26
//
// ---------------------------------------------------------------

/// \class MUV3Efficiency
/// \Brief
/// MUV3 efficiency evaluation using muon halo or Kmu2 muons
/// \EndBrief
/// \Detailed
/// The tool uses spectrometer information, CDA, LKr E/p, RICH likelihood and MUV1,2 energy cuts
/// to select sample of halo or Kmu2 muons.
/// It then calculates the tile-by-tile efficiency of the MUV3 subdetector
/// using the standard SpectrometerMUV3Association class.
/// The analyzer can be run in two modes:
/// 1) reading reconstructed data (without using the --histo command line option);
/// 2) in the HISTO mode (using the --histo command line option), it reads its own
/// output and produces final report in the form of a PDF file.
/// Using default parameters, the analyzer runs in "NOT strict selection mode".
/// It automatically decides whether to measure the efficiency on halo
/// or Kmu2 events on an event-by-event basis. Only control events are used in this mode.
/// User can change this behaviour using predefined parameters.
/// \author Lubos Bician (lubos.bician@cern.ch)
/// \EndDetailed

#include "TROOT.h"
#include "TF1.h"
#include <TLatex.h>
#include <TBox.h>
#include "MUV3Efficiency.hh"
#include "ConfigSettings.hh"
#include "TriggerConditions.hh"
#include "BeamParameters.hh"
#include "Event.hh"
#include "GeometricAcceptance.hh"
#include "NA62ConditionsService.hh"

#include "TRecoMUV3Event.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

MUV3Efficiency::MUV3Efficiency(Core::BaseAnalysis *ba) : Analyzer(ba, "MUV3Efficiency") {

  Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts
  RequestL0Data();
  RequestL1Data();
  RequestBeamSpecialTrigger();
  RequestL0SpecialTrigger();

  RequestTree("MUV3", new TRecoMUV3Event);

  fCHODZPos = GeometricAcceptance::GetInstance()->GetZCHODHPlane();
  fMUV1ZPos = GeometricAcceptance::GetInstance()->GetZMUV1();
  fMUV2ZPos = GeometricAcceptance::GetInstance()->GetZMUV2();
  fMUV3ZPos = GeometricAcceptance::GetInstance()->GetZMUV3();
  fMUV3Rmin = GeometricAcceptance::GetInstance()->GetMUV3Rmin();
  fMUV3Rmax = GeometricAcceptance::GetInstance()->GetMUV3Rmax();

  fOutPDFFileName = fAnalyzerName + ".pdf";
  AddParam("MaxNBursts",        &fMaxNBursts, 5000);        // Max number of bins in histograms
  AddParam("Kmu2Enabled",       &fKmu2Enabled, true);	    // Use Kmu2 selection?
  AddParam("UseControlTrigger", &fUseControlTrigger, true); // Use Control trigger?
  AddParam("StrictSelection",   &fStrictSelection, false);  // Use strict selection?
  fMUV3Geometry = new MUV3Geometry();

  fhTileVsBurstID = nullptr;
  fhExpected = nullptr;
  fhMatched = nullptr;
  fhEfficiency = nullptr;
  fhMatchedVsBurstID = nullptr;
  fhExpectedVsBurstID = nullptr;
  fhDeltaT = nullptr;
  fhDeltaTTriggerMUV3 = nullptr;
  fhEfficiency1DLowMom = nullptr;
  fhEfficiency1DHighMom = nullptr;

  // The only viable trigger stream containing MUV3 conditions is multi-track
  // (but also the control trigger is used, see settings above)
  fTriggerID = TriggerConditions::GetInstance()->GetL0TriggerID("RICH-QX");
}

MUV3Efficiency::~MUV3Efficiency() {
  if (fMUV3Geometry) delete fMUV3Geometry;
}

void MUV3Efficiency::InitHist() {

  fReadingData = GetIsTree();

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;
    BookHisto(new TH1F("hCut", "hCut", 20, 0, 20));
    BookHisto(new TH2F("hMUV1DistanceEnergy", "MUV1DistanceEnergy;Distance [mm];Energy [GeV]",
                       100, 0.0, 1000, 100, 0.0, 50));
    BookHisto(new TH2F("hMUV2DistanceEnergy", "MUV2DistanceEnergy;Distance [mm];Energy [GeV]",
		       100, 0.0, 1000, 100, 0.0, 50));

    BookHisto(new TH2F("hMatched",    "hMatched",    20, 0., 100., 152, 0, 152));
    BookHisto(new TH2F("hExpected",   "hExpected",   20, 0., 100., 152, 0, 152));
    BookHisto(new TH2F("hEfficiency", "hEfficiency", 20, 0., 100., 152, 0, 152));
    BookHisto(new TH1F("hNGoodTracks", "hNGoodTracks", 100, -0.5, 99.5));
    BookHisto(new TH1F("hTracksCDA", "hTracksCDA", 100, 0, 1000));
    BookHisto(new TH1F("hTracksChi2", "hTracksChi2", 100, 0, 100));
    BookHisto(new TH1F("hTracksMomentum", "hTracksMomentum", 1500, 0, 150));
    BookHisto(new TH1F("hRICHlikelihoods", "hRICHlikelihoods", 100, 0, 10.));
    BookHisto(new TH1F("hD", "hD", 500, 0, 500.));
    BookHisto(new TH1F("hE", "hE", 700, 0, 70000));
    BookHisto(new TH1F("hEoP", "hEoP", 120, 0, 1.2));
    BookHisto(new TH1F("hMmiss2_before", "hMmiss2_before", 100, -0.05, 0.05));
    BookHisto(new TH1F("hMmiss2_after", "hMmiss2_after", 100, -0.05, 0.05));

    BookHisto("hBurstID", new TH1F
	      ("hBurstID", "hBurstID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hTileVsBurstID", new TH2F
	      ("hTileVsBurstID", "hTileVsBurstID;Burst ID;Tile ID",
	       fMaxNBursts, -0.5, fMaxNBursts-0.5, 152, -0.5, 151.5));
    BookHisto("hMatchedVsBurstID", new TH1F
	      ("hMatchedVsBurstID", "hMatchedVsBurstID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hExpectedVsBurstID", new TH1F
	      ("hExpectedVsBurstID", "hExpectedVsBurstID", fMaxNBursts, -0.5, fMaxNBursts-0.5));

    BookHisto(new TH2F("hDeltaTLooseHits",
		       "Time difference between two channels for loose hits in the same tile;#Deltat (higher channel - lower channel) [ns];Tile ID",
		       200, -50, 50, 152, 0, 152));

    BookHisto(new TH1F("hDeltaTCHODMUV3",
		       "Time difference between CHOD and MUV3;t_{CHOD} - t_{MUV3} [ns];Entries/1ns",
		       200, -100., 100.));
    BookHisto(new TH1F("hDeltaTRICHMUV3",
		       "Time difference between RICH and MUV3;t_{RICH} - t_{MUV3} [ns];Entries/1ns",
		       200, -100., 100.));
    BookHisto(new TH1F("hDeltaTTriggerMUV3",
		       "Time difference between Trigger and MUV3;t_{Trigger} - t_{MUV3} [ns];Entries/1ns",
		       200, -100., 100.));
    BookHisto(new TH1F("hDeltaTTriggerRICH",
		       "Time difference between Trigger and RICH;t_{Trigger} - t_{RICH} [ns];Entries/1ns",
		       200, -100., 100.));
    BookHisto(new TH1F("hDeltaTTriggerLKr",
		       "Time difference between Trigger and LKr;t_{Trigger} - t_{LKr} [ns];Entries/1ns",
		       200, -100., 100.));

    // Set up the online monitor
    CreateCanvas("MUV3EfficiencyCanvas");
    PlacePlotOnCanvas("hEfficiency",	  "MUV3EfficiencyCanvas");
    PlacePlotOnCanvas("hMatched",         "MUV3EfficiencyCanvas");
    PlacePlotOnCanvas("hExpected",	  "MUV3EfficiencyCanvas");
    PlacePlotOnCanvas("hDeltaTLooseHits", "MUV3EfficiencyCanvas");
    SetUpdateInterval(50000);
  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fhTileVsBurstID     = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hTileVsBurstID", true));
    fhMatched           = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hMatched", true));
    fhExpected          = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hExpected", true));
    fhEfficiency        = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEfficiency", true));
    fhMatchedVsBurstID  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hMatchedVsBurstID",  true));
    fhExpectedVsBurstID = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hExpectedVsBurstID", true));
    fhDeltaT     	= static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hDeltaTLooseHits", true));
    fhDeltaTTriggerMUV3 = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hDeltaTTriggerMUV3", true));
  }
}

void MUV3Efficiency::StartOfBurstUser() {
  if (!fReadingData) return;
  FillHisto("hBurstID", GetBurstID());
}

void MUV3Efficiency::Process(Int_t) {

  // Should the selection running be the Kmu2 one?
  enumEventType EventType;
  if (fKmu2Enabled) EventType = kKmu2;
  else              EventType = kHalo;

  if (!fReadingData) return;

  // Fill the MUV3 tile profile
  TRecoMUV3Event *MUV3event = GetEvent<TRecoMUV3Event>();
  for (int i=0; i<MUV3event->GetNCandidates(); i++) {
    TRecoMUV3Candidate* Ccand = static_cast<TRecoMUV3Candidate*>(MUV3event->GetCandidate(i));
    FillHisto("hTileVsBurstID", GetBurstID(), Ccand->GetTileID());
  }

  FillHisto("hCut", 0);
  if (GetWithMC()) {
    fKmu2Enabled = true;
    EventType = kKmu2;
    fStrictSelection = true;
  }
  checkTrigger();
  if (!fTriggerOK) return;
  FillHisto("hCut", 1);
  Double_t TriggerTime = GetL0Data()->GetReferenceFineTime()*TdcCalib;

  std::vector<DownstreamTrack> Tracks =
    *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

  // CDA cuts to ensure presence of either Kmu2 or halo muon
  UInt_t trackID = -1;
  UInt_t NGoodTracks = 0;
  double Mmiss2 = -9999.;
  for (UInt_t i=0; i<Tracks.size(); i++) {
    FillHisto("hTracksCDA", Tracks[i].GetBeamAxisCDA());
    Bool_t goodCDA = (EventType==kKmu2) ?
      Tracks[i].GetBeamAxisCDA()<50. : Tracks[i].GetBeamAxisCDA()>150.;
    if (goodCDA) {
      trackID = i;
      NGoodTracks++;
    }
  }

  // When in NOT strict selection and the default selection option fails, try the other one:
  // Kmu2 option fails if NGoodTracks != 1 (we want a clear Kmu2 event)
  // Halo option fails if NGoodTracks == 0 (if there is no track compatible with halo)
  if (!fStrictSelection) {
    if (EventType==kKmu2 && NGoodTracks!=1) EventType = kHalo;
    else if (EventType==kHalo && NGoodTracks==0) EventType = kKmu2;
    NGoodTracks = 0;
    for (UInt_t i=0; i<Tracks.size(); i++) {
      Bool_t goodCDA = (EventType==kKmu2) ?
	Tracks[i].GetBeamAxisCDA()<50. : Tracks[i].GetBeamAxisCDA()>150.;
      if (goodCDA) {
	trackID = i;
	NGoodTracks++;
      }
    }
  }
  FillHisto("hNGoodTracks", NGoodTracks);

  if (EventType==kKmu2) {
    if (NGoodTracks!=1) return;
    if (Tracks[trackID].GetCharge()<0.) return;
    TLorentzVector muon, kaon;
    muon.SetVectM(Tracks[trackID].GetMomentumBeforeMagnet(), MMU);
    kaon.SetVectM(BeamParameters::GetInstance()->GetBeamThreeMomentum(), MKCH);
    Mmiss2 = (kaon-muon).M2() / 1e6;
    FillHisto("hMmiss2_before", Mmiss2);
  }
  else {
    if (NGoodTracks==0) return;
  }
  FillHisto("hCut", 2);

  Double_t TrackMomentum = Tracks[trackID].GetMomentum();
  Double_t TrackChi2     = Tracks[trackID].GetChi2();
  FillHisto("hTracksMomentum", TrackMomentum/1e3);
  if (TrackMomentum<5000. || TrackMomentum>100000.) return;
  FillHisto("hTracksChi2", TrackChi2);
  if (TrackChi2>10.0) return;
  FillHisto("hCut", 3);

  TVector2 TrackPositionAtMUV1 = TVector2(Tracks[trackID].xAt(fMUV1ZPos), Tracks[trackID].yAt(fMUV1ZPos));
  TVector2 TrackPositionAtMUV2 = TVector2(Tracks[trackID].xAt(fMUV2ZPos), Tracks[trackID].yAt(fMUV2ZPos));
  TVector2 TrackPositionAtMUV3 = TVector2(Tracks[trackID].xAt(fMUV3ZPos), Tracks[trackID].yAt(fMUV3ZPos));

  // Acceptance checks
  Bool_t InCHODAccept = GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[trackID], kCHOD);
  Bool_t InSTRAWAccept = true;
  for (Int_t ich=0; ich<4; ich++)
    InSTRAWAccept &= GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[trackID], kSpectrometer, ich);
  Double_t InLKrAccept = GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[trackID], kLKr);
  Double_t SearchRadius = Tracks[trackID].GetMUV3SearchRadius();
  Bool_t InMUV3Accept = GeometricAcceptance::GetInstance()->InAcceptance
    (&Tracks[trackID], kMUV3, 0, fMUV3Rmin+SearchRadius, fMUV3Rmax-SearchRadius);

  if (!InCHODAccept)  return;
  if (!InSTRAWAccept) return;
  if (!InLKrAccept)   return;
  if (!InMUV3Accept)  return;
  FillHisto("hCut", 4);

  // MUV1, MUV2, RICH
  if (Tracks[trackID].MUV1AssociationExists()) {
    Double_t Energy   = Tracks[trackID].GetMUV1ClusterEnergy();
    TVector2 Position = Tracks[trackID].GetMUV1ClusterPosition();
    Double_t Distance = (Position-TrackPositionAtMUV1).Mod();
    FillHisto("hMUV1DistanceEnergy", Distance, 0.001*Energy);
    if (Distance<100.0 && Energy>5000.0) return;
  }
  FillHisto("hCut", 5);

  if (Tracks[trackID].MUV2AssociationExists()) {
    Double_t Energy   = Tracks[trackID].GetMUV2ClusterEnergy();
    TVector2 Position = Tracks[trackID].GetMUV2ClusterPosition();
    Double_t Distance = (Position-TrackPositionAtMUV2).Mod();
    FillHisto("hMUV2DistanceEnergy", Distance, 0.001*Energy);
    if (Distance<100.0 && Energy>5000.0) return;
  }
  FillHisto("hCut", 6);

  Bool_t UseRICH = TrackMomentum<40000.;
  Double_t likelihood_e = Tracks[trackID].GetRICHLikelihoodElectron();
  Double_t likelihood_m = Tracks[trackID].GetRICHLikelihoodMuon();
  Double_t likelihood_p = Tracks[trackID].GetRICHLikelihoodPion();
  Double_t likelihood_R = likelihood_m/fmax(likelihood_e, likelihood_p);
  Double_t RICHTime = Tracks[trackID].GetRICHRingTime(kRICHHypothesisMuon);
  if (UseRICH) FillHisto("hDeltaTTriggerRICH", TriggerTime - RICHTime);
  if (UseRICH && fabs(TriggerTime - RICHTime + 2.5)>5.) return;
  FillHisto("hCut", 7);
  if (UseRICH) FillHisto("hRICHlikelihoods", likelihood_R);
  if (UseRICH && likelihood_R<1.2) return;
  FillHisto("hCut", 8);

  if (Tracks[trackID].GetLKrAssociationOutput().GetNAssociationRecords()==0) return;
  FillHisto("hCut", 9);
  Double_t LKrTime = Tracks[trackID].GetLKrAssociationOutput().GetBestAssociationRecord()->GetLKrCandidate()->GetTime();
  FillHisto("hDeltaTTriggerLKr", TriggerTime - LKrTime);
  if (fabs(TriggerTime - LKrTime + 3.)>10.) return;
  FillHisto("hCut", 10);

  FillHisto("hD", Tracks[trackID].GetLKrClusterDistance());
  if (Tracks[trackID].GetLKrClusterDistance()>30.) return;
  FillHisto("hCut", 11);

  FillHisto("hE", Tracks[trackID].GetLKrEnergy());
  if (Tracks[trackID].GetLKrEnergy()==0. || Tracks[trackID].GetLKrEnergy()>1500.) return;
  FillHisto("hCut", 12);

  FillHisto("hEoP", Tracks[trackID].GetLKrEoP());
  if (Tracks[trackID].GetLKrEoP()>0.1) return;
  FillHisto("hCut", 13);

  if (EventType==kKmu2) {
    FillHisto("hMmiss2_after", Mmiss2);
    if (fabs(Mmiss2)>0.01) return;
  }
  FillHisto("hCut", 14);

  Int_t TrackTileID = fMUV3Geometry->GetTileID(TrackPositionAtMUV3);
  if (TrackTileID==200) return; // 200 = track is outside MUV3 acceptance
  FillHisto("hCut", 15);

  if (!Tracks[trackID].CHODAssociationExists()) return;
  FillHisto("hCut", 16);
  Double_t CHODTime = Tracks[trackID].GetCHODTime();

  // Fill the "expected hits" histograms
  FillHisto("hExpected", 1e-3*TrackMomentum, TrackTileID);
  FillHisto("hExpectedVsBurstID", GetBurstID());

  if (Tracks[trackID].MUV3AssociationExists()) {

    // Fill DeltaT histogram for pairs of Loose hits in the same tile
    Int_t NMUV3Assos = Tracks[trackID].GetNMUV3AssociationRecords();
    if (NMUV3Assos>=2) {
      for (Int_t iHit1=0; iHit1<NMUV3Assos; iHit1++) {
	for (Int_t iHit2=iHit1+1; iHit2<NMUV3Assos; iHit2++) {
	  TRecoMUV3Candidate *Hit1 = Tracks[trackID].GetMUV3Candidate(iHit1);
	  TRecoMUV3Candidate *Hit2 = Tracks[trackID].GetMUV3Candidate(iHit2);
	  if (Hit1->GetType()==kLooseCandidate && Hit2->GetType()==kLooseCandidate &&
	      Hit1->GetTileID()==Hit2->GetTileID() &&
	      Hit1->GetChannel1()!=Hit2->GetChannel1()) {
	    Double_t t1 = Tracks[trackID].GetMUV3Time(iHit1);
	    Double_t t2 = Tracks[trackID].GetMUV3Time(iHit2);
	    Double_t T1 = (Hit2->GetChannel1()>Hit1->GetChannel1()) ? t1 : t2;
	    Double_t T2 = (Hit2->GetChannel1()>Hit1->GetChannel1()) ? t2 : t1;
	    FillHisto("hDeltaTLooseHits", T2-T1, Hit1->GetTileID());
	  }
	}
      }
    } // end of "NMUV3Assos>=2"

    // Choose muon closest to the trigger time
    Double_t MuonTime = -999.;
    Double_t MuonTrig_diff = 99999.;
    for (Int_t imu=0; imu<NMUV3Assos; imu++) {
      Double_t mu_t = Tracks[trackID].GetMUV3Time(imu);
      if (fabs(mu_t - TriggerTime)<MuonTrig_diff) {
	MuonTrig_diff = fabs(mu_t - TriggerTime);
	MuonTime = mu_t;
      }
    }

    FillHisto("hDeltaTCHODMUV3", CHODTime - MuonTime);
    FillHisto("hDeltaTRICHMUV3", RICHTime - MuonTime);
    FillHisto("hDeltaTTriggerMUV3", TriggerTime - MuonTime);

    // Fill the "matched hits" histograms
    Double_t MaxTriggerMuonTime = 15.;
    if (fStrictSelection) MaxTriggerMuonTime = 10.0;
    if (fabs(TriggerTime - MuonTime + 2.)<MaxTriggerMuonTime) {
      FillHisto("hMatched", 1e-3*TrackMomentum, TrackTileID);
      FillHisto("hMatchedVsBurstID", GetBurstID());
    }
  }
}

void MUV3Efficiency::EndOfJobUser() {
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  ////////////
  // DATA mode

  if (fReadingData) {
    fhMatched  = (TH2F*)fHisto.GetTH2("hMatched");
    fhExpected = (TH2F*)fHisto.GetTH2("hExpected");
    fhEfficiency = (TH2F*)fHisto.GetTH2("hEfficiency");
    fhDeltaT	 = (TH2F*)fHisto.GetTH2("hDeltaTLooseHits");
    fhDeltaTTriggerMUV3 = (TH1F*)fHisto.GetTH1("hDeltaTTriggerMUV3");

    fhMatched->SetOption("colz");
    fhMatched->SetTitle("Matched muons");
    fhMatched->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhMatched->GetYaxis()->SetTitle("Tile ID");
    fhMatched->SetStats(0);

    fhExpected->SetOption("colz");
    fhExpected->SetTitle("Expected muons");
    fhExpected->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhExpected->GetYaxis()->SetTitle("Tile ID");
    fhExpected->SetStats(0);

    fhEfficiency->Divide(fhMatched, fhExpected, 1., 1., "B");
    fhEfficiency->SetOption("colz");
    fhEfficiency->SetTitle("MUV3 efficiency");
    fhEfficiency->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhEfficiency->GetYaxis()->SetTitle("Tile ID");
    fhEfficiency->SetStats(0);

    fhDeltaT->SetOption("colz");
    fhDeltaT->SetStats(0);
    fhDeltaTTriggerMUV3->SetStats(0);
    fhMatched->Sumw2();
    fhExpected->Sumw2();
  }

  /////////////
  // HISTO mode

  if (!fReadingData) {
    if (!fhMatched) {
      cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
      return;
    }

    //////////////////////////////////////////
    // Efficiency vs burst ID & bad burst list

    ofstream BadBurstFile;
    BadBurstFile.open("./MUV3Efficiency_BadBursts.dat");
    fhEffVsBurstID = new TGraphErrors();
    Int_t minbin = 99999, maxbin = -99999, goodbins = 0;
    for (Int_t iBur=1; iBur<=fMaxNBursts; iBur++) {
      Bool_t BurstOK = (fhTileVsBurstID && fhTileVsBurstID->Integral(iBur, iBur)>0.5);
      if (!BurstOK) continue;

      Bool_t BadBurstStat = false, BadBurstEff = false, BadBurstHole = false;

      // Bad bursts by overall MUV3 efficiency
      Double_t n = fhMatchedVsBurstID->GetBinContent(iBur);
      Double_t N = fhExpectedVsBurstID->GetBinContent(iBur);
      if (N>0.5) {
	Double_t e = n/N;
	Double_t de = sqrt(e*(1.0-e)/N);
	if (e<0.96) BadBurstEff = true;
	goodbins++;
	if (iBur<minbin) minbin = iBur;
	if (iBur>maxbin) maxbin = iBur;
	fhEffVsBurstID->Set(fhEffVsBurstID->GetN()+1);
	fhEffVsBurstID->SetPoint(fhEffVsBurstID->GetN()-1, iBur-1, e); // x,y
	fhEffVsBurstID->SetPointError(fhEffVsBurstID->GetN()-1, 0.0, de); // dx,dy
      }

      // Bad bursts by the holes in the MUV3 tile profile:
      // there should be at least one candidate in every tile
      Double_t TotalCandidates = fhTileVsBurstID->Integral(iBur, iBur, 1, 152);
      if (TotalCandidates<10000.5) {
	BadBurstStat = true;
      }
      else {
	for (Int_t iTile=0; iTile<152; iTile++) {
	  // these tiles do not exist
	  if (iTile==65 || iTile==66 || iTile==77 || iTile==78) continue;
	  // corner tiles are not checked
	  if (iTile==0 || iTile==11 || iTile==132 || iTile==143) continue;
	  Double_t Nc = fhTileVsBurstID->Integral(iBur, iBur, iTile+1, iTile+1);
	  if (Nc<0.5) BadBurstHole = true;
	}
      }

      // Print the bad burst ID and tags
      if (BadBurstEff || BadBurstStat || BadBurstHole) {
	BadBurstFile << Form("BadBurst %06d %04d", GetRunID(), iBur-1);
	if (BadBurstEff)  BadBurstFile << " EFF";
	if (BadBurstStat) BadBurstFile << " STAT";
	if (BadBurstHole) BadBurstFile << " HOLE";
	BadBurstFile << endl;
      }
    }
    BadBurstFile.close();

    fhEffVsBurstID->SetTitle("MUV3 efficiency vs burst ID");
    fhEffVsBurstID->SetMarkerStyle(20);
    fhEffVsBurstID->SetMarkerColor(4);
    fhEffVsBurstID->SetMarkerSize(0.3);
    fhEffVsBurstID->GetXaxis()->SetTitle("Burst ID");
    fhEffVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fhEffVsBurstID->GetYaxis()->SetRangeUser(-0.05, 1.05);
    if (goodbins) fhEffVsBurstID->GetXaxis()->SetRangeUser(minbin-1.5, maxbin-0.5);

    /////////////////////////
    // Produce the PDF output

    fhMatched->SetOption("colz");
    fhMatched->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhMatched->GetYaxis()->SetTitle("Tile ID");
    fhMatched->SetStats(0);
    fhExpected->SetOption("colz");
    fhExpected->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhExpected->GetYaxis()->SetTitle("Tile ID");
    fhExpected->SetStats(0);
    fhEfficiency->Divide(fhMatched, fhExpected, 1., 1., "B");
    fhEfficiency->SetOption("colz");
    fhEfficiency->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhEfficiency->GetYaxis()->SetTitle("Tile ID");
    fhEfficiency->SetMaximum(1.0);
    fhEfficiency->SetMinimum(0.5);
    fhEfficiency->SetStats(0);

    // Efficiency vs TileID vs track momentum plot
    fCanvas = new TCanvas("MUV3EfficiencyCanvas", "MUV3EfficiencyCanvas");
    fhEfficiency->SetTitle("MUV3 Efficiency vs TileID vs track momentum");
    fhEfficiency->Draw();
    fCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas

    Double_t MinY = 0.;

    // Cumulative efficiency VS Track momentum plot (integrated over TileID)
    fCanvas->SetRightMargin(0.02);
    fCanvas->SetGrid(1,1);
    TH1D* hMatchedVSMom    = (TH1D*)fhMatched->ProjectionX("hMatchedVSMom");
    TH1D* hExpectedVSMom   = (TH1D*)fhExpected->ProjectionX("hExpectedMom");
    TH1D* hEfficiencyVSMom = (TH1D*)hMatchedVSMom->Clone("hEfficiencyVSMom");
    hEfficiencyVSMom->Divide(hMatchedVSMom, hExpectedVSMom, 1., 1., "B");
    MinY = hEfficiencyVSMom->GetMinimum(0.);
    hEfficiencyVSMom->SetTitle("MUV3 Efficiency VS Track momentum");
    hEfficiencyVSMom->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    hEfficiencyVSMom->GetYaxis()->SetTitle("Efficiency");
    hEfficiencyVSMom->SetMarkerStyle(20);
    hEfficiencyVSMom->SetMarkerSize(1);
    hEfficiencyVSMom->SetMarkerColor(1);
    hEfficiencyVSMom->SetLineWidth(2);
    hEfficiencyVSMom->SetLineColor(2);
    hEfficiencyVSMom->SetMinimum(0.95*MinY);
    hEfficiencyVSMom->SetMaximum(1.);
    hEfficiencyVSMom->SetStats(0);
    hEfficiencyVSMom->Draw("E1");
    fCanvas->Print(fOutPDFFileName, "pdf");

    ////////////////////////////////////////////////////////
    // Efficiency vs TileID (integrated over track momentum)

    // Efficiency vs tile ID: low momentum
    Int_t LowMomBin = 2;
    Int_t HighMomBin = 8;
    TH1D* hMatched1DLowMom = (TH1D*)fhMatched->ProjectionY("hMatched1DLowMom", LowMomBin, HighMomBin);
    TH1D* hExpected1DLowMom = (TH1D*)fhExpected->ProjectionY("hExpected1DLowMom", LowMomBin, HighMomBin);
    fCanvas->GetPad(0)->SetRightMargin(0.02);
    TH1D* hMatchedVSTileIDL    = hMatched1DLowMom;
    TH1D* hExpectedVSTileIDL   = hExpected1DLowMom;
    TH1D* hEfficiencyVSTileIDL = (TH1D*)hMatchedVSTileIDL->Clone("hEfficiencyVSTileIDL");
    hEfficiencyVSTileIDL-> Divide(hMatchedVSTileIDL, hExpectedVSTileIDL, 1., 1., "B");

    cout << user_normal() << "Overall    inefficiency (low mom)  = " <<
      1.0 - hMatchedVSTileIDL->Integral() / hExpectedVSTileIDL->Integral() << endl;
    cout << user_normal() << "Inner tile inefficiency (low mom)  = " <<
      1.0 - hMatchedVSTileIDL->Integral(145,152) / hExpectedVSTileIDL->Integral(145,152) << endl;
    cout << user_normal() << "Outer tile inefficiency (low mom)  = " <<
      1.0 - hMatchedVSTileIDL->Integral(1,144) / hExpectedVSTileIDL->Integral(1,144) << endl;

    TF1 *fpol1l = new TF1("fpol1l", "pol0", 0, 144);
    hEfficiencyVSTileIDL->Fit(fpol1l, "Q0R");
    TF1 *fpol2l = new TF1("fpol2l", "pol0", 144, 151);
    hEfficiencyVSTileIDL->Fit(fpol2l, "Q0R");

    MinY = hEfficiencyVSTileIDL->GetMinimum(0.);
    hEfficiencyVSTileIDL->SetTitle("MUV3 Efficiency VS Tile ID,  p #in #LT5, 40#GT GeV/c");
    hEfficiencyVSTileIDL->GetXaxis()->SetTitle("Tile ID");
    hEfficiencyVSTileIDL->GetYaxis()->SetTitle("Efficiency");
    hEfficiencyVSTileIDL->SetMinimum(0.95 * MinY);
    hEfficiencyVSTileIDL->SetMaximum(1.);
    hEfficiencyVSTileIDL->SetStats(0);
    hEfficiencyVSTileIDL->Draw();
    fpol1l->Draw("same");
    fpol2l->Draw("same");
    fCanvas->Print(fOutPDFFileName, "pdf");

    // Cumulative tile-by-tile efficiency plot (integrated over low momentum)
    fhEfficiency1DLowMom = (TH1F*)hMatched1DLowMom->Clone("hEfficiency1DLowMom");
    fhEfficiency1DLowMom->Divide(hMatched1DLowMom, hExpected1DLowMom, 1., 1., "B");
    GenerateInefficiencyPlot(fhEfficiency1DLowMom, "MUV3 Inefficiency, p #in #LT5, 40#GT GeV/c", false);

    // Efficiency vs tile ID: high momentum
    LowMomBin = 9;
    HighMomBin = 31;
    TH1D* hMatched1DHighMom = (TH1D*)fhMatched->ProjectionY("hMatched1DHighMom", LowMomBin, HighMomBin);
    TH1D* hExpected1DHighMom = (TH1D*)fhExpected->ProjectionY("hExpected1DHighMom", LowMomBin, HighMomBin);
    fCanvas->GetPad(0)->SetRightMargin(0.02);
    TH1D* hMatchedVSTileIDH    = hMatched1DHighMom;
    TH1D* hExpectedVSTileIDH   = hExpected1DHighMom;
    TH1D* hEfficiencyVSTileIDH = (TH1D*)hMatchedVSTileIDH->Clone("hEfficiencyVSTileIDH");
    hEfficiencyVSTileIDH->Divide(hMatchedVSTileIDH, hExpectedVSTileIDH, 1., 1., "B");

    /*
    cout <<"[MUV3Efficiency] Overall    inefficiency (high mom) = " <<
      1.0 - hMatchedVSTileIDH->Integral() / hExpectedVSTileIDH->Integral() << endl;
    cout <<"[MUV3Efficiency] Inner tile inefficiency (high mom) = " <<
      1.0 - hMatchedVSTileIDH->Integral(145,152) / hExpectedVSTileIDH->Integral(145,152) << endl;
    cout <<"[MUV3Efficiency] Outer tile inefficiency (high mom) = " <<
      1.0 - hMatchedVSTileIDH->Integral(1,144) / hExpectedVSTileIDH->Integral(1,144) << endl;
    */

    TF1 *fpol1h = new TF1("fpol1h", "pol0", 0, 144);
    hEfficiencyVSTileIDH->Fit(fpol1h, "Q0R");
    TF1 *fpol2h = new TF1("fpol2h", "pol0", 144, 151);
    hEfficiencyVSTileIDH->Fit(fpol2h, "Q0R");

    MinY = hEfficiencyVSTileIDH->GetMinimum(0.);
    hEfficiencyVSTileIDH->SetTitle("MUV3 Efficiency VS Tile ID,  p #in #LT40, 150#GT GeV/c");
    hEfficiencyVSTileIDH->GetXaxis()->SetTitle("Tile ID");
    hEfficiencyVSTileIDH->GetYaxis()->SetTitle("Efficiency");
    hEfficiencyVSTileIDH->SetMinimum(0.95 * MinY);
    hEfficiencyVSTileIDH->SetMaximum(1.);
    hEfficiencyVSTileIDH->SetStats(0);
    hEfficiencyVSTileIDH->Draw();
    fpol1h->Draw("same");
    fpol2h->Draw("same");
    fCanvas->Print(fOutPDFFileName, "pdf");

    // Cumulative tile-by-tile efficiency plot (integrated over high momentum)
    fhEfficiency1DHighMom = (TH1F*)hMatched1DHighMom->Clone("hEfficiency1DHighMom");
    fhEfficiency1DHighMom->Divide(hMatched1DHighMom, hExpected1DHighMom, 1., 1., "B");
    GenerateInefficiencyPlot(fhEfficiency1DHighMom, "MUV3 Inefficiency, p #in #LT40, 150#GT GeV/c", false);

    // DeltaT plot for loose hits in the same tile
    fCanvas->Clear();
    fCanvas->SetGrid(0,0);
    fCanvas->Divide(2,1);
    fCanvas->cd(1);
    fCanvas->GetPad(1)->SetGrid(1,0);
    fhDeltaT->SetOption("colz");
    fhDeltaT->SetStats(0);
    fhDeltaT->Draw();
    fhDeltaT->Write();
    fCanvas->cd(2);
    fCanvas->GetPad(2)->SetGrid(1,0);
    TH1D *hDeltaT1D = fhDeltaT->ProjectionX("hDeltaTIntegrated", 1, fhDeltaT->GetNbinsY());
    hDeltaT1D->SetTitle("Time difference between two channels for loose hits in the same tile, integrated;#Deltat (higher channel - lower channel) [ns];Entries/0.5ns");
    hDeltaT1D->SetStats(0);
    hDeltaT1D->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // DeltaT between trigger and MUV3 time
    fCanvas->Clear();
    fCanvas->SetGrid(1,0);
    fCanvas->SetLogy(1);
    fCanvas->SetRightMargin(0.02);
    fhDeltaTTriggerMUV3->SetStats(0);
    fhDeltaTTriggerMUV3->Draw();
    fhDeltaTTriggerMUV3->Write();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // Efficiency vs burst ID
    fCanvas->SetTopMargin(0.07);
    fCanvas->SetRightMargin(0.09);
    fCanvas->SetLeftMargin(0.08);
    fCanvas->SetBottomMargin(0.08);
    fCanvas->SetGrid(0,1);
    fCanvas->SetLogy(0);
    fhEffVsBurstID->Draw("AP");
    fCanvas->Print(Form(fOutPDFFileName + ")"), "pdf");
  }

  SaveAllPlots();
  gErrorIgnoreLevel = -1; // restore the default
}

void MUV3Efficiency::GenerateInefficiencyPlot(TH1F* hEfficiency1D, TString Name, Bool_t CloseFile) {

  gROOT->Reset();
  gStyle->SetOptStat(0);
  const Int_t NRGBs = 6;
  const Int_t NCont = 600;

  Double_t stops[NRGBs] = {0.00, 0.25, 0.50, 0.75, 0.85, 1.00};
  Double_t red[NRGBs]   = {0.00, 0.00, 0.00, 1.00, 1.00, 1.00};
  Double_t green[NRGBs] = {0.00, 0.40, 1.00, 1.00, 0.50, 0.00};
  Double_t blue[NRGBs]  = {1.00, 0.80, 0.00, 0.00, 0.00, 0.00};
  TColor::CreateGradientColorTable(NRGBs, stops, red, green, blue, NCont);

  gStyle->SetNumberContours(NCont);
  gStyle->SetNumberContours(999);
  TLatex *Text = new TLatex();
  gStyle->SetTextSize(0.02);
  Text->SetTextSize(0.02);
  gStyle->SetTextColor(kBlack);
  gStyle->SetTextAlign(11);

  TH2F *HistoOneMinusOuter = new
    TH2F(Name.Data(), Name.Data(), 12, -fMUV3Rmax, fMUV3Rmax, 12, -fMUV3Rmax, fMUV3Rmax);
  TH2F *HistoOneMinusInner = new
    TH2F("HI", "HI", 3, -fMUV3Rmax/6., fMUV3Rmax/6., 3, -fMUV3Rmax/6., fMUV3Rmax/6.);

  // fill inefficiency histos
  for (Int_t i=0; i<=151; i++) {
    TVector2 Pos   = fMUV3Geometry->GetTileCentre(i);
    Double_t error = hEfficiency1D->GetBinError(i+1);
    Double_t effi  = roundf(hEfficiency1D->GetBinContent(i+1)*10000.)/10000.;
    if (i>=144) { // inner
      Int_t iBin = HistoOneMinusInner->FindBin(Pos.X(), Pos.Y());
      if (effi!=0) {
	    HistoOneMinusInner->SetBinContent(iBin, 1. - effi);
		HistoOneMinusInner->SetBinError(iBin, error);
      }
    }
    else { // outer
      Int_t iBin = HistoOneMinusOuter->FindBin(Pos.X(), Pos.Y());
      if (effi!=0) {
        HistoOneMinusOuter->SetBinContent(iBin, 1. - effi);
        HistoOneMinusOuter->SetBinError(iBin, error);
      }
    }
  }

  TBox *box = new TBox();
  box->SetFillStyle(0);
  box->SetLineColor(kBlack);

  /// Prepare canvas
  fCanvas->SetGrid(0,0);
  fCanvas->SetLogz();
  fCanvas->SetTopMargin(0.07);
  fCanvas->SetRightMargin(0.12);
  fCanvas->SetLeftMargin(0.08);
  fCanvas->SetBottomMargin(0.08);

  //outer
  HistoOneMinusOuter->SetMaximum(1.);
  HistoOneMinusOuter->SetMinimum(1e-4);
  HistoOneMinusInner->SetMaximum(1.);
  HistoOneMinusInner->SetMinimum(1e-4);
  HistoOneMinusOuter->SetContour(99);
  HistoOneMinusInner->SetContour(99);
  HistoOneMinusOuter->GetXaxis()->SetTitle("x (mm)");
  HistoOneMinusOuter->GetYaxis()->SetTitle("y (mm)");
  HistoOneMinusOuter->SetStats(0);
  HistoOneMinusOuter->Draw("colztext");
  for (Int_t i=0; i<=143; i++) if (i!=65 && i!=66 && i!=77 && i!=78) {
    TVector2 Pos = fMUV3Geometry->GetTileCentre(i);
    Text->SetTextSize(0.02);
    Text->DrawText(Pos.X() - 90, Pos.Y() + 50, Form("Tile %i", i));
    Text->SetTextSize(0.02);
    if (hEfficiency1D->GetBinContent(i+1)!=0.) Text->DrawLatex(Pos.X() - 70, Pos.Y() - 75, Form("#sigma = %.3f", hEfficiency1D->GetBinError(i+1)));
    if (hEfficiency1D->GetBinContent(i+1)==1.) Text->DrawText(Pos.X() - 35, Pos.Y() - 25, "0.000");
    box->DrawBox(Pos.X()-110, Pos.Y()-110, Pos.X()+110, Pos.Y()+110);
  }

  //inner
  HistoOneMinusInner->SetStats(0);
  HistoOneMinusInner->Draw("coltext,same");
  for (Int_t i=144; i<=151; i++) {
    TVector2 Pos = fMUV3Geometry->GetTileCentre(i);
    Text->SetTextSize(0.015);
    Text->DrawText(Pos.X()-60, Pos.Y()+30, Form("Tile %i", i));
    Text->SetTextSize(0.012);
    if (hEfficiency1D->GetBinContent(i+1)!=0.) Text->DrawLatex(Pos.X()-50, Pos.Y()-55, Form("#sigma = %.3f", hEfficiency1D->GetBinError(i+1)));
    if (hEfficiency1D->GetBinContent(i+1)==1.) Text->DrawText(Pos.X()-25, Pos.Y()-25, "0.000");
    box->DrawBox(Pos.X()-73.33, Pos.Y()-73.33, Pos.X()+73.33, Pos.Y()+73.33);
  }

  if (CloseFile) {
    fCanvas->Print(Form(fOutPDFFileName + ")"), "pdf"); // print into the output PDF file and close it
  } else {
    fCanvas->Print(fOutPDFFileName, "pdf"); // print into the output PDF file
  }

  delete box;
  delete Text;
  delete HistoOneMinusInner;
  delete HistoOneMinusOuter;
}

void MUV3Efficiency::checkTrigger() {
  if (GetWithMC()) {
    fTriggerOK = true;
    return;
  }
  if (fUseControlTrigger && TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data())) {
    fTriggerOK = true;
    return;
  }
  // Multi-track trigger
  Bool_t triggerActive = TriggerConditions::GetInstance()->L0TriggerEnabled(GetRunID(), fTriggerID);
  Bool_t L0ON = TriggerConditions::GetInstance()->L0TriggerOn(GetRunID(), GetL0Data(), fTriggerID);
  fTriggerOK = triggerActive && L0ON;
}
