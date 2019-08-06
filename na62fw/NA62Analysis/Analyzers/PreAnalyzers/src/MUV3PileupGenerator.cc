// ---------------------------------------------------------------
//
// History:
//
// Created by Lubos Bician (lubos.bician@cern.ch) 2018-02-16
//
// ---------------------------------------------------------------

/// \class MUV3PileupGenerator
/// \Brief
/// MUV3 pileup generator injecting random MUV3 candidates into MC events
/// \EndBrief
/// \Detailed
/// Accidental MUV3 candidates are injected into MC events based on rate and hit map obtained from data.
/// The standard hit map obtained from 2017A data is usedl it is
/// stored in NA62Tools/Conditions/MC/MUV3PileupHitMap.root
/// In this case, MUV3PileupGenerator should be used as a PREANALYZER. The code checks
/// whether it's running on data or MC automatically, so the user can use one executable on data and MC.
/// The following explains how one can obtain the hit map using this preanalyzer.
/// The tool is a 3-step (pre-)analyzer. The steps are the following:
/// 1. First step involves running over data. In this step the analyzer fills necessary spatial and timing information
/// about MUV3 pileup.
/// 2. In the second step, one has to run the analyzer on its own output. The analyzer looks at the previously filled
/// histograms and generates a cumulative distribution functions for later use on the MC. The information is stored
/// in current directory as 'MUV3PileupHitMap.dat'
/// 3. In the last step, the MUV3PilepGenerator has to be run as a preanalyzer on MC. It reads the 'MUV3PileupHitMap.dat' file,
/// path to which the user can modify using the 'HitMapFilePath' parameter, and injects random accidental candidates to the MUV3
/// MC event. The generation is explained in more detail in function 'generateAccidentalsInMC'.
/// \author Lubos Bician (lubos.bician@cern.ch)
/// \EndDetailed

#include <iostream>
#include "MUV3PileupGenerator.hh"
#include "Event.hh"
#include "TriggerConditions.hh"
#include "GeometricAcceptance.hh"
#include "BeamIntensityGenerator.hh"
#include "NA62ConditionsService.hh"
#include "TRecoMUV3Event.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

MUV3PileupGenerator::MUV3PileupGenerator(Core::BaseAnalysis *ba) : Analyzer(ba, "MUV3PileupGenerator") {
  RequestBeamData();
  RequestL0Data();
  RequestL1Data();
  RequestBeamSpecialTrigger();
  RequestL0SpecialTrigger();
  RequestTree(new TRecoMUV3Event, "Reco");

  AddParam("CutHitTriggerTime", "double", &fCutHitTriggerTimeDiff, 45.);
  AddParam("TriggerName", &fTriggerName, "Control");
  AddParam("EventTimeWindowLowEdge",  "double", &fEventTimeWindowLowEdge, -70.);
  AddParam("EventTimeWindowHighEdge", "double", &fEventTimeWindowHighEdge, 70.);

  fGeo = MUV3Geometry::GetInstance();
  fRandom = new TRandom2();

  fHitMapFile = nullptr;
  fhHitMap = nullptr;
  for (Int_t i=0; i<200; i++) fHistoProfile[i] = nullptr;

  fScaleFactor = 1.1; // a scale factor to be applied to the number of MUV3 candidates
}

MUV3PileupGenerator::~MUV3PileupGenerator() {
  if (fRandom) delete fRandom;
  for (Int_t i=0; i<200; i++) if (fHistoProfile[i]) delete fHistoProfile[i];
}

void MUV3PileupGenerator::InitOutput() {
  RegisterOutput("NAccidentalCandidates", &fNAccidentalCandidates);
}

void MUV3PileupGenerator::InitHist() {
  if (GetIsTree()) {
    for (UInt_t i=0; i<2; i++) {
      TString folder = (i==0) ? "DATA/" : "MC/";
      BookHisto(new TH1F(folder+"hMUV3hitTriggerTime", "MUV3 cand time - trigger time; #Delta t [ns]; Entries / (0.5 ns)", 800, -200, 200));
      BookHisto(new TH1F(folder+"hMUV3hitmap", "Total number of cands in tile; Tile ID; Entries", 152, 0, 152));
      BookHisto(new TH2F(folder+"hMUV3hitmap2Douter", "Total number of cands in tile; X [mm]; Y[mm]", 12, -1320, 1320, 12, -1320, 1320));
      BookHisto(new TH2F(folder+"hMUV3hitmap2Dinner", "Total number of cands in tile; X [mm]; Y[mm]", 3, -220, 220, 3, -220, 220));
      BookHisto(new TH1F(folder+"hMUV3nCands", "Number of candidates in event; N candidates; Events", 100, -0.5, 99.5));
      BookHisto(new TH1F(folder+"hMUV3nHits", "Number of hits in event; N hits; Events", 100, -0.5, 99.5));
      BookHisto(new TH1F(folder+"hMUV3nAccidentalCands", "Number of accidental candidates in event; N candidates; Events", 100, -0.5, 99.5));
      BookHisto(new TH1F(folder+"hMUV3nAccidentalCandsOverBeamIntensity", "N_{accidental candidates} / Beam Intensity; N/I [1/MHz]; Entries", 300, 0., 0.15));
      BookHisto(new TH2F(folder+"hMUV3nAccidentalCandsVsBeamIntensity", "N_{accidental candidates} vs Beam Intensity; I [MHz]; N", 100, 0., 1000., 100, 0, 100));
    }
    BookHisto(new TH1F("MC/hMUV3nCandsAfter", "Number of candidates in event after adding accidentals; N candidates; Events", 100, -0.5, 99.5));
    BookHisto(new TH1F("MC/hMUV3nHitsAfter", "Number of hits in event after adding accidentals; N hits; Events", 100, -0.5, 99.5));
  }
  else {
    fhnAccidentalCandsVsBeamIntensity =
      static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "DATA/hMUV3nAccidentalCandsVsBeamIntensity", true));
    fhHitMap = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "DATA/hMUV3hitmap", true));
  }

  ///////////////////////////////////////////
  // Initialization depending on data/MC mode

  if (!GetIsTree()) {
    cout << user_normal() << "This is HISTO mode, I'm going to generate MUV3 hitmap file" << endl;
    return;
  }

  if (GetWithMC()) { // MC
    cout << user_normal() << "This is MC, I'm going to generate accidental MUV3 hits" << endl;
    TString FileName = NA62ConditionsService::GetInstance()->GetFullPath("MUV3PileupHitMap.root");
    fHitMapFile = new TFile(FileName, "READ");
    if (fHitMapFile->IsOpen()) {
      fhHitMap = (TH1F*)fHitMapFile->Get("MUV3PileupGenerator/DATA/hMUV3hitmap");
      for (Int_t i=0; i<200; i++) {
	fHistoProfile[i] = (TH1F*)fHitMapFile->Get(Form("MUV3PileupGenerator/NaccidentalsInIntensityBin_%i", i+1));
      }
      //fHitMapFile->Close(); it crashes when using these histograms if the file is closed!
    }
    else {
      cout << user_normal() << "Fatal error: cannot open hitmap file" << endl;
      exit(kGenericError);
    }
  }
  else { // DATA
    cout << user_normal() << "This is DATA, I'm going to fill MUV3 hitmap" << endl;
    // the structure of accidentals could be trigger-dependent
    fTriggerID = 9999;
    if (fTriggerName != "Control") {
      fTriggerID = TriggerConditions::GetInstance()->GetL0TriggerID(fTriggerName);
    }
    cout << user_normal() << "Reading trigger mask " << fTriggerName << " with ID = " << fTriggerID << endl;
  }
}

void MUV3PileupGenerator::StartOfBurstUser() {
  if (!GetIsTree()) return;
  fRandom->SetSeed(GetEventHeader()->GetBurstID()); // to ensure reproducibility
}

void MUV3PileupGenerator::Process(Int_t) {
  if (!GetIsTree()) return;
  fNAccidentalCandidates = 0;
  SetOutputState("NAccidentalCandidates", kOValid);

  if (GetWithMC()) { // MC
    generateAccidentalsInMC();
  }
  else { // Data
    bool passed_L0, passed_L1;
    if (fTriggerName != "Control") {
      passed_L0 = TriggerConditions::GetInstance()->L0TriggerOn(GetRunID(), GetL0Data(), fTriggerID);
      passed_L1 = TriggerConditions::GetInstance()->L1TriggerOn(GetRunID(), GetL1Data(), fTriggerID);
    }
    else {
      passed_L0 = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data());
      passed_L1 = true;
    }
    bool passed_TR = passed_L0 && passed_L1;
    Int_t Ref = 0;
    if (passed_TR) {
      Ref = GetL0Data()->GetReferenceFineTime();
    } else return;
    fTriggerTime = Ref*TdcCalib;
    generateHitMapFromData();
  }
}

void MUV3PileupGenerator::generateHitMapFromData() {
  // ask for MUV3 event and remember number of hits and candidates, count the number of events
  TRecoMUV3Event *event = GetEvent<TRecoMUV3Event>();
  UInt_t nMUV3hits = event->GetNHits();
  UInt_t nMUV3cands = event->GetNCandidates();
  FillHisto("DATA/hMUV3nHits", nMUV3hits);
  FillHisto("DATA/hMUV3nCands", nMUV3cands);
  fNAccidentalCandidates = 0;
  for (UInt_t ic=0; ic<nMUV3cands; ic++) {
    TRecoMUV3Candidate* cand = static_cast<TRecoMUV3Candidate*>(event->GetCandidate(ic));
    double time = cand->GetTime();
    UInt_t tile = cand->GetTileID();
    FillHisto("DATA/hMUV3hitTriggerTime", time - fTriggerTime);
    if (fabs(time-fTriggerTime)<fCutHitTriggerTimeDiff) continue;
    if (time-fTriggerTime > fEventTimeWindowHighEdge) continue;
    if (time-fTriggerTime < fEventTimeWindowLowEdge) continue;
    FillHisto("DATA/hMUV3hitmap", tile);
    fNAccidentalCandidates++;
    TVector3 pos = cand->GetPosition();
    if (cand->IsInner()) FillHisto("DATA/hMUV3hitmap2Dinner", pos.X(), pos.Y());
    else                 FillHisto("DATA/hMUV3hitmap2Douter", pos.X(), pos.Y());
  }

  fBeamIntensity = GetBeamData()->GetInstantaneousIntensity();
  FillHisto("DATA/hMUV3nAccidentalCands", fNAccidentalCandidates);
  FillHisto("DATA/hMUV3nAccidentalCandsOverBeamIntensity", fNAccidentalCandidates/fBeamIntensity);
  FillHisto("DATA/hMUV3nAccidentalCandsVsBeamIntensity", fBeamIntensity, fNAccidentalCandidates);
}

//////////////////////////////////////////////
// Add accidental MUV3 candidates to MC events

void MUV3PileupGenerator::generateAccidentalsInMC() {
  double EventTimeWindow = fEventTimeWindowHighEdge-fEventTimeWindowLowEdge; // default = 140ns
  double TriggerHitWindow = 2.*fCutHitTriggerTimeDiff; // default = 2*45ns = 90ns
  double TimeWindowFromData = (EventTimeWindow - TriggerHitWindow); // default = 50ns

  // Randomly generate the number of expected accidentals based on the mean rate.
  // The generation is performed in a time window 4x bigger than the one used to read
  // the accidental rate from data. This means +-100ns on default settings.

  fBeamIntensity = *(Double_t*)GetOutput("BeamIntensityGenerator.BeamIntensity");
  fNAccidentalCandidates = 0;
  for (Int_t i=0; i<4; i++) fNAccidentalCandidates += getNaccidentals(fBeamIntensity);

  FillHisto("MC/hMUV3nAccidentalCands", fNAccidentalCandidates);
  FillHisto("MC/hMUV3nAccidentalCandsOverBeamIntensity", fNAccidentalCandidates/fBeamIntensity);
  FillHisto("MC/hMUV3nAccidentalCandsVsBeamIntensity", fBeamIntensity, fNAccidentalCandidates);

  TRecoMUV3Event *event = GetEvent<TRecoMUV3Event>();
  UInt_t nMUV3hits  = event->GetNHits();
  UInt_t nMUV3cands = event->GetNCandidates();
  FillHisto("MC/hMUV3nHits",  nMUV3hits);
  FillHisto("MC/hMUV3nCands", nMUV3cands);

  // The accidentals are added into the MUV3 event as new candidates
  for (UInt_t iCand=0; iCand<fNAccidentalCandidates; iCand++) {
    TRandom *r = gRandom;
    gRandom = fRandom; // to ensure reproducibility (to be tested...)
    double tile = floor(fhHitMap->GetRandom());
    gRandom = r;

    double time = TimeWindowFromData * (4.0*fRandom->Uniform() - 2.0);
    TVector2 pos(fGeo->GetTileCentreX(tile), fGeo->GetTileCentreY(tile));

    TRecoMUV3Candidate *cand = static_cast<TRecoMUV3Candidate*>(event->AddCandidate());
    cand->SetType(kTightCandidate);
    cand->SetTileID(tile);
    cand->SetChannel1(tile);
    cand->SetChannel2(tile+200);
    cand->SetX(pos.X());
    cand->SetY(pos.Y());
    cand->SetTime(time);
    cand->SetTimeNoT0(time);
    cand->SetTimeNoTileT0(time);
    cand->SetTime1(time);
    cand->SetTime2(time);
    cand->SetTime1NoT0(time);
    cand->SetTime2NoT0(time);

    TRecoMUV3Hit *hit1 = new TRecoMUV3Hit();
    hit1->SetChannelID(tile);
    hit1->SetPosition
      (TVector3(pos.X(), pos.Y(), GeometricAcceptance::GetInstance()->GetZMUV3()));
    hit1->SetTime(time);
    hit1->SetTimeNoT0(time);
    event->AddHit(hit1);
    delete hit1;

    TRecoMUV3Hit *hit2 = new TRecoMUV3Hit();
    hit2->SetChannelID(tile+200);
    hit2->SetPosition
      (TVector3(pos.X(), pos.Y(), GeometricAcceptance::GetInstance()->GetZMUV3()));
    hit2->SetTime(time);
    hit2->SetTimeNoT0(time);
    event->AddHit(hit2);
    delete hit2;

    FillHisto("MC/hMUV3hitmap", tile);
    FillHisto("MC/hMUV3hitTriggerTime", time);
    if (tile>=144) FillHisto("MC/hMUV3hitmap2Dinner", pos.X(), pos.Y());
    else           FillHisto("MC/hMUV3hitmap2Douter", pos.X(), pos.Y());
  }
  nMUV3cands = event->GetNCandidates();
  nMUV3hits  = event->GetNHits();
  FillHisto("MC/hMUV3nCandsAfter", nMUV3cands);
  FillHisto("MC/hMUV3nHitsAfter", nMUV3hits);
}

UInt_t MUV3PileupGenerator::getNaccidentals(Double_t intensity) {
  Int_t intensityBin = (Int_t)(intensity/10);
  if (intensityBin<1)   intensityBin = 1;
  if (intensityBin>200) intensityBin = 200;
  TRandom *r = gRandom;
  gRandom = fRandom;
  Int_t Naccidentals = floor(fScaleFactor * fHistoProfile[intensityBin-1]->GetRandom());
  gRandom = r;
  return Naccidentals;
}

void MUV3PileupGenerator::EndOfJobUser() {
  if (!GetIsTree()) { // histo mode: save N_accidentals profiles in beam intensity bins
    TH2F* h2D = fhnAccidentalCandsVsBeamIntensity;
    if (h2D) {
      for (Int_t ibin=1; ibin<=h2D->GetXaxis()->GetNbins(); ibin++) {
	TH1D* hSlice = h2D->ProjectionY(Form("NaccidentalsInIntensityBin_%i", ibin), ibin, ibin);
	hSlice->Write();
      }
    }
  }
  SaveAllPlots();
}
