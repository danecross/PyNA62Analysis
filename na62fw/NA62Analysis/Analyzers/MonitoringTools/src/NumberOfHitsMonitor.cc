// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-04-04
// Modified by Viacheslav Duk (Viacheslav.Duk@cern.ch) 28.11.2017
//
// ---------------------------------------------------------------

#include "NumberOfHitsMonitor.hh"
#include "BaseAnalysis.hh"
#include "TString.h"
#include "TLine.h"
#include "ConfigSettings.hh"
#include "NA62Exceptions.hh"
#include "TriggerConditions.hh"
#include "TF1.h"

#include "NA62ConditionsService.hh"
#include "TRecoVEvent.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class NumberOfHitsMonitor
/// \Brief
/// Monitor the stability of numbers of hits & candidates in subdetectors per control trigger
/// \EndBrief
/// \Detailed
/// The analyzer should be run consecutively in two modes.
/// 1) Read the reconstructed data and produce intermediate output (bad bursts are not skipped).
/// 2) Read its own output (using the --histo command line option) and produce the
/// histograms of numbers of hits and candidates per trigger (as a root file and a PDF report)
/// and the bad burst lists. A burst can be classified as bad for two reasons:
/// 1) low statistics, or 2) anomalous in-time hit or candidate counts in control triggers
/// in at least one subdetector.
/// Important: data from different runs should be processed separately as BursID is used as
/// the unique identifier of the burst.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

NumberOfHitsMonitor::NumberOfHitsMonitor(Core::BaseAnalysis *ba) :
  Analyzer(ba, "NumberOfHitsMonitor") {

  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  RequestAllRecoTrees(false); // false = do not request special triggers
  fReadingData = kTRUE;
  fRunNumber = 0;

  // User-defined parameters for running over the data
  AddParam("MaxNBursts", &fMaxNBursts, 3000); // max number of bins in histograms

  fDetectorNames =
    {"Cedar", "CHANTI", "CHOD", "GigaTracker", "HAC",
     "IRC", "LAV", "LKr", "MUV0", "MUV1", "MUV2", "MUV3",
     "NewCHOD", "RICH", "SAC", "SAV", "Spectrometer"};
  fDetectorProducesCandidates =
    {true, true, true, true, true,
     false, true, true, false, true, true, true,
     false, true, false, true, true};
  fNDetectors = fDetectorNames.size();

  fHHitsPerBurst.clear(); fHHitsPerBurst.resize(fNDetectors);
  fHCandidatesPerBurst.clear(); fHCandidatesPerBurst.resize(fNDetectors);
  fHHitsPerControlEvent.clear(); fHHitsPerControlEvent.resize(fNDetectors);
  fHCandidatesPerControlEvent.clear(); fHCandidatesPerControlEvent.resize(fNDetectors);
  fHHitsInTimePerBurst.clear(); fHHitsInTimePerBurst.resize(fNDetectors);
  fHCandidatesInTimePerBurst.clear(); fHCandidatesInTimePerBurst.resize(fNDetectors);
  fHHitsInTimePerControlEvent.clear(); fHHitsInTimePerControlEvent.resize(fNDetectors);
  fHCandidatesInTimePerControlEvent.clear(); fHCandidatesInTimePerControlEvent.resize(fNDetectors);
  fMinHitsPerControlTrigger.clear(); fMinHitsPerControlTrigger.resize(fNDetectors);
  fMaxHitsPerControlTrigger.clear(); fMaxHitsPerControlTrigger.resize(fNDetectors);
  fMinCandidatesPerControlTrigger.clear(); fMinCandidatesPerControlTrigger.resize(fNDetectors);
  fMaxCandidatesPerControlTrigger.clear(); fMaxCandidatesPerControlTrigger.resize(fNDetectors);
  fBadBurst.clear(); fBadBurst.resize(fNDetectors);

  fDetectorNameBadPeriod.clear();
  fFirstBurstBadPeriod.clear();
  fLastBurstBadPeriod.clear();

  /////////////////////////////////////////////////////////////////////////
  // "Statistics" bad burst selection criteria for running on my own output

  fMinPhysicsEventsInBurst = 100; // bursts with fewer physics events are considered bad
  fMinControlEventsInBurst =  50; // bursts with fewer control events are considered bad
}

void NumberOfHitsMonitor::InitHist() {

  fReadingData = GetIsTree();

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;
    BookHisto("hTotalEventsPerBurst", new
    	      TH1F("TotalEventsPerBurst", "Total events per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hControlEventsPerBurst", new
	      TH1F("ControlEventsPerBurst", "Control events per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hPhysicsEventsPerBurst", new
	      TH1F("PhysicsEventsPerBurst", "Physics events per burst;Burst ID",
		   fMaxNBursts, -0.5, fMaxNBursts-0.5));

    for (auto name : fDetectorNames) {
      TString name1a = name + "HitsPerBurst";
      TString name2a = name + "CandidatesPerBurst";
      TString name3a = name + "HitsInTimePerBurst";
      TString name4a = name + "CandidatesInTimePerBurst";
      TString name1b = name + " hits in control events per burst;Burst ID";
      TString name2b = name + " candidates in control events per burst;Burst ID";
      TString name3b = name + " hits in control events (|t|<10ns) per burst;Burst ID";
      TString name4b = name + " candidates in control events (|t|<10ns) per burst;Burst ID";
      BookHisto(name1a, new TH1F(name1a, name1b, fMaxNBursts, -0.5, fMaxNBursts-0.5));
      BookHisto(name2a, new TH1F(name2a, name2b, fMaxNBursts, -0.5, fMaxNBursts-0.5));
      BookHisto(name3a, new TH1F(name3a, name3b, fMaxNBursts, -0.5, fMaxNBursts-0.5));
      BookHisto(name4a, new TH1F(name4a, name4b, fMaxNBursts, -0.5, fMaxNBursts-0.5));
      TString nameT1a = name + "HitTime";
      TString nameT2a = name + "CandidateTime";
      TString nameT1b = name + " hit time;Time [ns]";
      TString nameT2b = name + " candidate time;Time [ns]";
      BookHisto(nameT1a, new TH1F(nameT1a, nameT1b, 300, -150, 150));
      BookHisto(nameT2a, new TH1F(nameT2a, nameT2b, 300, -150, 150));
    }
  }

  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHTotalEventsPerBurst   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "TotalEventsPerBurst",   true));
    fHPhysicsEventsPerBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "PhysicsEventsPerBurst", true));
    fHControlEventsPerBurst = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ControlEventsPerBurst", true));
    for (UInt_t i=0; i<fDetectorNames.size(); i++) {
      fHHitsPerBurst[i] = (TH1F*)RequestHistogram
	(fAnalyzerName, fDetectorNames[i]+"HitsPerBurst", true);
      fHCandidatesPerBurst[i] = (TH1F*)RequestHistogram
	(fAnalyzerName, fDetectorNames[i]+"CandidatesPerBurst", true);
      fHHitsInTimePerBurst[i] = (TH1F*)RequestHistogram
	(fAnalyzerName, fDetectorNames[i]+"HitsInTimePerBurst", true);
      fHCandidatesInTimePerBurst[i] = (TH1F*)RequestHistogram
	(fAnalyzerName, fDetectorNames[i]+"CandidatesInTimePerBurst", true);
    }
  }
}

void NumberOfHitsMonitor::StartOfBurstUser() {
  if (!fReadingData) return;
  if (!fRunNumber) fRunNumber = GetRunID();
}

void NumberOfHitsMonitor::Process(Int_t) {
  if (!fReadingData) return; // no action if reading its own output in --histo mode

  Int_t BurstID = GetBurstID();
  FillHisto("hTotalEventsPerBurst", BurstID);
  if (TriggerConditions::GetInstance()->IsPhysicsTrigger(GetL0Data()))
    FillHisto("hPhysicsEventsPerBurst", BurstID);

  // Process control triggers only
  if (!TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data())) return;
  FillHisto("hControlEventsPerBurst", BurstID);

  Double_t RefTime = GetEventHeader()->GetFineTime() * TdcCalib;
  for (auto name : fDetectorNames) {
    TRecoVEvent *event = static_cast<TRecoVEvent*>(GetEvent(name));
    if (event->GetNHits()) FillHisto(name+"HitsPerBurst", BurstID, event->GetNHits());
    if (event->GetNCandidates()) FillHisto(name+"CandidatesPerBurst", BurstID, event->GetNCandidates());
    for (Int_t i=0; i<event->GetNHits(); i++) {
      TRecoVHit* hit = static_cast<TRecoVHit*>(event->GetHit(i));
      Double_t Time = hit->GetTime() - RefTime;
      FillHisto(name+"HitTime", Time);
      if (fabs(Time)<10.0) FillHisto(name+"HitsInTimePerBurst", BurstID);
    }
    for (Int_t i=0; i<event->GetNCandidates(); i++) {
      TRecoVCandidate* cand = static_cast<TRecoVCandidate*>(event->GetCandidate(i));
      Double_t Time = cand->GetTime() - RefTime;
      FillHisto(name+"CandidateTime", Time);
      if (fabs(Time)<10.0) FillHisto(name+"CandidatesInTimePerBurst", BurstID);
    }
  }
}

void NumberOfHitsMonitor::EndOfJobUser() {
  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }
  if (!fHControlEventsPerBurst) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
    return;
  }

  ////////////////////////////////////
  // Histo mode: analyze my own output

  fRunNumber = GetRunID(); // Extract the run number
  if (fRunNumber>=0) {
    cout << user_normal() << "Run number found: " << fRunNumber << endl;
  }
  else {
    cout << user_normal() << "Invalid run number (" << fRunNumber << "): exiting " << endl;
    return;
  }
  if (fHControlEventsPerBurst->Integral()<0.5) {
    cout << user_normal() << "No control triggers found: exiting" << endl;
    return;
  }
  fMaxNBursts = fHControlEventsPerBurst->GetNbinsX();

  ////////////////////////////////////////////
  // Build step-2 histograms for each detector

  for (auto histo : fHHitsInTimePerBurst) histo->Sumw2();
  for (auto histo : fHCandidatesInTimePerBurst) histo->Sumw2();

  for (UInt_t i=0; i<fDetectorNames.size(); i++) {
    TString name0 = Form("Run %d: ", fRunNumber);
    TString name1 = name0+fDetectorNames[i]+"HitsPerControlEvent";
    TString name2 = name0+fDetectorNames[i]+" hits per control trigger;Burst ID;Hits/trigger";
    TString name3 = name0+fDetectorNames[i]+"CandidatesPerControlEvent";
    TString name4 = name0+fDetectorNames[i]+" candidates per control trigger;Burst ID;Candidates/trigger";
    TString name5 = name0+fDetectorNames[i]+"HitsInTimePerControlEvent";
    TString name6 = name0+fDetectorNames[i]+" hits (|t|<10ns) per control trigger;Burst ID;Hits/trigger";
    TString name7 = name0+fDetectorNames[i]+"CandidatesInTimePerControlEvent";
    TString name8 = name0+fDetectorNames[i]+" candidates (|t|<10ns) per control trigger;Burst ID;Candidates/trigger";

    fHHitsPerControlEvent[i] = new TH1F(name1, name2, fMaxNBursts, -0.5, fMaxNBursts-0.5);
    fHCandidatesPerControlEvent[i] = new TH1F(name3, name4, fMaxNBursts, -0.5, fMaxNBursts-0.5);
    fHHitsPerControlEvent[i]->Divide(fHHitsPerBurst[i], fHControlEventsPerBurst);
    fHCandidatesPerControlEvent[i]->Divide(fHCandidatesPerBurst[i], fHControlEventsPerBurst);

    fHHitsInTimePerControlEvent[i] = new TH1F(name5, name6, fMaxNBursts, -0.5, fMaxNBursts-0.5);
    fHCandidatesInTimePerControlEvent[i] = new TH1F(name7, name8, fMaxNBursts, -0.5, fMaxNBursts-0.5);
    fHHitsInTimePerControlEvent[i]->Divide(fHHitsInTimePerBurst[i], fHControlEventsPerBurst);
    fHCandidatesInTimePerControlEvent[i]->Divide(fHCandidatesInTimePerBurst[i], fHControlEventsPerBurst);

    for (Int_t j=1; j<=fMaxNBursts; j++) {
      if (fHControlEventsPerBurst->GetBinContent(j)<fMinControlEventsInBurst) {
	fHHitsPerControlEvent[i]->SetBinContent(j, -1);
	fHHitsPerControlEvent[i]->SetBinError(j, 0);
        fHCandidatesPerControlEvent[i]->SetBinContent(j, -1);
        fHCandidatesPerControlEvent[i]->SetBinError(j, 0);
	fHHitsInTimePerControlEvent[i]->SetBinContent(j, -1);
	fHHitsInTimePerControlEvent[i]->SetBinError(j, 0);
        fHCandidatesInTimePerControlEvent[i]->SetBinContent(j, -1);
        fHCandidatesInTimePerControlEvent[i]->SetBinError(j, 0);
      }
    }
  }

  //////////////////////////////////////////////
  // Save step-2 histograms into the output file

  fHTotalEventsPerBurst->Write();
  fHControlEventsPerBurst->Write();
  for (UInt_t i=0; i<fDetectorNames.size(); i++) {
    fHHitsPerBurst[i]->Write();
    fHCandidatesPerBurst[i]->Write();
    fHHitsPerControlEvent[i]->Write();
    fHCandidatesPerControlEvent[i]->Write();
    fHHitsInTimePerBurst[i]->Write();
    fHCandidatesInTimePerBurst[i]->Write();
    fHHitsInTimePerControlEvent[i]->Write();
    fHCandidatesInTimePerControlEvent[i]->Write();
  }

  //////////////////////////
  // Produce bad burst lists

  Int_t NBursts = 0;
  Int_t NBadBursts = 0;

  cout << user_normal() << "Min/max limits for numbers of hits and candidates" << endl;
  TH1F *hHitsInTime[20], *hCandidatesInTime[20];
  TF1  *fHitsInTime[20], *fCandidatesInTime[20];
  Double_t MinHits[20], MaxHits[20], MinCand[20], MaxCand[20];
  for (UInt_t iDet=0; iDet<fNDetectors; iDet++) {
    TString name1 = "hHitsInTime_"       + fDetectorNames[iDet];
    TString name2 = "hCandidatesInTime_" + fDetectorNames[iDet];
    TString name3 = "fHitsInTime_"       + fDetectorNames[iDet];
    TString name4 = "fCandidatesInTime_" + fDetectorNames[iDet];

    // Determine the histogram ranges.
    // When computing the lower boundaries, excliude empty bins (with contents set to -1)
    MaxHits[iDet] =
      fHHitsInTimePerControlEvent[iDet]->
      GetBinContent(fHHitsInTimePerControlEvent[iDet]->GetMaximumBin());
    MaxCand[iDet] =
      fHCandidatesInTimePerControlEvent[iDet]->
      GetBinContent(fHCandidatesInTimePerControlEvent[iDet]->GetMaximumBin());
    MinHits[iDet] = MaxHits[iDet];
    MinCand[iDet] = MaxCand[iDet];
    for (Int_t iBur=0; iBur<fMaxNBursts; iBur++) {
      if (fHHitsInTimePerControlEvent[iDet]->GetBinContent(iBur+1) < MinHits[iDet] &&
	  fHHitsInTimePerControlEvent[iDet]->GetBinContent(iBur+1) > 0.0)
	MinHits[iDet] = fHHitsInTimePerControlEvent[iDet]->GetBinContent(iBur+1);
      if (fHCandidatesInTimePerControlEvent[iDet]->GetBinContent(iBur+1) < MinCand[iDet] &&
	  fHCandidatesInTimePerControlEvent[iDet]->GetBinContent(iBur+1) > 0.0)
	MinCand[iDet] = fHCandidatesInTimePerControlEvent[iDet]->GetBinContent(iBur+1);
    }
    cout << user_normal() << fRunNumber << " " << fDetectorNames[iDet] << " " <<
      MinHits[iDet] << " " << MaxHits[iDet] << " " << MinCand[iDet] << " " << MaxCand[iDet]<<endl;

    hHitsInTime[iDet]       = new TH1F(name1, name1, 40, MinHits[iDet], MaxHits[iDet]);
    hCandidatesInTime[iDet] = new TH1F(name2, name2, 40, MinCand[iDet], MaxCand[iDet]);
    fHitsInTime[iDet]       = new TF1(name3, "gaus", MinHits[iDet], MaxHits[iDet]);
    fCandidatesInTime[iDet] = new TF1(name4, "gaus", MinCand[iDet], MaxCand[iDet]);
  }

  Bool_t BadBurstStat[5000];
  for (Int_t iBur=0; iBur<fMaxNBursts; iBur++) { // loop over bursts
    BadBurstStat[iBur] = false;
    if (fHTotalEventsPerBurst->GetBinContent(iBur+1)<0.5) continue;
    NBursts++; // non-empty burst count

    Double_t NPhysicsEvents = fHPhysicsEventsPerBurst->GetBinContent(iBur+1);
    Double_t NControlEvents = fHControlEventsPerBurst->GetBinContent(iBur+1);
    if (NPhysicsEvents<fMinPhysicsEventsInBurst || NControlEvents<fMinControlEventsInBurst)
      BadBurstStat[iBur] = true;
    if (BadBurstStat[iBur]) continue;

    // Distributions of the numbers of in-time hits and candidates for non-empty bursts
    for (UInt_t iDet=0; iDet<fNDetectors; iDet++) {
      hHitsInTime[iDet]->Fill(fHHitsInTimePerControlEvent[iDet]->GetBinContent(iBur+1));
      hCandidatesInTime[iDet]->Fill(fHCandidatesInTimePerControlEvent[iDet]->GetBinContent(iBur+1));
    }
  }

  //////////////////////////////////////////////////////
  // Define the "good" range of hit and candidate counts

  for (UInt_t iDet=0; iDet<fNDetectors; iDet++) {
    fMinHitsPerControlTrigger[iDet] = -999;
    fMaxHitsPerControlTrigger[iDet] = +999;
    if (hHitsInTime[iDet]->Integral()<0.5) continue;
    Double_t mean = hHitsInTime[iDet]->GetMean();
    Double_t rms  = hHitsInTime[iDet]->GetRMS();
    hHitsInTime[iDet]->Fit(fHitsInTime[iDet], "0Q", "", mean-rms, mean+rms);
    fMinHitsPerControlTrigger[iDet] =
      fHitsInTime[iDet]->GetParameter(1) - 6.0*fHitsInTime[iDet]->GetParameter(2);
    fMaxHitsPerControlTrigger[iDet] =
      fHitsInTime[iDet]->GetParameter(1) + 6.0*fHitsInTime[iDet]->GetParameter(2);
    hHitsInTime[iDet]->Write();
    fHitsInTime[iDet]->Write();
  }

  for (UInt_t iDet=0; iDet<fNDetectors; iDet++) {
    fMinCandidatesPerControlTrigger[iDet] = -999;
    fMaxCandidatesPerControlTrigger[iDet] = +999;
    if (!fDetectorProducesCandidates[iDet]) continue;
    Double_t mean = hCandidatesInTime[iDet]->GetMean();
    Double_t rms  = hCandidatesInTime[iDet]->GetRMS();
    hCandidatesInTime[iDet]->Fit(fCandidatesInTime[iDet], "0Q", "", mean-rms, mean+rms);
    fMinCandidatesPerControlTrigger[iDet] =
      fCandidatesInTime[iDet]->GetParameter(1) - 6.0*fCandidatesInTime[iDet]->GetParameter(2);
    fMaxCandidatesPerControlTrigger[iDet] =
      fCandidatesInTime[iDet]->GetParameter(1) + 6.0*fCandidatesInTime[iDet]->GetParameter(2);
    hCandidatesInTime[iDet]->Write();
    fCandidatesInTime[iDet]->Write();
  }

  ///////////////////////////////////////////////////////
  // Read the Nhits/Ncandidates hand-made ranges for runs
  // where the automatic procedure fails

  TString BadBurstRangeFileName("NOHM-BadBurstRanges.dat");
  NA62ConditionsService::GetInstance()->Open(BadBurstRangeFileName);

  TString Line;
  Bool_t RunFound = false;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(BadBurstRangeFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    if (l->GetEntries()!=6) continue; // safety check
    Int_t ThisRun = ((TObjString*)(l->At(0)))->GetString().Atoi();
    TString ThisDetectorName;

    if (fRunNumber==ThisRun) {
      RunFound = true;
      ThisDetectorName = ((TObjString*)(l->At(1)))->GetString();
      // find the detector number
      Int_t ThisDetectorNumber = -1;
      for (Int_t i=0; i<20; i++) {
        if (fDetectorNames[i]==ThisDetectorName) ThisDetectorNumber = i;
      }
      if (ThisDetectorNumber>(-1)) {
        if (((TObjString*)(l->At(2)))->GetString().Atof()>(-1))
          fMinHitsPerControlTrigger[ThisDetectorNumber]       = ((TObjString*)(l->At(2)))->GetString().Atof();
        if (((TObjString*)(l->At(3)))->GetString().Atof()>(-1))
          fMaxHitsPerControlTrigger[ThisDetectorNumber]       = ((TObjString*)(l->At(3)))->GetString().Atof();
        if (((TObjString*)(l->At(4)))->GetString().Atof()>(-1))
          fMinCandidatesPerControlTrigger[ThisDetectorNumber] = ((TObjString*)(l->At(4)))->GetString().Atof();
        if (((TObjString*)(l->At(5)))->GetString().Atof()>(-1))
          fMaxCandidatesPerControlTrigger[ThisDetectorNumber] = ((TObjString*)(l->At(5)))->GetString().Atof();
      }
      else {
        cout << user_normal() << "Wrong detector name" << endl;
      }
    }
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(BadBurstRangeFileName);
  if (RunFound) {
    cout << user_normal() << "NHits and NCandidates range redefined for the run " << fRunNumber << endl;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Identify bad bursts due to anomalous hit/candidate counts in subdetectors

  Bool_t BadBurstDet[5000][20];
  for (Int_t iBur=0; iBur<fMaxNBursts; iBur++) { // loop over bursts
    for (UInt_t iDet=0; iDet<fNDetectors; iDet++) BadBurstDet[iBur][iDet] = false;
    if (fHTotalEventsPerBurst->GetBinContent(iBur+1)<0.5) continue;
    if (BadBurstStat[iBur]) continue;
    for (UInt_t iDet=0; iDet<fNDetectors; iDet++) {
      if (MaxHits[iDet]/MinHits[iDet]<1.03) continue; // stable operation
      if (fHHitsInTimePerControlEvent[iDet]->GetBinContent(iBur+1) < fMinHitsPerControlTrigger[iDet] ||
	  fHHitsInTimePerControlEvent[iDet]->GetBinContent(iBur+1) > fMaxHitsPerControlTrigger[iDet])
	BadBurstDet[iBur][iDet] = true;

      if (!fDetectorProducesCandidates[iDet]) continue; // no candidates produced in this subdetector
      if (MaxCand[iDet]/MinCand[iDet]<1.03) continue; // stable operation
      if (fHCandidatesInTimePerControlEvent[iDet]->GetBinContent(iBur+1) < fMinCandidatesPerControlTrigger[iDet] ||
	  fHCandidatesInTimePerControlEvent[iDet]->GetBinContent(iBur+1) > fMaxCandidatesPerControlTrigger[iDet])
	BadBurstDet[iBur][iDet] = true;
    }
  }

  /////////////////////////////////////////////
  // Identify bad bursts from bad burst periods

  Bool_t  BadPeriodsExist = false;
  Bool_t  BadPeriodRangeIncorrect = false;

  TString BadBurstPeriodsFileName("NOHM-BadBurstPeriods.dat");
  NA62ConditionsService::GetInstance()->Open(BadBurstPeriodsFileName);

  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(BadBurstPeriodsFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    if (l->GetEntries()<4) continue; // safety check
    Int_t     ThisRunNumber           = ((TObjString*)(l->At(0)))->GetString().Atoi();
    if (fRunNumber==ThisRunNumber) {
      Int_t   ThisFirstBurstBadPeriod = ((TObjString*)(l->At(1)))->GetString().Atoi();
      Int_t   ThisLastBurstBadPeriod  = ((TObjString*)(l->At(2)))->GetString().Atoi();
      // check if the bad period range is correct
      if (ThisFirstBurstBadPeriod>=fMaxNBursts || ThisFirstBurstBadPeriod>ThisLastBurstBadPeriod)
	BadPeriodRangeIncorrect = true; // set the flag just for printing

      for (Int_t i=3; i<l->GetEntries(); i++) {
	TString ThisDetectorName = ((TObjString*)(l->At(i)))->GetString();
	for (UInt_t iDet=0; iDet<fNDetectors; iDet++) { // loop over detectors
	  if ( (ThisDetectorName==fDetectorNames[iDet] || ThisDetectorName=="all") &&
	       ThisFirstBurstBadPeriod<fMaxNBursts && ThisFirstBurstBadPeriod<=ThisLastBurstBadPeriod) { // select only correct range
	    fDetectorNameBadPeriod.push_back(fDetectorNames[iDet]);
	    fFirstBurstBadPeriod.push_back(ThisFirstBurstBadPeriod);
	    fLastBurstBadPeriod.push_back(ThisLastBurstBadPeriod);
	  }
	} // end of loop over detectors
      }
    }
  }
  NA62ConditionsService::GetInstance()->Close(BadBurstPeriodsFileName);

  for (Int_t iBur=0; iBur<fMaxNBursts; iBur++) { // loop over bursts
    for (UInt_t iDet=0; iDet<fNDetectors; iDet++) { // loop over detectors
      for (UInt_t iLine=0; iLine<fFirstBurstBadPeriod.size(); iLine++) { // loop over the vector
        // identify bad bursts from bad periods
        if (fDetectorNameBadPeriod[iLine]==fDetectorNames[iDet] &&
          iBur>=fFirstBurstBadPeriod[iLine] && iBur<=fLastBurstBadPeriod[iLine]) {
          BadBurstDet[iBur][iDet] = true;
          BadPeriodsExist = true;
        }
      }
    } // end of loop over detectors
  } // end of loop over bursts

  if (BadPeriodsExist)
    cout << user_normal() << "Bad burst period exist(s) for this run" << endl;
  if (BadPeriodRangeIncorrect)
    cout << user_normal() << "incorrect range of bad burst periods" << endl;

  ///////////////////////
  // Print out bad bursts

  ofstream BadBurstFile;
  BadBurstFile.open("./NumberOfHitsMonitor_BadBursts.dat");

  for (Int_t iBur=0; iBur<fMaxNBursts; iBur++) { // loop over bursts
    if (fHTotalEventsPerBurst->GetBinContent(iBur+1)<0.5) continue;
    Bool_t BadBurstAnyDetector = false;
    for (UInt_t iDet=0; iDet<fNDetectors; iDet++) BadBurstAnyDetector |= BadBurstDet[iBur][iDet];
    if (!BadBurstStat[iBur] && !BadBurstAnyDetector) continue;
    NBadBursts++;
    TString s = Form("BadBurst %06d %04d", fRunNumber, iBur);
    if (BadBurstStat[iBur]) s += " Statistics";
    for (UInt_t iDet=0; iDet<fNDetectors; iDet++) {
      if (BadBurstDet[iBur][iDet]) s += (" "+fDetectorNames[iDet]);
    }
    BadBurstFile << s << endl;
  }

  BadBurstFile.close();
  cout << user_normal() << "Total = good + bad bursts: " <<
    NBursts << " = " << NBursts-NBadBursts << " + " << NBadBursts << endl;

  ////////////////////////////
  // Produce a PDF report file

  BuildPDFReport();
}

void NumberOfHitsMonitor::BuildPDFReport() {
  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  Int_t MinBurst = 0;
  Int_t MaxBurst = fMaxNBursts;
  if (fHControlEventsPerBurst->Integral()) {
    while (!fHControlEventsPerBurst->GetBinContent(MinBurst+1)) MinBurst++;
    while (!fHControlEventsPerBurst->GetBinContent(MaxBurst+1)) MaxBurst--;
  }
  fHTotalEventsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHPhysicsEventsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  fHControlEventsPerBurst->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
  for (UInt_t i=0; i<fDetectorNames.size(); i++) {
    fHHitsPerBurst[i]->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHCandidatesPerBurst[i]->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHHitsPerControlEvent[i]->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHCandidatesPerControlEvent[i]->GetXaxis()->SetRangeUser(MinBurst-0.5, MaxBurst+0.5);
    fHHitsPerControlEvent[i]->SetMinimum(0.0);
    fHCandidatesPerControlEvent[i]->SetMinimum(0.0);
  }

  TCanvas *Canvas0 = new TCanvas("Canvas0");
  Canvas0->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
  Canvas0->SetLogy();
  Canvas0->SetLeftMargin(0.07);
  Canvas0->SetRightMargin(0.01);
  Canvas0->SetBottomMargin(0.08);

  gStyle->SetTitleX(0.05);
  gStyle->SetTitleAlign(13); // move histogram titles to the left to make room for legends
  fHTotalEventsPerBurst->SetLineColor(kBlue);
  fHTotalEventsPerBurst->SetMarkerColor(kBlue);
  fHTotalEventsPerBurst->SetTitle(Form("Run %d: total, physics & control triggers", fRunNumber));
  fHTotalEventsPerBurst->SetMinimum(0.9);
  fHTotalEventsPerBurst->GetYaxis()->SetTitle("Number of events");
  fHTotalEventsPerBurst->GetYaxis()->SetTitleOffset(0.9);
  fHTotalEventsPerBurst->Draw("p");
  fHPhysicsEventsPerBurst->SetLineColor(kRed);
  fHPhysicsEventsPerBurst->SetMarkerColor(kRed);
  fHPhysicsEventsPerBurst->Draw("p same");
  fHControlEventsPerBurst->SetLineColor(kGreen+2);
  fHControlEventsPerBurst->SetMarkerColor(kGreen+2);
  fHControlEventsPerBurst->Draw("p same");

  // Indicate the bad burst criteria
  TLine *l = new TLine();
  l->SetLineWidth(1);
  l->SetLineColor(kRed);
  l->DrawLine(MinBurst-0.5, fMinPhysicsEventsInBurst, MaxBurst+0.5, fMinPhysicsEventsInBurst);
  l->SetLineColor(kGreen+2);
  l->DrawLine(MinBurst-0.5, fMinControlEventsInBurst, MaxBurst+0.5, fMinControlEventsInBurst);

  TLine *l2 = new TLine();
  l2->SetLineWidth(1);
  l2->SetLineColor(kMagenta);

  TBox *box = new TBox();
  box->SetFillColorAlpha(kGray, 0.7);

  TLegend *leg1 = new TLegend(0.78,0.90,1.00,1.00);
  leg1->SetFillColor(0);
  leg1->AddEntry(fHTotalEventsPerBurst,   "Total triggers", "pl");
  leg1->AddEntry(fHPhysicsEventsPerBurst, "Physics events", "pl");
  leg1->AddEntry(fHControlEventsPerBurst, "Control events", "pl");
  leg1->Draw();
  Canvas0->Print(OutputPDFFileName, "pdf");

  // Subdetector stability plots vs burst ID
  TCanvas *Canvas1 = new TCanvas("Canvas");
  Canvas1->SetLeftMargin(0.07);
  Canvas1->SetRightMargin(0.01);
  TLegend *leg2 = new TLegend(0.87,0.94,1.00,1.00);
  leg2->SetFillColor(0);
  leg2->AddEntry(fHHitsPerControlEvent[0], "All", "pl");
  leg2->AddEntry(fHHitsInTimePerControlEvent[0], "In-time", "pl");

  for (UInt_t i=0; i<fDetectorNames.size(); i++) {

    // Plot number of hits/trigger
    fHHitsPerControlEvent[i]->SetLineColor(kRed);
    fHHitsPerControlEvent[i]->SetMarkerColor(kRed);
    fHHitsInTimePerControlEvent[i]->SetLineColor(kBlue);
    fHHitsInTimePerControlEvent[i]->SetMarkerColor(kBlue);
    fHHitsPerControlEvent[i]->GetYaxis()->SetTitleOffset(0.8);
    fHHitsPerControlEvent[i]->Draw();
    fHHitsInTimePerControlEvent[i]->Draw("same");
    l->DrawLine(MinBurst-0.5, fMinHitsPerControlTrigger[i], MaxBurst+0.5, fMinHitsPerControlTrigger[i]);
    l->DrawLine(MinBurst-0.5, fMaxHitsPerControlTrigger[i], MaxBurst+0.5, fMaxHitsPerControlTrigger[i]);
    // draw the lines for the bad burst period(s)
    for (UInt_t iLine=0; iLine<fFirstBurstBadPeriod.size(); iLine++) {
      if (fDetectorNameBadPeriod[iLine]==fDetectorNames[i]) {
	Double_t ThisFirstBurstBadPeriod = fFirstBurstBadPeriod[iLine] - 0.4;
	Double_t ThisLastBurstBadPeriod  = fLastBurstBadPeriod[iLine]  + 0.4;
	if (ThisFirstBurstBadPeriod < MinBurst) ThisFirstBurstBadPeriod = MinBurst - 0.4; // correct the left border if necessary
	if (ThisLastBurstBadPeriod  > MaxBurst) ThisLastBurstBadPeriod  = MaxBurst + 0.4; // correct the right border if necessary
	Double_t UpperLimit = (fHHitsPerControlEvent[i]->GetBinContent(fHHitsPerControlEvent[i]->GetMaximumBin()) +
			       fHHitsPerControlEvent[i]->GetBinError(fHHitsPerControlEvent[i]->GetMaximumBin())) * 1.05;
	fHHitsPerControlEvent[i]->SetMaximum(UpperLimit);
	l2->DrawLine(ThisFirstBurstBadPeriod,  fHHitsPerControlEvent[i]->GetMinimum(), ThisFirstBurstBadPeriod, UpperLimit);
	l2->DrawLine(ThisLastBurstBadPeriod,   fHHitsPerControlEvent[i]->GetMinimum(), ThisLastBurstBadPeriod,  UpperLimit);
	box->DrawBox(ThisFirstBurstBadPeriod,  fHHitsPerControlEvent[i]->GetMinimum(), ThisLastBurstBadPeriod,  UpperLimit);
      }
    }

    leg2->Draw();
    Canvas1->Print(OutputPDFFileName, "pdf");

    // Plot number of candidates/trigger
    if (!fDetectorProducesCandidates[i]) continue;
    fHCandidatesPerControlEvent[i]->SetLineColor(kRed);
    fHCandidatesPerControlEvent[i]->SetMarkerColor(kRed);
    fHCandidatesInTimePerControlEvent[i]->SetLineColor(kBlue);
    fHCandidatesInTimePerControlEvent[i]->SetMarkerColor(kBlue);
    fHCandidatesPerControlEvent[i]->GetYaxis()->SetTitleOffset(0.8);
    fHCandidatesPerControlEvent[i]->Draw();
    fHCandidatesInTimePerControlEvent[i]->Draw("same");
    l->DrawLine(MinBurst-0.5, fMinCandidatesPerControlTrigger[i], MaxBurst+0.5, fMinCandidatesPerControlTrigger[i]);
    l->DrawLine(MinBurst-0.5, fMaxCandidatesPerControlTrigger[i], MaxBurst+0.5, fMaxCandidatesPerControlTrigger[i]);
    // draw the lines for the bad burst period(s)
    for (UInt_t iLine=0; iLine<fFirstBurstBadPeriod.size(); iLine++) {
      if (fDetectorNameBadPeriod[iLine]==fDetectorNames[i]) {
        Double_t ThisFirstBurstBadPeriod = fFirstBurstBadPeriod[iLine] - 0.4;
        Double_t ThisLastBurstBadPeriod  = fLastBurstBadPeriod[iLine]  + 0.4;
        if (ThisFirstBurstBadPeriod < MinBurst) ThisFirstBurstBadPeriod = MinBurst - 0.4; // correct the left border if necessary
        if (ThisLastBurstBadPeriod  > MaxBurst) ThisLastBurstBadPeriod  = MaxBurst + 0.4; // correct the right border if necessary
        Double_t UpperLimit = (fHCandidatesPerControlEvent[i]->GetBinContent(fHCandidatesPerControlEvent[i]->GetMaximumBin()) +
                               fHCandidatesPerControlEvent[i]->GetBinError(fHCandidatesPerControlEvent[i]->GetMaximumBin())) * 1.05;
        fHCandidatesPerControlEvent[i]->SetMaximum(UpperLimit);
        l2->DrawLine(ThisFirstBurstBadPeriod,  fHCandidatesPerControlEvent[i]->GetMinimum(), ThisFirstBurstBadPeriod, UpperLimit);
        l2->DrawLine(ThisLastBurstBadPeriod,   fHCandidatesPerControlEvent[i]->GetMinimum(), ThisLastBurstBadPeriod,  UpperLimit);
        box->DrawBox(ThisFirstBurstBadPeriod,  fHCandidatesPerControlEvent[i]->GetMinimum(), ThisLastBurstBadPeriod,  UpperLimit);
      }
    }

    leg2->Draw();
    Canvas1->Print(OutputPDFFileName, "pdf");
  }

  Canvas0->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  delete Canvas0;
  delete Canvas1;
  delete leg1;
  delete leg2;
  delete box;
  delete l;
  delete l2;
}
