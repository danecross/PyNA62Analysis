// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-05
//
// ---------------------------------------------------------------

#include "DetectorOnOffMonitor.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class DetectorOnOffMonitor
/// \Brief
/// Check the integrated numbers of hits & candidates in each subdetector
/// \EndBrief
/// \Detailed
/// The analyzer can be run in two modes: 1) read the reconstructed data and produce the intermediate output;
/// 2) read its own intermediate output (using the --histo command line option) and produce the final
/// histograms of numbers of candidates and hits per trigger (as a root file and a PDF report).
/// Four histograms are created are printed into a pdf file: the numbers of hits and candidates in each subdetector,
/// raw and normalised to the number of events.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

DetectorOnOffMonitor::DetectorOnOffMonitor(Core::BaseAnalysis *ba) : Analyzer(ba, "DetectorOnOffMonitor") {
  RequestAllRecoTrees();
}

void DetectorOnOffMonitor::InitHist() {
  fReadingData = GetIsTree();

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;

    BookHisto("RecoHitsInDetectors",
	      new TH1F("RecoHitsInDetectors", "RecoHitsInDetectors", 21, -0.5, 20.5));
    BookHisto("CandidatesInDetectors",
	      new TH1F("CandidatesInDetectors", "CandidatesInDetectors", 21, -0.5, 20.5));

    // Detector IDs are 1-20; first bin (ID=0) is used to count events
    TString DetectorNames[21] =
      {"Events",
       "KTAG", "GTK", "CHANTI", "LAV", "Spectrometer", "CHOD", "RICH", "IRC",
       "LKr", "MUV1", "MUV2", "MUV3", "SAC", "NewCHOD", "HAC",
       "L0TP", "L1TP", "L2EB", "DIM", "MUV0"};
    for (Int_t i=0; i<21; i++) {
      fHisto.GetHisto("RecoHitsInDetectors")  ->GetXaxis()->SetBinLabel(i+1, DetectorNames[i]);
      fHisto.GetHisto("CandidatesInDetectors")->GetXaxis()->SetBinLabel(i+1, DetectorNames[i]);
    }
  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHRecoHitsInDetectors   = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "RecoHitsInDetectors",   true));
    fHCandidatesInDetectors = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "CandidatesInDetectors", true));
  }
}

void DetectorOnOffMonitor::Process(int) {
  if (!fReadingData) return; // no action if reading its own output in --histo mode

  TRecoCedarEvent*        CEDARevent   = GetEvent<TRecoCedarEvent>();
  TRecoCHANTIEvent*       CHANTIevent  = GetEvent<TRecoCHANTIEvent>();
  TRecoCHODEvent*         CHODevent    = GetEvent<TRecoCHODEvent>();
  TRecoGigaTrackerEvent*  GTKevent     = GetEvent<TRecoGigaTrackerEvent>();
  TRecoHACEvent*          HACevent     = GetEvent<TRecoHACEvent>();
  TRecoIRCEvent*          IRCevent     = GetEvent<TRecoIRCEvent>();
  TRecoLAVEvent*          LAVevent     = GetEvent<TRecoLAVEvent>();
  TRecoLKrEvent*          LKRevent     = GetEvent<TRecoLKrEvent>();
  TRecoMUV0Event*         MUV0event    = GetEvent<TRecoMUV0Event>();
  TRecoMUV1Event*         MUV1event    = GetEvent<TRecoMUV1Event>();
  TRecoMUV2Event*         MUV2event    = GetEvent<TRecoMUV2Event>();
  TRecoMUV3Event*         MUV3event    = GetEvent<TRecoMUV3Event>();
  TRecoNewCHODEvent*      NewCHODevent = GetEvent<TRecoNewCHODEvent>();
  TRecoRICHEvent*         RICHevent    = GetEvent<TRecoRICHEvent>();
  TRecoSACEvent*          SACevent     = GetEvent<TRecoSACEvent>();
  TRecoSpectrometerEvent* STRAWevent   = GetEvent<TRecoSpectrometerEvent>();

  FillHisto("RecoHitsInDetectors", 0.0); // event counter

  if (CEDARevent)   FillHisto("RecoHitsInDetectors", kCedar,        CEDARevent->GetNHits());
  if (CHANTIevent)  FillHisto("RecoHitsInDetectors", kCHANTI,       CHANTIevent->GetNHits());
  if (CHODevent)    FillHisto("RecoHitsInDetectors", kCHOD,         CHODevent->GetNHits());
  if (GTKevent)     FillHisto("RecoHitsInDetectors", kGigaTracker,  GTKevent->GetNHits());
  if (HACevent)     FillHisto("RecoHitsInDetectors", kHAC,          HACevent->GetNHits());
  if (IRCevent)     FillHisto("RecoHitsInDetectors", kIRC,          IRCevent->GetNHits());
  if (LAVevent)     FillHisto("RecoHitsInDetectors", kLAV,          LAVevent->GetNHits());
  if (LKRevent)     FillHisto("RecoHitsInDetectors", kLKr,          LKRevent->GetNHits());
  if (MUV0event)    FillHisto("RecoHitsInDetectors", kMUV0,         MUV0event->GetNHits());
  if (MUV1event)    FillHisto("RecoHitsInDetectors", kMUV1,         MUV1event->GetNHits());
  if (MUV2event)    FillHisto("RecoHitsInDetectors", kMUV2,         MUV2event->GetNHits());
  if (MUV3event)    FillHisto("RecoHitsInDetectors", kMUV3,         MUV3event->GetNHits());
  if (NewCHODevent) FillHisto("RecoHitsInDetectors", kNewCHOD,      NewCHODevent->GetNHits());
  if (RICHevent)    FillHisto("RecoHitsInDetectors", kRICH,         RICHevent->GetNHits());
  if (SACevent)     FillHisto("RecoHitsInDetectors", kSAC,          SACevent->GetNHits());
  if (STRAWevent)   FillHisto("RecoHitsInDetectors", kSpectrometer, STRAWevent->GetNHits());

  if (CEDARevent)   FillHisto("CandidatesInDetectors", kCedar,        CEDARevent->GetNCandidates());
  if (CHANTIevent)  FillHisto("CandidatesInDetectors", kCHANTI,       CHANTIevent->GetNCandidates());
  if (CHODevent)    FillHisto("CandidatesInDetectors", kCHOD,         CHODevent->GetNCandidates());
  if (GTKevent)     FillHisto("CandidatesInDetectors", kGigaTracker,  GTKevent->GetNCandidates());
  if (HACevent)     FillHisto("CandidatesInDetectors", kHAC,          HACevent->GetNCandidates());
  if (IRCevent)     FillHisto("CandidatesInDetectors", kIRC,          IRCevent->GetNCandidates());
  if (LAVevent)     FillHisto("CandidatesInDetectors", kLAV,          LAVevent->GetNCandidates());
  if (LKRevent)     FillHisto("CandidatesInDetectors", kLKr,          LKRevent->GetNCandidates());
  if (MUV0event)    FillHisto("CandidatesInDetectors", kMUV0,         MUV0event->GetNCandidates());
  if (MUV1event)    FillHisto("CandidatesInDetectors", kMUV1,         MUV1event->GetNCandidates());
  if (MUV2event)    FillHisto("CandidatesInDetectors", kMUV2,         MUV2event->GetNCandidates());
  if (MUV3event)    FillHisto("CandidatesInDetectors", kMUV3,         MUV3event->GetNCandidates());
  if (NewCHODevent) FillHisto("CandidatesInDetectors", kNewCHOD,      NewCHODevent->GetNCandidates());
  if (RICHevent)    FillHisto("CandidatesInDetectors", kRICH,         RICHevent->GetNCandidates());
  if (SACevent)     FillHisto("CandidatesInDetectors", kSAC,          SACevent->GetNCandidates());
  if (STRAWevent)   FillHisto("CandidatesInDetectors", kSpectrometer, STRAWevent->GetNCandidates());
}

void DetectorOnOffMonitor::EndOfJobUser() {
  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }
  if (!fHRecoHitsInDetectors) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
    return;
  }

  ///////////////////////////////////////////////////
  // Histo mode: save histograms into the output file

  fHRecoHitsInDetectors->Write();
  fHCandidatesInDetectors->Write();

  /////////////////////////////////////
  // Histo mode: analyze the histograms

  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(0);
  TCanvas *Canvas = new TCanvas(fAnalyzerName);

  Canvas->Print(fAnalyzerName+".pdf[", "pdf"); // open file

  fHRecoHitsInDetectors->SetLineWidth(2);
  fHRecoHitsInDetectors->SetLineColor(kBlue);
  fHRecoHitsInDetectors->SetMarkerColor(kBlue);
  fHRecoHitsInDetectors->SetMarkerStyle(kFullCircle);
  fHCandidatesInDetectors->SetLineWidth(2);
  fHCandidatesInDetectors->SetLineColor(kRed);
  fHCandidatesInDetectors->SetMarkerColor(kRed);
  fHCandidatesInDetectors->SetMarkerStyle(kFullSquare);

  // Plot the initial histograms
  Canvas->SetLogy();
  Canvas->SetGridx();
  fHRecoHitsInDetectors->SetMinimum(1.0);
  fHRecoHitsInDetectors->SetTitle("Numbers of RecoHits and Candidates");
  fHRecoHitsInDetectors->Draw();
  fHCandidatesInDetectors->Draw("same");
  Canvas->Print(fAnalyzerName+".pdf", "pdf");

  // Plots histograms normalized per event
  Double_t SF = fHRecoHitsInDetectors->GetBinContent(1);
  if (SF>0) SF = 1.0/SF;
  fHRecoHitsInDetectors->Scale(SF);
  fHCandidatesInDetectors->Scale(SF);
  fHRecoHitsInDetectors->SetTitle("Numbers of RecoHits and Candidates per event");
  Canvas->SetLogy(0);
  fHRecoHitsInDetectors->SetMinimum(0);
  fHRecoHitsInDetectors->Draw();
  fHCandidatesInDetectors->Draw("same");
  Canvas->Print(fAnalyzerName+".pdf", "pdf");

  Canvas->Print(fAnalyzerName+".pdf]", "pdf"); // close file

  gErrorIgnoreLevel = -1; // restore the default
}
