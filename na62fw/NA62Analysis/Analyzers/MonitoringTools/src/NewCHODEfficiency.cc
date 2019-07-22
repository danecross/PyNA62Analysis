// ---------------------------------------------------------------
//
// History:
//
// Created by Lubos Bician (lubos.bician@cern.ch) 2016-02-16
// Refurbushed by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk), 2016-12-14
//
// ---------------------------------------------------------------

/// \class NewCHODEfficiency
/// \Brief
/// Tool for NewCHOD efficiency evaluation
/// \EndBrief
/// \Detailed
/// The tool uses Spectrometer and CHOD information to select sample of charged particles,
/// whose trajectories are extrapolated into the NewCHOD plane and the
/// tile-by-tile efficiency of the NewCHOD subdetector is calculated.
/// The tool uses the standard SpectrometerNewCHODAssociation class.
/// The analyzer can be run in two modes:
/// 1) read reconstructed data;
/// 2) in the HISTO mode (using the --histo command line option), it reads its own
/// output and produces a report in the form of a PDF file, as well as a printput
/// of integrated efficiencies for each burst.
/// \author Lubos Bician (lubos.bician@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TLatex.h>
#include <TColor.h>
#include <TBox.h>
#include "NewCHODEfficiency.hh"
#include "BaseAnalysis.hh"
#include "ConfigSettings.hh"

#include "TRecoCHODEvent.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

NewCHODEfficiency::NewCHODEfficiency(Core::BaseAnalysis *ba) :
  Analyzer(ba, "NewCHODEfficiency") {

  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  RequestTree("CHOD", new TRecoCHODEvent);
  RequestL0Data();

  fCHODZPos    = GeometricAcceptance::GetInstance()->GetZCHODHPlane();
  fNewCHODZPos = GeometricAcceptance::GetInstance()->GetZNewCHOD();
  fNewCHODRmin = GeometricAcceptance::GetInstance()->GetNewCHODRmin();
  fNewCHODRmax = GeometricAcceptance::GetInstance()->GetNewCHODRmax();

  fOutPDFFileName = fAnalyzerName + ".pdf";
  AddParam("MaxNBursts", &fMaxNBursts, 5000); // max number of bins in histograms
  fNewCHODGeometry = new NewCHODGeometry();
}

NewCHODEfficiency::~NewCHODEfficiency() {
  delete fNewCHODGeometry;
}

void NewCHODEfficiency::InitHist() {

  fReadingData = GetIsTree();

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;

    BookHisto(new TH2F("hMatched",    "hMatched",    20, 0, 100, 339, 100, 439));
    BookHisto(new TH2F("hExpected",   "hExpected",   20, 0, 100, 339, 100, 439));
    BookHisto(new TH2F("hEfficiency", "hEfficiency", 20, 0, 100, 339, 100, 439));
    BookHisto(new TH1F("hTracksChi2", "hTracksChi2", 100, 0, 100));
    BookHisto(new TH1F("hTracksMomentum", "hTracksMomentum", 1500, 0, 150));

    BookHisto("hMatchedVsBurstID", new
	      TH1F("hMatchedVsBurstID", "hMatchedVsBurstID", fMaxNBursts, -0.5, fMaxNBursts-0.5));
    BookHisto("hExpectedVsBurstID", new
	      TH1F("hExpectedVsBurstID", "hExpectedVsBurstID", fMaxNBursts, -0.5, fMaxNBursts-0.5));

    BookHisto(new TH2F("hDeltaTLooseHits",
		       "Time difference between two channels for loose hits in the same tile;#Deltat (higher channel - lower channel) [ns];Tile ID",
		       200, -50., 50., 350, 100, 450));
    BookHisto(new TH1F("hDeltaTCHODNewCHOD",
		       "Time difference between CHOD and NewCHOD; t_{CHOD} - t_{NewCHOD} [ns];Entries/1ns",
		       200, -100., 100.));
    BookHisto(new TH1F("hDeltaTTriggerNewCHOD",
		       "Time difference between Trigger and NewCHOD;t_{Trigger} - t_{NewCHOD} [ns];Entries/1ns",
		       200, -100., 100.));
    BookHisto(new TH1F("hDeltaTTriggerCHOD",
		       "Time difference between Trigger and CHOD;t_{Trigger} - t_{CHOD} [ns];Entries/1ns",
		       200, -100., 100.));


    // Set up the online monitor
    CreateCanvas("NewCHODEfficiencyCanvas");
    PlacePlotOnCanvas("hEfficiency",	  "NewCHODEfficiencyCanvas");
    PlacePlotOnCanvas("hMatched",	  "NewCHODEfficiencyCanvas");
    PlacePlotOnCanvas("hExpected",	  "NewCHODEfficiencyCanvas");
    PlacePlotOnCanvas("hDeltaTLooseHits", "NewCHODEfficiencyCanvas");
    SetUpdateInterval(50000);
  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fhMatched    = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hMatched",    true));
    fhExpected   = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hExpected",   true));
    fhEfficiency = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEfficiency", true));
    fhMatchedVsBurstID  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hMatchedVsBurstID",  true));
    fhExpectedVsBurstID = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hExpectedVsBurstID", true));
    fhDeltaT            = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hDeltaTLooseHits",   true));
    fhDeltaTCHODNewCHOD = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hDeltaTCHODNewCHOD", true));
  }
}

void NewCHODEfficiency::Process(Int_t) {

  if (!fReadingData) return;
  if (!(GetL0Data()->GetDataType() & 0x10)) return; // process control triggers only
  Double_t TriggerTime = GetL0Data()->GetReferenceFineTime()*TdcCalib;

  Int_t BurstID = GetBurstID();
  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (Tracks.size()!=1) return;

  Double_t TrackMomentum = Tracks[0].GetMomentum();
  Double_t TrackChi2     = Tracks[0].GetChi2();
  FillHisto("hTracksMomentum", TrackMomentum/1e3);
  if (TrackMomentum<5000. || TrackMomentum>150000.) return;
  FillHisto("hTracksChi2", TrackChi2);
  if (TrackChi2>10.0) return;

  TVector2 TrackPositionAtNewCHOD =
    TVector2(Tracks[0].xAt(fNewCHODZPos), Tracks[0].yAt(fNewCHODZPos));

  // Acceptance checks
  Bool_t InCHODAccept = GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kCHOD);
  Bool_t InSTRAWAccept = true;
  for (Int_t ich=0; ich<4; ich++)
    InSTRAWAccept &= GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, ich);
  Double_t SearchRadius = Tracks[0].GetNewCHODSearchRadius();
  Bool_t InNewCHODAccept = GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kNewCHOD, 0, fNewCHODRmin + SearchRadius, fNewCHODRmax - SearchRadius);

  if (!InCHODAccept)    return;
  if (!InSTRAWAccept)   return;
  if (!InNewCHODAccept) return;

  // Apply selection cuts and fill Matched and Expected histograms
  TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();
  if (CHODEvent->GetNCandidates()!=1) return;
  if (!Tracks[0].CHODAssociationExists()) return;
  Double_t CHODTime = Tracks[0].GetCHODTime();
  FillHisto("hDeltaTTriggerCHOD", TriggerTime-CHODTime);
  if (fabs(TriggerTime - CHODTime + 2.)>5.) return;

  Int_t TrackTileID = fNewCHODGeometry->GetTileID(TrackPositionAtNewCHOD);
  if (TrackTileID==999) return; // 999 = track outside NewCHOD acceptance

  // Fill the "expected hits" histograms
  FillHisto("hExpected", 1e-3*TrackMomentum, TrackTileID);
  FillHisto("hExpectedVsBurstID", BurstID);

  if (Tracks[0].NewCHODAssociationExists()) {

    // Fill DeltaT histogram for pairs of loose hits in the same tile
    UInt_t NNewCHODAssos = Tracks[0].GetNNewCHODAssociationRecords();
    if (NNewCHODAssos>=2) {
      for (UInt_t iHit1=0; iHit1<NNewCHODAssos; iHit1++) {
	for (UInt_t iHit2=iHit1+1; iHit2<NNewCHODAssos; iHit2++) {
	  TRecoNewCHODHit *Hit1 = Tracks[0].GetNewCHODCandidate(iHit1);
	  TRecoNewCHODHit *Hit2 = Tracks[0].GetNewCHODCandidate(iHit2);
	  if (Hit1->GetType()==kLooseCandidate && Hit2->GetType()==kLooseCandidate &&
	      Hit1->GetTileID()==Hit2->GetTileID() &&
	      Hit1->GetChannel1()!=Hit2->GetChannel1()) {
	    Double_t t1 = Tracks[0].GetNewCHODCandidateTime(iHit1);
	    Double_t t2 = Tracks[0].GetNewCHODCandidateTime(iHit2);
	    Double_t T1 = (Hit2->GetChannel1()>Hit1->GetChannel1()) ? t1 : t2;
	    Double_t T2 = (Hit2->GetChannel1()>Hit1->GetChannel1()) ? t2 : t1;
	    FillHisto("hDeltaTLooseHits", T2-T1, Hit1->GetTileID());
	  }
	}
      }
    }

    // Get NewCHOD time as the closest time to the trigger time
    Double_t NewCHODTime = 9999.;
    for (UInt_t iAsso = 0; iAsso<NNewCHODAssos; iAsso++) {
      Double_t time = Tracks[0].GetNewCHODCandidateTime(iAsso);
      if (fabs(time-TriggerTime) < fabs(NewCHODTime-TriggerTime)) {
	NewCHODTime = time;
      }
    }

    // Fill the "matched hits" histograms
    FillHisto("hDeltaTCHODNewCHOD", CHODTime-NewCHODTime);
    FillHisto("hDeltaTTriggerNewCHOD", TriggerTime-NewCHODTime);
    if (fabs(CHODTime-NewCHODTime)<10.0) {
      FillHisto("hMatched", 1e-3*TrackMomentum, TrackTileID);
      FillHisto("hMatchedVsBurstID", BurstID);
    }
  }
}

void NewCHODEfficiency::EndOfJobUser() {
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  ////////////
  // DATA mode

  if (fReadingData) {
    fhMatched  = static_cast<TH2F*>(fHisto.GetTH2("hMatched"));
    fhExpected = static_cast<TH2F*>(fHisto.GetTH2("hExpected"));
    fhEfficiency = static_cast<TH2F*>(fHisto.GetTH2("hEfficiency"));
    fhDeltaT	 = static_cast<TH2F*>(fHisto.GetTH2("hDeltaTLooseHits"));
    fhDeltaTCHODNewCHOD	= static_cast<TH1F*>(fHisto.GetTH1("hDeltaTCHODNewCHOD"));

    fhMatched->SetOption("colz");
    fhMatched->SetTitle("Matched tracks");
    fhMatched->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhMatched->GetYaxis()->SetTitle("Tile ID");
    fhMatched->SetStats(0);

    fhExpected->SetOption("colz");
    fhExpected->SetTitle("Expected tracks");
    fhExpected->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhExpected->GetYaxis()->SetTitle("Tile ID");
    fhExpected->SetStats(0);

    fhEfficiency->Divide(fhMatched, fhExpected, 1., 1., "B");
    fhEfficiency->SetTitle("NewCHOD efficiency");
    fhEfficiency->SetOption("colz");
    fhEfficiency->GetXaxis()->SetTitle("Track momentum [GeV/c]");
    fhEfficiency->GetYaxis()->SetTitle("Tile ID");
    fhEfficiency->SetStats(0);

    fhDeltaT->SetOption("colz");
    fhDeltaT->SetStats(0);
    fhDeltaTCHODNewCHOD->SetStats(0);
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
    BadBurstFile.open("./NewCHODEfficiency_BadBursts.dat");
    fhEffVsBurstID = new TGraphErrors();
    Int_t minbin = 99999, maxbin = -99999, goodbins = 0;
    for (Int_t i=1; i<=fMaxNBursts; i++) {
      Double_t n = fhMatchedVsBurstID->GetBinContent(i);
      Double_t N = fhExpectedVsBurstID->GetBinContent(i);
      if (N<0.5) continue;
      goodbins++;
      if (i<minbin) minbin = i;
      if (i>maxbin) maxbin = i;
      Double_t e = n/N;
      Double_t de = sqrt(e*(1.0-e)/N);
      fhEffVsBurstID->Set(fhEffVsBurstID->GetN()+1);
      fhEffVsBurstID->SetPoint(fhEffVsBurstID->GetN()-1, i-1, e); // x,y
      fhEffVsBurstID->SetPointError(fhEffVsBurstID->GetN()-1, 0.0, de); // dx,dy
      if (e<0.96) BadBurstFile << Form("BadBurst %06d %04d\n", GetRunID(), i-1);
    }
    BadBurstFile.close();
    fhEffVsBurstID->SetTitle("NewCHOD efficiency vs burst ID");
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
    fCanvas = new TCanvas("Canvas");
    fhEfficiency->SetTitle("NewCHOD Efficiency vs TileID vs track momentum");
    fhEfficiency->Draw();
    fCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas

    Double_t MinY = 0.;

    // Cumulative efficiency vs Track momentum plot (integrated over TileID)
    fCanvas->SetRightMargin(0.02);
    fCanvas->SetGrid(1,1);
    TH1D* hMatchedVSMom    = (TH1D*)fhMatched->ProjectionX("hMatchedVSMom");
    TH1D* hExpectedVSMom   = (TH1D*)fhExpected->ProjectionX("hExpectedMom");
    TH1D* hEfficiencyVSMom = (TH1D*)hMatchedVSMom->Clone("hEfficiencyVSMom");
    hEfficiencyVSMom->Divide(hMatchedVSMom, hExpectedVSMom, 1., 1., "B");
    MinY = hEfficiencyVSMom->GetMinimum(0.);
    hEfficiencyVSMom->SetTitle("NewCHOD Efficiency VS Track momentum");
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
    hEfficiencyVSMom->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // Efficiency vs TileID (integrated over track momentum)
    fCanvas->GetPad(0)->SetRightMargin(0.02);
    TH1D* hMatchedVSTileID    = (TH1D*)fhMatched->ProjectionY("hMatchedVSMom");
    TH1D* hExpectedVSTileID   = (TH1D*)fhExpected->ProjectionY("hExpectedMom");
    TH1D* hEfficiencyVSTileID = (TH1D*)hMatchedVSTileID->Clone("hEfficiencyVSTileID");
    hEfficiencyVSTileID-> Divide(hMatchedVSTileID, hExpectedVSTileID, 1., 1., "B");
    MinY = hEfficiencyVSTileID->GetMinimum(0.);
    hEfficiencyVSTileID->SetTitle("NewCHOD Efficiency VS Tile ID");
    hEfficiencyVSTileID->GetXaxis()->SetTitle("Tile ID");
    hEfficiencyVSTileID->GetYaxis()->SetTitle("Efficiency");
    hEfficiencyVSTileID->SetMarkerStyle(20);
    hEfficiencyVSTileID->SetMarkerSize(.5);
    hEfficiencyVSTileID->SetMarkerColor(1);
    hEfficiencyVSTileID->SetLineWidth(2);
    hEfficiencyVSTileID->SetLineColor(2);
    hEfficiencyVSTileID->SetMinimum(0.95 * MinY);
    hEfficiencyVSTileID->SetMaximum(1.);
    hEfficiencyVSTileID->SetStats(0);
    hEfficiencyVSTileID->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // DeltaT plot for loose hits in the same tile
    fCanvas->Clear();
    fCanvas->SetGrid(0,0);
    fCanvas->Divide(2,1);
    fCanvas->cd(1);
    fCanvas->GetPad(1)->SetGrid(1,0);
    fhDeltaT->SetOption("colz");
    fhDeltaT->SetStats(0);
    fhDeltaT->Draw();
    fCanvas->cd(2);
    fCanvas->GetPad(2)->SetGrid(1,0);
    TH1D *hDeltaT1D;
    hDeltaT1D = fhDeltaT->ProjectionX("hDeltaTIntegrated", 1, fhDeltaT->GetNbinsY());
    hDeltaT1D->SetTitle("Time difference between two channels for loose hits in the same tile, integrated;#Deltat (higher channel - lower channel) [ns];Entries / 0.5ns");
    hDeltaT1D->SetStats(0);
    hDeltaT1D->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // DeltaT between CHOD and NewCHOD time
    fCanvas->Clear();
    fCanvas->SetGrid(1,0);
    fCanvas->SetLogy(1);
    fCanvas->SetRightMargin(0.02);
    fhDeltaTCHODNewCHOD->SetStats(0);
    fhDeltaTCHODNewCHOD->Draw();
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
