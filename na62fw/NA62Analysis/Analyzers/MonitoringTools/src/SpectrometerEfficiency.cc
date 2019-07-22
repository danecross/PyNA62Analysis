// ---------------------------------------------------------------
//
// History:
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 05.2018
// and Michal Koval (michal.koval@cern.ch) 05.2018
//
// Updated by Zuzana Kucerova 11.2018 (new K2piSelectionNoSpectrometer)
// Updated by Zuzana Kucerova 01.2019 (Ke3SelectionNoSpectrometer and Kmu3SelectionNoSpectrometer)
// ---------------------------------------------------------------
/// \class SpectrometerEfficiency
/// \Brief
/// Spectrometer reconstruction efficiency for the single track events
/// \EndBrief
/// \Detailed
/// K2piSelectionNoSpectrometer is used to select k2pi events without the use of the STRAW spectrometer information.
/// Momentum and position of a pi+ pointer is requested in each event.
/// Pi+ pointer is tracked through te BlueTube field from the vertex position to z=180m
/// (before the STRAW magnet).
/// The pointer is checked for being inside the geometrical acceptance of all 4 STRAW chambers.
/// Number of events with such pointer enters the denominator of the efficiency.
/// A STRAW candidate matching the pointer by position and momentum is looked for.
/// Number of event with at least one such candidate enters the numerator of the efficiency.
/// The efficiency is calculated for all STRAW candidates and for 4-chamber candidates separately.
///
/// This is a two-step analyzer.
/// In the first step, the histograms for the efficiency calculation are filled
/// (numerator and denominator as functions of the track momentum, track position before magnet,
/// track distance from the hole center before magnet, measure of the beam intensity lambda).
/// In the second step (--histo command line option) the efficiencies are calculated reading the output
/// from the first step. A pdf output (fAnalyzerName.pdf) with efficiency plots, inefficiency
/// (due to no candidate in the STRAW and due to no matching candidate found)
/// plots and plots showing the matching conditions is created.
/// A file ./SpectrometerEfficiency_BadBursts.dat with a list of bad bursts is created as well.
/// A burst is considered bad if the efficiency+error_up (95% CL) is lower than 0.9.
/// Format of the list is "BadBurst RunID BurstID".
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// and Michal Koval (michal.kovalcern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <TChain.h>
#include "SpectrometerEfficiency.hh"
#include "TriggerConditions.hh"
#include "GeometricAcceptance.hh"
#include "ConfigSettings.hh"
#include "Persistency.hh"
#include "BaseAnalysis.hh"
#include "functions.hh"
#include "Event.hh"
#include <TGraphAsymmErrors.h>
#include <TLine.h>
#include <TLatex.h>
#include <TMath.h>
#include "BlueTubeTracker.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerEfficiency::SpectrometerEfficiency(Core::BaseAnalysis *ba)
    : Analyzer(ba, "SpectrometerEfficiency"),
      fGBurstQuality{nullptr}, fGMissingCovers{nullptr}, fGMissingStraws{nullptr}
{
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  RequestTree("Spectrometer", new TRecoSpectrometerEvent);
  RequestBeamSpecialTrigger();
  RequestL0Data();

  AddParam("CutMinClusterEnergy", "double", &fCutMinClusterEnergy, 3000.);  //Pi0Selection
  AddParam("UseGTK", "bool", &fUseGTK, true);
  AddParam("MaxNBursts", &fMaxNBursts, 5000);
  AddParam("TriggerSelected", "int", &fTrigger, 1); //control
  AddParam("CutTimeDifferenceForSidebands", "double", &fCutTimeDifferenceForSidebands, 25.);
  AddParam("WantGeometricalAcceptance", "bool", &fWantGeometricalAcceptance, true);
  AddParam("MinDistanceWrtHoleCenter", "double", &fMinDistanceWrtHoleCenter, 100.); // in mm, default -1.0
  AddParam("WantCloseInMom", "bool", &fWantCloseInMom, true);
  AddParam("WantCloseInPos", "bool", &fWantCloseInPos, true);
  AddParam("CutCloseInMom", "double", &fCutCloseInMom, 5000.);
  AddParam("CutCloseInPosK", "double", &fCutCloseInPosK, -0.0025);
  AddParam("CutCloseInPosQ", "double", &fCutCloseInPosQ, 212.5);
  AddParam("MNP33kick", "double", &fMNP33kick, 270.);

  fOutPDFfileName = fAnalyzerName + ".pdf";

  gen = "GeneralInfo/";
  nia = "NotInAcceptance/";
  nc = "NoCandidate/";
  nm = "NotMatched/";
  bef = "BeforeMatching/";
  mat = "Matched/";
  fBadBurstFileName = "./SpectrometerEfficiency_BadBursts.dat";
}

void SpectrometerEfficiency::InitOutput(){
  RegisterOutput("NoCand", &fNoCand);
  RegisterOutput("NoMatch", &fNoMatch);
  if (!GetIsTree() && GetIsHisto()) {
      ofstream badBurstFile;
      badBurstFile.open(fBadBurstFileName, ios::out);
      badBurstFile << "### Spectrometer bad bursts identified by SpectrometerEfficiency analyzer" << endl;
      badBurstFile.close();
  }
}

void SpectrometerEfficiency::InitHist(){
  fReadingData = GetIsTree();

  TString pre[] = {"K2pi/", "Kmu3/", "Ke3/"};

  if(fReadingData){
    double lambda_bins[] = {0., 0.25, 0.5, 0.75, 1., 1.25, 1.5, 1.75, 2., 2.5, 3., 5.}; //11 bins
    cout << user_normal() << "Reading reconstructed data" << endl;

    ReconfigureAnalyzer("K2piSelectionNoSpectrometer", "UseGTK", fUseGTK);
    ReconfigureAnalyzer("Pi0Selection", "UseGTK", fUseGTK);
    // ReconfigureAnalyzer("Pi0Selection", "CutMinClusterEnergy", fCutMinClusterEnergy); //MeV

    for(int i=0; i<3; i++){
      BookHisto(new TH1I(pre[i]+gen+"hNStrawCandidates", "N STRAW candidates in event; N candidates; events", 20, 0, 20));
      BookHisto(new TH1F(pre[i]+gen+"hPosVertex_Z", "K_{2#pi} vertex position; vertex z [mm]", 1400, 60000., 200000.));
      BookHisto(new TH1I(pre[i]+gen+"hNGTKHits", "N GTK hits; N hits; events", 1000, 0, 10000));
      BookHisto(new TH1F(pre[i]+gen+"hTimeDiffGTKHitTrigger", "#Delta T (GTK hit - trigger); (t_{GTK hit} - t_{trigger}) [ns]; hits", 500, -50., 50.));
      BookHisto(new TH1I(pre[i]+gen+"hNGTKHitsInSidebands", "N GTK hits in Sidebands; N hits; events", 1000, 0, 1000));
      BookHisto(new TH1F(pre[i]+gen+"hLambda", "#lambda in event; #lambda; events", 11, lambda_bins));

      BookHisto(new TH2F(pre[i]+nia+"hPos_YvsX_CH1", "Pointer position at Chamber 1; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH2F(pre[i]+nia+"hPos_YvsX_CH2", "Pointer position at Chamber 2; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH2F(pre[i]+nia+"hPos_YvsX_CH3", "Pointer position at Chamber 3; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH2F(pre[i]+nia+"hPos_YvsX_CH4", "Pointer position at Chamber 4; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH1F(pre[i]+nia+"hMomTrack", "Pointer momentum; p [MeV/c]; N pointers", 30, 0., 75000.));
      BookHisto(new TH1F(pre[i]+nia+"hLambda", "#lambda in event; #lambda; events", 11, lambda_bins));

      BookHisto(new TH1I(pre[i]+nc+"hNoCandidateInStraw", "No candidate in STRAW; No candidate?; events", 2, 0, 2));
      BookHisto(new TH2F(pre[i]+nc+"hPosTrack_YvsX", "Pointer position before magnet (z=180m); x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH1F(pre[i]+nc+"hMomTrack", "Pointer momentum; p [MeV/c]; tracks", 30, 0., 75000.));
      BookHisto(new TH1F(pre[i]+nc+"hLambda", "#lambda; #lambda; events", 11, lambda_bins));
      BookHisto(new TH1I(pre[i]+nc+"hNHits", "N hits in event; N hits; events", 1000, 0, 1000));
      BookHisto(new TH1I(pre[i]+nc+"hHitChamberID_ViewID", "Chamber ID and View ID of hits; Chamber ID, View ID; N hits", 16, 0, 16));

      BookHisto(new TH2F(pre[i]+bef+"hPosTrack_YvsX", "Pointer position before magnet (z=180m); x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH1F(pre[i]+bef+"hMomTrack", "Pointer momentum before magnet; p [MeV/c]; N pointers", 30, 0., 75000.));
      BookHisto(new TH2F(pre[i]+bef+"hMomTrack_YvsX", "Pointer momentum before magnet; p_{x} [MeV/c]; p_{y} [MeV/c]", 200, -500., 500., 200, -500., 500.));
      BookHisto(new TH1I(pre[i]+bef+"hNGTKHitsInSidebands", "N GTK hits in Sidebands; N hits; events", 1000, 0, 1000));
      BookHisto(new TH1F(pre[i]+bef+"hLambda", "#lambda; #lambda; events", 11, lambda_bins));
      BookHisto(new TH1F(pre[i]+bef+"hBurstID", "Events per burst; Burst ID; events", fMaxNBursts, 0, fMaxNBursts));
      BookHisto(new TH1F(pre[i]+bef+"hPosTrack_disc", "Pointer distance from hole center;R [mm];events", 14, 0., 1050. ));
      BookHisto(new TH1I(pre[i]+bef+"hCandidatesNChambers", "STRAW candidate from N chambers; N chambers; candidates", 4, 1, 5));
      BookHisto(new TH2F(pre[i]+bef+"hMomDiffVSPosDiff", "Momentum difference vs position difference (pointer - candidate); #Delta D [mm]; #Delta P [GeV/c]", 750, 0., 1500., 500, 0., 50.));
      BookHisto(new TH2F(pre[i]+bef+"hPosDiffVSMom", "Position difference (pointer - candidate) vs pointer momentum; P [MeV/c]; #Delta D [mm]", 30, 0., 75000., 75, 0., 1500.));
      BookHisto(new TH1F(pre[i]+bef+"hMomDiffTrackCand_Mag", "Momentum difference magnitude (pointer - candidate); |#vec{p}_{pointer} - #vec{p}_{candidate}| [MeV/c]; candidates", 350, 0., 50000.));
      BookHisto(new TH1F(pre[i]+bef+"hMomDiffTrackCand_X", "Momentum difference X (pointer - candidate); p_{pointer, x} - p_{candidate, x} [MeV/c]; candidates", 400, -2000., 2000.));
      BookHisto(new TH1F(pre[i]+bef+"hMomDiffTrackCand_Y", "Momentum difference Y (pointer - candidate); p_{pointer, y} - p_{candidate, y} [MeV/c]; candidates", 400, -2000., 2000.));
      BookHisto(new TH1F(pre[i]+bef+"hMomDiffTrackCand_Z", "Momentum difference Z (pointer - candidate); p_{pointer, z} - p_{candidate, z} [MeV/c]; candidates", 300, -15000., 15000.));
      BookHisto(new TH1F(pre[i]+bef+"hPosDiffTrackCand_Mag", "Position difference magnitude (pointer - candidate); |#vec{r}_{pointer} - #vec{r}_{candidate}| [mm]; candidates", 500, 0., 500.));
      BookHisto(new TH1F(pre[i]+bef+"hPosDiffTrackCand_X", "Position difference X (pointer - candidate); x_{pointer} - x_{candidate} [mm]; candidates", 100, -500., 500.));
      BookHisto(new TH1F(pre[i]+bef+"hPosDiffTrackCand_Y", "Position difference Y (pointer - candidate); y_{pointer} - y_{candidate} [mm]; candidates", 100, -500., 500.));
      BookHisto(new TH1I(pre[i]+bef+"hNCloseInPos", "N STRAW candidates close in position to pointer; N candidates; events", 20, 0, 20));
      BookHisto(new TH2F(pre[i]+bef+"hNCloseInPosVsTrackMom", "N STRAW candidates close in position to pointer; p_{pointer} [MeV/c]; N candidates", 150, 0., 75000., 20, 0, 20));
      BookHisto(new TH2F(pre[i]+bef+"hNCloseInPosVsTrackPos", "N STRAW candidates close in position to pointer; d_{pointer} [mm]; N candidates", 150, 0., 1500., 20, 0, 20));
      BookHisto(new TH1I(pre[i]+bef+"hNCloseInMomCloseInPos", "N STRAW candidates close in position and momentum to pointer; N candidates; events", 20, 0, 20));
      BookHisto(new TH2F(pre[i]+bef+"hNCloseInMomCloseInPosVsTrackMom", "N STRAW candidates close in position and momentum to pointer; p_{pointer} [MeV/c]; candidates", 150, 0., 75000., 20, 0, 20));

      BookHisto(new TH2F(pre[i]+nm+"hMomDiffVsPosDiff", "Momentum vs position difference (pointer - candidate); #Delta D [mm]; #Delta P [MeV/c]", 1000, 0., 2000., 5000, 0., 50000.));
      BookHisto(new TH1I(pre[i]+nm+"hNCandidates", "N STRAW candidates; N candidates; events", 10, 0, 10));
      BookHisto(new TH1F(pre[i]+nm+"hTrackM", "Positive pointer mass; #sqrt{P_{track^{+}}^{2}} [MeV/c^{2}]; events", 4000, -200., 200.));
      BookHisto(new TH2F(pre[i]+nm+"hPosTrack_YvsX", "Pointer position; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH1F(pre[i]+nm+"hMomTrack", "Pointer momentum; p [MeV/c]; N pointers", 30, 0., 75000.));
      BookHisto(new TH1F(pre[i]+nm+"hLambda", "#lambda; #lambda; events", 11, lambda_bins));
      BookHisto(new TH1I(pre[i]+nm+"hCandidatesNChambers", "STRAW candidate from N chambers; N chambers; candidates", 4, 1, 5));

      BookHisto(new TH2F(pre[i]+mat+"hPosTrack_YvsX", "Pointer position before magnet; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH1F(pre[i]+mat+"hMomTrack", "Pointer momentum; p [MeV/c]; N pointers", 30, 0., 75000.));
      BookHisto(new TH1I(pre[i]+mat+"hNGTKHitsInSidebands", "N GTK hits in Sidebands; N hits; events", 100, 0, 100));
      BookHisto(new TH1F(pre[i]+mat+"hLambda", "#lambda; #lambda; events", 11, lambda_bins));
      BookHisto(new TH1F(pre[i]+mat+"hBurstID", "Events per burst; Burst ID; events", fMaxNBursts, 0, fMaxNBursts));
      BookHisto(new TH1F(pre[i]+mat+"hPosTrack_disc", "Pointer distance from hole center;R [mm];events", 14, 0., 1050. ));
      BookHisto(new TH1I(pre[i]+mat+"hMatchedCandidateNChambers", "Matched STRAW candidate from N chambers; N chambers; candidates", 4, 1, 5));
      BookHisto(new TH2F(pre[i]+mat+"4CH/hPosTrack_YvsX", "Pointer position before magnet; x [mm]; y [mm]", 160, -1200., 1200., 160, -1200., 1200.));
      BookHisto(new TH1F(pre[i]+mat+"4CH/hMomTrack", "Pointer momentum; p [MeV/c]; N pointers", 30, 0., 75000.));
      BookHisto(new TH1I(pre[i]+mat+"4CH/hNGTKHitsInSidebands", "N GTK hits in Sidebands; N hits; events", 100, 0, 100));
      BookHisto(new TH1F(pre[i]+mat+"4CH/hLambda", "#lambda; #lambda; events", 11, lambda_bins));
      BookHisto(new TH1F(pre[i]+mat+"4CH/hBurstID", "Events per burst; Burst ID; events", fMaxNBursts, 0, fMaxNBursts));
      BookHisto(new TH1F(pre[i]+mat+"4CH/hPosTrack_disc", "Pointer distance from hole center;R [mm];events", 14, 0., 1050. ));
    };
  }else{
    cout << user_normal() << "Reading my own output" << endl;

    fhMatched_mom = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"hMomTrack", true));
    fhExpected_mom = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hMomTrack", true));

    fhMatched_pos = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"hPosTrack_YvsX", true));
    fhExpected_pos = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hPosTrack_YvsX", true));

    fhMatched_lambda = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"hLambda", true));
    fhExpected_lambda = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hLambda", true));

    fhMatched_burstID  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"hBurstID",  true));
    fhExpected_burstID = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hBurstID", true));

    fhNoCand_mom = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+nc+"hMomTrack", true));

    fhNoMatch_mom = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+nm+"hMomTrack", true));

    fhMomDiff = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hMomDiffTrackCand_Mag", true));

    fhPosDiffVSMom = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hPosDiffVSMom", true));

    fhExpected_disc = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+bef+"hPosTrack_disc", true));
    fhMatched_disc = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"hPosTrack_disc", true));

    //matching only to candidates from 4 chambers
    fhMatched_mom_4CH = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"4CH/hMomTrack", true));
    fhMatched_pos_4CH = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"4CH/hPosTrack_YvsX", true));
    fhMatched_lambda_4CH = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"4CH/hLambda", true));
    fhMatched_burstID_4CH  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"4CH/hBurstID",  true));
    fhMatched_disc_4CH = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, pre[0]+mat+"4CH/hPosTrack_disc", true));
  }
}

void SpectrometerEfficiency::DefineMCSimple(){
}

void SpectrometerEfficiency::StartOfRunUser(){
}

void SpectrometerEfficiency::StartOfBurstUser(){
}

void SpectrometerEfficiency::Process(int){
  if (!fReadingData) return;
  if (!(GetL0Data()->GetDataType() & 0x10)) return; // process control triggers only

  fBurstID = GetBurstID();
  fTriggerTime = GetTriggerTime(fTrigger);
  if(fTriggerTime==9999999.) return; //return if trigger is not control

  //events
  fSTRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  fGTKEvent = GetEvent<TRecoGigaTrackerEvent>();

  OutputState state_k2pi;
  auto k2piSelected = *GetOutput<bool>("K2piSelectionNoSpectrometer.EventSelected", state_k2pi);
  auto trackFourMom_k2pi =
    *GetOutput<TLorentzVector>("K2piSelectionNoSpectrometer.K2piPionFourMomentum", state_k2pi);
  auto trackPos_k2pi =
    *GetOutput<TVector3>("K2piSelectionNoSpectrometer.K2piVertexPosition", state_k2pi);

  OutputState state_kmu3;
  auto kmu3Selected = *GetOutput<bool>("Kmu3SelectionNoSpectrometer.EventSelected", state_kmu3);
  auto trackFourMom_kmu3 =
    *GetOutput<TLorentzVector>("Kmu3SelectionNoSpectrometer.Kmu3MuonFourMomentum", state_kmu3);
  auto trackPos_kmu3 =
    *GetOutput<TVector3>("Kmu3SelectionNoSpectrometer.Kmu3MuonPosition", state_kmu3);

  OutputState state_ke3;
  auto ke3Selected = *GetOutput<bool>("Ke3SelectionNoSpectrometer.EventSelected", state_ke3);
  auto trackFourMom_ke3 =
    *GetOutput<TLorentzVector>("Ke3SelectionNoSpectrometer.Ke3PositronFourMomentum", state_ke3);
  auto trackPos_ke3 =
    *GetOutput<TVector3>("Ke3SelectionNoSpectrometer.Ke3PositronPosition", state_ke3);

  if((state_k2pi==kOValid) && k2piSelected) FillHistogramsForEfficiency(trackFourMom_k2pi, trackPos_k2pi, "K2pi/");
  if((state_kmu3==kOValid) && kmu3Selected) FillHistogramsForEfficiency(trackFourMom_kmu3, trackPos_kmu3, "Kmu3/");
  if((state_ke3==kOValid) && ke3Selected) FillHistogramsForEfficiency(trackFourMom_ke3, trackPos_ke3, "Ke3/");
}

void SpectrometerEfficiency::PostProcess(){
}

void SpectrometerEfficiency::EndOfBurstUser(){
}

void SpectrometerEfficiency::EndOfRunUser(){
}

void SpectrometerEfficiency::EndOfJobUser(){
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  /////////////////////////////////////////////////////////////////////////////////////////
  // HISTO mode

  if(!fReadingData){
    if(!fhMatched_mom){
      cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
      return;
    }

    TEfficiency *eff;
    TEfficiency *geff;
    TLatex t;
    t.SetNDC();
    TLine l;
    double efficiency, error_low, error_up;
    double mv1, mv2, mv2err;
    double errL = 0.;
    double errU = 0.;


    ///////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt momentum
    fhMatched_mom->Sumw2();
    fhExpected_mom->Sumw2();

    fCanvas = new TCanvas("SpectrometerEfficiencyCanvas", "SpectrometerEfficiencyCanvas");
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_mom, *fhExpected_mom);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";p(track^{+}) [MeV / c]; Track Reconstruction Efficiency");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_mom->Integral());
    geff->SetPassedEvents(1, fhMatched_mom->Integral());
    efficiency = geff->GetEfficiency(1);
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    mv2 = 0.;
    mv1 = 1.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val<mv1) && (val<1.) && (val>0.)){
	mv1 = val;
      }
      if((val>mv2) && (val<1.)){
	mv2 = val;
	mv2err = eff->GetEfficiencyErrorUp(eff->GetGlobalBin(i));
      }
    }
    if((mv2+2*mv2err)>0.999999){
      eff->GetPaintedGraph()->SetMaximum(0.999999);
    }else{
      eff->GetPaintedGraph()->SetMaximum(mv2+2*mv2err);
    }
    eff->GetPaintedGraph()->SetMinimum(mv1-0.15*(efficiency-mv1));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 65000.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 65000., efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(fOutPDFfileName + "(", "pdf");
    double EF = efficiency;
    double EF_U = errU;
    double EF_L = errL;

    delete eff;
    delete geff;

    ///////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt momentum - only candidates from 4 chambers
    fhMatched_mom_4CH->Sumw2();
    fhExpected_mom->Sumw2();

    fCanvas = new TCanvas("SpectrometerEfficiencyCanvas", "SpectrometerEfficiencyCanvas");
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_mom_4CH, *fhExpected_mom);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";p(track^{+}) [MeV / c]; Track Reconstruction Efficiency (4CH candidates)");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_mom->Integral());
    geff->SetPassedEvents(1, fhMatched_mom_4CH->Integral());
    efficiency = geff->GetEfficiency(1);
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    mv2 = 0.;
    mv1 = 1.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val<mv1) && (val<1.) && (val>0.)){
	mv1 = val;
      }
      if((val>mv2) && (val<1.)){
	mv2 = val;
	mv2err = eff->GetEfficiencyErrorUp(eff->GetGlobalBin(i));
      }
    }
    if((mv2+2*mv2err)>0.999999){
      eff->GetPaintedGraph()->SetMaximum(0.999999);
    }else{
      eff->GetPaintedGraph()->SetMaximum(mv2+2*mv2err);
    }
    eff->GetPaintedGraph()->SetMinimum(mv1-0.15*(efficiency-mv1));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 65000.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 65000., efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(fOutPDFfileName, "pdf");

    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    /////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt position
    fCanvas->Clear();
    fhEfficiency_pos = static_cast<TH2F*>(fhMatched_pos->Clone("hEfficiency_pos"));
    fhEfficiency_pos->Divide(fhExpected_pos);
    fhEfficiency_pos->SetStats(0);
    fhEfficiency_pos->SetTitle("Track Reconstruction Efficiency Before Magnet (z=180m);x [mm];y [mm]");
    fhEfficiency_pos->GetZaxis()->SetRangeUser(efficiency*0.7, 1.);
    fhEfficiency_pos->GetYaxis()->SetTitleOffset(1.2);
    fhEfficiency_pos->Draw("colz");

    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    // t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", EF, EF_U, EF_L));

    gPad->Update();

    fCanvas->Print(Form(fOutPDFfileName), "pdf");

    /////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt position - only candidates from 4 chambers
    fCanvas->Clear();
    fhEfficiency_pos = static_cast<TH2F*>(fhMatched_pos_4CH->Clone("hEfficiency_pos_4CH"));
    fhEfficiency_pos->Divide(fhExpected_pos);
    fhEfficiency_pos->SetStats(0);
    fhEfficiency_pos->SetTitle("Track Reconstruction Efficiency Before Magnet (z=180m), (4CH candidates);x [mm];y [mm]");
    fhEfficiency_pos->GetZaxis()->SetRangeUser(efficiency*0.7, 1.);
    fhEfficiency_pos->GetYaxis()->SetTitleOffset(1.2);
    fhEfficiency_pos->Draw("colz");

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    gPad->Update();

    fCanvas->Print(Form(fOutPDFfileName), "pdf");

    //////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt position (disks)
    fhMatched_disc->Sumw2();
    fhExpected_disc->Sumw2();

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_disc, *fhExpected_disc);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";R [mm]; Track Reconstruction Efficiency");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_disc->Integral());
    geff->SetPassedEvents(1, fhMatched_disc->Integral());
    efficiency = geff->GetEfficiency(1);
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    mv2 = 0.;
    mv1 = 1.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val<mv1) && (val<1.) && (val>0.)){
	mv1 = val;
      }
      if((val>mv2) && (val<1.)){
	mv2 = val;
	mv2err = eff->GetEfficiencyErrorUp(eff->GetGlobalBin(i));
      }
    }
    if((mv2+2*mv2err)>0.999999){
      eff->GetPaintedGraph()->SetMaximum(0.999999);
    }else{
      eff->GetPaintedGraph()->SetMaximum(mv2+2*mv2err);
    }
    eff->GetPaintedGraph()->SetMinimum(mv1-0.15*(efficiency-mv1));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 1050.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 1050., efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");
    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    //////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt position (disks) - only candidates from 4 chambers
    fhMatched_disc_4CH->Sumw2();
    fhExpected_disc->Sumw2();

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_disc_4CH, *fhExpected_disc);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";R [mm]; Track Reconstruction Efficiency (4CH candidates)");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_disc->Integral());
    geff->SetPassedEvents(1, fhMatched_disc_4CH->Integral());
    efficiency = geff->GetEfficiency(1);
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    mv2 = 0.;
    mv1 = 1.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val<mv1) && (val<1.) && (val>0.)){
    	mv1 = val;
      }
      if((val>mv2) && (val<1.)){
    	mv2 = val;
    	mv2err = eff->GetEfficiencyErrorUp(eff->GetGlobalBin(i));
      }
    }
    if((mv2+2*mv2err)>0.999999){
      eff->GetPaintedGraph()->SetMaximum(0.999999);
    }else{
      eff->GetPaintedGraph()->SetMaximum(mv2+2*mv2err);
    }
    eff->GetPaintedGraph()->SetMinimum(mv1-0.15*(efficiency-mv1));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 1050.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 1050., efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");
    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    //////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt lambda
    fhMatched_lambda->Sumw2();
    fhExpected_lambda->Sumw2();

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_lambda, *fhExpected_lambda);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";#lambda; Track Reconstruction Efficiency");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_lambda->Integral());
    geff->SetPassedEvents(1, fhMatched_lambda->Integral());
    efficiency = geff->GetEfficiency(1);
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    mv2 = 0.;
    mv1 = 1.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val<mv1) && (val<1.) && (val>0.)){
    	mv1 = val;
      }
      if((val>mv2) && (val<1.)){
    	mv2 = val;
    	mv2err = eff->GetEfficiencyErrorUp(eff->GetGlobalBin(i));
      }
    }
    if((mv2+2*mv2err)>0.999999){
      eff->GetPaintedGraph()->SetMaximum(0.999999);
    }else{
      eff->GetPaintedGraph()->SetMaximum(mv2+2*mv2err);
    }
    eff->GetPaintedGraph()->SetMinimum(mv1-0.15*(efficiency-mv1));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 5.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 5., efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");
    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    //////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt lambda
    fhMatched_lambda_4CH->Sumw2();
    fhExpected_lambda->Sumw2();

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_lambda_4CH, *fhExpected_lambda);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";#lambda; Track Reconstruction Efficiency (4CH candidates)");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_lambda->Integral());
    geff->SetPassedEvents(1, fhMatched_lambda_4CH->Integral());
    efficiency = geff->GetEfficiency(1);
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    mv2 = 0.;
    mv1 = 1.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val<mv1) && (val<1.) && (val>0.)){
    	mv1 = val;
      }
      if((val>mv2) && (val<1.)){
    	mv2 = val;
    	mv2err = eff->GetEfficiencyErrorUp(eff->GetGlobalBin(i));
      }
    }
    if((mv2+2*mv2err)>0.999999){
      eff->GetPaintedGraph()->SetMaximum(0.999999);
    }else{
      eff->GetPaintedGraph()->SetMaximum(mv2+2*mv2err);
    }
    eff->GetPaintedGraph()->SetMinimum(mv1-0.15*(efficiency-mv1));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 5.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 5., efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");
    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    ////////////////////////////////////////////////////////////////////////////////////////
    //BadBurst list
    fhMatched_burstID->Sumw2();
    fhExpected_burstID->Sumw2();

    ofstream BadBurstFile;
    BadBurstFile.open(fBadBurstFileName, ios::app);
    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    for(int i=1; i<=fhExpected_burstID->GetNbinsX(); i++){
      geff->SetPassedEvents(1, 0);
      geff->SetTotalEvents(1, 0);
      double matched = fhMatched_burstID->GetBinContent(i);
      double expected = fhExpected_burstID->GetBinContent(i);
      if(expected==0) continue;
      if(geff->SetTotalEvents(1, expected) && geff->SetPassedEvents(1, matched)){
	efficiency = geff->GetEfficiency(1);
	error_up = geff->GetEfficiencyErrorUp(1);
   	if((efficiency+error_up)<0.9)
            BadBurstFile << Form("BadBurst %06d %04d [Efficiency = (%f + %f - %f)%%] \n",
                                 GetRunID(), i-1, geff->GetEfficiency(1), geff->GetEfficiencyErrorUp(1), geff->GetEfficiencyErrorLow(1));
      }
    }
    BadBurstFile.close();
    delete geff;
    geff = nullptr;

    ///////////////////////////////////////////////////////////////////////////////////////
    //efficiency wrt BurstID
    fhMatched_burstID->Sumw2();
    fhExpected_burstID->Sumw2();

    fhMatched_burstID->Rebin(5);
    fhExpected_burstID->Rebin(5);

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhMatched_burstID, *fhExpected_burstID);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";Burst ID (grouped by 5 bursts); Track Reconstruction Efficiency");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(1);
    eff->SetMarkerStyle(6);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_burstID->Integral());
    geff->SetPassedEvents(1, fhMatched_burstID->Integral());
    efficiency = geff->GetEfficiency(1); //hMatched->Integral() / hExpected->Integral();
    error_low = geff->GetEfficiencyErrorLow(1);
    error_up = geff->GetEfficiencyErrorUp(1);

    gPad->Update();
    eff->GetPaintedGraph()->SetMaximum(1.0);
    double minb = GetLowEdgeFirstFilledBin(fhExpected_burstID);
    double maxb = GetUpperEdgeLastFilledBin(fhExpected_burstID);
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(minb-1, maxb+1);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(minb-1, efficiency, maxb+1, efficiency);

    errL = TMath::Ceil(error_low*1000.)/1000.;
    errU = TMath::Ceil(error_up*1000.)/1000.;
    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("#varepsilon = %.3f ^{+%.3f}_{ -%.3f}", efficiency, errU, errL));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");

    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    //////////////////////////////////////////////////////////////////////////////////
    //inefficiency due to no candidate
    fhNoCand_mom->Sumw2();

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    eff = new TEfficiency(*fhNoCand_mom, *fhExpected_mom);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";p(track^{+}) [MeV / c]; Inefficiency (no candidate in Straw)");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_mom->Integral());
    geff->SetPassedEvents(1, fhNoCand_mom->Integral());
    efficiency = geff->GetEfficiency(1);

    gPad->Update();
    mv2 = 1.;
    mv1 = 0.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val>mv1) && (val>0.) && (val<1.)){
	mv1 = val;
      }else if((val<mv2) && (val>0.) && (val<1.)){
	mv2 = val;
	mv2err = eff->GetEfficiencyErrorLow(eff->GetGlobalBin(i));
      }
    }
    if((mv2-2*mv2err)<0.){
      eff->GetPaintedGraph()->SetMinimum(0.000001);
    }else{
      eff->GetPaintedGraph()->SetMinimum(mv2-2*mv2err);
    }
    eff->GetPaintedGraph()->SetMaximum(mv1+0.15*(mv1-efficiency));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 65000.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 65000., efficiency);

    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92, Form("1 - #varepsilon = %.1g", efficiency));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");
    delete eff;
    delete geff;
    eff = nullptr;
    geff = nullptr;

    //////////////////////////////////////////////////////////////////////////////////////////
    //inefficiency due to no match

    fCanvas->Clear();
    fCanvas->SetGrid(true, true);

    fhNoMatch_mom->Sumw2();

    eff = new TEfficiency(*fhNoMatch_mom, *fhExpected_mom);
    eff->SetConfidenceLevel(0.95);
    eff->SetDirectory(gDirectory);
    eff->SetTitle(";p(track^{+}) [MeV / c]; Inefficiency (no matched Straw candidate)");
    eff->SetLineColor(kBlack);
    eff->SetLineWidth(2);
    eff->SetMarkerStyle(8);
    eff->SetMarkerSize(1);
    eff->Draw("AP");

    geff = new TEfficiency("geff", "", 1, 0, 1);
    geff->SetConfidenceLevel(0.95);
    geff->SetTotalEvents(1, fhExpected_mom->Integral());
    geff->SetPassedEvents(1, fhNoMatch_mom->Integral());
    efficiency = geff->GetEfficiency(1);

    gPad->Update();
    mv2 = 1.;
    mv1 = 0.;
    mv2err = 0.;
    for(int i=1; i<(eff->GetPaintedGraph()->GetHistogram()->GetNbinsX()+1); i++){
      double val = eff->GetEfficiency(eff->GetGlobalBin(i));
      if((val>mv1) && (val>0.) && (val<1.)){
	mv1 = val;
      }else if((val<mv2) && (val>0.) && (val<1.)){
	mv2 = val;
	mv2err = eff->GetEfficiencyErrorLow(eff->GetGlobalBin(i));
      }
    }
    if((mv2-2*mv2err)<0.){
      eff->GetPaintedGraph()->SetMinimum(0.000001);
    }else{
      eff->GetPaintedGraph()->SetMinimum(mv2-2*mv2err);
    }
    eff->GetPaintedGraph()->SetMaximum(mv1+0.15*(mv1-efficiency));
    eff->GetPaintedGraph()->GetXaxis()->SetLimits(0., 65000.);
    eff->GetPaintedGraph()->GetYaxis()->SetTitleOffset(1.2);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(2);
    l.SetLineStyle(2);
    l.DrawLine(0., efficiency, 65000., efficiency);

    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92,  Form("1 - #varepsilon = %.1g", efficiency));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");
    delete eff;
    delete geff;

    /////////////////////////////////////////////////////////////////////////////////////////
    //cuts used to select STRAW candidate compatible with Pi+ track
    //momentum difference

    fCanvas->Clear();
    fhMomDiff->SetStats(0);
    fhMomDiff->GetYaxis()->SetTitleOffset(1.2);
    fhMomDiff->GetXaxis()->SetTitle("#Delta P [MeV/c]");
    fhMomDiff->Draw();
    gPad->SetLogy(1);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(3);
    l.SetLineStyle(1);
    l.DrawLine(fCutCloseInMom, 0., fCutCloseInMom, fhMomDiff->GetMaximum());

    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92,  Form("#DeltaP < %.0f GeV/c", fCutCloseInMom/1e3));

    fCanvas->Print(Form(fOutPDFfileName), "pdf");

    /////////////////////////////////////////////////////////////////////////////////////////
    //position difference

    gPad->SetLogy(0);
    fCanvas->Clear();
    fhPosDiffVSMom->SetStats(0);
    fhPosDiffVSMom->GetYaxis()->SetTitleOffset(1.2);
    fhPosDiffVSMom->Draw("colz");
    gPad->SetLogz(1);
    gPad->Update();

    l.SetLineColor(kRed+2);
    l.SetLineWidth(3);
    l.SetLineStyle(1);
    l.DrawLine(5000., 200., 65000., 50.);

    t.SetTextSize(0.03);
    t.SetTextColor(kRed+2);
    t.DrawLatex(0.11, 0.92,  Form("#DeltaD < (%.4f*P + %.1f) mm", fCutCloseInPosK, fCutCloseInPosQ));

    fCanvas->Print(Form(fOutPDFfileName + ")"), "pdf");
  }
  SaveAllPlots();
  gErrorIgnoreLevel = -1; // restore the default
}

void SpectrometerEfficiency::DrawPlot(){}

SpectrometerEfficiency::~SpectrometerEfficiency(){}

double SpectrometerEfficiency::GetTriggerTime(int trigger){
 double triggerTime = 0.;
 bool eventPassedTrigger = false;
 if(GetWithMC()){
    eventPassedTrigger = true;
 }else{
   if(trigger==1){ //control
     bool controlTriggerOK = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data());
     if(controlTriggerOK){
       double fineTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();
       triggerTime = fineTime*TdcCalib;
       eventPassedTrigger = true;
     }
   }else if(trigger==2){ //PiNuNu
     bool PhysicsData = TriggerConditions::GetInstance()->IsPhysicsTrigger(GetL0Data());
     bool On = TriggerConditions::GetInstance()->L0TriggerBitOn(GetL0Data(), 1);
     if (PhysicsData && On) {
       double fineTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
       triggerTime = fineTime*TdcCalib;
       eventPassedTrigger = true;
     }
   }
 }
 if(eventPassedTrigger){
   return triggerTime;
 }else{
   return 9999999.;
 }
}

void SpectrometerEfficiency::ApplyBlueTube(int charge, TVector3 oldPos, TVector3 oldMom, double finalZ, TVector3 *newPos, TVector3 *newMom){
  BlueTubeTracker::GetInstance()->SetCharge(charge);
  BlueTubeTracker::GetInstance()->SetInitialPosition(oldPos);
  BlueTubeTracker::GetInstance()->SetInitialMomentum(oldMom);
  BlueTubeTracker::GetInstance()->SetZFinal(finalZ);
  BlueTubeTracker::GetInstance()->TrackParticle();
  TVector3 mom = BlueTubeTracker::GetInstance()->GetFinalMomentum();
  TVector3 pos = BlueTubeTracker::GetInstance()->GetFinalPosition();
  newMom->SetXYZ(mom.X(), mom.Y(), mom.Z());
  newPos->SetXYZ(pos.X(), pos.Y(), pos.Z());
}

int SpectrometerEfficiency::FindClosestInPosition(TVector3 trackPos, TVector3 trackMom, TString s, bool histo, bool wantCloseInMom){
  double closestDist = 99999999.;
  int closestID = -1;
  TVector3 candPos(0., 0., 0.);
  TVector3 candMom(0., 0., 0.);
  for(int i=0; i<fSTRAWEvent->GetNCandidates(); i++){
    TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(i));
    if(trackPos.Z()==180000.){
      candPos = StrawCand->GetPositionBeforeMagnet();
      candMom = StrawCand->GetThreeMomentumBeforeMagnet();
    }else if(trackPos.Z()==222000.){
      candPos = StrawCand->GetPositionAfterMagnet();
      candMom = StrawCand->GetThreeMomentumAfterMagnet();
    }else{
      cout<< user_normal() << "asking for wrong z position"<<endl;
    }
    if(trackPos.Z()!=candPos.Z()){
      if(histo) FillHisto("hWrongZplanes_pos"+s, candPos.Z());
      cout<< user_normal() << candPos.Z()<<" or "<<trackPos.Z()<<endl;
      cout<< user_normal() << "comparing position of track and candidate at different z planes"<<endl;
      continue;
    }
    if(wantCloseInMom && !IsCloseInMom(trackMom, candMom, fCutCloseInMom, 0, s)) continue;
    double dist = (trackPos - candPos).Mag();
    if(dist<closestDist){
      closestDist = dist;
      closestID = i;
    }
  }
  if(closestID==-1){
    if(histo) FillHisto(s+"hNoCandClosestInPositionForZ", trackPos.Z());
  }else{
    TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(closestID));
    if(trackPos.Z()==180000.){
      candPos = StrawCand->GetPositionBeforeMagnet();
      candMom = StrawCand->GetThreeMomentumBeforeMagnet();
    }else if(trackPos.Z()==222000.){
      candPos = StrawCand->GetPositionAfterMagnet();
      candMom = StrawCand->GetThreeMomentumAfterMagnet();
    }else{
      cout<< user_normal() << "asking for wrong z position"<<endl;
    }
    if(histo){
      FillHisto(s+"hPosDiffClosestInPos", closestDist);
      FillHisto(s+"hPosDiffVsMomDiffClosestInPos", (trackMom - candMom).Mag(), closestDist);
      FillHisto(s+"hTrackXVsClosestInPosX", candPos.X(), trackPos.X());
      FillHisto(s+"hTrackYVsClosestInPosY", candPos.Y(), trackPos.Y());
    }
  }
  return closestID;
}

int SpectrometerEfficiency::FindClosestInMomentum(TVector3 trackPos, TVector3 trackMom, TString s, bool histo, bool wantCloseInPos){
  double closestMomDiff = 99999999.;
  int closestID = -1;
  TVector3 candMom(0., 0., 0.);
  TVector3 candPos(0., 0., 0.);
  for(int i=0; i<fSTRAWEvent->GetNCandidates(); i++){
    TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(i));
    if(trackPos.Z()==180000.){
      candPos = StrawCand->GetPositionBeforeMagnet();
      candMom = StrawCand->GetThreeMomentumBeforeMagnet();
    }else if(trackPos.Z()==222000.){
      candPos = StrawCand->GetPositionAfterMagnet();
      candMom = StrawCand->GetThreeMomentumAfterMagnet();
    }else{
      cout<< user_normal() << "asking for wrong z position"<<endl;
    }
    if(trackPos.Z()!=candPos.Z()){
      if(histo) FillHisto("hWrongZplanes_mom"+s, candPos.Z());
      cout<< user_normal() << candPos.Z()<<" or "<<trackPos.Z()<<endl;
      cout<< user_normal() << "comparing momentum of track and candidate at different z planes"<<endl;
      continue;
    }
    if(wantCloseInPos && !IsCloseInPos(trackPos, candPos, trackMom.Mag(), fCutCloseInPosK, fCutCloseInPosQ, 0, s)) continue;
    double momDiff = (trackMom - candMom).Mag();
    if(momDiff<closestMomDiff){
      closestMomDiff = momDiff;
      closestID = i;
    }
  }
  if(closestID==-1){
    if(histo){
      FillHisto(s+"hNoCandClosestInMomentumForZ", trackPos.Z());
      FillHisto(s+"hNoCandClosestInMomentumMom", trackMom.Mag());
    }
  }else{
    TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(closestID));
    if(trackPos.Z()==180000.){
      candPos = StrawCand->GetPositionBeforeMagnet();
      candMom = StrawCand->GetThreeMomentumBeforeMagnet();
    }else if(trackPos.Z()==222000.){
      candPos = StrawCand->GetPositionAfterMagnet();
      candMom = StrawCand->GetThreeMomentumAfterMagnet();
    }
    if(histo){
      FillHisto(s+"hMomDiffClosestInMom", closestMomDiff);
      FillHisto(s+"hMomDiffVsPosDiffClosestInMom", (trackPos - candPos).Mag(), closestMomDiff);
      FillHisto(s+"hTrackMomXVsClosestInMomX", candMom.X(), trackMom.X());
      FillHisto(s+"hTrackMomYVsClosestInMomY", candMom.Y(), trackMom.Y());
    }
  }
  return closestID;
}

bool SpectrometerEfficiency::IsCloseInMom(TVector3 refMom, TVector3 mom, double cut, bool histo, TString s){
  TVector3 momdiff = refMom - mom;
  if(histo){
    FillHisto(s+"hMomDiffTrackCand_Mag", momdiff.Mag());
    FillHisto(s+"hMomDiffTrackCand_X", momdiff.X());
    FillHisto(s+"hMomDiffTrackCand_Y", momdiff.Y());
    FillHisto(s+"hMomDiffTrackCand_Z", momdiff.Z());
  }
  if(momdiff.Mag()<cut){
    return true;
  }else{
    return false;
  }
}

bool SpectrometerEfficiency::IsCloseInPos(TVector3 refPos, TVector3 pos, double mom, double cutK, double cutQ, bool histo, TString s){
  TVector3 posdiff = refPos - pos;
  if(histo){
    FillHisto(s+"hPosDiffTrackCand_Mag", posdiff.Mag());
    FillHisto(s+"hPosDiffTrackCand_X", posdiff.X());
    FillHisto(s+"hPosDiffTrackCand_Y", posdiff.Y());
    FillHisto(s+"hPosDiffVSMom", mom, posdiff.Mag());
  }
  if(posdiff.Mag()<(cutK*mom+cutQ)){
    return true;
  }else{
    return false;
  }
}

TVector3 SpectrometerEfficiency::GetPositionAtZ(TVector3 mom, TVector3 oldPos, double newZ){
  if(((newZ<=197600.)&&(oldPos.Z()<=197600.)) || ((newZ>=197600.)&&(oldPos.Z()>=197600.))){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - newZ);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - newZ);
    TVector3 newPos(xAtZ, yAtZ, newZ);
    return newPos;
  }else if((newZ>197600.)&&(oldPos.Z()<197600.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 197600.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 197600.);
    TVector3 posAtMagnet(xAtZ, yAtZ, 197600.);
    TVector3 newMom = MomAfterKick(mom, fMNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(197600. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(197600. - newZ);
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else if((newZ<197600.)&&(oldPos.Z()>197600.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 197600.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 197600.);
    TVector3 posAtMagnet(xAtZ, yAtZ, 197600.);
    TVector3 newMom = MomAfterKick(mom, -fMNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(197600. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(197600. - newZ);
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else{
    cout<< user_normal() << "cannot calculate the position"<<endl;
    return oldPos;
  }
}

TVector3 SpectrometerEfficiency::MomAfterKick(TVector3 oldMom, double kick){
  TVector3 mom = oldMom;
  TVector2 pTvec(mom.X(), mom.Z());
  double pT = pTvec.Mod();
  double pxNew = mom.X() - kick;
  double beta = acos(mom.X()/pT);
  double theta = acos(pxNew/pT);
  double alpha = theta - beta;
  mom.RotateY(-alpha);
  TVector3 newMom = mom;

  return newMom;
}

double SpectrometerEfficiency::GetLambdaGTK(double triggerTime){
  int f1[24] = {0};
  int f3[24] = {0};
  for(int i=0; i<fGTKEvent->GetNHits(); i++){
    TRecoGigaTrackerHit *GTKHit = static_cast<TRecoGigaTrackerHit*>(fGTKEvent->GetHit(i));
    double timeDiff = GTKHit->GetTime() - triggerTime;
    if((fabs(timeDiff)>=30.)) continue;
    int c = (int)(timeDiff/2.5);
    if(GTKHit->GetStationNo()==0){
      f1[c+11]++;
    }else if(GTKHit->GetStationNo()==2){
      f3[c+11]++;
    }
  }
  double sum = 0.;
  int nbins = 0;
  for(int j=0; j<24; j++){
    int c = j-11;
    double td = c*2.5;
    if(td<=-25. || td>30. || (td>-5. && td<=5.)) continue;
    nbins++;
    sum += (f1[j] + f3[j])/2.;
  }
  double lambda = sum/nbins;
  return lambda;
}

double SpectrometerEfficiency::GetLowEdgeFirstFilledBin(TH1 *h){
  int nbins = h->GetNbinsX();
  double lowEdge = -9999999.;
  for(int i=1; i<nbins+1; i++){
    double v = h->GetBinContent(i);
    if(v!=0){
      lowEdge = h->GetBinLowEdge(i);
      break;
    }
  }
  return lowEdge;
}

double SpectrometerEfficiency::GetUpperEdgeLastFilledBin(TH1 *h){
  int nbins = h->GetNbinsX();
  double upperEdge = 9999999.;
  for(int i=nbins; i>0; i--){
    double v = h->GetBinContent(i);
    if(v!=0){
      upperEdge = h->GetBinLowEdge(i) + h->GetBinWidth(i);
      break;
    }
  }
  return upperEdge;
}

double SpectrometerEfficiency::GetDistanceFromHoleCenter(double z, TVector3 pos, TVector3 mom){
  TVector3 npos;
  if(pos.Z()!=z){
    npos = GetPositionAtZ(mom, pos, z);
  }else{
    npos = pos;
  }

  double strY = 0.;
  double str0centerX = GeometricAcceptance::GetInstance()->GetXStrawChamberCentre(0);
  double str1centerX = GeometricAcceptance::GetInstance()->GetXStrawChamberCentre(1);
  double str0centerZ = GeometricAcceptance::GetInstance()->GetZStraw(0);
  double str1centerZ = GeometricAcceptance::GetInstance()->GetZStraw(1);

  double k = (str1centerX - str0centerX)/(str1centerZ - str0centerZ);
  double q = str1centerX - k*str1centerZ;

  double centerXatZ = k*z + q;
  double distance = sqrt(pow((npos.X()-centerXatZ), 2) + pow((npos.Y()-strY), 2));
  return distance;
}

void SpectrometerEfficiency::FillHistogramsForEfficiency(TLorentzVector trackFourMom, TVector3 trackPos, TString s){
  fNoCand = false;
  fNoMatch = false;

  //general info from K decay selection
  FillHisto(s+gen+"hNStrawCandidates", fSTRAWEvent->GetNCandidates());
  FillHisto(s+gen+"hPosVertex_Z", trackPos.Z());

  //GTK info
  double lambda = GetLambdaGTK(fTriggerTime);
  int nGTKHitsInSidebands = 0;
  FillHisto(s+gen+"hNGTKHits", fGTKEvent->GetNHits());
  for(int i=0; i<fGTKEvent->GetNHits(); i++){
    TRecoGigaTrackerHit *GTKHit = static_cast<TRecoGigaTrackerHit*>(fGTKEvent->GetHit(i));
    double time = GTKHit->GetTime();
    FillHisto(s+gen+"hTimeDiffGTKHitTrigger", time - fTriggerTime);
    if(fabs(time - fTriggerTime)>fCutTimeDifferenceForSidebands) nGTKHitsInSidebands++;
  }
  FillHisto(s+gen+"hNGTKHitsInSidebands", nGTKHitsInSidebands);
  FillHisto(s+gen+"hLambda", lambda);

  //track pointer from vertex to z=180m (before STRAW magnet)
  TVector3 newPos;
  TVector3 newMom;
  ApplyBlueTube(1, trackPos, trackFourMom.Vect(), 180000., &newPos, &newMom);

  //check if pointer is in geometrical acceptance
  if(fWantGeometricalAcceptance){
    if(!GeometricAcceptance::GetInstance()->InAcceptance(GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(0)).X(), GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(0)).Y(), kSpectrometer, 0, fMinDistanceWrtHoleCenter) ||
       !GeometricAcceptance::GetInstance()->InAcceptance(GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(1)).X(), GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(1)).Y(), kSpectrometer, 1, fMinDistanceWrtHoleCenter) ||
       !GeometricAcceptance::GetInstance()->InAcceptance(GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(2)).X(), GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(2)).Y(), kSpectrometer, 2, fMinDistanceWrtHoleCenter) ||
       !GeometricAcceptance::GetInstance()->InAcceptance(GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(3)).X(), GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(3)).Y(), kSpectrometer, 3, fMinDistanceWrtHoleCenter)){
      double x = 0.;
      double y = 0.;
      x = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(0)).X();
      y = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(0)).Y();
      FillHisto(s+nia+"hPos_YvsX_CH1", x, y);
      x = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(1)).X();
      y = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(1)).Y();
      FillHisto(s+nia+"hPos_YvsX_CH2", x, y);
      x = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(2)).X();
      y = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(2)).Y();
      FillHisto(s+nia+"hPos_YvsX_CH3", x, y);
      x = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(3)).X();
      y = GetPositionAtZ(newMom, newPos, GeometricAcceptance::GetInstance()->GetZStraw(3)).Y();
      FillHisto(s+nia+"hPos_YvsX_CH4", x, y);

      FillHisto(s+nia+"hMomTrack", newMom.Mag());
      FillHisto(s+nia+"hLambda", lambda);
      return;
    }
  }

  //check if there is a STRAW candidate
  if(fSTRAWEvent->GetNCandidates()==0){
    fNoCand = true;
    FillHisto(s+nc+"hNoCandidateInStraw", 1);
    FillHisto(s+nc+"hPosTrack_YvsX", newPos.X(), newPos.Y());
    FillHisto(s+nc+"hMomTrack", newMom.Mag());
    FillHisto(s+nc+"hLambda", lambda);
    FillHisto(s+nc+"hNHits", fSTRAWEvent->GetNHits());
    int nHitsCHV[4][4] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}};
    for(int i=0; i<fSTRAWEvent->GetNHits(); i++){
      TRecoSpectrometerHit *hit = static_cast<TRecoSpectrometerHit*>(fSTRAWEvent->GetHit(i));
      FillHisto(s+nc+"hHitChamberID_ViewID", hit->GetChamberID()*4 + hit->GetViewID());
      nHitsCHV[hit->GetChamberID()][hit->GetViewID()]++;
    }
  }

  //info at z=180m (before STRAW magnet)
  FillHisto(s+bef+"hPosTrack_YvsX", newPos.X(), newPos.Y());
  FillHisto(s+bef+"hMomTrack", newMom.Mag()); //DENOMINATOR of Efficiency
  FillHisto(s+bef+"hMomTrack_YvsX", newMom.X(), newMom.Y());
  FillHisto(s+bef+"hNGTKHitsInSidebands", nGTKHitsInSidebands);
  FillHisto(s+bef+"hLambda", lambda);
  FillHisto(s+bef+"hBurstID", fBurstID);
  FillHisto(s+bef+"hPosTrack_disc", GetDistanceFromHoleCenter(180000., newPos, newMom));

  if(fSTRAWEvent->GetNCandidates()!=0){
    //Look for candidate close in momentum and position
    int countNCloseInMomCloseInPos = 0;
    int countNCloseInPos = 0;
    int matchedID = -1;
    for(int i=0; i<fSTRAWEvent->GetNCandidates(); i++){
      TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(i));
      FillHisto(s+bef+"hCandidatesNChambers", StrawCand->GetNChambers());
      FillHisto(s+bef+"hMomDiffVSPosDiff", (newPos - StrawCand->GetPositionBeforeMagnet()).Mag(), (newMom - StrawCand->GetThreeMomentumBeforeMagnet()).Mag()/1e3);
      bool closeInMom = IsCloseInMom(newMom, StrawCand->GetThreeMomentumBeforeMagnet(), fCutCloseInMom, 1, s+bef);
      bool closeInPos = IsCloseInPos(newPos, StrawCand->GetPositionBeforeMagnet(), newMom.Mag(), fCutCloseInPosK, fCutCloseInPosQ, 1, s+bef);
      if(closeInMom && closeInPos){
	countNCloseInMomCloseInPos++;
	matchedID = i;
      }
      if(closeInPos) countNCloseInPos++;
      if(!closeInMom || !closeInPos){
	TVector3 posdiff = StrawCand->GetPositionBeforeMagnet() - newPos;
	TVector3 momdiff = StrawCand->GetThreeMomentumBeforeMagnet() - newMom;
	FillHisto(s+nm+"hMomDiffVsPosDiff", posdiff.Mag(), momdiff.Mag());
      }
    }
    FillHisto(s+bef+"hNCloseInPos", countNCloseInPos);
    FillHisto(s+bef+"hNCloseInPosVsTrackMom", newMom.Mag(), countNCloseInPos);
    FillHisto(s+bef+"hNCloseInPosVsTrackPos", TMath::Sqrt(newPos.X()*newPos.X() + newPos.Y()*newPos.Y()), countNCloseInPos);
    FillHisto(s+bef+"hNCloseInMomCloseInPos", countNCloseInMomCloseInPos);
    FillHisto(s+bef+"hNCloseInMomCloseInPosVsTrackMom", newMom.Mag(), countNCloseInMomCloseInPos);

    //no matching candidate found
    if(countNCloseInMomCloseInPos==0){
      fNoMatch = true;
      FillHisto(s+nm+"hNCandidates", fSTRAWEvent->GetNCandidates());
      FillHisto(s+nm+"hTrackM", trackFourMom.M());
      FillHisto(s+nm+"hPosTrack_YvsX", newPos.X(), newPos.Y());
      FillHisto(s+nm+"hMomTrack", newMom.Mag());
      FillHisto(s+nm+"hLambda", lambda);
      for(int i=0; i<fSTRAWEvent->GetNCandidates(); i++){
	TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(i));
	FillHisto(s+nm+"hCandidatesNChambers", StrawCand->GetNChambers());
      }
    }

    //candidate matching the track found
    if(countNCloseInMomCloseInPos!=0){
      FilterAccept();
      FillHisto(s+mat+"hPosTrack_YvsX", newPos.X(), newPos.Y());
      FillHisto(s+mat+"hMomTrack", newMom.Mag()); //NUMERATOR of Efficiency
      FillHisto(s+mat+"hNGTKHitsInSidebands", nGTKHitsInSidebands);
      FillHisto(s+mat+"hLambda", lambda);
      FillHisto(s+mat+"hBurstID", fBurstID);
      FillHisto(s+mat+"hPosTrack_disc", GetDistanceFromHoleCenter(180000., newPos, newMom));
      TRecoSpectrometerCandidate *StrawCand = static_cast<TRecoSpectrometerCandidate*>(fSTRAWEvent->GetCandidate(matchedID));
      FillHisto(s+mat+"hMatchedCandidateNChambers", StrawCand->GetNChambers());
      if(StrawCand->GetNChambers()==4){
	FillHisto(s+mat+"4CH/hPosTrack_YvsX", newPos.X(), newPos.Y());
	FillHisto(s+mat+"4CH/hMomTrack", newMom.Mag()); //NUMERATOR of Efficiency
	FillHisto(s+mat+"4CH/hNGTKHitsInSidebands", nGTKHitsInSidebands);
	FillHisto(s+mat+"4CH/hLambda", lambda);
	FillHisto(s+mat+"4CH/hBurstID", fBurstID);
	FillHisto(s+mat+"4CH/hPosTrack_disc", GetDistanceFromHoleCenter(180000., newPos, newMom));
      }
    }
  }
}
