// ---------------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-01-30
//
// ---------------------------------------------------------------

/// \class LKrEopMonitor
/// \Brief
/// LKr cell-by-cell E/p monitor
/// \EndBrief
/// \Detailed
/// The Cell-by-Cell E/p efficiency is evaluated using a Ke3 sample and requiring the E/p to be in the range [0.9,1.1]
/// In addition, E/p peak centre is monitored on a burst-by-burst basis
/// Only L0 masks without requirement on LKr are used (CTRL + Mask0), in order to avoid bias.
/// The analyzer can be run in two modes:
/// 1) Reading reconstructed data (without using the --histo command line option);
/// 2) In the HISTO mode (using the --histo command line option), it reads its own
/// output and produces final report in the form of a PDF file.
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TLatex.h>
#include <TLegend.h>
#include <TColor.h>
#include <TBox.h>
#include <TF1.h>
#include <TLine.h>
#include "BaseAnalysis.hh"
#include "LKrBadCells.hh"
#include "LKrEopMonitor.hh"
#include "DownstreamTrack.hh"
#include "EnergyCluster.hh"
#include "GeometricAcceptance.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

LKrEopMonitor::LKrEopMonitor(Core::BaseAnalysis *ba): Analyzer(ba, "LKrEopMonitor"), fCanvas(nullptr), fc_dx_y(nullptr), fc_dy_x(nullptr), fBadCellsMask(0), fReadingData(false), fEoPEfficiencyThreshold(0.), fMeanEoPThreshold(0.), fMaxNCellsInDeadClusterThreshold(0), fNManyHitsThreshold(0), fDeltaEoPForEfficiency(0.), fArgonionCountsMin(0.), fNSelectedTriggersMin(0.), fDeltaEoPPBin(0.), fOutPDFFileName(""), fBurstID(0), fArgonionCounts(0.), fNTriggers(0), fNSelectedTriggers(0), fNEoPMatchedPerBurst(0.), fNEoPExpectedPerBurst(0.), fNEoPMatchedStrictPerBurst(0.), fNEoPExpectedStrictPerBurst(0.), fNHotCellsFromHitMap(0), fNHotCellsFromQualityWarnings(0), fNHotCellsFromPedestals(0), fNDeadCellsFromHitMap(0), fMaxNCellsInDeadCluster(0), fNBitFlipCells(0), fNEventsWithManyHits(0), fHNHits(nullptr), fHNQualityWarningsVsROChannel(nullptr), fHEoPMatched(nullptr), fHEoPExpected(nullptr), fHEoPEfficiency(nullptr), fHEoP(nullptr), fHEoPVsP(nullptr), fHEoPVsTrkClsTime(nullptr), fHEoPVsChannel(nullptr), fHdx_vs_y(nullptr), fHdy_vs_x(nullptr), fHEoPEfficiencyVsBurstID(nullptr), fHEoPEfficiencyStrictVsBurstID(nullptr), fHMeanEoPVsBurstID(nullptr), fHSigmaEoPVsBurstID(nullptr), fHEoPFitPar0VsBurstID(nullptr), fHEoPFitPar1VsBurstID(nullptr), fHNEoPExpectedVsBurstID(nullptr), fHNEoPExpectedStrictVsBurstID(nullptr), fHArgonionCountsVsBurstID(nullptr), fHNTriggersVsBurstID(nullptr), fHNSelectedTriggersVsBurstID(nullptr), fHNHotCellsFromHitMapVsBurstID(nullptr), fHNHotCellsFromQualityWarningsVsBurstID(nullptr), fHNHotCellsFromPedestalsVsBurstID(nullptr), fHNDeadCellsFromHitMapVsBurstID(nullptr), fHMaxNCellsInDeadClusterVsBurstID(nullptr), fHNBitFlipCellsVsBurstID(nullptr), fHNEventsWithManyHitsVsBurstID(nullptr), fGEoPVsP(nullptr), fGdx_vs_y(nullptr), fGdy_vs_x(nullptr), fFdx_vs_y(nullptr), fFdy_vs_x(nullptr) {

  Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts
  RequestL0Data();
  RequestL0SpecialTrigger();
  RequestBeamSpecialTrigger();

  fOutPDFFileName = fAnalyzerName + ".pdf";

  AddParam("EoPEfficiencyThreshold",         &fEoPEfficiencyThreshold, 0.8);
  AddParam("MeanEoPThreshold",               &fMeanEoPThreshold, 0.975);
  AddParam("MaxNCellsInDeadClusterThreshold",&fMaxNCellsInDeadClusterThreshold, 10);
  AddParam("NManyHitsThreshold",             &fNManyHitsThreshold, 2000);
  AddParam("DeltaEoP",                       &fDeltaEoPForEfficiency, 0.10);
  AddParam("ArgonionCountsMin",              &fArgonionCountsMin, 1.e5);
  AddParam("NSelectedTriggersMin",           &fNSelectedTriggersMin, 1.e3);
  AddParam("DeltaEoPPBin",                   &fDeltaEoPPBin, 2500.); //Eop in 2.5 GeV momentum bins
}

LKrEopMonitor::~LKrEopMonitor() {
}

void LKrEopMonitor::InitHist() {

  fReadingData = GetIsTree();
  Int_t NBinsX = 1280;
  Double_t XMin = -1280.5;
  Double_t XMax = 1279.5;
  Int_t NBinsY = 1280;
  Double_t YMin = -1280.5;
  Double_t YMax = 1279.5;
  Int_t NBinsP = 120;
  Double_t PMin = 0;
  Double_t PMax = 60000.;

  fBadCellsMask = *(Int_t*) GetOutput("LKrBadCells.BadCellsMask");
  //force ignoring dead cells
  fBadCellsMask = fBadCellsMask&0xfffe;

  if (fReadingData) {
    std::cout << user_normal() << "Reading reconstructed data" << std::endl;

    fHNHits                       = static_cast<TH1F*>(RequestHistogram("LKrMonitor", "NHits",                       false)); // from the reconstructed file
    fHNQualityWarningsVsROChannel = static_cast<TH1F*>(RequestHistogram("LKrMonitor", "NQualityWarningsVsROChannel", false)); // from the reconstructed file

    BookHisto("hEoPMatched",      new TH2F("hEoPMatched",  "hEoPMatched",  NBinsX, XMin, XMax, NBinsY, YMin, YMax));
    BookHisto("hEoPExpected",     new TH2F("hEoPExpected", "hEoPExpected", NBinsX, XMin, XMax, NBinsY, YMin, YMax));
    BookHisto("hEoP",             new TH1F("hEoP",         "hEoP",         1200, -0.001, 1.199));
    BookHisto("hEoPVsP",          new TH2F("hEoPVsP",      "hEoPVsP",      NBinsP, PMin, PMax, 1200, -0.001, 1.199));
    BookHisto("hEoPVsTrkClsTime", new TH2F("hEoPVsTrkClsTime", "hEoPVsTrkClsTime", 800, -200, 200, 1200, -0.001, 1.199));
    BookHisto("hEoPVsChannel",    new TH2F("hEoPVsChannel", "hEoPVsChannel", 16384, -0.5,16383.5, 400, 0.799, 1.199));
    BookHisto("hTrackTime",       new TH1F("hTrackTime",   "hTrackTime",   1000, -200., 200.));
    BookHisto("hTrkClsDxVsX",     new TH2F("hTrkClsDxVsX", "hTrkClsDxVsX", 256,-1285.,1275., 100, -50.,50.));
    BookHisto("hTrkClsDxVsY",     new TH2F("hTrkClsDxVsY", "hTrkClsDxVsY", 256,-1285.,1275., 100, -50.,50.));
    BookHisto("hTrkClsDyVsX",     new TH2F("hTrkClsDyVsX", "hTrkClsDyVsX", 256,-1285.,1275., 100, -50.,50.));
    BookHisto("hTrkClsDyVsY",     new TH2F("hTrkClsDyVsY", "hTrkClsDyVsY", 256,-1285.,1275., 100, -50.,50.));

    fHEoPVsP = new TH2F("hEoPVsPPerBurst", "hEoPVsPPerBurst", NBinsP, PMin, PMax, 240, -0.005, 1.195); // to be reset every burst
    BookHisto("hEoPEfficiencyVsBurstID", new TGraphErrors());
    fHEoPEfficiencyVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEoPEfficiencyVsBurstID");
    fHEoPEfficiencyVsBurstID->SetName("hEoPEfficiencyVsBurstID");
    fHEoPEfficiencyVsBurstID->Set(0);
    BookHisto("hEoPEfficiencyStrictVsBurstID", new TGraphErrors());
    fHEoPEfficiencyStrictVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEoPEfficiencyStrictVsBurstID");
    fHEoPEfficiencyStrictVsBurstID->SetName("hEoPEfficiencyStrictVsBurstID");
    fHEoPEfficiencyStrictVsBurstID->Set(0);
    BookHisto("hMeanEoPVsBurstID", new TGraphErrors());
    fHMeanEoPVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hMeanEoPVsBurstID");
    fHMeanEoPVsBurstID->SetName("hMeanEoPVsBurstID");
    fHMeanEoPVsBurstID->Set(0);
    BookHisto("hSigmaEoPVsBurstID", new TGraphErrors());
    fHSigmaEoPVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hSigmaEoPVsBurstID");
    fHSigmaEoPVsBurstID->SetName("hSigmaEoPVsBurstID");
    fHSigmaEoPVsBurstID->Set(0);
    BookHisto("hEoPFitPar0VsBurstID", new TGraphErrors());
    fHEoPFitPar0VsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEoPFitPar0VsBurstID");
    fHEoPFitPar0VsBurstID->SetName("hEoPFitPar0VsBurstID");
    fHEoPFitPar0VsBurstID->Set(0);
    BookHisto("hEoPFitPar1VsBurstID", new TGraphErrors());
    fHEoPFitPar1VsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEoPFitPar1VsBurstID");
    fHEoPFitPar1VsBurstID->SetName("hEoPFitPar1VsBurstID");
    fHEoPFitPar1VsBurstID->Set(0);
    BookHisto("hNEoPExpectedVsBurstID", new TGraphErrors());
    fHNEoPExpectedVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNEoPExpectedVsBurstID");
    fHNEoPExpectedVsBurstID->SetName("hNEoPExpectedVsBurstID");
    fHNEoPExpectedVsBurstID->Set(0);
    BookHisto("hNEoPExpectedStrictVsBurstID", new TGraphErrors());
    fHNEoPExpectedStrictVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNEoPExpectedStrictVsBurstID");
    fHNEoPExpectedStrictVsBurstID->SetName("hNEoPExpectedStrictVsBurstID");
    fHNEoPExpectedStrictVsBurstID->Set(0);
    BookHisto("hArgonionCountsVsBurstID", new TGraphErrors());
    fHArgonionCountsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->SetName("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->Set(0);
    BookHisto("hNTriggersVsBurstID", new TGraphErrors());
    fHNTriggersVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->SetName("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->Set(0);
    BookHisto("hNSelectedTriggersVsBurstID", new TGraphErrors());
    fHNSelectedTriggersVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->SetName("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->Set(0);
    BookHisto("hNHotCellsFromHitMapVsBurstID", new TGraphErrors());
    fHNHotCellsFromHitMapVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNHotCellsFromHitMapVsBurstID");
    fHNHotCellsFromHitMapVsBurstID->SetName("hNHotCellsFromHitMapVsBurstID");
    fHNHotCellsFromHitMapVsBurstID->Set(0);
    BookHisto("hNHotCellsFromQualityWarningsVsBurstID", new TGraphErrors());
    fHNHotCellsFromQualityWarningsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNHotCellsFromQualityWarningsVsBurstID");
    fHNHotCellsFromQualityWarningsVsBurstID->SetName("hNHotCellsFromQualityWarningsVsBurstID");
    fHNHotCellsFromQualityWarningsVsBurstID->Set(0);
    BookHisto("hNHotCellsFromPedestalsVsBurstID", new TGraphErrors());
    fHNHotCellsFromPedestalsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNHotCellsFromPedestalsVsBurstID");
    fHNHotCellsFromPedestalsVsBurstID->SetName("hNHotCellsFromPedestalsVsBurstID");
    fHNHotCellsFromPedestalsVsBurstID->Set(0);
    BookHisto("hNDeadCellsFromHitMapVsBurstID", new TGraphErrors());
    fHNDeadCellsFromHitMapVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNDeadCellsFromHitMapVsBurstID");
    fHNDeadCellsFromHitMapVsBurstID->SetName("hNDeadCellsFromHitMapVsBurstID");
    fHNDeadCellsFromHitMapVsBurstID->Set(0);
    BookHisto("hMaxNCellsInDeadClusterVsBurstID", new TGraphErrors());
    fHMaxNCellsInDeadClusterVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hMaxNCellsInDeadClusterVsBurstID");
    fHMaxNCellsInDeadClusterVsBurstID->SetName("hMaxNCellsInDeadClusterVsBurstID");
    fHMaxNCellsInDeadClusterVsBurstID->Set(0);
    BookHisto("hNBitFlipCellsVsBurstID", new TGraphErrors());
    fHNBitFlipCellsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNBitFlipCellsVsBurstID");
    fHNBitFlipCellsVsBurstID->SetName("hNBitFlipCellsVsBurstID");
    fHNBitFlipCellsVsBurstID->Set(0);
    BookHisto("hNEventsWithManyHitsVsBurstID", new TGraphErrors());
    fHNEventsWithManyHitsVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNEventsWithManyHitsVsBurstID");
    fHNEventsWithManyHitsVsBurstID->SetName("hNEventsWithManyHitsVsBurstID");
    fHNEventsWithManyHitsVsBurstID->Set(0);

    // Set up the online monitor
    CreateCanvas("LKrEopMonitorCanvas");
    PlacePlotOnCanvas("hEoPEfficiencyVsBurstID", "LKrEopMonitorCanvas",1,1);
    PlacePlotOnCanvas("hMeanEoPVsBurstID",    "LKrEopMonitorCanvas",1,2);
    SetUpdateInterval(50000);
  }
  else {
    std::cout << user_normal() << "Reading my own output" << std::endl;

    fHEoPExpected = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEoPExpected", true));
    fHEoPMatched  = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEoPMatched",  true));
    if (!fHEoPMatched) {
      std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      return;
    }

    fHEoPEfficiencyVsBurstID                = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hEoPEfficiencyVsBurstID",       true);
    fHEoPEfficiencyStrictVsBurstID          = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hEoPEfficiencyStrictVsBurstID", true);
    fHMeanEoPVsBurstID                      = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hMeanEoPVsBurstID",             true);
    fHSigmaEoPVsBurstID                     = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hSigmaEoPVsBurstID",            true);
    fHEoPFitPar0VsBurstID                   = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hEoPFitPar0VsBurstID",          true);
    fHEoPFitPar1VsBurstID                   = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hEoPFitPar1VsBurstID",          true);
    fHNEoPExpectedVsBurstID                 = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNEoPExpectedVsBurstID",        true);
    fHNEoPExpectedStrictVsBurstID           = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNEoPExpectedStrictVsBurstID",  true);
    fHArgonionCountsVsBurstID               = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hArgonionCountsVsBurstID",      true);
    fHNTriggersVsBurstID                    = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNTriggersVsBurstID",           true);
    fHNSelectedTriggersVsBurstID            = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNSelectedTriggersVsBurstID",   true);
    fHNHotCellsFromHitMapVsBurstID          = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNHotCellsFromHitMapVsBurstID",         true);
    fHNHotCellsFromQualityWarningsVsBurstID = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNHotCellsFromQualityWarningsVsBurstID",true);
    fHNHotCellsFromPedestalsVsBurstID       = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNHotCellsFromPedestalsVsBurstID",      true);
    fHNDeadCellsFromHitMapVsBurstID         = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNDeadCellsFromHitMapVsBurstID",        true);
    fHMaxNCellsInDeadClusterVsBurstID       = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hMaxNCellsInDeadClusterVsBurstID",      true);
    fHNBitFlipCellsVsBurstID                = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNBitFlipCellsVsBurstID",       true);
    fHNEventsWithManyHitsVsBurstID          = (TGraphErrors*)RequestHistogram(fAnalyzerName, "hNEventsWithManyHitsVsBurstID", true);

    BookHisto(new TH2F("hEoPEfficiency", "hEoPEfficiency", fHEoPExpected->GetNbinsX(), XMin, XMax, fHEoPExpected->GetNbinsY(), YMin, YMax));
    fHEoPEfficiency = static_cast<TH2F*>(fHisto.GetTH2("hEoPEfficiency"));
    fHEoP    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "hEoP", true));
    fHEoPVsP = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEoPVsP", true));
    fHEoPVsTrkClsTime = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEoPVsTrkClsTime", true));
    fHEoPVsChannel = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hEoPVsChannel", true));

    // Histograms for LKr-STRAW alignment
    fHdx_vs_y = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hTrkClsDxVsY", true));
    fHdy_vs_x = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hTrkClsDyVsX", true));
  }
}

void LKrEopMonitor::ProcessSpecialTriggerUser(int, unsigned int triggerType){
  if(triggerType!=0x23) return; // only EOB
  fArgonionCounts = GetBeamSpecialTrigger()->GetCountsARGONION()/1.e9;
}

void LKrEopMonitor::Process(Int_t) {

  if (!fReadingData) return;

  fBurstID = GetEventHeader()->GetBurstID();

  fNTriggers++;

  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData   = L0DataType & 0x1;
  Bool_t CTRLTrigger   = L0DataType & 0x10;
  //Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0x1)) || CTRLTrigger; //MASK0 only + CTRL
  Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  if (!TriggerOK) return; // process control triggers and selected MASKS only

  fNSelectedTriggers++;
  const double LKrStartPos = GeometricAcceptance::GetInstance()->GetZLKr();

  Bool_t   Ke3Selected = *(Bool_t*)  GetOutput("Ke3Selection.EventSelected");
  Double_t Ke3Time     = *(Double_t*)GetOutput("Ke3Selection.Ke3Time");
  Int_t    Ke3TrackID  = *(Int_t*)   GetOutput("Ke3Selection.Ke3TrackID");
  if(!Ke3Selected) return;

  std::vector<EnergyCluster> Clusters = *(std::vector<EnergyCluster>*)GetOutput("EnergyClusterBuilder.Output");
  Bool_t Ke3Strict = false;
  Int_t NClustersInTime = 0;
  for(UInt_t iCluster=0; iCluster<Clusters.size();iCluster++){
    if(fabs(Clusters[iCluster].GetLKrCandidate()->GetTime()-Ke3Time)>3.) continue; // not in time with Ke3 track
    //if(Clusters[iCluster].GetLKrCandidate()->GetClusterEnergy()<1000.)   continue; // less than 1 GeV
    NClustersInTime++;
  }
  if(NClustersInTime==3) Ke3Strict = true;

  std::vector<DownstreamTrack> Tracks = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  FillHisto("hTrackTime",Tracks[Ke3TrackID].GetCHODTime()-Ke3Time);
  FillHisto("hEoPVsP",Tracks[Ke3TrackID].GetMomentum(),Tracks[Ke3TrackID].GetLKrEoP());
  if(Tracks[Ke3TrackID].LKrAssociationExists()) {
    TRecoLKrCandidate* LKrCand = Tracks[Ke3TrackID].GetLKrAssociationOutput().GetBestAssociationRecord()->GetLKrCandidate();
    FillHisto("hEoPVsTrkClsTime",Tracks[Ke3TrackID].GetCHODTime()-LKrCand->GetTime(),Tracks[Ke3TrackID].GetLKrEoP());
    FillHisto("hTrkClsDxVsX",Tracks[Ke3TrackID].xAt(LKrStartPos),Tracks[Ke3TrackID].xAt(LKrStartPos)-LKrCand->GetClusterX());
    FillHisto("hTrkClsDxVsY",Tracks[Ke3TrackID].yAt(LKrStartPos),Tracks[Ke3TrackID].xAt(LKrStartPos)-LKrCand->GetClusterX());
    FillHisto("hTrkClsDyVsX",Tracks[Ke3TrackID].xAt(LKrStartPos),Tracks[Ke3TrackID].yAt(LKrStartPos)-LKrCand->GetClusterY());
    FillHisto("hTrkClsDyVsY",Tracks[Ke3TrackID].yAt(LKrStartPos),Tracks[Ke3TrackID].yAt(LKrStartPos)-LKrCand->GetClusterY());
  }
  FillHisto("hEoP",Tracks[Ke3TrackID].GetLKrEoP());
  if(Tracks[Ke3TrackID].GetLKrXCellID()>=0 && Tracks[Ke3TrackID].GetLKrYCellID()>=0){
    Int_t CellID = 128*Tracks[Ke3TrackID].GetLKrXCellID()+Tracks[Ke3TrackID].GetLKrYCellID();
    FillHisto("hEoPVsChannel",CellID,Tracks[Ke3TrackID].GetLKrEoP());
  }
  fHEoPVsP->Fill(Tracks[Ke3TrackID].GetMomentum(),Tracks[Ke3TrackID].GetLKrEoP());
  if(fabs(Tracks[Ke3TrackID].GetLKrEoP()-1.)<fDeltaEoPForEfficiency) {
    FillHisto("hEoPMatched",Tracks[Ke3TrackID].xAtAfterMagnet(LKrStartPos),Tracks[Ke3TrackID].yAtAfterMagnet(LKrStartPos));
    fNEoPMatchedPerBurst++;
    if(Ke3Strict) fNEoPMatchedStrictPerBurst++;
  }
  FillHisto("hEoPExpected",Tracks[Ke3TrackID].xAtAfterMagnet(LKrStartPos),Tracks[Ke3TrackID].yAtAfterMagnet(LKrStartPos));
  fNEoPExpectedPerBurst++;
  if(Ke3Strict) fNEoPExpectedStrictPerBurst++;
}

void LKrEopMonitor::EndOfJobUser() {

  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

  /////////////
  // HISTO mode

  if (!fReadingData) {
    if (!fHEoPMatched) {
      std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      return;
    }

    /////////////////////////
    // Create efficiency histos

    // Rebin histos to get enough statistics in each bin
    fHEoPExpected->Rebin2D(5,5);
    fHEoPMatched->Rebin2D(5,5);
    fHEoPEfficiency->Rebin2D(5,5);
    fHEoPEfficiency->Divide(fHEoPMatched, fHEoPExpected, 1., 1., "B");

    ////////////////////////////////////////////
    // Compute the LKr-STRAW alignment constants

    ComputeLKrStrawALignmentConstants();

    /////////////////////////
    // Create the BadBurst list

    CreateBadBurstList();

    // Remove empty bursts from the TGraphs
    for(Int_t iPoint=0;iPoint<fHMeanEoPVsBurstID->GetN();iPoint++){
      double  BurstID=0., NSelectedTriggers=0.;
      fHNSelectedTriggersVsBurstID->GetPoint(iPoint,BurstID,NSelectedTriggers);
      if(NSelectedTriggers<fNSelectedTriggersMin){
        fHEoPEfficiencyVsBurstID->RemovePoint(iPoint);
        fHEoPEfficiencyStrictVsBurstID->RemovePoint(iPoint);
        fHMeanEoPVsBurstID->RemovePoint(iPoint);
        fHSigmaEoPVsBurstID->RemovePoint(iPoint);
        fHEoPFitPar0VsBurstID->RemovePoint(iPoint);
        fHEoPFitPar1VsBurstID->RemovePoint(iPoint);
        fHNEoPExpectedVsBurstID->RemovePoint(iPoint);
        fHNEoPExpectedStrictVsBurstID->RemovePoint(iPoint);
        fHArgonionCountsVsBurstID->RemovePoint(iPoint);
        fHNTriggersVsBurstID->RemovePoint(iPoint);
        fHNSelectedTriggersVsBurstID->RemovePoint(iPoint);
        fHNHotCellsFromHitMapVsBurstID->RemovePoint(iPoint);
        fHNHotCellsFromQualityWarningsVsBurstID->RemovePoint(iPoint);
        fHNHotCellsFromPedestalsVsBurstID->RemovePoint(iPoint);
        fHNDeadCellsFromHitMapVsBurstID->RemovePoint(iPoint);
        fHMaxNCellsInDeadClusterVsBurstID->RemovePoint(iPoint);
        fHNBitFlipCellsVsBurstID->RemovePoint(iPoint);
        fHNEventsWithManyHitsVsBurstID->RemovePoint(iPoint);
        iPoint--;
      }
    }

    /////////////////////////
    // Produce the PDF output

    BuildPDFReport();
  }

  SaveAllPlots();
  gErrorIgnoreLevel = -1; // restore the default
}

void LKrEopMonitor::StartOfBurstUser() {
  fBurstID = GetBurstID();
  fArgonionCounts = 0.;
  fNTriggers = 0;
  fNSelectedTriggers = 0;
  fNEoPMatchedPerBurst = 0.;
  fNEoPExpectedPerBurst = 0.;
  fNEoPMatchedStrictPerBurst = 0.;
  fNEoPExpectedStrictPerBurst = 0.;
  fNHotCellsFromHitMap=0;
  fNHotCellsFromQualityWarnings=0;
  fNHotCellsFromPedestals=0;
  fNDeadCellsFromHitMap=0;
  fMaxNCellsInDeadCluster=0;
  fNBitFlipCells = 0;
  fNEventsWithManyHits = 0;
  if (fReadingData)  fHEoPVsP->Reset("M"); // Resetting EoPPerBurst
}

void LKrEopMonitor::EndOfBurstUser() {
  if (fReadingData) {
    fNHotCellsFromHitMap          = (*(std::vector<LKrCell>*) GetOutput("LKrBadCells.HotCellsFromHitMap")).size();
    fNHotCellsFromQualityWarnings = (*(std::vector<LKrCell>*) GetOutput("LKrBadCells.HotCellsFromQualityWarnings")).size();
    fNHotCellsFromPedestals       = (*(std::vector<LKrCell>*) GetOutput("LKrBadCells.HotCellsFromPedestals")).size();
    fNDeadCellsFromHitMap         = (*(std::vector<LKrCell>*) GetOutput("LKrBadCells.DeadCellsFromHitMap")).size();
    fMaxNCellsInDeadCluster       = *(Int_t*) GetOutput("LKrBadCells.MaxNCellsInDeadCluster");
    if(fArgonionCounts<0.3){ // < 20% nominal intensity, ignore dead cells in the peripheral cells
      fMaxNCellsInDeadCluster     = *(Int_t*) GetOutput("LKrBadCells.MaxNCellsInDeadClusterInnerCircle");
    }
    if(fHNQualityWarningsVsROChannel){
      Double_t NEntries = GetTree("Reco")->GetTree()->GetEntries();
      for(Int_t iBin=1;iBin<=fHNQualityWarningsVsROChannel->GetNbinsX();iBin++){
        if(fHNQualityWarningsVsROChannel->GetBinContent(iBin)>1.e-3*NEntries) fNBitFlipCells++;
      }
    }
    else fNBitFlipCells = -1; //histo not found
    if(fHNHits){
      fNEventsWithManyHits = fHNHits->Integral(fHNHits->FindBin(fNManyHitsThreshold),fHNHits->GetNbinsX());
    }
    else fNEventsWithManyHits = -1; //histo not found
    fHNEoPExpectedVsBurstID->Set(fHNEoPExpectedVsBurstID->GetN()+1);
    fHNEoPExpectedVsBurstID->SetPoint(fHNEoPExpectedVsBurstID->GetN()-1,fBurstID,fNEoPExpectedPerBurst);
    fHNEoPExpectedVsBurstID->SetPointError(fHNEoPExpectedVsBurstID->GetN()-1,0,sqrt(fNEoPExpectedPerBurst));
    fHNEoPExpectedStrictVsBurstID->Set(fHNEoPExpectedStrictVsBurstID->GetN()+1);
    fHNEoPExpectedStrictVsBurstID->SetPoint(fHNEoPExpectedStrictVsBurstID->GetN()-1,fBurstID,fNEoPExpectedStrictPerBurst);
    fHNEoPExpectedStrictVsBurstID->SetPointError(fHNEoPExpectedStrictVsBurstID->GetN()-1,0,sqrt(fNEoPExpectedStrictPerBurst));
    double Efficiency=0., eEfficiency=0., EfficiencyStrict=0., eEfficiencyStrict=0.;
    if(fNEoPExpectedPerBurst){
      Efficiency  = fNEoPMatchedPerBurst/fNEoPExpectedPerBurst;
      eEfficiency = sqrt(Efficiency*(1.-Efficiency)/fNEoPExpectedPerBurst);
      EfficiencyStrict  = fNEoPMatchedStrictPerBurst/fNEoPExpectedPerBurst;
      eEfficiencyStrict = sqrt(EfficiencyStrict*(1.-EfficiencyStrict)/fNEoPExpectedPerBurst);
    }
    //if(fNEoPExpectedStrictPerBurst){
    //  EfficiencyStrict  = fNEoPMatchedStrictPerBurst/fNEoPExpectedStrictPerBurst;
    //  eEfficiencyStrict = sqrt(EfficiencyStrict*(1.-EfficiencyStrict)/fNEoPExpectedStrictPerBurst);
    //}
    fHEoPEfficiencyVsBurstID->Set(fHEoPEfficiencyVsBurstID->GetN()+1);
    fHEoPEfficiencyVsBurstID->SetPoint(fHEoPEfficiencyVsBurstID->GetN()-1,fBurstID,Efficiency);
    fHEoPEfficiencyVsBurstID->SetPointError(fHEoPEfficiencyVsBurstID->GetN()-1,0,eEfficiency);
    fHEoPEfficiencyStrictVsBurstID->Set(fHEoPEfficiencyStrictVsBurstID->GetN()+1);
    fHEoPEfficiencyStrictVsBurstID->SetPoint(fHEoPEfficiencyStrictVsBurstID->GetN()-1,fBurstID,EfficiencyStrict);
    fHEoPEfficiencyStrictVsBurstID->SetPointError(fHEoPEfficiencyStrictVsBurstID->GetN()-1,0,eEfficiencyStrict);
    fHArgonionCountsVsBurstID->Set(fHArgonionCountsVsBurstID->GetN()+1);
    fHArgonionCountsVsBurstID->SetPoint(fHArgonionCountsVsBurstID->GetN()-1,fBurstID, fArgonionCounts);
    fHArgonionCountsVsBurstID->SetPointError(fHArgonionCountsVsBurstID->GetN()-1,0,0);
    fHNTriggersVsBurstID->Set(fHNTriggersVsBurstID->GetN()+1);
    fHNTriggersVsBurstID->SetPoint(fHNTriggersVsBurstID->GetN()-1,fBurstID, fNTriggers);
    fHNTriggersVsBurstID->SetPointError(fHNTriggersVsBurstID->GetN()-1,0,0);
    fHNSelectedTriggersVsBurstID->Set(fHNSelectedTriggersVsBurstID->GetN()+1);
    fHNSelectedTriggersVsBurstID->SetPoint(fHNSelectedTriggersVsBurstID->GetN()-1,fBurstID, fNSelectedTriggers);
    fHNSelectedTriggersVsBurstID->SetPointError(fHNSelectedTriggersVsBurstID->GetN()-1,0,0);
    fHNHotCellsFromHitMapVsBurstID->Set(fHNHotCellsFromHitMapVsBurstID->GetN()+1);
    fHNHotCellsFromHitMapVsBurstID->SetPoint(fHNHotCellsFromHitMapVsBurstID->GetN()-1,fBurstID,fNHotCellsFromHitMap);
    fHNHotCellsFromHitMapVsBurstID->SetPointError(fHNHotCellsFromHitMapVsBurstID->GetN()-1,0,0);
    fHNHotCellsFromQualityWarningsVsBurstID->Set(fHNHotCellsFromQualityWarningsVsBurstID->GetN()+1);
    fHNHotCellsFromQualityWarningsVsBurstID->SetPoint(fHNHotCellsFromQualityWarningsVsBurstID->GetN()-1,fBurstID,fNHotCellsFromQualityWarnings);
    fHNHotCellsFromQualityWarningsVsBurstID->SetPointError(fHNHotCellsFromQualityWarningsVsBurstID->GetN()-1,0,0);
    fHNHotCellsFromPedestalsVsBurstID->Set(fHNHotCellsFromPedestalsVsBurstID->GetN()+1);
    fHNHotCellsFromPedestalsVsBurstID->SetPoint(fHNHotCellsFromPedestalsVsBurstID->GetN()-1,fBurstID,fNHotCellsFromPedestals);
    fHNHotCellsFromPedestalsVsBurstID->SetPointError(fHNHotCellsFromPedestalsVsBurstID->GetN()-1,0,0);
    fHNDeadCellsFromHitMapVsBurstID->Set(fHNDeadCellsFromHitMapVsBurstID->GetN()+1);
    fHNDeadCellsFromHitMapVsBurstID->SetPoint(fHNDeadCellsFromHitMapVsBurstID->GetN()-1,fBurstID,fNDeadCellsFromHitMap);
    fHNDeadCellsFromHitMapVsBurstID->SetPointError(fHNDeadCellsFromHitMapVsBurstID->GetN()-1,0,0);
    fHMaxNCellsInDeadClusterVsBurstID->Set(fHMaxNCellsInDeadClusterVsBurstID->GetN()+1);
    fHMaxNCellsInDeadClusterVsBurstID->SetPoint(fHMaxNCellsInDeadClusterVsBurstID->GetN()-1,fBurstID,fMaxNCellsInDeadCluster);
    fHMaxNCellsInDeadClusterVsBurstID->SetPointError(fHMaxNCellsInDeadClusterVsBurstID->GetN()-1,0,0);
    fHNBitFlipCellsVsBurstID->Set(fHNBitFlipCellsVsBurstID->GetN()+1);
    fHNBitFlipCellsVsBurstID->SetPoint(fHNBitFlipCellsVsBurstID->GetN()-1,fBurstID, fNBitFlipCells);
    fHNBitFlipCellsVsBurstID->SetPointError(fHNBitFlipCellsVsBurstID->GetN()-1,0,0);
    fHNEventsWithManyHitsVsBurstID->Set(fHNEventsWithManyHitsVsBurstID->GetN()+1);
    fHNEventsWithManyHitsVsBurstID->SetPoint(fHNEventsWithManyHitsVsBurstID->GetN()-1,fBurstID, fNEventsWithManyHits);
    fHNEventsWithManyHitsVsBurstID->SetPointError(fHNEventsWithManyHitsVsBurstID->GetN()-1,0,0);
    // Get burst-by-burst Eop corrections
    std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> MeanEoPFitResult = LKrEopMonitor::MeanEoPFit();
    std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> EopVsPFitResult  = LKrEopMonitor::EoPVsPFit();
    fHMeanEoPVsBurstID->Set(fHMeanEoPVsBurstID->GetN()+1);
    fHMeanEoPVsBurstID->SetPoint(fHMeanEoPVsBurstID->GetN()-1,fBurstID,MeanEoPFitResult.first.first);
    fHMeanEoPVsBurstID->SetPointError(fHMeanEoPVsBurstID->GetN()-1,0,MeanEoPFitResult.first.second);
    fHSigmaEoPVsBurstID->Set(fHSigmaEoPVsBurstID->GetN()+1);
    fHSigmaEoPVsBurstID->SetPoint(fHSigmaEoPVsBurstID->GetN()-1,fBurstID,MeanEoPFitResult.second.first);
    fHSigmaEoPVsBurstID->SetPointError(fHSigmaEoPVsBurstID->GetN()-1,0,MeanEoPFitResult.second.second);
    fHEoPFitPar0VsBurstID->Set(fHEoPFitPar0VsBurstID->GetN()+1);
    fHEoPFitPar0VsBurstID->SetPoint(fHEoPFitPar0VsBurstID->GetN()-1,fBurstID,EopVsPFitResult.first.first);
    fHEoPFitPar0VsBurstID->SetPointError(fHEoPFitPar0VsBurstID->GetN()-1,0,EopVsPFitResult.first.second);
    fHEoPFitPar1VsBurstID->Set(fHEoPFitPar1VsBurstID->GetN()+1);
    fHEoPFitPar1VsBurstID->SetPoint(fHEoPFitPar1VsBurstID->GetN()-1,fBurstID,EopVsPFitResult.second.first);
    fHEoPFitPar1VsBurstID->SetPointError(fHEoPFitPar1VsBurstID->GetN()-1,0,EopVsPFitResult.second.second);
  }
}

void LKrEopMonitor::BuildPDFReport() {
  if(fCanvas) delete fCanvas;
  fCanvas = new TCanvas("Canvas");
  fCanvas->Print(Form(fOutPDFFileName + "["), "pdf");

  // EoPEfficiency
  if(fHEoPEfficiency){
    fHEoPEfficiency->SetTitle(Form("LKr cell-by-cell efficiency for run %d",GetRunID()));
    fHEoPEfficiency->SetStats(0);
    fHEoPEfficiency->Draw("COLZ");
    fHEoPEfficiency->GetXaxis()->SetTitle("x [mm]");
    fHEoPEfficiency->GetYaxis()->SetTitle("y [mm]");
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // Eop Vs P
  if(fHEoPVsP){
    fHEoPVsP->SetTitle(Form("EoP Vs Momentum for run %d",GetRunID()));
    fHEoPVsP->SetStats(0);
    fHEoPVsP->Draw("COLZ");
    fHEoPVsP->GetXaxis()->SetTitle("Momentum [MeV/c]");
    fHEoPVsP->GetYaxis()->SetTitle("E/p");
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // Eop Vs TrkClsTime
  if(fHEoPVsTrkClsTime){
    fHEoPVsTrkClsTime->SetTitle(Form("EoP Vs TrkClsTime for run %d",GetRunID()));
    fHEoPVsTrkClsTime->SetStats(0);
    fHEoPVsTrkClsTime->Draw("COLZ");
    fHEoPVsTrkClsTime->GetXaxis()->SetTitle("Track-Cluster Time [ns]");
    fHEoPVsTrkClsTime->GetYaxis()->SetTitle("E/p");
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // Eop Vs Channel
  if(fHEoPVsChannel){
    fHEoPVsChannel->SetTitle(Form("EoP Vs Channel for run %d",GetRunID()));
    fHEoPVsChannel->SetStats(0);
    fHEoPVsChannel->Draw("COLZ");
    fHEoPVsChannel->GetXaxis()->SetTitle("Channel ID");
    fHEoPVsChannel->GetYaxis()->SetTitle("E/p");
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // Eop
  if(fHEoP){
    fHEoP->SetTitle(Form("EoP for run %d",GetRunID()));
    fHEoP->SetStats(11111);
    fHEoP->Draw("");
    fHEoP->GetXaxis()->SetTitle("E/p");
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // EoPEfficiency Vs BurstID
  if(fHEoPEfficiencyVsBurstID && fHEoPEfficiencyVsBurstID->GetN()){
    TLegend* Legend = new TLegend(0.13,0.13,0.3,0.25);
    Legend->SetFillColor(kWhite);
    fHEoPEfficiencyVsBurstID->SetTitle(Form("LKr E/p efficiency Vs BurstID for run %d",GetRunID()));
    fHEoPEfficiencyVsBurstID->Draw("AP");
    fHEoPEfficiencyVsBurstID->SetMarkerStyle(20);
    fHEoPEfficiencyVsBurstID->SetMarkerSize(0.3);
    fHEoPEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEoPEfficiencyVsBurstID->GetYaxis()->SetTitle("E/p Efficiency");
    fHEoPEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    Legend->AddEntry(fHEoPEfficiencyVsBurstID,"Ke3","pl");
    fHEoPEfficiencyStrictVsBurstID->Draw("P");
    fHEoPEfficiencyStrictVsBurstID->SetMarkerStyle(20);
    fHEoPEfficiencyStrictVsBurstID->SetMarkerSize(0.3);
    fHEoPEfficiencyStrictVsBurstID->SetMarkerColor(kBlue);
    Legend->AddEntry(fHEoPEfficiencyStrictVsBurstID,"Strict Ke3","pl");
    TLine* EoPEfficiencyThrBurst = new TLine(fHEoPEfficiencyVsBurstID->GetXaxis()->GetXmin(),fEoPEfficiencyThreshold,fHEoPEfficiencyVsBurstID->GetXaxis()->GetXmax(),fEoPEfficiencyThreshold);
    EoPEfficiencyThrBurst->SetLineColor(kRed);
    EoPEfficiencyThrBurst->Draw();
    Legend->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // Zoom
    fHEoPEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.75,1.05);
    fHEoPEfficiencyVsBurstID->Draw("AP");
    fHEoPEfficiencyStrictVsBurstID->Draw("P");
    EoPEfficiencyThrBurst->Draw();
    Legend->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");
    delete EoPEfficiencyThrBurst;
    delete Legend;
  }

  // MeanEoP Vs BurstID
  if(fHMeanEoPVsBurstID && fHMeanEoPVsBurstID->GetN()){
    fHMeanEoPVsBurstID->SetTitle(Form("Mean E/p value Vs BurstID for run %d",GetRunID()));
    fHMeanEoPVsBurstID->Draw("AP");
    fHMeanEoPVsBurstID->SetMarkerStyle(20);
    fHMeanEoPVsBurstID->SetMarkerSize(0.3);
    fHMeanEoPVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHMeanEoPVsBurstID->GetYaxis()->SetTitle("MeanEoP");
    fHMeanEoPVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    TLine* MeanEoPThrBurst = new TLine(fHMeanEoPVsBurstID->GetXaxis()->GetXmin(),fMeanEoPThreshold,fHMeanEoPVsBurstID->GetXaxis()->GetXmax(),fMeanEoPThreshold);
    MeanEoPThrBurst->SetLineColor(kRed);
    MeanEoPThrBurst->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");

    // Zoom
    fHMeanEoPVsBurstID->GetYaxis()->SetRangeUser(0.9,1.05);
    fHMeanEoPVsBurstID->Draw("AP");
    MeanEoPThrBurst->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");
    delete MeanEoPThrBurst;
  }

  // SigmaEoP Vs BurstID
  if(fHSigmaEoPVsBurstID && fHSigmaEoPVsBurstID->GetN()){
    fHSigmaEoPVsBurstID->SetTitle(Form("Sigma E/p value Vs BurstID for run %d",GetRunID()));
    fHSigmaEoPVsBurstID->Draw("AP");
    fHSigmaEoPVsBurstID->SetMarkerStyle(20);
    fHSigmaEoPVsBurstID->SetMarkerSize(0.3);
    fHSigmaEoPVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHSigmaEoPVsBurstID->GetYaxis()->SetTitle("SigmaEoP");
    fHSigmaEoPVsBurstID->GetYaxis()->SetRangeUser(0.,0.5);
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // GEoPVsP (used for the fit)
  if(fGEoPVsP){
    fGEoPVsP->Draw("AP");
    fGEoPVsP->GetYaxis()->SetRangeUser(0.8,1.05);
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // EoPFitPar0 Vs BurstID
  if(fHEoPFitPar0VsBurstID && fHEoPFitPar0VsBurstID->GetN()){
    fHEoPFitPar0VsBurstID->SetTitle(Form("E/p fit par0 value Vs BurstID for run %d",GetRunID()));
    fHEoPFitPar0VsBurstID->Draw("AP");
    fHEoPFitPar0VsBurstID->SetMarkerStyle(20);
    fHEoPFitPar0VsBurstID->SetMarkerSize(0.3);
    fHEoPFitPar0VsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEoPFitPar0VsBurstID->GetYaxis()->SetTitle("EoPFitPar0");
    fHEoPFitPar0VsBurstID->GetYaxis()->SetRangeUser(0.9,1.05);
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // EoPFitPar1 Vs BurstID
  if(fHEoPFitPar1VsBurstID && fHEoPFitPar1VsBurstID->GetN()){
    fHEoPFitPar1VsBurstID->SetTitle(Form("E/p fit par1 value Vs BurstID for run %d",GetRunID()));
    fHEoPFitPar1VsBurstID->Draw("AP");
    fHEoPFitPar1VsBurstID->SetMarkerStyle(20);
    fHEoPFitPar1VsBurstID->SetMarkerSize(0.3);
    fHEoPFitPar1VsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEoPFitPar1VsBurstID->GetYaxis()->SetTitle("EoPFitPar1");
    fHEoPFitPar1VsBurstID->GetYaxis()->SetRangeUser(-1500.,1500.);
    fCanvas->Print(fOutPDFFileName, "pdf");
  }

  // NHotCellsFromHitMap Vs BurstID
  if(fHNHotCellsFromHitMapVsBurstID && fHNHotCellsFromHitMapVsBurstID->GetN()){
    fHNHotCellsFromHitMapVsBurstID->SetTitle(Form("Number of hot cells Vs BurstID for run %d",GetRunID()));
    fHNHotCellsFromHitMapVsBurstID->Draw("AP");
    fHNHotCellsFromHitMapVsBurstID->SetMarkerStyle(20);
    fHNHotCellsFromHitMapVsBurstID->SetMarkerSize(0.5);
    fHNHotCellsFromHitMapVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHNHotCellsFromHitMapVsBurstID->GetYaxis()->SetTitle("NHotCellsFromHitMap");
    TLine* NHotCellsFromHitMapThrBurst = nullptr;
    if(fBadCellsMask&(1<<LKrBadCells::kHotHitMapBit)) {
      NHotCellsFromHitMapThrBurst = new TLine(fHNHotCellsFromHitMapVsBurstID->GetXaxis()->GetXmin(),0.5,fHNHotCellsFromHitMapVsBurstID->GetXaxis()->GetXmax(),0.5);
      NHotCellsFromHitMapThrBurst->SetLineColor(kRed);
      NHotCellsFromHitMapThrBurst->Draw();
    }
    fCanvas->Print(fOutPDFFileName, "pdf");
    if(NHotCellsFromHitMapThrBurst) delete NHotCellsFromHitMapThrBurst;
  }

  // NHotCellsFromQualityWarnings Vs BurstID
  if(fHNHotCellsFromQualityWarningsVsBurstID && fHNHotCellsFromQualityWarningsVsBurstID->GetN()){
    fHNHotCellsFromQualityWarningsVsBurstID->SetTitle(Form("Number of hot cells Vs BurstID for run %d",GetRunID()));
    fHNHotCellsFromQualityWarningsVsBurstID->Draw("AP");
    fHNHotCellsFromQualityWarningsVsBurstID->SetMarkerStyle(20);
    fHNHotCellsFromQualityWarningsVsBurstID->SetMarkerSize(0.5);
    fHNHotCellsFromQualityWarningsVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHNHotCellsFromQualityWarningsVsBurstID->GetYaxis()->SetTitle("NHotCellsFromQualityWarnings");
    TLine* NHotCellsFromQualityWarningsThrBurst = nullptr;
    if(fBadCellsMask&(1<<LKrBadCells::kHotQualityWarningsBit)) {
      NHotCellsFromQualityWarningsThrBurst = new TLine(fHNHotCellsFromQualityWarningsVsBurstID->GetXaxis()->GetXmin(),0.5,fHNHotCellsFromQualityWarningsVsBurstID->GetXaxis()->GetXmax(),0.5);
      NHotCellsFromQualityWarningsThrBurst->SetLineColor(kRed);
      NHotCellsFromQualityWarningsThrBurst->Draw();
    }
    fCanvas->Print(fOutPDFFileName, "pdf");
    if(NHotCellsFromQualityWarningsThrBurst) delete NHotCellsFromQualityWarningsThrBurst;
  }

  // NHotCellsFromPedestals Vs BurstID
  if(fHNHotCellsFromPedestalsVsBurstID && fHNHotCellsFromPedestalsVsBurstID->GetN()){
    fHNHotCellsFromPedestalsVsBurstID->SetTitle(Form("Number of hot cells Vs BurstID for run %d",GetRunID()));
    fHNHotCellsFromPedestalsVsBurstID->Draw("AP");
    fHNHotCellsFromPedestalsVsBurstID->SetMarkerStyle(20);
    fHNHotCellsFromPedestalsVsBurstID->SetMarkerSize(0.5);
    fHNHotCellsFromPedestalsVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHNHotCellsFromPedestalsVsBurstID->GetYaxis()->SetTitle("NHotCellsFromPedestals");
    TLine* NHotCellsFromPedestalsThrBurst = nullptr;
    if(fBadCellsMask&(1<<LKrBadCells::kHotPedestalsBit)) {
      NHotCellsFromPedestalsThrBurst = new TLine(fHNHotCellsFromPedestalsVsBurstID->GetXaxis()->GetXmin(),0.5,fHNHotCellsFromPedestalsVsBurstID->GetXaxis()->GetXmax(),0.5);
      NHotCellsFromPedestalsThrBurst->SetLineColor(kRed);
      NHotCellsFromPedestalsThrBurst->Draw();
    }
    fCanvas->Print(fOutPDFFileName, "pdf");
    if(NHotCellsFromPedestalsThrBurst) delete NHotCellsFromPedestalsThrBurst;
  }

  // MaxNCellsInDeadCluster Vs BurstID
  if(fHMaxNCellsInDeadClusterVsBurstID && fHMaxNCellsInDeadClusterVsBurstID->GetN()){
    fHMaxNCellsInDeadClusterVsBurstID->SetTitle(Form("Max number of cells in a dead cluster Vs BurstID for run %d",GetRunID()));
    fHMaxNCellsInDeadClusterVsBurstID->Draw("AP");
    fHMaxNCellsInDeadClusterVsBurstID->SetMarkerStyle(20);
    fHMaxNCellsInDeadClusterVsBurstID->SetMarkerSize(0.5);
    fHMaxNCellsInDeadClusterVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHMaxNCellsInDeadClusterVsBurstID->GetYaxis()->SetTitle("MaxNCellsInDeadCluster");
    TLine* MaxNCellsInDeadClusterThrBurst = new TLine(fHMaxNCellsInDeadClusterVsBurstID->GetXaxis()->GetXmin(),fMaxNCellsInDeadClusterThreshold+0.5,fHMaxNCellsInDeadClusterVsBurstID->GetXaxis()->GetXmax(),fMaxNCellsInDeadClusterThreshold+0.5);
    MaxNCellsInDeadClusterThrBurst->SetLineColor(kRed);
    MaxNCellsInDeadClusterThrBurst->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");
    if(MaxNCellsInDeadClusterThrBurst) delete MaxNCellsInDeadClusterThrBurst;
  }

  // NDeadCellsFromHitMap Vs BurstID
  if(fHNDeadCellsFromHitMapVsBurstID && fHNDeadCellsFromHitMapVsBurstID->GetN()){
    fHNDeadCellsFromHitMapVsBurstID->SetTitle(Form("Number of dead cells Vs BurstID for run %d",GetRunID()));
    fHNDeadCellsFromHitMapVsBurstID->Draw("AP");
    fHNDeadCellsFromHitMapVsBurstID->SetMarkerStyle(20);
    fHNDeadCellsFromHitMapVsBurstID->SetMarkerSize(0.5);
    fHNDeadCellsFromHitMapVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHNDeadCellsFromHitMapVsBurstID->GetYaxis()->SetTitle("NDeadCellsFromHitMap");
    TLine* NDeadCellsFromHitMapThrBurst = nullptr;
    if(fBadCellsMask&(1<<LKrBadCells::kDeadHitMapBit)) {
      NDeadCellsFromHitMapThrBurst = new TLine(fHNDeadCellsFromHitMapVsBurstID->GetXaxis()->GetXmin(),0.5,fHNDeadCellsFromHitMapVsBurstID->GetXaxis()->GetXmax(),0.5);
      NDeadCellsFromHitMapThrBurst->SetLineColor(kRed);
      NDeadCellsFromHitMapThrBurst->Draw();
    }
    fCanvas->Print(fOutPDFFileName, "pdf");
    if(NDeadCellsFromHitMapThrBurst) delete NDeadCellsFromHitMapThrBurst;
  }

  // NBitFlipCells Vs BurstID
  if(fHNBitFlipCellsVsBurstID && fHNBitFlipCellsVsBurstID->GetN()){
    fHNBitFlipCellsVsBurstID->SetTitle(Form("Number of bitflip cells Vs BurstID for run %d",GetRunID()));
    fHNBitFlipCellsVsBurstID->Draw("AP");
    fHNBitFlipCellsVsBurstID->SetMarkerStyle(20);
    fHNBitFlipCellsVsBurstID->SetMarkerSize(0.5);
    fHNBitFlipCellsVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHNBitFlipCellsVsBurstID->GetYaxis()->SetTitle("NBitFlipCells");
    //TLine* NBitFlipCellsThrBurst = new TLine(fHNBitFlipCellsVsBurstID->GetXaxis()->GetXmin(),0.5,fHNBitFlipCellsVsBurstID->GetXaxis()->GetXmax(),0.5);
    //NBitFlipCellsThrBurst->SetLineColor(kRed);
    //NBitFlipCellsThrBurst->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");
    //delete NBitFlipCellsThrBurst;
  }

  // NEventsWithManyHits Vs BurstID
  if(fHNEventsWithManyHitsVsBurstID && fHNEventsWithManyHitsVsBurstID->GetN()){
    fHNEventsWithManyHitsVsBurstID->SetTitle(Form("Number of events with more than %d hits for run %d",fNManyHitsThreshold,GetRunID()));
    fHNEventsWithManyHitsVsBurstID->Draw("AP");
    fHNEventsWithManyHitsVsBurstID->SetMarkerStyle(20);
    fHNEventsWithManyHitsVsBurstID->SetMarkerSize(0.5);
    fHNEventsWithManyHitsVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHNEventsWithManyHitsVsBurstID->GetYaxis()->SetTitle("NEventsWithManyHits");
    TLine* NEventsWithManyHitsThrBurst = new TLine(fHNEventsWithManyHitsVsBurstID->GetXaxis()->GetXmin(),10.5,fHNEventsWithManyHitsVsBurstID->GetXaxis()->GetXmax(),10.5);
    NEventsWithManyHitsThrBurst->SetLineColor(kRed);
    NEventsWithManyHitsThrBurst->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");
    delete NEventsWithManyHitsThrBurst;
  }

  ///////////////////////////
  // LKr-STRAW alignment plot

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  // Two pages of alignment plots in x,y bins are commented out (useful only for debugging)
  //fc_dx_y->Print(fOutPDFFileName, "pdf");
  //fc_dy_x->Print(fOutPDFFileName, "pdf");
  TH2F *h = new TH2F("h1", "Straw#minusLKr mean track positions in LKr Z plane;x_{track}, y_{track} [mm];#Deltay, #Deltax [mm]",
		     1, -1400, 1400, 1, -3, 3);
  h->Draw();
  fGdx_vs_y->Draw("p same");
  fGdy_vs_x->Draw("p same");
  TLegend* Legend = new TLegend(0.13, 0.13, 0.33, 0.25);
  Legend->AddEntry(fGdx_vs_y, "#Deltax vs y_{LKr}", "pl");
  Legend->AddEntry(fGdy_vs_x, "#Deltay vs x_{LKr}", "pl");
  Legend->Draw();
  fCanvas->Print(fOutPDFFileName, "pdf");
  delete h;
  delete Legend;

  fCanvas->Print(Form(fOutPDFFileName + "]"), "pdf");
  delete fCanvas;
}

// Computation of the LKr-straw alignment constants: A.Romano, E.Goudzovski Nov 2018
void LKrEopMonitor::ComputeLKrStrawALignmentConstants() {

  // Number of samples: 256 (x,y) bins regrouped by 10
  const Int_t N = 25;

  Double_t meanX[N], meanY[N], meanDxVsY[N], meanDyVsX[N];
  Double_t sigmaX[N], sigmaY[N], sigmaDxVsY[N], sigmaDyVsX[N];
  TH1F *hp_dx_y[N], *hp_dy_x[N];

  fc_dx_y = new TCanvas();
  fc_dy_x = new TCanvas();
  fc_dx_y->Divide(5,5);
  fc_dy_x->Divide(5,5);

  Int_t nStep = 10;
  Int_t nBins = fHdy_vs_x->GetNbinsX();
  TF1 *f1 = new TF1("falign", "gaus", -8, 8);

  Int_t i = 0; // ID of the (x,y) point
  for (Int_t iBin=1; iBin+nStep-1<=nBins; iBin+=nStep) {
    hp_dx_y[i] = (TH1F*)fHdx_vs_y->ProjectionY(Form("hp_dx_y_%i",i), iBin, iBin+nStep-1);
    hp_dy_x[i] = (TH1F*)fHdy_vs_x->ProjectionY(Form("hp_dy_x_%i",i), iBin, iBin+nStep-1);

    meanX[i] = 0.5*(fHdy_vs_x->GetXaxis()->GetBinCenter(iBin)+
		    fHdy_vs_x->GetXaxis()->GetBinCenter(iBin+nStep-1));
    meanY[i] = 0.5*(fHdx_vs_y->GetXaxis()->GetBinCenter(iBin)+
		    fHdx_vs_y->GetXaxis()->GetBinCenter(iBin+nStep-1));
    sigmaX[i] = sigmaY[i] = 0.0;

    fc_dx_y->cd(i+1);
    hp_dx_y[i]->Fit(f1, "QR");
    meanDxVsY[i]  = f1->GetParameter(1);
    sigmaDxVsY[i] = f1->GetParError(1);
    hp_dx_y[i]->Draw();

    fc_dy_x->cd(i+1);
    hp_dy_x[i]->Fit(f1, "QR");
    meanDyVsX[i]  = f1->GetParameter(1);
    sigmaDyVsX[i] = f1->GetParError(1);
    hp_dy_x[i]->Draw();

    i++;
  }

  // Compute aligmnent and rotation constants from the results of fits in (x,y) bins
  fFdx_vs_y = new TF1("fdx_vs_y", "pol1", -1000, 1000);
  fFdy_vs_x = new TF1("fdy_vs_x", "pol1", -1000, 1000);

  fGdx_vs_y = new TGraphErrors(N, meanY, meanDxVsY, sigmaY, sigmaDxVsY);
  fGdx_vs_y->SetMarkerStyle(kFullCircle);
  fGdx_vs_y->SetMarkerSize(1.2);
  fGdx_vs_y->SetMarkerColor(kBlue);
  fGdx_vs_y->Fit(fFdx_vs_y, "QR");

  fGdy_vs_x = new TGraphErrors(N, meanX, meanDyVsX, sigmaX, sigmaDyVsX);
  fGdy_vs_x->SetMarkerStyle(kFullSquare);
  fGdy_vs_x->SetMarkerSize(1.2);
  fGdy_vs_x->SetMarkerColor(kGreen+2);
  fGdy_vs_x->Fit(fFdy_vs_x, "QR");

  Double_t dx = fFdx_vs_y->GetParameter(0);
  Double_t dy = fFdy_vs_x->GetParameter(0);
  Double_t phi_from_x = fFdx_vs_y->GetParameter(1);
  Double_t phi_from_y = fFdy_vs_x->GetParameter(1);
  Double_t phi = 0.5*(phi_from_x-phi_from_y);

  // Dump the results into a file
  ofstream LKrAlignmenentFile;
  LKrAlignmenentFile.open(Form("LKr-Alignment.run%06d_0000-run%06d_9999.dat", GetRunID(), GetRunID()));
  LKrAlignmenentFile << "# LKr-spectrometer alignment constants" << endl;
  LKrAlignmenentFile << "# Format: RunID dx[mm] dy[mm] phi[mrad]" << endl;
  LKrAlignmenentFile << Form("%06d %5.2f %5.2f %6.3f\n", GetRunID(), dx, dy, 1e3*phi);
  LKrAlignmenentFile.close();
}

void LKrEopMonitor::CreateBadBurstList() {

  Int_t NEmptyBursts=0, NBadLowStat=0, NBadMeanEoP=0, NBadEoPEfficiency=0,NBadHotCell=0,NBadDeadCell=0,NBadDeadClus=0,NBadBitFlipCell=0,NBadNHits=0;
  ofstream BadBurstList;
  ofstream EopOutputFile; // dump the EoP values for good bursts on a file
  BadBurstList.open(Form("LKrEopMonitor.BadBursts.thrs%.3f-%.3f.dat",fEoPEfficiencyThreshold,fMeanEoPThreshold));
  EopOutputFile.open(Form("LKrEopMonitor.EopOutput.thrs%.3f-%.3f.dat",fEoPEfficiencyThreshold,fMeanEoPThreshold));
  EopOutputFile << "# Format: RunID BurstID MeanEop eMeanEop EopFitPar0 eEopFitPar0 EopFitPar1 eEopFitPar1" << std::endl;
  for(Int_t iPoint=0;iPoint<fHEoPEfficiencyVsBurstID->GetN();iPoint++){
    double BurstID=0., EoPEfficiency=0., MeanEoP=0., EoPFitPar0=0., EoPFitPar1=0., Argonion=0.;
    double NTriggers=0., NSelectedTriggers=0., NEoPExpected=0., NHotCellsFromHitMap=0.;
    double NHotCellsFromQualityWarnings=0., NHotCellsFromPedestals=0., NDeadCellsFromHitMap=0., MaxNCellsInDeadCluster=0., NEventsWithManyHits=0.;// , NBitFlipCells=0.;
    double EoPEfficiencyStrict=0., NEoPExpectedStrict=0.;
    double eMeanEoP=0., eEoPFitPar0=0., eEoPFitPar1=0., eEoPEfficiency=0., eEoPEfficiencyStrict=0.;
    if(fHEoPEfficiencyVsBurstID)       fHEoPEfficiencyVsBurstID->GetPoint(iPoint,BurstID,EoPEfficiency);
    if(fHEoPEfficiencyStrictVsBurstID) fHEoPEfficiencyStrictVsBurstID->GetPoint(iPoint,BurstID,EoPEfficiencyStrict);
    if(fHMeanEoPVsBurstID)             fHMeanEoPVsBurstID->GetPoint(iPoint,BurstID,MeanEoP);
    if(fHEoPFitPar0VsBurstID)          fHEoPFitPar0VsBurstID->GetPoint(iPoint,BurstID,EoPFitPar0);
    if(fHEoPFitPar1VsBurstID)          fHEoPFitPar1VsBurstID->GetPoint(iPoint,BurstID,EoPFitPar1);
    if(fHMeanEoPVsBurstID)             fHMeanEoPVsBurstID->GetPoint(iPoint,BurstID,MeanEoP);
    if(fHNEoPExpectedVsBurstID)        fHNEoPExpectedVsBurstID->GetPoint(iPoint,BurstID,NEoPExpected);
    if(fHNEoPExpectedStrictVsBurstID)  fHNEoPExpectedStrictVsBurstID->GetPoint(iPoint,BurstID,NEoPExpectedStrict);
    if(fHArgonionCountsVsBurstID)      fHArgonionCountsVsBurstID->GetPoint(iPoint,BurstID,Argonion);
    if(fHNTriggersVsBurstID)           fHNTriggersVsBurstID->GetPoint(iPoint,BurstID,NTriggers);
    if(fHNSelectedTriggersVsBurstID)   fHNSelectedTriggersVsBurstID->GetPoint(iPoint,BurstID,NSelectedTriggers);
    if(fHNHotCellsFromHitMapVsBurstID)          fHNHotCellsFromHitMapVsBurstID->GetPoint(iPoint,BurstID,NHotCellsFromHitMap);
    if(fHNHotCellsFromQualityWarningsVsBurstID) fHNHotCellsFromQualityWarningsVsBurstID->GetPoint(iPoint,BurstID,NHotCellsFromQualityWarnings);
    if(fHNHotCellsFromPedestalsVsBurstID)       fHNHotCellsFromPedestalsVsBurstID->GetPoint(iPoint,BurstID,NHotCellsFromPedestals);
    if(fHNDeadCellsFromHitMapVsBurstID)         fHNDeadCellsFromHitMapVsBurstID->GetPoint(iPoint,BurstID,NDeadCellsFromHitMap);
    if(fHMaxNCellsInDeadClusterVsBurstID)       fHMaxNCellsInDeadClusterVsBurstID->GetPoint(iPoint,BurstID,MaxNCellsInDeadCluster);
    //if(fHNBitFlipCellsVsBurstID)       fHNBitFlipCellsVsBurstID->GetPoint(iPoint,BurstID,NBitFlipCells);
    if(fHNEventsWithManyHitsVsBurstID) fHNEventsWithManyHitsVsBurstID->GetPoint(iPoint,BurstID,NEventsWithManyHits);
    if(fHMeanEoPVsBurstID)             eMeanEoP             = fHMeanEoPVsBurstID->GetErrorY(iPoint);
    if(fHEoPFitPar0VsBurstID)          eEoPFitPar0          = fHEoPFitPar0VsBurstID->GetErrorY(iPoint);
    if(fHEoPFitPar1VsBurstID)          eEoPFitPar1          = fHEoPFitPar1VsBurstID->GetErrorY(iPoint);
    if(fHEoPEfficiencyVsBurstID)       eEoPEfficiency       = fHEoPEfficiencyVsBurstID->GetErrorY(iPoint);
    if(fHEoPEfficiencyStrictVsBurstID) eEoPEfficiencyStrict = fHEoPEfficiencyStrictVsBurstID->GetErrorY(iPoint);
    if(NSelectedTriggers<fNSelectedTriggersMin){
      if(NTriggers==0 && Argonion==0){ // Corrupted file
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " BADFILE " << std::endl;
      }
      else if(Argonion==0){ // No Argonion info
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " NO_ARGN " << std::endl;
      }
      else if(Argonion*1.e9<fArgonionCountsMin){ // Low number of triggers due to no beam
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWARGN " << Argonion*1.e9 << std::endl;
      }
      else {
        BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWTRIG " << NSelectedTriggers << std::endl;
      }
      NEmptyBursts++;
    }
    else if(NEoPExpected<10 || eMeanEoP>0.01){ // Low stat
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " LOWSTAT " << NEoPExpected << ", MeanEoP: " <<  MeanEoP << " +/- " << eMeanEoP << " [NEoPExpectedStrict: " << NEoPExpectedStrict << ", SelectedTriggers: " << NSelectedTriggers << "]" << std::endl;

      NBadLowStat++;
    }
    else if(MeanEoP<fMeanEoPThreshold){ // bad Mean E/p
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " MEANEOP " << MeanEoP << " +/- " << eMeanEoP << std::endl;
      NBadMeanEoP++;
    }
    else if(EoPEfficiency<fEoPEfficiencyThreshold){ // bad E/p efficiency
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " EOPEFFI " << EoPEfficiency << " +/- " << eEoPEfficiency << std::endl;
      NBadEoPEfficiency++;
    }
    else if((fBadCellsMask&(1<<LKrBadCells::kHotHitMapBit) && NHotCellsFromHitMap>0) ||
        (fBadCellsMask&(1<<LKrBadCells::kHotQualityWarningsBit) && NHotCellsFromQualityWarnings>0) ||
        (fBadCellsMask&(1<<LKrBadCells::kHotPedestalsBit) && NHotCellsFromPedestals>0) || NEoPExpectedStrict<10){ // hot cell
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " HOTCELL " << NHotCellsFromHitMap << " " << NHotCellsFromQualityWarnings << " " << NHotCellsFromPedestals << " (BadCellsMask: " << fBadCellsMask << ") " << NEoPExpectedStrict << ", eff(strict): " << EoPEfficiencyStrict << " +/- " << eEoPEfficiencyStrict << " [NEoPExpected: " << NEoPExpected << ", SelectedTriggers: " << NSelectedTriggers << "]" << std::endl;

      NBadHotCell++;
    }
    else if(NEventsWithManyHits>10){ // problematic burst
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " MANYHIT " << NEventsWithManyHits << " " << fNManyHitsThreshold << std::endl;
      NBadNHits++;
    }
    else if(fBadCellsMask&(1<<LKrBadCells::kDeadHitMapBit) && NDeadCellsFromHitMap>0) { //unexpected dead cells
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " DEADCELL " << NDeadCellsFromHitMap << " [SelectedTriggers: " << NSelectedTriggers << "]" << std::endl;

      NBadDeadCell++;
    }
    else if(MaxNCellsInDeadCluster>fMaxNCellsInDeadClusterThreshold) { //dead cell cluster found
      BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " DEADCLUS " << MaxNCellsInDeadCluster << " [Threshold: " << fMaxNCellsInDeadClusterThreshold << ", SelectedTriggers: " << NSelectedTriggers << "]" << std::endl;

      NBadDeadClus++;
    }
    //else if(NBitFlipCells>0){ // bitflip cell [ignored for now]
    //  BadBurstList << Form("BadBurst %06d %04d",GetRunID(),(Int_t)BurstID) << " BITFLIP " << NBitFlipCells << " [NEoPExpected: " << NEoPExpected << ", SelectedTriggers: " << NSelectedTriggers << "]" << std::endl;

    //  NBadBitFlipCell++;
    //}
    else { //good burst
      EopOutputFile << Form("EopOutput %06d %04d",GetRunID(),(Int_t)BurstID) << " " <<  MeanEoP << " " << eMeanEoP << " " << EoPFitPar0 << " " << eEoPFitPar0 << " " << EoPFitPar1 << " " << eEoPFitPar1 << std::endl;
    }
  }
  BadBurstList.close();
  std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> MeanEoPFitResult = MeanEoPFit();
  std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> EoPVsPFitResult  = EoPVsPFit();
  EopOutputFile << Form("EopOutput %06d ALLB",GetRunID()) << " " << MeanEoPFitResult.first.first << " " << MeanEoPFitResult.first.second << " " << EoPVsPFitResult.first.first << " " << EoPVsPFitResult.first.second << " " << EoPVsPFitResult.second.first << " " << EoPVsPFitResult.second.second << std::endl;
  EopOutputFile.close();
  // Create LKr fine correction file
  ofstream LKrFineCalibrationFile; // dump the EoP values for good bursts on a file
  LKrFineCalibrationFile.open(Form("LKr-FineCalibration.run%06d_0000-run%06d_9999.dat",GetRunID(),GetRunID()));
  LKrFineCalibrationFile << "# Format: RunID 0 MeanEop eMeanEop EopFitPar0 eEopFitPar0 EopFitPar1 eEopFitPar1" << std::endl;
  LKrFineCalibrationFile << Form("%06d 0",GetRunID()) << " " << MeanEoPFitResult.first.first << " " << MeanEoPFitResult.first.second << " " << EoPVsPFitResult.first.first << " " << EoPVsPFitResult.first.second << " " << EoPVsPFitResult.second.first << " " << EoPVsPFitResult.second.second << std::endl;
  LKrFineCalibrationFile.close();
  std::cout << user_standard() << "***** BADBURST SUMMARY ***** " << std::endl;
  std::cout << user_standard() << "Bad(LowStat):  " << NBadLowStat << std::endl;
  std::cout << user_standard() << "Bad(MeanEoP):  " << NBadMeanEoP << std::endl;
  std::cout << user_standard() << "Bad(EoPEffi):  " << NBadEoPEfficiency << std::endl;
  std::cout << user_standard() << "Bad(HotCell):  " << NBadHotCell << std::endl;
  std::cout << user_standard() << "Bad(DeadCell): " << NBadDeadCell << std::endl;
  std::cout << user_standard() << "Bad(DeadClus): " << NBadDeadClus << std::endl;
  std::cout << user_standard() << "Bad(BitFlip):  " << NBadBitFlipCell << std::endl;
  std::cout << user_standard() << "Bad(NHits):    " << NBadNHits << std::endl;
  std::cout << user_standard() << "Bad(LKrTotal): " << NBadLowStat+NBadMeanEoP+NBadEoPEfficiency+NBadHotCell+NBadDeadCell+NBadDeadClus+NBadBitFlipCell+NBadNHits;
  std::cout << user_standard() << " over " << fHMeanEoPVsBurstID->GetN()-NEmptyBursts << " non-empty bursts (NEmptyBursts: " << NEmptyBursts<< ")"<< std::endl;
  std::cout << user_standard() << "**************************** " << std::endl;
}

std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> LKrEopMonitor::MeanEoPFit(){
  TH1D* hEoP = fHEoPVsP->ProjectionY("hEoPWholePRange");
  TF1 * f = new TF1("fgaus","gaus",hEoP->GetBinCenter(hEoP->GetMaximumBin())-fDeltaEoPForEfficiency,hEoP->GetBinCenter(hEoP->GetMaximumBin())+fDeltaEoPForEfficiency);
  hEoP->Fit("fgaus","R");
  std::pair<Double_t,Double_t> MeanEop = std::make_pair(f->GetParameter(1),f->GetParError(1));
  std::pair<Double_t,Double_t> Sigma   = std::make_pair(f->GetParameter(2),f->GetParError(2));
  delete hEoP;
  delete f;
  return std::make_pair(MeanEop,Sigma);
}

std::pair<std::pair<Double_t,Double_t>,std::pair<Double_t,Double_t>> LKrEopMonitor::EoPVsPFit(){
  const UInt_t NEoPPBins = fHEoPVsP->GetXaxis()->GetXmax()/fDeltaEoPPBin;
  if(fGEoPVsP) delete fGEoPVsP;
  fGEoPVsP = new TGraphErrors();
  fGEoPVsP->SetName("fGEoPVsP");
  fGEoPVsP->Set(NEoPPBins);
  for(UInt_t iEoPPBin=0;iEoPPBin<NEoPPBins;iEoPPBin++){
    Int_t iFirstPBin = iEoPPBin*fHEoPVsP->GetNbinsX()/NEoPPBins+1;
    Int_t iLastPBin  = (iEoPPBin+1)*fHEoPVsP->GetNbinsX()/NEoPPBins;
    TH1D* hEoP = fHEoPVsP->ProjectionY(Form("hEoP_PBin%0d",iEoPPBin),iFirstPBin,iLastPBin);
    TF1 * f = new TF1("fgaus","gaus",hEoP->GetBinCenter(hEoP->GetMaximumBin())-fDeltaEoPForEfficiency,hEoP->GetBinCenter(hEoP->GetMaximumBin())+fDeltaEoPForEfficiency);
    hEoP->Fit("fgaus","R");
    if(hEoP->GetEntries()) { //non-empty momentum bin
      double PBin = (iEoPPBin+0.5)*fDeltaEoPPBin;
      fGEoPVsP->SetPoint(iEoPPBin,PBin,f->GetParameter(1));
      fGEoPVsP->SetPointError(iEoPPBin,0.,f->GetParError(1));
    }
    delete hEoP;
    delete f;
  }
  TF1* fEoPVsP = new TF1("fEoPVsP","[0]+[1]/x"); //EOP(P) = EOP_TRUE + DELTA_E/P
  fEoPVsP->SetParameter(0,1.);
  fEoPVsP->SetParLimits(0,0.975,1.025);
  fEoPVsP->SetParameter(1,-500.);
  fEoPVsP->SetParLimits(1,-1000.,0.);
  Int_t FitStatus = fGEoPVsP->Fit(fEoPVsP,"Q"); //0: success; any other values: failure
  Double_t  EOP_True  = 1.; //default
  Double_t  Delta_Eop = 0.; //default
  Double_t eEOP_True  = 0.;
  Double_t eDelta_Eop = 0.;
  if(!FitStatus){
    EOP_True   = fEoPVsP->GetParameter(0);
    Delta_Eop  = fEoPVsP->GetParameter(1);
    eEOP_True  = fEoPVsP->GetParError(0);
    eDelta_Eop = fEoPVsP->GetParError(1);
  }
  std::cout << user_normal() << "EOP_True: " << EOP_True  << " +/- " << eEOP_True  << std::endl;
  std::cout << user_normal() << "DeltaE:   " << Delta_Eop << " +/- " << eDelta_Eop << std::endl;
  delete fEoPVsP;
  return std::make_pair(std::make_pair(EOP_True,eEOP_True),std::make_pair(Delta_Eop,eDelta_Eop));
}
