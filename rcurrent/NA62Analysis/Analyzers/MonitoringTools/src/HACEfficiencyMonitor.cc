//------------------------------------------------------------------------------
//
// History:
//
// Created by Stefan Ghinescu (stefan.alexandru.ghinescu@cern.ch) 2017-04-05
//
//------------------------------------------------------------------------------

/// \class HACEfficiencyMonitor
/// \Brief
/// Burst by burst efficiency monitor for HAC
/// \EndBrief
///
/// \Detailed
/// The efficiency is evaluated using a nonstandard K3pi sample.
/// Sample selection algorithm:
/// -Loop over each pair of good tracks in Spectrometer and request same KTAG and GTK id
/// -Check if the missing mass and the charge are consistent with pi+
/// -If yes, propagate the pi+ at front, middle and back plane of HAC
/// -If the projection is inside HAC accpetance, chaeck for HAC signal.
/// The analyzer can be run in two modes:
/// 1) Reading reconstructed data (without using the --histo command line option); 
/// 2) In the HISTO mode (using the --histo command line option), it reads its own 
/// output and produces final report in the form of a PDF file.
/// \author Stefan Ghinescu
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <TChain.h>
#include "HACEfficiencyMonitor.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BaseAnalysis.hh"
#include "TLine.h"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

HACEfficiencyMonitor::HACEfficiencyMonitor(Core::BaseAnalysis *ba) : Analyzer(ba, "HACEfficiencyMonitor") {
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts
  RequestTree("Cedar", new TRecoCedarEvent);
  RequestTree("CHOD", new TRecoCHODEvent);
  RequestTree("Spectrometer", new TRecoSpectrometerEvent);
  RequestTree("HAC", new TRecoHACEvent);
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent);
  RequestL0Data();
  RequestL0SpecialTrigger();
  RequestBeamSpecialTrigger();

  fOutPDFFileName = fAnalyzerName + ".pdf";

  AddParam("ArgonionCountsMin", &fArgonionCountsMin, 1.e5);
  AddParam("NSelectedTriggersMin", &fNSelectedTriggersMin, 1.e3);
  AddParam("EoPEfficiencyThreshold", &fEfficiencyThreshold, 0.2);
}

HACEfficiencyMonitor::~HACEfficiencyMonitor() {
}

void HACEfficiencyMonitor::InitHist() {
  Double_t XMin = -1649.5;
  Double_t XMax = 1050.5;
  Int_t fNBinsX = 1350;
  Double_t YMin = -1349.5;
  Double_t YMax = 1350.5;
  Int_t fNBinsY = 1350;
  Int_t fMaxNBursts = 5000;
  fReadingData = GetIsTree();

  if (fReadingData) {
    std::cout << user_normal() << "Reading reconstructed data" << std::endl;

    BookHisto(new TH1F("K3pi_pmiss_outside_acc", "", 200, 0, 100.));
    BookHisto(new TH2F("PosAtHASCMag", "", 1000, -1500, 1500, 1000, -1500, 1500));

    BookHisto("hExpected_front", new TH2F("hExpected_front", "hExpected_front", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto("hExpected_middle", new TH2F("hExpected_middle", "hExpected_middle", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto("hExpected_back", new TH2F("hExpected_back", "hExpected_back", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto("hMatched_front", new TH2F("hMatched_front", "hMatched_front", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto("hMatched_middle", new TH2F("hMatched_middle", "hMatched_middle", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto("hMatched_back", new TH2F("hMatched_back", "hMatched_back", fNBinsX, XMin, XMax, fNBinsY, YMin, YMax));
    BookHisto("hMatchedVsBurstID", new TH1F("hMatchedVsBurstID", "hMatchedVsBurstID", fMaxNBursts, -0.5, fMaxNBursts - 0.5));

    BookHisto("hEfficiencyVsBurstID", new TGraphErrors());
    fHEfficiencyVsBurstID = (TGraphErrors*) fHisto.GetTGraph("hEfficiencyVsBurstID");
    fHEfficiencyVsBurstID->SetName("hEfficiencyVsBurstID");
    fHEfficiencyVsBurstID->Set(0);

    BookHisto("hExpectedVsBurstID", new TGraphErrors());
    fHExpectedVsBurstID = (TGraphErrors*) fHisto.GetTGraph("hExpectedVsBurstID");
    fHExpectedVsBurstID->SetName("hExpectedVsBurstID");
    fHExpectedVsBurstID->Set(0);

    BookHisto("hArgonionCountsVsBurstID", new TGraphErrors());
    fHArgonionCountsVsBurstID = (TGraphErrors*) fHisto.GetTGraph("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->SetName("hArgonionCountsVsBurstID");
    fHArgonionCountsVsBurstID->Set(0);

    BookHisto("hNTriggersVsBurstID", new TGraphErrors());
    fHNTriggersVsBurstID = (TGraphErrors*) fHisto.GetTGraph("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->SetName("hNTriggersVsBurstID");
    fHNTriggersVsBurstID->Set(0);

    BookHisto("hNSelectedTriggersVsBurstID", new TGraphErrors());
    fHNSelectedTriggersVsBurstID = (TGraphErrors*) fHisto.GetTGraph("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->SetName("hNSelectedTriggersVsBurstID");
    fHNSelectedTriggersVsBurstID->Set(0);
  } else {
    std::cout << user_normal() << "Reading my own output" << std::endl;

    fHExpected_back =   static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hExpected_back", true));
    fHExpected_middle = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hExpected_middle", true));
    fHExpected_front =  static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hExpected_front", true));
    fHMatched_back =    static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hMatched_back", true));
    fHMatched_middle =  static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hMatched_middle", true));
    fHMatched_front =   static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hMatched_front", true));

    // Rebin histos to get enough statistics in each bin
    // Rebin to 1binx * 1biny = 10cm*10cm
    fHExpected_front->Rebin2D(50, 50);
    fHExpected_middle->Rebin2D(50, 50);
    fHExpected_back->Rebin2D(50, 50);
    fHMatched_front->Rebin2D(50, 50);
    fHMatched_middle->Rebin2D(50, 50);
    fHMatched_back->Rebin2D(50, 50);

    fHEfficiencyVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "hEfficiencyVsBurstID", true);
    fHExpectedVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "hExpectedVsBurstID", true);
    fHArgonionCountsVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "hArgonionCountsVsBurstID", true);
    fHNTriggersVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "hNTriggersVsBurstID", true);
    fHNSelectedTriggersVsBurstID = (TGraphErrors*) RequestHistogram(fAnalyzerName, "hNSelectedTriggersVsBurstID", true);

    BookHisto(new TH2F("hEfficiency_back", "hEfficiency_back", fHExpected_back->GetNbinsX(), XMin, XMax, fHExpected_back->GetNbinsY(), YMin, YMax));
    BookHisto(new TH2F("hEfficiency_middle", "hEfficiency_middle", fHExpected_middle->GetNbinsX(), XMin, XMax, fHExpected_middle->GetNbinsY(), YMin, YMax));
    BookHisto(new TH2F("hEfficiency_front", "hEfficiency_front", fHExpected_front->GetNbinsX(), XMin, XMax, fHExpected_front->GetNbinsY(), YMin, YMax));
    fHEfficiency_back =   static_cast<TH2F*>(fHisto.GetTH1("hEfficiency_back"));
    fHEfficiency_middle = static_cast<TH2F*>(fHisto.GetTH1("hEfficiency_middle"));
    fHEfficiency_front =  static_cast<TH2F*>(fHisto.GetTH1("hEfficiency_front"));
  }
}

void HACEfficiencyMonitor::ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType) {
  if (triggerType != 0x23) return; // only EOB   
  fArgonionCounts = GetBeamSpecialTrigger()->GetCountsARGONION() / 1.e9;
}

void HACEfficiencyMonitor::Process(int iEvent) {
  if (!fReadingData) return;
  if (fMCSimple.fStatus == MCSimple::kMissing) {
    printIncompleteMCWarning(iEvent);
    return;
  }
  /*
  if (fMCSimple.fStatus == MCSimple::kEmpty) {
    printNoMCWarning();
    return;
  }*/
  fCedarEvent = GetEvent<TRecoCedarEvent>();
  fCHODEvent = GetEvent<TRecoCHODEvent>();
  fSpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  fHACEvent = GetEvent<TRecoHACEvent>();
  fGigaTrackerEvent = GetEvent<TRecoGigaTrackerEvent>();

  bool HACValue = GetEventHeader()->GetEventQualityMask()&(1 << kHAC);
  if (HACValue == 1) return;
  fNTriggers++;
  Int_t L0DataType = GetL0Data()->GetDataType();
  Int_t L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData = L0DataType & 0x1;
  Bool_t CTRLTrigger = L0DataType & 0x10;
  //Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0x1)) || CTRLTrigger; //MASK0 only + CTRL   
  Bool_t TriggerOK = (PhysicsData && (L0TriggerWord & 0xFF)) || CTRLTrigger; //ALL MASKS + CTRL   
  if (!TriggerOK) return; // process control triggers and selected MASKS only
  fNSelectedTriggers++;

  Double_t piplusmass = 0.13957018;
  Double_t kaonmass = 0.493677;

  Int_t fBurstID = GetEventHeader()->GetBurstID();

  //Position and momenta of the two tracks. Used to compute the momentum of the
  //missing track
  TVector3 postracks[2];
  TLorentzVector ptracks[2];

  //Main loop over pairs starts here
  for (Int_t iTrack = 0; iTrack < fSpectrometerEvent->GetNCandidates(); iTrack++) {
    TRecoSpectrometerCandidate *fCand1 = static_cast<TRecoSpectrometerCandidate*>(fSpectrometerEvent->GetCandidate(iTrack));
    Int_t isGoodStraw1 = IsGoodStrawCandidate(fCand1);
    if (isGoodStraw1 == -1) continue;

    //track should have chod matching. CHOD time more accurate than straw time.
    Double_t time1 = -999.;
    Int_t isGoodCHOD1 = IsGoodCHODCandidate(fCand1, &time1);
    if (isGoodCHOD1 == -1) continue;

    //Set track properties: charge, position, momentum
    Int_t charge1 = fCand1->GetCharge();
    postracks[0] = fCand1->GetPositionBeforeMagnet();
    ptracks[0] = Get4Momentum(fCand1->GetMomentum(), fCand1->GetSlopeXBeforeMagnet(), fCand1->GetSlopeYBeforeMagnet(), piplusmass);

    //check ktag candidate matching and set ktag candidate time
    Double_t fKtagTime1 = 999.;
    Int_t isGoodKtag1 = IsGoodKTAGCandidate(time1, &fKtagTime1);
    if (isGoodKtag1 == -1) continue;

    //check GTK match and store kaon momentum from matchings
    TLorentzVector kaonmom;
    TVector3 kaonpos;
    Int_t isGoodGTK1 = IsGoodGTKCandidate(postracks[0], ptracks[0], time1, &kaonmom, &kaonpos);
    if (isGoodGTK1 == -1) continue;

    //loop over rest of tracks
    for (Int_t jTrack = iTrack + 1; jTrack < fSpectrometerEvent->GetNCandidates(); jTrack++) {
      TRecoSpectrometerCandidate *fCand2 = static_cast<TRecoSpectrometerCandidate*>(fSpectrometerEvent->GetCandidate(jTrack));
      Int_t isGoodStraw2 = IsGoodStrawCandidate(fCand2);
      if (isGoodStraw2 == -1) continue;

      Double_t fTime2 = 999.;
      Int_t isGoodCHOD2 = IsGoodCHODCandidate(fCand2, &fTime2);
      if (isGoodCHOD2 == -1) continue;

      //Set track properties
      Int_t charge2 = fCand2->GetCharge();
      if ((charge1 + charge2) != 0) continue; //pair should be formed by pi+ and pi-
      postracks[1] = fCand2->GetPositionBeforeMagnet();
      ptracks[1] = Get4Momentum(fCand2->GetMomentum(), fCand2->GetSlopeXBeforeMagnet(), fCand2->GetSlopeYBeforeMagnet(), piplusmass);

      //check ktag candidate matching. Must be the same kaon for both tracks
      Double_t fKtagTime2 = 999.;
      Int_t isGoodKtag2 = IsGoodKTAGCandidate(fTime2, &fKtagTime2);
      if (isGoodKtag2 != isGoodKtag2) continue; //same ktag because the pair should come from same kaon decay

      //Match with GTK. Same KTAG gives good approximation for same GTK.
      Int_t isGoodGTK2 = IsGoodGTKCandidate(postracks[1], ptracks[1], fTime2, &kaonmom, &kaonpos);
      if (isGoodGTK1 != isGoodGTK2) continue;

      //Plot the missing mass2 using tracks momenta and kaon momentum. Keep only pairs for which mmiss2
      //is consistent with the mass of pi+
      TLorentzVector pmiss = kaonmom - ptracks[0] - ptracks[1];
      Double_t mmiss = pmiss.Mag2();
      if (mmiss < 0.015 || mmiss > 0.025) continue;

      //Now Kaon momentum and position should be the same. Compute common vertex of the two tracks.
      //The common vertex will be the starting point of the "missing" track.
      //Cut against the zvertex: 60m < zvertex < 170m.
      Double_t cda = -999.;
      TVector3 vtx = MultiTrackVertexSimple(2, ptracks, postracks, &cda);

      if (vtx.Z() < 60000. || vtx.Z() > 170000.) continue;

      //Missing track should not be in acceptance of any downstream detector in order to be able to arrive at hasc.
      //The flag isAcc has the value 0 is track can arrive at hasc and 1 otherwise. It is set to Int_t due to debugging purposes.
      TVector3 pmommiss = pmiss.Vect();
      TVector3 posmiss = {vtx.X() / 10., vtx.Y() / 10., vtx.Z() / 10.};
      Int_t isAcc = IsInAcceptanceDownstream(pmommiss, posmiss, 1);
      //FillHisto("K3pi_missing_track_in_acc_downstream", isAcc);
      if (isAcc != 0) continue;
      //popagate "missing" track at front of HASC Magnet
      Double_t thetaxafter = -999.;
      TVector3 posAtMagnet = Propagate(1, &pmommiss, &posmiss, &thetaxafter, 24720., 1);
      FillHisto("PosAtHASCMag", posmiss.X(), posmiss.Y());
      //propagate from front of magnet to front face of hasc. If track is not in magnet acceptance continue normal propagation
      TVector3 posAtHASC_front;
      TVector3 posAtHASC_middle;
      TVector3 posAtHASC_back;
      pmommiss.SetZ(sqrt((pmommiss.Mag2() - pmommiss.Y() * pmommiss.Y()) / 1 + thetaxafter * thetaxafter));
      pmommiss.SetX(pmommiss.Z() * thetaxafter);

      //propagate at various hasc planes.
      if (sqrt(posAtMagnet.X() * posAtMagnet.X() + posAtMagnet.Y() * posAtMagnet.Y()) > 15.) {
        posAtHASC_front = Propagate(1, &pmommiss, &posAtMagnet, &thetaxafter, 25165., 1);
        posAtHASC_middle = Propagate(1, &pmommiss, &posAtMagnet, &thetaxafter, 25250., 1);
        posAtHASC_back = Propagate(1, &pmommiss, &posAtMagnet, &thetaxafter, 25335., 1);

        posAtHASC_front.SetX(posAtHASC_front.X()*10.);
        posAtHASC_middle.SetX(posAtHASC_middle.X()*10.);
        posAtHASC_back.SetX(posAtHASC_back.X()*10.);

        posAtHASC_front.SetY(posAtHASC_front.Y()*10.);
        posAtHASC_middle.SetY(posAtHASC_middle.Y()*10.);
        posAtHASC_back.SetY(posAtHASC_back.Y()*10.);

        posAtHASC_front.SetZ(251650.);
        posAtHASC_middle.SetZ(252500.);
        posAtHASC_back.SetZ(253350.);
      } else {
        //posAtMagnet = {posAtMagnet.X() *10., posAtMagnet.Y() * 10, posAtMagnet.Z() * 10};
        //pmommiss.SetXYZ(pmommiss.X()*1000., pmommiss.Y()*1000, pmommiss.Z()*1000);
        posAtHASC_front = Propagate(1, &pmommiss, &posAtMagnet, &thetaxafter, 25165., 2);
        posAtHASC_middle = Propagate(1, &pmommiss, &posAtMagnet, &thetaxafter, 25250., 2);
        posAtHASC_back = Propagate(1, &pmommiss, &posAtMagnet, &thetaxafter, 25335., 2);

        posAtHASC_front.SetX(posAtHASC_front.X()*10.);
        posAtHASC_middle.SetX(posAtHASC_middle.X()*10.);
        posAtHASC_back.SetX(posAtHASC_back.X()*10.);

        posAtHASC_front.SetY(posAtHASC_front.Y()*10.);
        posAtHASC_middle.SetY(posAtHASC_middle.Y()*10.);
        posAtHASC_back.SetY(posAtHASC_back.Y()*10.);

        posAtHASC_front.SetZ(251650.);
        posAtHASC_middle.SetZ(252500.);
        posAtHASC_back.SetZ(253350.);
      }

      //Monitoring of efficiency starts here.
      FillHisto("hExpected_front", posAtHASC_front.X(), posAtHASC_front.Y());
      FillHisto("hExpected_middle", posAtHASC_middle.X(), posAtHASC_middle.Y());
      FillHisto("hExpected_back", posAtHASC_back.X(), posAtHASC_back.Y());
      if (fabs(posAtHASC_back.X()) < 750. && fabs(posAtHASC_back.Y()) < 750.) {
        fNExpectedPerBurst++; //Efficiency monitored only if projection is inside HASC acceptance.
        //Loose condition.
      }

      if (fHACEvent->GetNHits() == 0) continue;

      FillHisto("hMatched_front", posAtHASC_front.X(), posAtHASC_front.Y());
      FillHisto("hMatched_middle", posAtHASC_middle.X(), posAtHASC_middle.Y());
      FillHisto("hMatched_back", posAtHASC_back.X(), posAtHASC_back.Y());
      if (fabs(posAtHASC_back.X()) < 750. && fabs(posAtHASC_back.Y()) < 750.) {
        fNMatchedPerBurst++;
      } else FillHisto("K3pi_pmiss_outside_acc", pmommiss.Z());
    }
  }
}

void HACEfficiencyMonitor::EndOfJobUser() {
  gErrorIgnoreLevel = 5000;

  if (!fReadingData) {
    if (!fHMatched_back) {
      std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      return;
    }

    fHEfficiency_back->Divide(fHMatched_back, fHExpected_back, 1., 1., "B");
    fHEfficiency_middle->Divide(fHMatched_middle, fHExpected_middle, 1., 1., "B");
    fHEfficiency_front->Divide(fHMatched_front, fHExpected_front, 1., 1., "B");

    //Create Bad burst list
    CreateBadBurstList();

    //Remove Empty bursts from TGraph
    for (Int_t iPoint = 0; iPoint < fHEfficiencyVsBurstID->GetN(); iPoint++) {
      double BurstID = 0., NSelectedTriggers = 0.;
      fHNSelectedTriggersVsBurstID->GetPoint(iPoint, BurstID, NSelectedTriggers);
      if (NSelectedTriggers < fNSelectedTriggersMin) {
        fHEfficiencyVsBurstID->RemovePoint(iPoint);
        fHExpectedVsBurstID->RemovePoint(iPoint);
        fHArgonionCountsVsBurstID->RemovePoint(iPoint);
        fHNTriggersVsBurstID->RemovePoint(iPoint);
        fHNSelectedTriggersVsBurstID->RemovePoint(iPoint);
        iPoint--;
      }
    }
    BuildPFDReport();
  }

  SaveAllPlots();
  gErrorIgnoreLevel = -1; // restore the default
}

void HACEfficiencyMonitor::StartOfBurstUser() {
  fBurstID = GetBurstID();
  fArgonionCounts = 0.;
  fNTriggers = 0;
  fNSelectedTriggers = 0;
  fNMatchedPerBurst = 0.;
  fNExpectedPerBurst = 0.;
}

void HACEfficiencyMonitor::EndOfBurstUser() {
  if (fReadingData) {
    double Efficiency = 0., eEfficiency = 0.;
    if (fNExpectedPerBurst) {
      Efficiency = fNMatchedPerBurst / fNExpectedPerBurst;
      eEfficiency = sqrt(Efficiency * (1. - Efficiency) / fNExpectedPerBurst);
    } else {
      fHExpectedVsBurstID->Set(fHExpectedVsBurstID->GetN() + 1);
      fHExpectedVsBurstID->SetPoint(fHExpectedVsBurstID->GetN() - 1, fBurstID, fNExpectedPerBurst);
      fHExpectedVsBurstID->SetPointError(fHExpectedVsBurstID->GetN() - 1, 0, 0);
      return;
    }
    fHEfficiencyVsBurstID->Set(fHEfficiencyVsBurstID->GetN() + 1);
    fHEfficiencyVsBurstID->SetPoint(fHEfficiencyVsBurstID->GetN() - 1, fBurstID, Efficiency);
    fHEfficiencyVsBurstID->SetPointError(fHEfficiencyVsBurstID->GetN() - 1, 0, eEfficiency);
    
    fHExpectedVsBurstID->Set(fHExpectedVsBurstID->GetN() + 1);
    fHExpectedVsBurstID->SetPoint(fHExpectedVsBurstID->GetN() - 1, fBurstID, fNExpectedPerBurst);
    fHExpectedVsBurstID->SetPointError(fHExpectedVsBurstID->GetN() - 1, 0, 1/sqrt(fNExpectedPerBurst));
    
    fHArgonionCountsVsBurstID->Set(fHArgonionCountsVsBurstID->GetN() + 1);
    fHArgonionCountsVsBurstID->SetPoint(fHArgonionCountsVsBurstID->GetN() - 1, fBurstID, fArgonionCounts);
    fHArgonionCountsVsBurstID->SetPointError(fHArgonionCountsVsBurstID->GetN() - 1, 0, 0);
    
    fHNTriggersVsBurstID->Set(fHNTriggersVsBurstID->GetN() + 1);
    fHNTriggersVsBurstID->SetPoint(fHNTriggersVsBurstID->GetN() - 1, fBurstID, fNTriggers);
    fHNTriggersVsBurstID->SetPointError(fHNTriggersVsBurstID->GetN() - 1, 0, 0);
    
    fHNSelectedTriggersVsBurstID->Set(fHNSelectedTriggersVsBurstID->GetN() + 1);
    fHNSelectedTriggersVsBurstID->SetPoint(fHNSelectedTriggersVsBurstID->GetN() - 1, fBurstID, fNSelectedTriggers);
    fHNSelectedTriggersVsBurstID->SetPointError(fHNSelectedTriggersVsBurstID->GetN() - 1, 0, 0);
  }
}

void HACEfficiencyMonitor::BuildPFDReport() {
  if(fCanvas) delete fCanvas;
  fCanvas = new TCanvas("Canvas");
  fCanvas->Divide(3, 1);
  fHEfficiency_back->SetTitle(Form("HASC efficiency back plane for run %d", GetRunID()));
  fHEfficiency_middle->SetTitle(Form("HASC efficiency middle plane for run %d", GetRunID()));
  fHEfficiency_front->SetTitle(Form("HASC efficiency front plane for run %d", GetRunID()));
  fHEfficiency_back->SetStats(0);
  fHEfficiency_middle->SetStats(0);
  fHEfficiency_front->SetStats(0);
  fCanvas->cd(1);
  fHEfficiency_back->Draw("COLZ");
  fCanvas->cd(2);
  fHEfficiency_middle->Draw("COLZ");
  fCanvas->cd(3);
  fHEfficiency_front->Draw("COLZ");
  fHEfficiency_back->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency_middle->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency_front->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency_back->GetYaxis()->SetTitle("y [mm]");
  fHEfficiency_middle->GetYaxis()->SetTitle("y [mm]");
  fHEfficiency_front->GetYaxis()->SetTitle("y [mm]");
  fCanvas->Print(Form(fOutPDFFileName + "("), "pdf");
  fCanvas->Delete();
  delete fCanvas;

  fCanvas = new TCanvas("Canvas");

  if (fHEfficiencyVsBurstID->GetN()) {
    fHEfficiencyVsBurstID->SetTitle(Form("HASC efficiency Vs BurstID for run %d", GetRunID()));
    fHEfficiencyVsBurstID->Draw("AP");
    fHEfficiencyVsBurstID->SetMarkerStyle(20);
    fHEfficiencyVsBurstID->SetMarkerSize(0.3);
    fHEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEfficiencyVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fHEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0., 1.1);
    TLine* EfficiencyThrBurst = new TLine(fHEfficiencyVsBurstID->GetXaxis()->GetXmin(), fEfficiencyThreshold, fHEfficiencyVsBurstID->GetXaxis()->GetXmax(), fEfficiencyThreshold);
    EfficiencyThrBurst->SetLineColor(kRed);
    EfficiencyThrBurst->Draw();
    fCanvas->Print(fOutPDFFileName, "pdf");
  }
  fCanvas->Print(Form(fOutPDFFileName + "]"), "pdf");
  delete fCanvas;
}

void HACEfficiencyMonitor::CreateBadBurstList() {
  Int_t NEmptyBursts = 0, NBadLowStat = 0, NBadEfficiency = 0;
  ofstream BadBurstList;
  BadBurstList.open(Form("HACEfficiencyMonitor.thrs%.3f.dat", fEfficiencyThreshold), ios::app);
  for (Int_t iPoint = 0; iPoint < fHEfficiencyVsBurstID->GetN(); iPoint++) {
    double BurstID = 0., Efficiency = 0., Argonion = 0., NTriggers = 0., NSelectedTriggers = 0., fNExpected = 0.;
    fHEfficiencyVsBurstID->GetPoint(iPoint, BurstID, Efficiency);
    fHExpectedVsBurstID->GetPoint(iPoint, BurstID, fNExpected);
    fHArgonionCountsVsBurstID->GetPoint(iPoint, BurstID, Argonion);
    fHNTriggersVsBurstID->GetPoint(iPoint, BurstID, NTriggers);
    fHNSelectedTriggersVsBurstID->GetPoint(iPoint, BurstID, NSelectedTriggers);
    double eEfficiency = fHEfficiencyVsBurstID->GetErrorY(iPoint);
    if (NSelectedTriggers < fNSelectedTriggersMin) {
      if (NTriggers == 0 && Argonion == 0) { // Corrupted file
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " BADFILE " << std::endl;
      } else if (Argonion == 0) { // No Argonion info
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " NO_ARGN " << std::endl;
      } else if (Argonion * 1.e9 < fArgonionCountsMin) { // Low number of triggers due to no beam
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " LOWARGN " << Argonion * 1.e9 << std::endl;
      } else {
        BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " LOWTRIG " << NSelectedTriggers << std::endl;
      }
      NEmptyBursts++;
    } else if (fNExpected < 2) {
      BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " LOWSTAT" << fNExpectedPerBurst << std::endl;
      NBadLowStat++;
    } else if (Efficiency < fEfficiencyThreshold) { // bad HASC efficiency
      BadBurstList << Form("BadBurst %06d %04d", GetRunID(), (Int_t) BurstID) << " EFFI " << Efficiency << " +/- " << eEfficiency << std::endl;
      NBadEfficiency++;
    }
  }
  BadBurstList.close();
  std::cout << user_standard() << "***** BADBURST SUMMARY ***** " << std::endl;
  std::cout << user_standard() << "Bad(Effi):  " << NBadEfficiency << std::endl;
  std::cout << user_standard() << "Bad(Expected): " << NBadLowStat << std::endl;
  std::cout << user_standard() << "Bad(HACTotal): " << NBadEfficiency;
  std::cout << user_standard() << " over " << fHEfficiencyVsBurstID->GetN() - NEmptyBursts << " non-empty bursts (NEmptyBursts: " << NEmptyBursts << ")" << std::endl;
  std::cout << user_standard() << "**************************** " << std::endl;
}

TLorentzVector HACEfficiencyMonitor::Get4Momentum(Double_t pmom, Double_t thetax, Double_t thetay, Double_t mass) {
  TLorentzVector momentum;
  Double_t pmag = pmom;
  Double_t pmomz = pmag / sqrt(1. + thetax * thetax + thetay * thetay);
  Double_t pmomx = pmomz*thetax;
  Double_t pmomy = pmomz*thetay;
  momentum.SetXYZM(pmomx / 1000, pmomy / 1000, pmomz / 1000, mass);
  return momentum;
}


//Good Track deffinition:
// 1) chi2 <20
// 2) pquality <= 20GeV
// 3) NChambers >=4

Int_t HACEfficiencyMonitor::IsGoodStrawCandidate(TRecoSpectrometerCandidate * fCand) {
  Int_t isGood = 1;

  Double_t qualityMomentum = fabs(fCand->GetMomentumBeforeFit()) - fCand->GetMomentum();
  if (fCand->GetChi2() > 20.) isGood = -1;
  if (fCand->GetNChambers() < 4) isGood = -1;
  if (qualityMomentum > 20000.) isGood = -1;

  return isGood;
}

//CHOD Matching, simple matching using chi2 like variable in distance between
// projection and chod candidate and delta time

Int_t HACEfficiencyMonitor::IsGoodCHODCandidate(TRecoSpectrometerCandidate* fCand, Double_t * fTime1) {
  Int_t minid = -1;

  TVector3 postrack = fCand->GetPositionAfterMagnet();
  Double_t slopeX = fCand->GetSlopeXAfterMagnet();
  Double_t slopeY = fCand->GetSlopeYAfterMagnet();
  Double_t timetrack = fCand->GetTime();

  //compute Position at CHOD
  Double_t xAtChod = postrack.X() + slopeX * (239009. - postrack.Z());
  Double_t yAtChod = postrack.Y() + slopeY * (239389. - postrack.Z());
  Double_t minchi2 = 999., mintime = 999.;

  //Match with chod candidate
  for (Int_t icand = 0; icand < fCHODEvent->GetNCandidates(); icand++) {
    TRecoCHODCandidate *fchod = static_cast<TRecoCHODCandidate*>(fCHODEvent->GetCandidate(icand));
    Double_t xchod = fchod->GetHitPosition().X();
    Double_t ychod = fchod->GetHitPosition().Y();
    Double_t dx = xchod - xAtChod;
    Double_t dy = ychod - yAtChod;
    Double_t dt = fchod->GetTime() - timetrack;
    Double_t chi2 = (dx * dx + dy * dy) / (26. * 26.) + dt * dt / (9. * 9.);
    if (chi2 < minchi2) {
      minchi2 = chi2;
      mintime = fchod->GetTime();
      minid = icand;
    }
  }
  if (minchi2 > 15.) return -1;
  *fTime1 = mintime;
  return minid;
}

//The KTAG matching is only time-based and there is no need for the Straw candidate as parameter

Int_t HACEfficiencyMonitor::IsGoodKTAGCandidate(Double_t fTime1, Double_t * fKTAGTime) {
  Int_t minid = -1;

  Double_t mindt = 999;
  for (Int_t icand = 0; icand < fCedarEvent->GetNCandidates(); icand++) {
    TRecoCedarCandidate *cand = static_cast<TRecoCedarCandidate*>(fCedarEvent->GetCandidate(icand));
    if (cand->GetNSectors() < 5) continue;
    Double_t dt = fTime1 - cand->GetTime();
    if (fabs(dt) < fabs(mindt)) {
      mindt = dt;
      minid = icand;
      *fKTAGTime = cand->GetTime();
    }
  }
  if (fabs(mindt) > 4) return -1;
  return minid;
}

Int_t HACEfficiencyMonitor::IsGoodGTKCandidate(TVector3 postrack, TLorentzVector ptrack, Double_t fTime1, TLorentzVector *kaonmomentum, TVector3 * kaonpos) {
  Int_t minid = -1;

  Double_t minchi2 = 999.;
  for (Int_t iCand = 0; iCand < fGigaTrackerEvent->GetNCandidates(); iCand++) {
    TRecoGigaTrackerCandidate *fCand = static_cast<TRecoGigaTrackerCandidate*>(fGigaTrackerEvent->GetCandidate(iCand));
    if (fCand->GetType() != 123) continue; //clean sample

    TVector3 pkaon = fCand->GetMomentum();
    Double_t time = fCand->GetTime();
    Double_t dt = time - fTime1;
    TVector3 poskaon = fCand->GetPosition(3);
    TLorentzVector candmom = Get4Momentum(pkaon.Mag(), pkaon.X() / pkaon.Z(), pkaon.Y() / pkaon.Z(), 0.493677);
    TLorentzVector ptracks[2];
    TVector3 postracks[2];
    Double_t cda = 999.;
    ptracks[0] = candmom;
    ptracks[1] = ptrack;
    postracks[0] = poskaon;
    postracks[1] = postrack;

    TVector3 fVertex = MultiTrackVertexSimple(2, ptracks, postracks, &cda);
    Double_t chi2 = cda * cda / (8 * 20 * 20) + dt * dt / (8 * 1.4 * 1.4);
    if (chi2 < minchi2) {
      minchi2 = chi2;
      minid = iCand;
      *kaonmomentum = candmom;
      *kaonpos = poskaon;
    }
  }
  if (minchi2 > 15.) return -1;
  return minid;
}

TVector3 HACEfficiencyMonitor::MultiTrackVertexSimple(Int_t nTracks, TLorentzVector* ptracks, TVector3* postracks, Double_t * cda) {
  TVector3 avPosition(0, 0, 0);
  TVector3 avSlope(0, 0, 0);
  TVector3 avSlope2(0, 0, 0);
  TVector3 avMixed(0, 0, 0);

  // Compute Z as the position of minimum apporach between tracks
  Double_t z0 = 0;
  for (Int_t j = 0; j < nTracks; j++) {
    TVector3 position = postracks[j];
    TLorentzVector momentum = ptracks[j];
    avPosition += position;
    TVector3 ddz = momentum.Vect()*(1. / momentum.Vect().Z());
    avSlope += ddz;
    avSlope2 += TVector3(ddz.X() * ddz.X(), ddz.Y() * ddz.Y(), ddz.Z() * ddz.Z());
    avMixed += TVector3(position.X() * ddz.X(), position.Y() * ddz.Y(), position.Z() * ddz.Z());
    z0 = position.Z();
  }
  avPosition = (1. / nTracks) * avPosition;
  avSlope = (1. / nTracks) * avSlope;
  avSlope2 = (1. / nTracks) * avSlope2;
  avMixed = (1. / nTracks) * avMixed;
  Double_t num = nTracks * (avMixed.X() + avMixed.Y()) - nTracks * (avPosition.X() * avSlope.X() + avPosition.Y() * avSlope.Y());
  Double_t den = nTracks * (avSlope2.X() + avSlope2.Y()) - nTracks * (avSlope.X() * avSlope.X() + avSlope.Y() * avSlope.Y());
  Double_t zvertex = z0 - num / den;

  // Compute the trasnverse position and the cda
  TVector3 avPosVtx(0, 0, 0);
  TVector3 avPosVtx2(0, 0, 0);
  for (Int_t j = 0; j < nTracks; j++) {
    TVector3 position = postracks[j];
    TLorentzVector momentum = ptracks[j];
    TVector3 posvtx = position + momentum.Vect()*(1. / momentum.Vect().Z())*(zvertex - position.Z());
    avPosVtx += posvtx;
    avPosVtx2 += TVector3(posvtx.X() * posvtx.X(), posvtx.Y() * posvtx.Y(), posvtx.Z() * posvtx.Z());
  }
  avPosVtx = (1. / nTracks) * avPosVtx;
  avPosVtx2 = (1. / nTracks) * avPosVtx2;
  *cda = sqrt(avPosVtx2.X() + avPosVtx2.Y() - avPosVtx.X() * avPosVtx.X() - avPosVtx.Y() * avPosVtx.Y());

  return TVector3(avPosVtx.X(), avPosVtx.Y(), zvertex);
}

Int_t HACEfficiencyMonitor::IsInAcceptanceDownstream(TVector3 pmom, TVector3 vertex, Int_t charge) {
  TVector3 position;
  Double_t thetaxafter = 0.;

  //check if it is in STRAW. 4 chambers to check
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 183508.0, 1);
  Bool_t isStraw0 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10, position.Y()*10, kSpectrometer, 0);
  //FillHisto("K3pi_isStraw0", isStraw0);
  if (isStraw0) return 1;

  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 194066.0, 1);
  Bool_t isStraw1 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10, position.Y()*10, kSpectrometer, 0);
  //FillHisto("K3pi_isStraw1", isStraw1);
  if (isStraw1) return 1;

  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 204459.0, 1);
  Bool_t isStraw2 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10, position.Y()*10, kSpectrometer, 0);
  //FillHisto("K3pi_isStraw2", isStraw2);
  if (isStraw2) return 1;

  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 218885.0, 1);
  Bool_t isStraw3 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10, position.Y()*10, kSpectrometer, 0);
  //FillHisto("K3pi_isStraw3", isStraw3);
  if (isStraw3) return 1;

  //check if it is in chod
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 239009.0, 1);
  Bool_t isCHOD = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10., position.Y()*10., kCHOD);
  //FillHisto("K3pi_isCHOD", isCHOD);
  if (isCHOD) return 1;

  //check if it is in new CHOD
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 238100.0, 1);
  Bool_t isNewCHOD = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10., position.Y()*10, kNewCHOD);
  //FillHisto("K3pi_isNewCHOD", isNewCHOD);
  if (isNewCHOD) return 1;

  //check if it is in LKr
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 241093.0, 1);
  Bool_t isLKr = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10., position.Y()*10, kLKr);
  //FillHisto("K3pi_isLKr", isLKr);
  if (isLKr) return 1;

  //check if it is in MUV1 
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 243418.0, 1);
  Bool_t isMUV1 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10., position.Y()*10, kMUV1);
  //FillHisto("K3pi_isMUV1", isMUV1);
  if (isMUV1) return 1;

  //check if it is in MUV2
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 244435.0, 1);
  Bool_t isMUV2 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10., position.Y()*10, kMUV2);
  //FillHisto("K3pi_isMUV2", isMUV2);
  if (isMUV2) return 1;

  //check if it is in MUV3
  position = Propagate(1, &pmom, &vertex, &thetaxafter, 0.1 * 246800.0, 1);
  Bool_t isMUV3 = GeometricAcceptance::GetInstance()->InAcceptance(position.X()*10., position.Y()*10, kMUV3);
  //FillHisto("K3pi_isMUV3", isMUV3);
  if (isMUV3) return 1;

  //return 0  if passed all tests
  return 0;
}

TVector3 HACEfficiencyMonitor::Propagate(Int_t charge, TVector3 *momentum, TVector3 *position, Double_t *thetaxafter, Double_t fZEnd, Int_t flag) {
  if (fZEnd <= 24720. || position->Z() > 24920. || flag == 1) {
    Double_t fEC = TMath::C()* 1.e-9 * 1.e-4 * 1.e-2;
    TVector3 fB(0., 0.6928 * 10000, 0.);
    TVector3 fPosExp;
    Double_t dMag = 0.1 * 1300;
    Double_t zMag = 0.1 * 196345;
    //  Double_t fStartX = 0.1*position->X();
    //  Double_t fStartY = 0.1*position->Y();
    //  Double_t fStartZ = 0.1*position->Z();
    Double_t fStartX = position->X();
    Double_t fStartY = position->Y();
    Double_t fStartZ = position->Z();
    Double_t fPartP = momentum->Mag();
    Double_t fPartThetaX = momentum->X() / momentum->Z();
    Double_t fPartThetaY = momentum->Y() / momentum->Z();
    //  fZEnd *= 0.1;
    Int_t fPartQ = charge;

    // fZEnd before magnet
    if ((fZEnd <= zMag && fStartZ <= zMag) || (fZEnd > zMag && fStartZ > zMag)) {
      fPosExp.SetX(fStartX + fPartThetaX * (fZEnd - fStartZ));
      fPosExp.SetY(fStartY + fPartThetaY * (fZEnd - fStartZ));
      fPosExp.SetZ(fZEnd);
      //    return fPosExp*10;
      *thetaxafter = fPartThetaX;
      return fPosExp;
    }

    // fZEnd after MNP33
    fPosExp.SetX(fStartX + fPartThetaX * (zMag - fStartZ));
    fPosExp.SetY(fStartY + fPartThetaY * (zMag - fStartZ));
    fPosExp.SetZ(zMag);
    TVector3 fP;
    fP.SetZ(fPartP / sqrt(1. + fPartThetaX * fPartThetaX + fPartThetaY * fPartThetaY));
    fP.SetX(fP.Z() * fPartThetaX);
    fP.SetY(fP.Z() * fPartThetaY);
    Int_t qb = fB.Y() > 0 ? 1 : -1;
    Double_t rho = (fP.Cross(fB)).Mag() / (fPartQ * fEC * fB.Mag2());
    Double_t delta = dMag / rho;
    Double_t sint = sin(atan(fPartThetaX));
    Double_t cost = cos(atan(fPartThetaX));
    Double_t dx = qb * rho * (-cost + sqrt(1 - (delta - qb * sint)*(delta - qb * sint)));
    fPosExp.SetX(fPosExp.X() + dx);
    fPosExp.SetY(fPosExp.Y() + fPartThetaY * dMag);
    fPosExp.SetZ(fPosExp.Z() + dMag);
    Double_t fThetaXAfter = -qb * (delta - qb * sint) / sqrt(1. - (delta - qb * sint)*(delta - qb * sint));
    fPosExp.SetX(fPosExp.X() + fThetaXAfter * (fZEnd - fPosExp.Z()));
    fPosExp.SetY(fPosExp.Y() + fPartThetaY * (fZEnd - fPosExp.Z()));
    fPosExp.SetZ(fZEnd);
    *thetaxafter = fThetaXAfter;

    //  return fPosExp*10;  
    return fPosExp;
  } else {
    Double_t fEC = TMath::C()* 1.e-9 * 1.e-4 * 1.e-2;
    TVector3 fB(0., 1.7012 * 10000, 0.);
    TVector3 fPosExp;
    Double_t dMag = 0.1 * 2000.;
    Double_t zMag = 0.1 * 247200.;

    Double_t fStartX = position->X();
    Double_t fStartY = position->Y();
    Double_t fStartZ = position->Z();
    Double_t fPartP = momentum->Mag();
    Double_t fPartThetaX = momentum->X() / momentum->Z();
    Double_t fPartThetaY = momentum->Y() / momentum->Z();
    Int_t fPartQ = charge;

    fPosExp.SetX(fStartX + fPartThetaX * (zMag - fStartZ));
    fPosExp.SetY(fStartY + fPartThetaY * (zMag - fStartZ));
    fPosExp.SetZ(zMag);

    TVector3 fP;
    fP.SetZ(fPartP / sqrt(1. + fPartThetaX * fPartThetaX + fPartThetaY * fPartThetaY));
    fP.SetX(fP.Z() * fPartThetaX);
    fP.SetY(fP.Z() * fPartThetaY);

    Int_t qb = fB.Y() > 0 ? 1 : -1;
    Double_t rho = (fP.Cross(fB)).Mag() / (fPartQ * fEC * fB.Mag2());
    Double_t delta = dMag / rho;
    Double_t sint = sin(atan(fPartThetaX));
    Double_t cost = cos(atan(fPartThetaX));
    Double_t dx = qb * rho * (-cost + sqrt(1 - (delta - qb * sint)*(delta - qb * sint)));

    fPosExp.SetX(fPosExp.X() + dx);
    fPosExp.SetY(fPosExp.Y() + fPartThetaY * dMag);
    fPosExp.SetZ(fPosExp.Z() + dMag);
    Double_t fThetaXAfter = -qb * (delta - qb * sint) / sqrt(1. - (delta - qb * sint)*(delta - qb * sint));

    fPosExp.SetX(fPosExp.X() + fThetaXAfter * (fZEnd - fPosExp.Z()));
    fPosExp.SetY(fPosExp.Y() + fPartThetaY * (fZEnd - fPosExp.Z()));
    fPosExp.SetZ(fZEnd);

    return fPosExp;
  }
}
