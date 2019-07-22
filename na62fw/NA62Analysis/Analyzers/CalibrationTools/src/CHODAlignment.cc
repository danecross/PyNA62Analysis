// ---------------------------------------------------------------
//
// History:
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 23.08.2018
//
// ---------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHODAlignment.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"

#include <TF1.h>
#include <TCanvas.h>
#include <TStyle.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CHODAlignment
/// \Brief
/// CHOD spatial alignment based on the cracks
/// between quadrants
/// \EndBrief
///
/// \Detailed
/// The analyzer calculates the position of CHOD V and H planes as seen by tracks.
/// Events with a good single track having MUV3 and NewCHOD associations are selected. 
/// The extrapolation of the track to the CHOD V(H) plane is plotted
/// for events with 0 hits in V(H) plane, i.e. for events with tracks pointing to cracks.
/// The plane position is determined from two central cracks and is described by two offsets OffsetX and OffsetY.
/// The offsets are calculated from the gaussian fits of the projections of
/// central cracks on a corresponding axis.
/// The offset values for both planes are printed out and stored in a CDB file CHOD-Alignment.dat.
/// The SpectrometerCHODAssociation analyzer uses only two offsets: OffsetX for the V-plane and OffsetY for the H-plane.
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

CHODAlignment::CHODAlignment(Core::BaseAnalysis *ba) : Analyzer(ba, "CHODAlignment")
{
  RequestTree("CHOD",new TRecoCHODEvent);

  fReadingData = kTRUE;
}

void CHODAlignment::InitHist(){

  fReadingData = GetIsTree();

  if (fReadingData) {
    BookHisto("hNGoodTracks", new TH1F("hNGoodTracks", "N good tracks", 30, -0.5, 29.5));
    BookHisto("hTrackAtCHODVPlane", new TH2F("hTrackAtCHODVPlane", "Track extrapolation to the CHOD V plane, no hits in CHOD V; X [mm]; Y [mm]", 
					     2200, -1100., 1100., 2200, -1100., 1100.));
    BookHisto("hTrackAtCHODHPlane", new TH2F("hTrackAtCHODHPlane", "Track extrapolation to the CHOD H plane, no hits in CHOD H; X [mm]; Y [mm]",
					     2200, -1100., 1100., 2200, -1100., 1100.));
  }
 else {
   cout << user_normal() << "Reading my own output" << endl;
   fHCHODCalibrationV = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hTrackAtCHODVPlane", true));
   fHCHODCalibrationH = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hTrackAtCHODHPlane", true));
 }
}

void CHODAlignment::Process(Int_t){

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();
  std::vector<DownstreamTrack> Tracks =
    *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");
    
  Double_t ZCHODV = GeometricAcceptance::GetInstance()->GetZCHODVPlane();
  Double_t ZCHODH = GeometricAcceptance::GetInstance()->GetZCHODHPlane();
    
  // select physics trigger
  Bool_t PhysicsTrigger = true;
  if (!GetWithMC()) { // data
    PhysicsTrigger = fTriggerConditions->IsPhysicsTrigger(GetL0Data());
  }
  if (!PhysicsTrigger) return;
  
  // Reference time
  Double_t RefTime = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
  RefTime *= TdcCalib;
  
  Int_t NHitsV_intime = 0;
  Int_t NHitsH_intime = 0;
  for (Int_t iHit=0; iHit<CHODEvent->GetNHits(); iHit++) {
    TRecoCHODHit *Hit = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(iHit));
    Int_t Plane = Hit->GetPlaneID();
    if (Plane==0 && fabs(Hit->GetTime()-RefTime)<20.) NHitsV_intime++;
    if (Plane==1 && fabs(Hit->GetTime()-RefTime)<20.) NHitsH_intime++;
  }
  
  Int_t NGoodTracks = 0;
  Int_t TrackIndex = -10;
  for(UInt_t i=0; i<Tracks.size(); i++){
    if (IsGoodTrack(Tracks[i])){
      NGoodTracks++;
      TrackIndex = i;
    }
  }
  FillHisto("hNGoodTracks", NGoodTracks);
  
  // single track events
  if (NGoodTracks!=1) return;
  
  // no hits in V plane
  if (NHitsV_intime==0) 
    FillHisto("hTrackAtCHODVPlane", Tracks[TrackIndex].xAtAfterMagnet(ZCHODV), Tracks[TrackIndex].yAtAfterMagnet(ZCHODV));
  
  // no hits in H plane
  if (NHitsH_intime==0)
    FillHisto("hTrackAtCHODHPlane", Tracks[TrackIndex].xAtAfterMagnet(ZCHODH), Tracks[TrackIndex].yAtAfterMagnet(ZCHODH));   
}

void CHODAlignment::EndOfJobUser(){
  if (fReadingData) {
    SaveAllPlots();
    return;
  }
  if (!fHCHODCalibrationV || !fHCHODCalibrationH) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot found it" << endl;
    return;
  }

  // Histo mode: calculate CHOD offsets
  Double_t OffsetVPlane_x = 0.;
  Double_t OffsetVPlane_y = 0.;
  Double_t OffsetHPlane_x = 0.;
  Double_t OffsetHPlane_y = 0.;

  if (fHCHODCalibrationV->Integral()>400. && fHCHODCalibrationH->Integral()>400.) {
    Int_t MaximumBin;
    Double_t FitCenter;
    Double_t FitHalfRange = 3.0; // half-range in mm

    fHCHODCalibrationV->SetAxisRange(-20., 20., "X");
    fHCHODCalibrationV->SetAxisRange(-1100., 1100., "Y");
    TH1D *hVPlaneOffsetX = fHCHODCalibrationV->ProjectionX("VX");
    fHCHODCalibrationV->SetAxisRange(-1100., 1100., "X");
    fHCHODCalibrationV->SetAxisRange(-20., 20.,"Y");
    TH1D *hVPlaneOffsetY = fHCHODCalibrationV->ProjectionY("VY");

    fHCHODCalibrationH->SetAxisRange(-20., 20., "X");
    fHCHODCalibrationH->SetAxisRange(-1100., 1100., "Y");
    TH1D *hHPlaneOffsetX = fHCHODCalibrationH->ProjectionX("HX");
    fHCHODCalibrationH->SetAxisRange(-1100., 1100., "X");
    fHCHODCalibrationH->SetAxisRange(-20., 20.,"Y");
    TH1D *hHPlaneOffsetY = fHCHODCalibrationH->ProjectionY("HY");

    // perform fits
    TF1* FitFunction1 = new TF1("f1", "gaus", -20., 20.);
    TF1* FitFunction2 = new TF1("f2", "gaus", -20., 20.);
    TF1* FitFunction3 = new TF1("f3", "gaus", -20., 20.);
    TF1* FitFunction4 = new TF1("f4", "gaus", -20., 20.);

    MaximumBin = hVPlaneOffsetX->GetMaximumBin();
    FitCenter = hVPlaneOffsetX->GetBinCenter(MaximumBin);
    FitFunction1->SetRange(FitCenter-FitHalfRange, FitCenter+FitHalfRange);
    hVPlaneOffsetX->Fit(FitFunction1, "R0Q");
    OffsetVPlane_x = FitFunction1->GetParameter(1);

    MaximumBin = hVPlaneOffsetY->GetMaximumBin();
    FitCenter = hVPlaneOffsetY->GetBinCenter(MaximumBin);
    FitFunction2->SetRange(FitCenter-FitHalfRange, FitCenter+FitHalfRange);
    hVPlaneOffsetY->Fit(FitFunction2, "R0Q");
    OffsetVPlane_y = FitFunction2->GetParameter(1);

    MaximumBin = hHPlaneOffsetX->GetMaximumBin();
    FitCenter = hHPlaneOffsetX->GetBinCenter(MaximumBin);
    FitFunction3->SetRange(FitCenter-FitHalfRange, FitCenter+FitHalfRange);
    hHPlaneOffsetX->Fit(FitFunction3, "R0Q");
    OffsetHPlane_x = FitFunction3->GetParameter(1);

    MaximumBin = hHPlaneOffsetY->GetMaximumBin();
    FitCenter = hHPlaneOffsetY->GetBinCenter(MaximumBin);
    FitFunction4->SetRange(FitCenter-FitHalfRange, FitCenter+FitHalfRange);
    hHPlaneOffsetY->Fit(FitFunction4, "R0Q");
    OffsetHPlane_y = FitFunction4->GetParameter(1);

    // build pdf report
    TString OutputPDFFileName = fAnalyzerName + ".pdf";
    gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
    gStyle->SetOptStat(11);

    TCanvas *Canvas0 = new TCanvas("CHODCalibration");
    Canvas0->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
    Canvas0->Divide(2,2);
    Canvas0->cd(1);
    hVPlaneOffsetX->DrawCopy();
    FitFunction1->Draw("same");
    Canvas0->cd(2);
    hVPlaneOffsetY->DrawCopy();
    FitFunction2->Draw("same");
    Canvas0->cd(3);
    hHPlaneOffsetX->DrawCopy();
    FitFunction3->Draw("same");
    Canvas0->cd(4);
    hHPlaneOffsetY->DrawCopy();
    FitFunction4->Draw("same");

    Canvas0->Print(OutputPDFFileName, "pdf");
    Canvas0->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file 
    gErrorIgnoreLevel = -1; // restore the default 

    delete FitFunction1;
    delete FitFunction2;
    delete FitFunction3;
    delete FitFunction4;
    
    // write offsets to a file 
    ofstream OffsetFile;
    OffsetFile.open(Form("CHOD-Alignment.run%06d_0000-run%06d_9999.dat", GetRunID(), GetRunID()));
    OffsetFile << "#" << endl;
    OffsetFile << "# Offsets calculated for the run " << GetRunID() << endl;
    OffsetFile << Form("%2.1f", OffsetVPlane_x) << " " << Form("%2.1f", OffsetVPlane_y) << " " << 
      Form("%2.1f", OffsetHPlane_x) << " " << Form("%2.1f", OffsetHPlane_y) << endl;
    OffsetFile << "#" << endl;
    OffsetFile.close();
  }

  cout << user_normal() << "Offsets for V plane: " << Form("%7.2f", OffsetVPlane_x) << " " << Form("%7.2f", OffsetVPlane_y) << endl;
  cout << user_normal() << "Offsets for H plane: " << Form("%7.2f", OffsetHPlane_x) << " " << Form("%7.2f", OffsetHPlane_y) << endl;

}

CHODAlignment::~CHODAlignment(){}

Bool_t CHODAlignment::IsGoodTrack(DownstreamTrack Track)
{
  Bool_t IsGood = true;
  Int_t ChambersInAcceptance;
  // check geometric acceptance in STRAW  
  ChambersInAcceptance = 0;
  for (Int_t iChamber=0; iChamber<4; iChamber++) {
    ChambersInAcceptance += GeometricAcceptance::GetInstance()->InAcceptance(&Track, kSpectrometer, iChamber);
  }
  // track is in acceptance of all 4 chambers
  if (ChambersInAcceptance!=4) IsGood = false;
  // quality cuts
  if (Track.GetChi2()>20. || fabs(Track.GetMomentumBeforeFit()-Track.GetMomentum())>20000.) IsGood = false;
  // momentum cut (p >= 5 GeV) 
  if (Track.GetMomentum() < 5000.) IsGood = false;
  // in NewCHOD acceptance
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Track, kNewCHOD)) IsGood = false;
  // in LKr acceptance 
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Track, kLKr)) IsGood = false;
  // in MUV3 acceptance 
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Track, kMUV3)) IsGood = false;

  // NewCHOD asso exists 
  if (!Track.NewCHODAssociationExists()) IsGood = false;
  // MUV3 asso exists
  if (!Track.MUV3AssociationExists()) IsGood = false;
  // NewCHOD time close to the trigger
  if (fabs(Track.GetNewCHODTime()-GetL0Data()->GetReferenceFineTime()*TdcCalib)>10.) IsGood = false;

  return IsGood;
}
