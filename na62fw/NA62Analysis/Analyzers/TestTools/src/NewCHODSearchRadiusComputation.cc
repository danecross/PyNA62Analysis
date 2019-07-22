#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "NewCHODSearchRadiusComputation.hh"
#include "DownstreamTrack.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

NewCHODSearchRadiusComputation::NewCHODSearchRadiusComputation(Core::BaseAnalysis *ba) : Analyzer(ba, "NewCHODSearchRadiusComputation") {
  RequestTree("Spectrometer",new TRecoSpectrometerEvent);
}

void NewCHODSearchRadiusComputation::InitOutput(){}

void NewCHODSearchRadiusComputation::InitHist(){
  BookHisto(new TH1F("hN1TrackEvts", "Number of 1-track events; Contained one track; Events", 2, 0, 2));
  BookHisto(new TH2F("hdXAtNewCHOD", "True muon and 'track muon' X-distance at NewCHOD plane; #DeltaX [mm]; p [MeV/c]", 1000, -100., 100., 1000, 0., 100000.));
  BookHisto(new TH2F("hdYAtNewCHOD", "True muon and 'track muon' Y-distance at NewCHOD plane; #DeltaY [mm]; p [MeV/c]", 1000, -100., 100., 1000, 0., 100000.));
  BookHisto(new TH1F("hSigmadXVsMomentum", "Muon and 'track muon' X-distance deviation at NewCHOD plane as a function of track momentum; p [MeV/c]; #sigmaX [mm]", 20, 0., 100000.));
  BookHisto(new TH1F("hSigmadYVsMomentum", "Muon and 'track muon' Y-distance deviation at NewCHOD plane as a function of track momentum; p [MeV/c]; #sigmaY [mm]", 20, 0., 100000.));
}

void NewCHODSearchRadiusComputation::DefineMCSimple(){
  fMCKaonID = fMCSimple.AddParticle(0, 321); //ask for beam Kaon
  fMCMuonID = fMCSimple.AddParticle(fMCKaonID, -13);  //ask for mu+
}

void NewCHODSearchRadiusComputation::StartOfRunUser(){
}

void NewCHODSearchRadiusComputation::StartOfBurstUser(){
}

void NewCHODSearchRadiusComputation::Process(int iEvent){

  double zNewCHOD = GeometricAcceptance::GetInstance()->GetZNewCHOD();
  TRecoSpectrometerEvent *STRAWEvent = static_cast<TRecoSpectrometerEvent*>(GetEvent("Spectrometer"));
  int NTracks = STRAWEvent->GetNCandidates();
  FillHisto("hN1TrackEvts", NTracks==1);

  if (GetWithMC() && NTracks==1) {
    if (fMCSimple.fStatus == MCSimple::kMissing) {
      printIncompleteMCWarning(iEvent);
      return;
    }
    if (fMCSimple.fStatus == MCSimple::kEmpty) {
      printNoMCWarning();
      return;
    }
    KinePart *muon = fMCSimple[-13][0];
    TVector3 muonPosCHOD = muon->GetPosCHODEntry();
    TVector3 muonMomCHOD = (muon->GetMomCHODEntry()).Vect();
    double xAtNewCHOD = muonPosCHOD.X() +
	      (muonMomCHOD.X()/muonMomCHOD.Z()) * (zNewCHOD-muonPosCHOD.Z());
    double yAtNewCHOD = muonPosCHOD.Y() +
	      (muonMomCHOD.Y()/muonMomCHOD.Z()) * (zNewCHOD-muonPosCHOD.Z());
    TVector2 MuonPosNewCHODTruth = TVector2(xAtNewCHOD, yAtNewCHOD);

    TRecoSpectrometerCandidate* Scand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(0));
    if (Scand->GetNChambers()!=4) return;

    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0) || 
	!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1) || 
	!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2) || 
	!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3) || 
	!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD)) return;
    
    TVector2 MuonPosNewCHODTrack = TVector2(Scand->xAt(zNewCHOD), Scand->yAt(zNewCHOD));

    double dX = (MuonPosNewCHODTrack - MuonPosNewCHODTruth).X();
    double dY = (MuonPosNewCHODTrack - MuonPosNewCHODTruth).Y();
    FillHisto("hdXAtNewCHOD", dX, Scand->GetMomentum());
    FillHisto("hdYAtNewCHOD", dY, Scand->GetMomentum());
  }
}

void NewCHODSearchRadiusComputation::PostProcess(){
}

void NewCHODSearchRadiusComputation::EndOfBurstUser(){
}

void NewCHODSearchRadiusComputation::EndOfRunUser(){
}

void NewCHODSearchRadiusComputation::EndOfJobUser(){
  TH2F *histodX	  = static_cast<TH2F*>(fHisto.GetTH2("hdXAtNewCHOD"));
  TH2F *histodY	  = static_cast<TH2F*>(fHisto.GetTH2("hdYAtNewCHOD"));
  TH1D *histoSigmadX = static_cast<TH1D*>(fHisto.GetTH1("hSigmadXVsMomentum"));
  TH1D *histoSigmadY = static_cast<TH1D*>(fHisto.GetTH1("hSigmadYVsMomentum"));

  for (int i=1; i<20; i++) {
    int bin1 = (i-1)*50 + 1;
    int bin2 = i*50;
    // dX
    TH1D *hProjdX;
    hProjdX = histodX->ProjectionX("hProjdX", bin1, bin2);
    TF1 *fitFdX = new TF1("fitFdX", "gaus", -100., 100.);
    fitFdX->SetParameters(100., 0., 10.);
    fitFdX->SetParLimits(2, 0., 10.);
    hProjdX->Fit("fitFdX", "", "", -10., 10.);
    double mean  = fitFdX->GetParameter(2);
    double sigma = fitFdX->GetParError(2);
    delete fitFdX;
    delete hProjdX;
    histoSigmadX->SetBinContent(i, mean);
    histoSigmadX->SetBinError(i, sigma);
    // dY
    TH1D *hProjdY;
    hProjdY = histodY->ProjectionX("hProjdY", bin1, bin2);
    TF1 *fitFdY = new TF1("fitFdY", "gaus", -100., 100.);
    fitFdY->SetParameters(100., 0., 10.);
    fitFdY->SetParLimits(2, 0., 10.);
    hProjdY->Fit("fitFdY", "", "", -10., 10.);
    mean  = fitFdY->GetParameter(2);
    sigma = fitFdY->GetParError(2);
    delete fitFdY;
    delete hProjdY;
    histoSigmadY->SetBinContent(i, mean);
    histoSigmadY->SetBinError(i, sigma); 
  }
  // Plot sigmaX as a function of momentum
  TF1 *FitFuncX = new TF1("FitFuncX", "[0]+[1]/x", 5000., 70000.);
  FitFuncX->SetParameters(0.1, 100.);
  histoSigmadX->Fit("FitFuncX", "", "", 5000., 70000.);
  TCanvas *canvasX = new TCanvas("CanvasX");
  canvasX->Clear();
  histoSigmadX->Draw();
  FitFuncX->Draw("same");
  canvasX->Write();
  canvasX->Print("FitOutputX.pdf", "pdf");
  delete FitFuncX;
  delete canvasX;

  // Plot sigmaY as a function of momentum
  TF1 *FitFuncY = new TF1("FitFuncY", "[0]+[1]/x", 5000., 70000.);
  FitFuncY->SetParameters(0.1, 100.);
  histoSigmadY->Fit("FitFuncY", "", "", 5000., 70000.);
  TCanvas *canvasY = new TCanvas("CanvasY");
  canvasY->Clear();
  histoSigmadY->Draw();
  FitFuncY->Draw("same");
  canvasY->Write();
  canvasY->Print("FitOutputY.pdf", "pdf");
  delete FitFuncY;
  delete canvasY;

  SaveAllPlots();
}

void NewCHODSearchRadiusComputation::DrawPlot(){
}

NewCHODSearchRadiusComputation::~NewCHODSearchRadiusComputation(){
}
