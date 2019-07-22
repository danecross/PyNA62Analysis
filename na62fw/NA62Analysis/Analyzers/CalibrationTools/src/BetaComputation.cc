#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "BetaComputation.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "SpectrometerVertexBuilder.hh"
#include <TF1.h>
#include <TGraphErrors.h>
#include <TGraph.h>
#include <TVector3.h>
#include <TLine.h>
#include <TLatex.h>
#include "TStyle.h"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class BetaComputation
/// \Brief
/// Evaluation of the spectrometer calibration constant beta
/// \EndBrief
/// \Detailed
/// \EndDetailed

BetaComputation::BetaComputation(Core::BaseAnalysis *ba) : Analyzer(ba, "BetaComputation"), fReadingData(false), fNBetaScan(0), fBeta(0.), fBStep(0.), fBetaError(0.), fBetaInit(0.), fBetaHisto(nullptr){
  RequestTree("Spectrometer",new TRecoSpectrometerEvent);

  AddParam("NumberOfBetaScanSteps",&fNBetaScan, 300); //number of beta scan steps
}

void BetaComputation::InitOutput(){}

void BetaComputation::InitHist(){

  // The GetIsTree() function checks if the user is running with
  // reconstructed data or in histo mode with the user`s own input
  fReadingData = GetIsTree();

  fBeta        = 0;
  fBetaInit    = -3.0E-3; //initial beta value for the beta scan
  fBStep       = 2.0E-5; //step size in beta

  if(fReadingData){
    cout << user_normal() << "Reading reconstructed data" << endl;
    for( Int_t iStep(0); iStep < fNBetaScan; iStep++){
      BookHisto(new TH2F(Form("h_Mass3pi_vs_PpiNeg_%.2f",(fBetaInit + (double)iStep*fBStep)*1e3 ),"",140,0,70000, 70,490,497)); //[MeV]
    }

  } else {
    cout << user_normal() << "Reading my own output" << endl;
    for( Int_t iStep(0); iStep < fNBetaScan; iStep++){
      fHMass.push_back(static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("h_Mass3pi_vs_PpiNeg_%.2f",(fBetaInit + (double)iStep*fBStep)*1e3 ), true)));
    }
  }
}

void BetaComputation::DefineMCSimple(){}

void BetaComputation::StartOfRunUser(){}

void BetaComputation::StartOfBurstUser(){}

void BetaComputation::ProcessSpecialTriggerUser(int, unsigned int){}

void BetaComputation::Process(int){

  if(!fReadingData) return; //return if running in --histo mode

  TRecoSpectrometerEvent *SEvent = GetEvent<TRecoSpectrometerEvent>();

  // Call K3pi selection
  Bool_t K3piSelected = *GetOutput<Bool_t>("K3piSelection.EventSelected");
  if(!K3piSelected) return;

  Int_t K3piIndex = *GetOutput<Int_t>("K3piSelection.VertexID");

  std::vector<SpectrometerTrackVertex> Vertices =
    *GetOutput<vector<SpectrometerTrackVertex>>("SpectrometerVertexBuilder.Output");

  // K3pi vertex
  SpectrometerTrackVertex vtx = Vertices[K3piIndex];

  // Find the negative track
  fNegativeMomentum = TVector3(0,0,0);
  for (UInt_t i=0; i<3; i++) {
    Int_t trackIndex = vtx.GetTrackIndex(i);
    TRecoSpectrometerCandidate* track = static_cast<TRecoSpectrometerCandidate*>(SEvent->GetCandidate(trackIndex));
    if(track->GetCharge() == -1) fNegativeMomentum = vtx.GetTrackThreeMomentum(i);
  }

  // For each beta scan step determine the kaon mass and plot versus the
  // momentum of the negative pion
  for(int iStep(0);iStep<fNBetaScan;++iStep){

    double beta = fBetaInit + (double)iStep*fBStep;

    TVector3 p0  = vtx.GetTrackThreeMomentum(0);
    TVector3 p1  = vtx.GetTrackThreeMomentum(1);
    TVector3 p2  = vtx.GetTrackThreeMomentum(2);

    //Correct the track momentum for beta (alpha is kept 0 during the beta scan!)
    p0.SetMag( p0.Mag() * (1.0 + beta) );
    p1.SetMag( p1.Mag() * (1.0 + beta) );
    p2.SetMag( p2.Mag() * (1.0 + beta) );

    TLorentzVector p4_0, p4_1, p4_2;

    p4_0.SetXYZM(p0.X(),p0.Y(),p0.Z(), 139.57); // [MeV]
    p4_1.SetXYZM(p1.X(),p1.Y(),p1.Z(), 139.57); // [MeV]
    p4_2.SetXYZM(p2.X(),p2.Y(),p2.Z(), 139.57); // [MeV]

    Double_t Mass3pi = (p4_0 + p4_1 + p4_2).M();// [MeV]

    FillHisto(Form("h_Mass3pi_vs_PpiNeg_%.2f",beta*1e3),fNegativeMomentum.Mag(), Mass3pi ); // [MeV]

  }

}

void BetaComputation::PostProcess(){}

void BetaComputation::EndOfBurstUser(){}

void BetaComputation::EndOfRunUser(){}

void BetaComputation::EndOfJobUser(){

  if (fHMass.empty() && !fReadingData) {
    cout << user_normal() << "Cannot find my own output, ending analysis" << endl;
    return;
  }

  if ( fHMass.size() > 1 && static_cast<TH2F*>(fHMass[0])->Integral() < 500) { // if no statistics, skip
    cout << user_normal() << "Little or no statistics, analysis skipping" << endl;
    fBeta = 0.0;
    fBetaError = 0.0;
    return;
  }

  // ==========================================
  // Determine beta only for HISTO MODE
  // ==========================================
  if (!fReadingData) {

    DetermineBeta();

    // Make .dat file for run
    ofstream BetaFile;
    BetaFile.open(Form("Beta.run%06d_0000-run%06d_9999.dat",GetRunID(),GetRunID()));
    BetaFile << Form("%06d %5.2fe-3", GetRunID(), fBeta*1e3) << endl;
    BetaFile.close();

    // Print result to screen
    cout << user_normal() << Form("FinalResult %i %5.2fe-3", GetRunID(), fBeta*1e3) << endl;

    PDFReport();
  } else {
    SaveAllPlots();
  }
}

void BetaComputation::DetermineBeta(){

  double BestBeta    = 99999;
  double DeltaMKBest = 99999;

  TH1D* h_mk_best = nullptr; //Store the fitted histogram for the best beta

  TGraphErrors* gr_mk  = new TGraphErrors(fNBetaScan);
  gr_mk->SetName( "Mean_Mk_vs_beta");
  gr_mk->SetTitle(Form("Beta scan for run %i", GetRunID()));

  // Perform the scan in beta. The kaon mass peak is fitted for each
  // beta and the one with mean closest to the PDG value is chosen.
  for( Int_t iHist(0); iHist < (Int_t)fHMass.size();iHist++){

    double beta = fBetaInit + (double)iHist*fBStep; //beta for each scan step

    TH2F* h_mk_dummy = static_cast<TH2F*>(fHMass[iHist]->Clone());
    TH1D* h_mk_for_fit = static_cast<TH1D*>(h_mk_dummy->ProjectionY(Form("%s M3pi",h_mk_dummy->GetName())));

    // Fit with gaus + pol0 the kaon mass peak
    TF1* mk_fit = new TF1("fmk","gaus(0)+pol0(3)",485,505);

    // Setting initial parameters for the fit
    mk_fit->SetParameter(0,4e-2*h_mk_for_fit->Integral());
    mk_fit->SetParameter(1,MKCH); //PDG Kaon mass from NA62Constants
    mk_fit->SetParLimits(1, MKCH - 1, MKCH + 1); // parameter limits +- 1 MeV
    mk_fit->SetParameter(2,0.9); //Typical MK resolution ~ 900 KeV
    mk_fit->SetParLimits(2, 0, 3);
    mk_fit->SetParameter(3,2e-4*h_mk_for_fit->Integral());

    h_mk_for_fit->Fit(mk_fit, "+Q", "", MKCH-4.0, MKCH+4.0);

    Double_t mean     = mk_fit->GetParameter(1);
    Double_t merr     = mk_fit->GetParError(1);
    Double_t diff     = MKCH - mean;

    gr_mk->SetPoint(iHist, beta, mean);
    gr_mk->SetPointError(iHist,0,merr);

    if(fabs(diff) < DeltaMKBest){
      DeltaMKBest = fabs(diff);
      BestBeta    = beta;
      if(h_mk_best) delete h_mk_best;
      h_mk_best   = static_cast<TH1D*>(h_mk_for_fit->Clone());
    }

    delete h_mk_dummy;
    delete h_mk_for_fit;
    delete mk_fit;
  }

  // Best beta correction chosen.
  fBeta      = BestBeta;
  fBetaHisto = static_cast<TH1D*>(h_mk_best->Clone());
  fMKGraph   = gr_mk;

  if(h_mk_best) delete h_mk_best;
}

void BetaComputation::PDFReport(){

  TCanvas* PDF = new TCanvas("BetaScan", "BetaScan", 1600,900);
  PDF->SetMargin(0.13,0.03,0.1,0.08);

  TString namePDF = fAnalyzerName + ".pdf";
  gPad->SetGrid();

  //  Setting the style of the graph
  gStyle->SetTitleFont(22);
  fMKGraph->SetMarkerStyle(20);
  fMKGraph->SetMarkerSize(0.7);
  fMKGraph->SetMarkerColor(kBlue);
  fMKGraph->SetLineColor(kBlack);
  fMKGraph->SetLineWidth((Width_t)1);
  fMKGraph->GetXaxis()->SetLabelSize(.04);
  fMKGraph->GetYaxis()->SetLabelSize(.04);
  fMKGraph->GetYaxis()->SetTitleOffset(1.3);
  fMKGraph->GetXaxis()->SetTitle("#beta");
  fMKGraph->GetYaxis()->SetTitle("M^{3#pi}_{K} [MeV/c^{2}]");
  fMKGraph->GetXaxis()->SetLabelFont(22);
  fMKGraph->GetYaxis()->SetLabelFont(22);
  fMKGraph->GetXaxis()->SetTitleSize(.04);
  fMKGraph->GetYaxis()->SetTitleSize(.04);
  fMKGraph->GetXaxis()->SetTitleFont(22);
  fMKGraph->GetYaxis()->SetTitleFont(22);

  // Lines corresponding to the PDG kaon mass +- 1sigma
  TLine* l  = new TLine(-0.003,493.677,0.003,493.677);
  l->SetLineColor(kRed);
  l->SetLineWidth(2);

  TLine* lp = new TLine(-0.003,493.677+0.016,0.003,493.677+0.016);
  lp->SetLineColor(kBlack);
  lp->SetLineWidth(2);

  TLine* lm = new TLine(-0.003,493.677-0.016,0.003,493.677-0.016);
  lm->SetLineColor(kBlack);
  lm->SetLineWidth(2);

  // Vertical dashed line for the computed Beta
  TLine* betaLine  = new TLine(fBeta,493.6,fBeta,493.8);
  betaLine->SetLineColor(kRed);
  betaLine->SetLineStyle(2);
  betaLine->SetLineWidth(3);

  fMKGraph->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  betaLine->Draw("same");

  // Print the best beta on the canvas
  TString Beta   = Form("#beta = %6.2f #times 10^{-3}",fBeta/1e-3);
  TLatex  tB(0.2,0.5,Beta);
  tB.SetTextSize(0.06);
  tB.SetNDC();

  tB.DrawLatex(0.25,0.8,Beta);
  PDF->Print(namePDF+"(");
  fBetaHisto->Draw();
  PDF->Print(namePDF+")");
}

void BetaComputation::DrawPlot(){}

BetaComputation::~BetaComputation(){}
