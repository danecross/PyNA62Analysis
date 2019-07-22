//-----------------------------------------------------
// History:
//
// Created by Radoslav Marchevski (rmarchev@cern.ch)
// Date: 30/10/2018
//
/// \class AlphaComputation
/// \Brief
/// Evaluation of the spectrometer calibration constant alpha
/// \EndBrief
/// \Detailed
/// Analyser to determine the track momentum correction alpha from
/// data once the beta correction is provided by the BetaComputation
/// analyzer. K3pi decays are selected calling K3piSelection.  The
/// momentum of the spectrometer candidates is corrected using the
/// following model:
/// \code
/// Pcorrected = (1 + beta) * (1 + charge*alpha*Pinitial)*Pinitial;
/// \endcode
/// The analyzer can be run in the standard way:
/// \code
/// ./AlphaComputation -l <DataList> -p "AlphaComputation:NumberOfAlphaScanSteps=40"
/// \endcode
/// Choosing the number of steps for the alpha scan. The step is fixed to 1e-9
/// The analyzer will create a root file with a set of histograms of
/// the invariant mass of the K3pi versus the momentum of the negative
/// pion for a given range of alpha values. Beta value is read from
/// a text file produced by the previous BetaComputation step.
/// The analyzer can be used in histogram mode to determine the 'best' value
/// of alpha:
/// ./AlphaComputation -l <DataList> --histo
/// \endcode
/// For each scan value of alpha the invariant K3pi mass is fitted in
/// bins of momentum in the range 15-40 GeV/c. A chi2 is build to
/// estimate the deviation of the K3pi mass from the PDG kaon
/// mass. The alpha value for which the chi2 is lowest is chosen as
/// the alpha correction.
/// \EndDetailed

#include "AlphaComputation.hh"
#include <stdlib.h>
#include <iostream>
#include <TChain.h>
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
#include "TAxis.h"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

AlphaComputation::AlphaComputation(Core::BaseAnalysis *ba) : Analyzer(ba, "AlphaComputation"), fReadingData(false), fNAlphaScan(0), fBeta(0.), fAlpha(0.), fAStep(0.), fAlphaInit(0.), fMKGraph(nullptr), fChi2Graph(nullptr){

  RequestTree("Spectrometer",new TRecoSpectrometerEvent);

  AddParam("NumberOfAlphaScanSteps",&fNAlphaScan, 100); //number of alpha scan steps
}

void AlphaComputation::InitOutput(){}

void AlphaComputation::InitHist(){

  // The GetIsTree() function checks if the user is running with
  // reconstructed data or in histogram mode with the user`s own input
  fReadingData = GetIsTree();

  fBeta         = 0;
  fAlpha        = 0;
  fAlphaInit    = -50.0E-9; //initial alpha value for the alpha scan
  fAStep        = 1.0E-9; //step size in alpha

  fHMass.clear();
  fHMass_vs_P.clear();

  if(fReadingData){
    cout << user_normal() << "Reading reconstructed data" << endl;
    for( Int_t iStep(0); iStep < fNAlphaScan; iStep++){
      BookHisto(new TH2F(Form("h_Mass3pi_vs_PpiNeg_%.1f",(fAlphaInit + (double)iStep*fAStep)*1e8 ),"",140,0,70000, 70,490,497)); //[MeV]
    }
  } else {
    cout << user_normal() << "Reading my own output" << endl;
    for( Int_t iStep(0); iStep < fNAlphaScan; iStep++){
      fHMass.push_back(static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("h_Mass3pi_vs_PpiNeg_%.1f",(fAlphaInit + (double)iStep*fAStep)*1e8), true)));
    }
  }
}

void AlphaComputation::DefineMCSimple(){}

void AlphaComputation::StartOfRunUser(){

  Bool_t ParametersFound = false;
  TString BetaFileName = "Beta.dat";

  // Read the beta correction already determined by the BetaComputation step
  if (NA62ConditionsService::GetInstance()->Open(BetaFileName)==kSuccess) {
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(BetaFileName))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      Int_t    Run = ((TObjString*)(l->At(0)))->GetString().Atoi();
      Double_t b   = ((TObjString*)(l->At(1)))->GetString().Atof();
      delete l;
      if (Run!=GetRunID()) continue; // wrong run number
      fBeta  = b; // dimensionless
      ParametersFound = true;
      cout << user_normal() << " Run = " << Run << " Beta = " << b << "\n";
    }
    NA62ConditionsService::GetInstance()->Close(BetaFileName);
  }

  if(!ParametersFound)
    cout << user_normal() << "BetaComputation file needed for the procedure not found!" << endl;
}

void AlphaComputation::StartOfBurstUser(){}

void AlphaComputation::ProcessSpecialTriggerUser(int, unsigned int){}

void AlphaComputation::Process(int){

  if(!fReadingData) return; //return if running in --histo mode

  TRecoSpectrometerEvent *SEvent = GetEvent<TRecoSpectrometerEvent>();

  // Call K3pi selection
  Bool_t K3piSelected = *(Bool_t*)GetOutput("K3piSelection.EventSelected");
  if(!K3piSelected) return;

  Int_t K3piIndex = *(Int_t*)GetOutput("K3piSelection.VertexID");
  if(K3piIndex < 0) return;

  std::vector<SpectrometerTrackVertex> Vertices =
    *(vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");

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
  for(int iStep(0);iStep<fNAlphaScan;++iStep){

    double alpha = fAlphaInit + (double)iStep*fAStep;

    TVector3 p0  = vtx.GetTrackThreeMomentum(0);
    TVector3 p1  = vtx.GetTrackThreeMomentum(1);
    TVector3 p2  = vtx.GetTrackThreeMomentum(2);

    Double_t q0  = vtx.GetTrackCharge(0);
    Double_t q1  = vtx.GetTrackCharge(1);
    Double_t q2  = vtx.GetTrackCharge(2);

    //Correct the track momentum for alpha, beta (beta is taken from the output file of the
    //BetaComputation analyzer!)
    p0.SetMag( p0.Mag() * (1.0 + fBeta) * (1.0 + q0 * alpha * p0.Mag()) );
    p1.SetMag( p1.Mag() * (1.0 + fBeta) * (1.0 + q1 * alpha * p1.Mag()) );
    p2.SetMag( p2.Mag() * (1.0 + fBeta) * (1.0 + q2 * alpha * p2.Mag()) );

    TLorentzVector p4_0, p4_1, p4_2;
    p4_0.SetXYZM(p0.X(),p0.Y(),p0.Z(), 139.57); // [MeV]
    p4_1.SetXYZM(p1.X(),p1.Y(),p1.Z(), 139.57); // [MeV]
    p4_2.SetXYZM(p2.X(),p2.Y(),p2.Z(), 139.57); // [MeV]

    Double_t Mass3pi = (p4_0 + p4_1 + p4_2).M();// [MeV]

    FillHisto(Form("h_Mass3pi_vs_PpiNeg_%.1f",alpha*1e8), fNegativeMomentum.Mag(), Mass3pi ); // [MeV]
  }
}

void AlphaComputation::PostProcess(){}

void AlphaComputation::EndOfBurstUser(){}

void AlphaComputation::EndOfRunUser(){}

void AlphaComputation::EndOfJobUser(){

  if (fHMass.empty() && !fReadingData) {
    cout << user_normal() << "Cannot find my own output, ending analysis" << endl;
    return;
  }

  if ( fHMass.size() > 1 && static_cast<TH2F*>(fHMass[0])->Integral() < 500) { // if no statistics, skip
    cout << user_normal() << "Little or no statistics, analysis skipping" << endl;
    fAlpha  = 0.0;
    return;
  }

  // ==========================================
  // Determine alpha only for HISTO MODE
  // ==========================================
  if (!fReadingData) {

    DetermineAlpha();

    // Make .dat file for run
    ofstream AlphaFile;
    AlphaFile.open(Form("AlphaBeta.run%06d_0000-run%06d_9999.dat",GetRunID(),GetRunID()));
    AlphaFile << Form("%06d %6.1fe-8 %5.2fe-3", GetRunID(), fAlpha*1e8, fBeta*1e3) << endl;
    AlphaFile.close();

    // Print result to screen
    cout << user_normal() << Form("FinalResult %i %6.2fe-8 %5.2fe-3", GetRunID(), fAlpha*1e8, fBeta*1e3) << endl;

    PDFReport();
  } else {
    SaveAllPlots();
  }
}

void AlphaComputation::DetermineAlpha(){

  if (fReadingData) return;//skip if reading data

  double BestAlpha  = 99999;
  double BestChi2   = 99999;
  int    BestHist   = 99999;

  //====================================================================
  // Perform the scan in alpha to determine the best value. For each
  // value of alpha the procedure goes as follows:
  //1) The kaon mass peak is fitted in bins of momentum
  //2) A chi2 is computed using the resulting <M_K> vs Ppi- distribution
  //3) The alpha correction for which the chi2 is the lowest is selected
  //====================================================================

  Double_t LowP        = 15000; //MeV
  Double_t HighP       = 40000; //MeV
  Double_t BinSize     = 1000;  //MeV

  Int_t NBins = (Int_t)(HighP - LowP)/BinSize;

  std::vector<Double_t> Momentum;
  for (int j(0);j<NBins;j++){
    Momentum.push_back(LowP + (double)j*BinSize);
  }

  TGraphErrors* gr_chi2  = new TGraphErrors(fNAlphaScan);
  gr_chi2->SetName( "Mean_Mk_vs_alpha");
  gr_chi2->SetTitle(Form("#chi^{2} of the #alpha scan for run %i", GetRunID()));

  for( Int_t iHist(0); iHist < (Int_t)fHMass.size();iHist++){

    Double_t alpha = fAlphaInit + (Double_t)iHist*fAStep; //alpha for each scan step
    Double_t chi2  = 0;

    //  Creating one graph for each alpha hypothesis that will
    //  contain the <M_K> and Ppi-
    TGraphErrors* gr  = new TGraphErrors(NBins);
    gr->SetName( Form("Mean M_{K}(P_{#pi^{-}}) for #alpha = %0.1fe-8",alpha*1e8));
    gr->SetTitle( Form("Run %i : Mean M_{K}(P_{#pi^{-}}) for #alpha = %0.1fe-8", GetRunID(), alpha*1e8));

    fHMass_vs_P.push_back(gr);

    //  Computing the chi2
    for( Int_t iP(0); iP < (Int_t)Momentum.size();iP++){

      TH2F* h_mk_dummy     = static_cast<TH2F*>(fHMass[iHist]->Clone());
      Double_t LowBinEdge  = h_mk_dummy->GetXaxis()->FindBin(Momentum[iP]);
      Double_t HighBinEdge = h_mk_dummy->GetXaxis()->FindBin(Momentum[iP] + BinSize);

      TH1D* h_mk_for_fit = (TH1D*)h_mk_dummy->ProjectionY(Form("%s slice : %.1f - %.1f",h_mk_dummy->GetName(), Momentum[iP], Momentum[iP] + BinSize),LowBinEdge, HighBinEdge);

      // Fit with gaus + pol0 the kaon mass peak
      TF1* mk_fit = new TF1("fmk", "gaus(0)+pol0(3)", MKCH-4.0, MKCH+4.0);

      // Setting initial parameters for the fit
      mk_fit->SetParameter(0,4e-2*h_mk_for_fit->Integral());
      mk_fit->SetParameter(1,MKCH); //PDG Kaon mass from NA62Constants
      mk_fit->SetParLimits(1, MKCH - 1, MKCH + 1); // parameter limits +- 1 MeV
      mk_fit->SetParameter(2,0.9); //Typical MK resolution ~ 900 KeV
      mk_fit->SetParLimits(2, 0, 3);
      mk_fit->SetParameter(3,2e-4*h_mk_for_fit->Integral());

      h_mk_for_fit->Fit(mk_fit, "+Q", "", MKCH-1.0, MKCH+1.0);

      Double_t mean     = mk_fit->GetParameter(1);
      Double_t merr     = mk_fit->GetParError(1);

      fHMass_vs_P[iHist]->SetPoint(iP, Momentum[iP] + BinSize/2, mean);
      fHMass_vs_P[iHist]->SetPointError(iP,0,merr);

      chi2 += TMath::Power( (mean - MKCH)/merr, 2);

      delete h_mk_dummy;
      delete h_mk_for_fit;
      delete mk_fit;
    }

    Double_t NDF = (Double_t)Momentum.size();
    Double_t ReducedChi2 = chi2/NDF;

    gr_chi2->SetPoint(iHist, alpha, ReducedChi2);

    // ================================
    //  Minimize the chi2
    // ================================
    if(chi2 < BestChi2){
      BestChi2  = chi2;
      BestAlpha = alpha;
      BestHist  = iHist;
    }
  }

  // ================================
  // Best alpha correction chosen.
  // ================================
  fAlpha     = BestAlpha;
  fMKGraph   = fHMass_vs_P[BestHist];
  fChi2Graph = gr_chi2;
}

void AlphaComputation::PDFReport(){

  TCanvas* PDF = new TCanvas("AlphaScan", "AlphaScan", 1600,900);
  PDF->SetMargin(0.13,0.05,0.1,0.08);

  TString namePDF = fAnalyzerName + ".pdf";
  gPad->SetGrid();

  //  Setting the style of the graph
  gStyle->SetTitleFont(22);
  fMKGraph= static_cast<TGraphErrors*>(TGraphAxisStyle(static_cast<TGraph*>(fMKGraph), "#alpha","M_{K} [MeV/c^{2}]", kBlue, kBlack));
  fMKGraph->GetYaxis()->SetRangeUser(493,494.7);
  // Lines corresponding to the PDG kaon mass +- 1sigma
  TLine* l  = new TLine(15000, MKCH, 40000, MKCH);
  l->SetLineColor(kRed);
  l->SetLineWidth(2);

  TLine* lp = new TLine(15000, MKCH + 0.016, 40000, MKCH + 0.016);
  lp->SetLineColor(kBlack);
  lp->SetLineWidth(2);

  TLine* lm = new TLine(15000, MKCH - 0.016, 40000, MKCH - 0.016);
  lm->SetLineColor(kBlack);
  lm->SetLineWidth(2);

  fMKGraph->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  fMKGraph->Draw("P same");

  // Print the best alpha/beta on the canvas
  TString Alpha   = Form("#alpha = %6.2f #times 10^{-8}",fAlpha*1e8);
  TString Beta    = Form("#beta  = %6.2f #times 10^{-3}",fBeta*1e3 );

  TLatex  tA(0.2,0.5,Alpha);
  tA.SetTextSize(0.06);
  tA.SetNDC();

  tA.DrawLatex(0.25,0.8,Alpha);

  TLatex  tB(0.2,0.5,Beta);
  tB.SetTextSize(0.06);
  tB.SetNDC();

  tB.DrawLatex(0.25,0.7,Beta);

  PDF->Print(namePDF+"(");

  fChi2Graph = static_cast<TGraphErrors*>(TGraphAxisStyle(static_cast<TGraph*>(fChi2Graph), "#alpha","#chi^{2}", kBlue, kBlack));

  fChi2Graph->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  PDF->Print(namePDF);

  // Plot the outcome of the scan for 4 different values of alpha
  // spaced equally between the lowest and highest alpha values
  Int_t plotIndex = fNAlphaScan/3;

  PDF->Clear();
  PDF->Divide(2,2);

  // =======================================================================
  // Top-left plot: Fitted MK vs P pi- for the first alpha value of the scan
  // =======================================================================
  PDF->cd(1);
  Alpha   = Form("#alpha = %6.2f #times 10^{-8}",fAlphaInit*1e8);
  fHMass_vs_P[0] = static_cast<TGraphErrors*>(TGraphAxisStyle(static_cast<TGraph*>(fHMass_vs_P[0]), "#alpha","M_{K} [MeV/c^{2}]", kBlue, kBlack));
  fHMass_vs_P[0]->GetYaxis()->SetRangeUser(493,494.7);

  fHMass_vs_P[0]->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  fHMass_vs_P[0]->Draw("P same");

  tA.DrawLatex(0.25,0.8,Alpha);
  tB.DrawLatex(0.25,0.7,Beta);

  // ===================================
  // Top-right plot: Fitted MK vs P pi-
  // ===================================
  PDF->cd(2);
  Alpha   = Form("#alpha = %6.2f #times 10^{-8}",(fAlphaInit + (double)plotIndex*fAStep)*1e8);

  fHMass_vs_P[plotIndex] = static_cast<TGraphErrors*>(TGraphAxisStyle(static_cast<TGraph*>(fHMass_vs_P[plotIndex]), "#alpha","M_{K} [MeV/c^{2}]", kBlue, kBlack));
  fHMass_vs_P[plotIndex]->GetYaxis()->SetRangeUser(493,494.7);

  fHMass_vs_P[plotIndex]->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  fHMass_vs_P[plotIndex]->Draw("P same");
  tA.DrawLatex(0.25,0.8,Alpha);
  tB.DrawLatex(0.25,0.7,Beta);

  // ===================================
  // Bottom-left plot: Fitted MK vs P pi-
  // ===================================
  PDF->cd(3);
  Alpha   = Form("#alpha = %6.2f #times 10^{-8}",(fAlphaInit + (double)plotIndex*2*fAStep)*1e8);
  fHMass_vs_P[plotIndex*2] = static_cast<TGraphErrors*>(TGraphAxisStyle(static_cast<TGraph*>(fHMass_vs_P[plotIndex*2]), "#alpha","M_{K} [MeV/c^{2}]", kBlue, kBlack));
  fHMass_vs_P[plotIndex*2]->GetYaxis()->SetRangeUser(493,494.7);

  fHMass_vs_P[plotIndex*2]->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  fHMass_vs_P[plotIndex*2]->Draw("P same");
  tA.DrawLatex(0.25,0.8,Alpha);
  tB.DrawLatex(0.25,0.7,Beta);

  // ===========================================================================
  // Bottom-right plot: Fitted MK vs P pi- for the last alpha value of the scan
  // ===========================================================================
  PDF->cd(4);
  Alpha   = Form("#alpha = %6.2f #times 10^{-8}",(fAlphaInit + (double)plotIndex *3*fAStep)*1e8);
  fHMass_vs_P[plotIndex *3] = static_cast<TGraphErrors*>(TGraphAxisStyle(static_cast<TGraph*>(fHMass_vs_P[plotIndex *3]), "#alpha","M_{K} [MeV/c^{2}]", kBlue, kBlack));
  fHMass_vs_P[plotIndex*3]->GetYaxis()->SetRangeUser(493,494.7);

  fHMass_vs_P[plotIndex *3]->Draw("AP");
  l->Draw("same");
  lp->Draw("same");
  lm->Draw("same");
  fHMass_vs_P[plotIndex *3]->Draw("P same");
  tA.DrawLatex(0.25,0.8,Alpha);
  tB.DrawLatex(0.25,0.7,Beta);
  PDF->Print(namePDF+")");
}

TGraph* AlphaComputation::TGraphAxisStyle(TGraph* g, const TString &tx, const TString &ty,const Int_t &mcol, const Int_t &lcol){

  // Line and marker styles
  g->SetMarkerStyle(20);
  g->SetMarkerSize(1.);
  g->SetMarkerColor(mcol);
  g->SetLineColor(lcol);
  g->SetLineWidth((Width_t)2);

  // X axis style
  g->GetXaxis()->SetLabelSize(.04);
  g->GetXaxis()->SetLabelFont(22);
  g->GetXaxis()->SetTitle(Form("%s",tx.Data()));
  g->GetXaxis()->SetTitleSize(.04);
  g->GetXaxis()->SetTitleFont(22);

  // Y axis style
  g->GetYaxis()->SetLabelSize(.04);
  g->GetYaxis()->SetTitleOffset(1.2);
  g->GetYaxis()->SetLabelFont(22);
  g->GetYaxis()->SetTitle(Form("%s",ty.Data()));
  g->GetYaxis()->SetTitleSize(.04);
  g->GetYaxis()->SetTitleFont(22);

  return g;
}

void AlphaComputation::DrawPlot(){}

AlphaComputation::~AlphaComputation(){}
