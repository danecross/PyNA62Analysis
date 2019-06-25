//-----------------------------------------------------
// History:
//
// Created by Andrew Sturgess (axs@hep.ph.bham.ac.uk)
// Date: 15/03/2016
//
// Updated: 17/11/2016 to remove CHOD/CHANTI/Cedar;
//          a/b now determined using only Spectrometer
//
// Updated: 11/01/2017 to use K3piSelection instead of
//          internal selection. Simplified code to a
//          standard two stage analyzer ideal for
//          reprocessing (one run at a time)
//
// Updated: 19/10/2017 to add a more detailed monitoring
//          pdf and an optimised alpha, beta 2D Fit
//-----------------------------------------------------

/// \class AlphaBetaComputation
/// \Brief
/// Evaluate the spectrometer calibration constants (alpha, beta)
/// \EndBrief
/// \Detailed
/// Analyser to determine the track momentum corrections (alpha and beta) from data.
/// K3pi decays are selected calling K3piSelection.
/// The momentum of the spectrometer candidates is corrected using the following model:
/// \code
/// Pcorrected = (1 + beta) * (1 + charge*alpha*Pinitial)*Pinitial;
/// \endcode
/// To determine the 'best' values of alpha and beta, a 2D scan of alpha and beta is used. For
/// every pair of alpha and beta values, the invariant mass of the three tracks is determined.
/// The distributions are divided into 18 bins in the momentum of the negative pion
/// in the range 9-45 GeV/c. Each distribution is fitted with a gaussian and the mean values in each
/// bin are compared to the PDG kaon mass by using the chi-squared test statistic. A 2D distribution of
/// alpha and beta against chi-square is created, and is then fitted with an elliptical paraboloid.
/// The minimum of the fit is then the ideal alpha/beta pair.
/// <br><br>
/// The analyser can be used in two modes. In the usual mode:
/// \code
/// ./AlphaBetaComputation -l <DataList> -p "SpectrometerTrackCorrections:ExternalAlpha=0;ExternalBeta=0"
/// \endcode
/// the analyser will create a root file with a 2D histogram containing all of the invariant
/// mass calculations from the three-pions in the data, plus the best alpha and beta values for those
/// files. Note that the (alpha, beta) corrections are switched off in the above call.
/// An alternative is NOT to include the SpectrometerTrackCorrections pre-analyzer
/// in the config file used for the compilation of AlphaBetaComputation.
/// The analyser can then be used in histogram mode:
/// \code
/// ./AlphaBetaComputation -l <DataModeOutputList> --histo
/// \endcode
/// where it should be passed a root-file (or list of root-files) from the data mode output.
/// Histogram mode is designed to analyse output from a list corresponding to one individual run
/// at a time, where it will determine the overall alpha/beta by adding the histogram from each subjob.
/// Histo mode will then produce a PDF containing the alpha/beta vs Chi2 fit, a mass-vs-momentum plot
/// (lowest Chi2) to give a rough visual of the correction, plus the stability of alpha/beta vs BurstID.
/// \EndDetailed

#include "AlphaBetaComputation.hh"
#include "BaseAnalysis.hh"
#include "SpectrometerVertexBuilder.hh"
#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include <TGraphErrors.h>
#include <TBox.h>
#include <fstream>
#include <iomanip>
#include <TMultiGraph.h>
#include <TLatex.h>
#include <TLegend.h>
#include <TMarker.h>
#include <THStack.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

AlphaBetaComputation::AlphaBetaComputation(Core::BaseAnalysis *ba) : Analyzer(ba, "AlphaBetaComputation") {
  gErrorIgnoreLevel = 5000; // suppress messages generated
  RequestTree("Spectrometer", new TRecoSpectrometerEvent);

  //! Tree will be read by histo-mode to produce stability plots
  OpenNewTree("DataModeAlphaBeta","DataModeAlphaBeta");
  AddBranch("DataModeAlphaBeta","RunID",&fRunID);
  AddBranch("DataModeAlphaBeta","BurstID",&fBurstID);
  AddBranch("DataModeAlphaBeta","NK3PI",&fNumberK3PI);
  AddBranch("DataModeAlphaBeta","Alpha",&fAlpha);
  AddBranch("DataModeAlphaBeta","Beta", &fBeta);
  AddBranch("DataModeAlphaBeta","AlphaErr",&fAlphaError);
  AddBranch("DataModeAlphaBeta","BetaErr",&fBetaError);
}

void AlphaBetaComputation::InitHist() {
  //!========================================================================
  //! Set the alpha/beta range, step sizes and number of bins in this function.
  //! The GetIsTree() function is used to tell if reading data or histograms
  //! the default histogram (per subjob) is created, or requested from
  //! the input root file for histogram mode

  fReadingData = GetIsTree();
  fNumberK3PI  = 0;
  fRunID       = 0;
  fBurstID     = 0;

  fNDF  = 0;       //number of points used in fit.
  fSF   = 10000.0; //scale factor for alpha to make 2D fit work first time
  fAMax = 2.0E-7;  //max alpha
  fBMax = 6.0E-3;  //max beta
  fAMin = -fAMax;  //min alpha
  fBMin = -fBMax;  //min beta

  fAStep = 4.0E-9; //step size in alpha
  fBStep = 2.0E-4; //step size in beta

  fABins  = 1.0 + (2.0*fAMax)/fAStep;   //number of alpha bins
  fBBins  = 1.0 + (2.0*fBMax)/fBStep;   //number of beta bins
  fMBins  = 18;                         //number of negative pion momentum bins
  fTBins  = fABins*fBBins*fMBins;       //total number for a 3D index
  fIMBins = 28;                         //number of bins in invariant mass spectra

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;
    BookHisto(new TH2I("hM3pi","Inv mass vs #alpha,#beta and P(#pi^{-}) index;mass(MeV);index",fIMBins,490,497,fTBins,0,fTBins));
  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHMass = static_cast<TH2I*>(RequestHistogram(fAnalyzerName, "hM3pi", true));
  }
}

void AlphaBetaComputation::Process(Int_t) {
  //!========================================================================
  //! HISTO MODE
  //!========================================================================
  //! In histo-mode, for each input file, read the tree containing alpha/beta
  //! and store information in a struct: used to make stability plots.

  if (!fReadingData) {
    gErrorIgnoreLevel = 5000; // suppress messages generated  
    //!Firstly, check the file is okay to open to avoid crashes.
    std::vector<TString> fileProtection = fParent->GetIOHandler()->GetListOfHistos(fAnalyzerName);
    if(!fileProtection.size()){
      cout << user_normal() << "Cannot find my own output for this file, skipping" << endl;
      return;
    }
    ReadDataModeTree();
    return;
  }

  //!=====================================================================================
  //! DATA MODE
  //!=====================================================================================
  //! If in data mode, determine invariant mass from spectrometer candidates.
  //! Only use events selected by the K3piSelection and use vertexing tool.

  if (!fRunID && !fBurstID) { //If NULL, set values once.
    fRunID   = GetRunID();
    fBurstID = GetBurstID();
  }

  //!Call K3pi selection
  Bool_t K3piSelected = *GetOutput<Bool_t>("K3piSelection.EventSelected");
  if(!K3piSelected) return;
  fNumberK3PI++;

  Int_t K3piIndex = *GetOutput<Int_t>("K3piSelection.VertexID");
  TRecoSpectrometerEvent* SpecEvent = GetEvent<TRecoSpectrometerEvent>();
  std::vector<SpectrometerTrackVertex> Vertices =
    *GetOutput<vector<SpectrometerTrackVertex>>("SpectrometerVertexBuilder.Output");

  SpectrometerTrackVertex vtx = Vertices[K3piIndex];
  std::vector<TRecoSpectrometerCandidate*> threeTracks;
  std::vector<TVector3> trackMomentum;

  for (UInt_t i=0; i<3; i++) {
    Int_t trackIndex = vtx.GetTrackIndex(i);
    threeTracks.push_back(static_cast<TRecoSpectrometerCandidate*>(SpecEvent->GetCandidate(trackIndex)));
    trackMomentum.push_back(vtx.GetTrackThreeMomentum(i));
  }

  //!=================================================================================================
  //! The alpha beta scan is performed for any event passing the selection.
  //! Scan alpha in first loop, then scan beta inside. Whilst in this loop, grab the track momentum
  //! set previously and apply the correction. Using the correction, determine the invariant mass.
  //! The invariant mass is filled into hInvMass depending on the negative track momentum. The Y-Axis
  //! of the histogram contains (a,b,P) information!

  for(Int_t a = 0; a < fABins; a++){
    Double_t alpha = fAMin + double(a)*fAStep;
    for(Int_t b = 0; b < fBBins; b++){
      Double_t beta = fBMin + double(b)*fBStep;
      Double_t NTM(0.);        //!To store the negative track momentum
      TVector3 TrackArray[3];  //!To correct the momentum of the tracks
      for (UInt_t t = 0; t < threeTracks.size(); t++){
	TVector3 Pu = trackMomentum[t];
	Double_t q  = threeTracks[t]->GetCharge();
	TrackArray[t].SetX(Pu.X()*(1.0 + beta)*(1.0 + alpha*q*Pu.X()));
	TrackArray[t].SetY(Pu.Y()*(1.0 + beta)*(1.0 + alpha*q*Pu.Y()));
	TrackArray[t].SetZ(Pu.Z()*(1.0 + beta)*(1.0 + alpha*q*Pu.Z()));
	if (q==-1) NTM = TrackArray[t].Mag()/1000.0;
      }

      vector<TLorentzVector> Pions;
      for(UInt_t t = 0; t < threeTracks.size(); t++) Pions.push_back(getParticle(TrackArray[t],1));
      TLorentzVector InvariantMass = threeParticle(Pions[0],Pions[1],Pions[2]);
      Double_t M3PI = InvariantMass.M()*1000;
      for(Int_t k = 0; k < fMBins; k++){
	Int_t Index3D = (a*fBBins*fMBins) + (b*fMBins) + k;
	Double_t MomLow  = 9.0  + double(k)*(2.0);
	Double_t MomHigh = 11.0 + double(k)*(2.0);
	if(NTM >= MomLow && NTM < MomHigh) FillHisto("hM3pi",M3PI,Index3D);
      }
    }
  }
}

void AlphaBetaComputation::EndOfJobUser() {
  //! At the end of the job, call function that determines the best alpha and beta in both data and
  //! histogram mode. This then prints the result to screen. If in histogram mode, there is a further
  //! step: we make the stability plots used in the PDF report, which is also produced.
 
  if (!fHMass && !fReadingData) {
    cout << user_normal() << "Cannot find my own output, ending analysis" << endl;
    return;
  }

  determineBestAlphaBetaPerJob();
  if (!fReadingData) {
    //!Make .dat file for run
    ofstream AlphaBetaFile;
    AlphaBetaFile.open(Form("AlphaBeta.run%06d_0000-run%06d_9999.dat",GetRunID(),GetRunID()));
    AlphaBetaFile << Form("%06d %6.2fe-8 %5.2fe-3", GetRunID(), fAlpha*1e8, fBeta*1e3) << endl;
    AlphaBetaFile.close();
 
    //!Or print result to screen
    cout << user_normal() << Form("FinalResult %i %6.2fe-8 %5.2fe-3", GetRunID(), fAlpha*1e8, fBeta*1e3) << endl;
    saveBurstStabilityPlots();
    makePDFReport();
  }
  else {
    SaveAllPlots();
  }
}

TLorentzVector AlphaBetaComputation::getParticle(TVector3 threemomentum, Int_t part) {
  double mass[4] = {0.493667, 0.139570, 0.10565837, 0.000511};
  threemomentum *= (1./1000.);
  TLorentzVector particle;
  particle.SetXYZM(threemomentum.X(), threemomentum.Y(), threemomentum.Z(), mass[part]);
  return particle;
}

TLorentzVector AlphaBetaComputation::threeParticle(TLorentzVector part1, TLorentzVector part2, TLorentzVector part3) {
  TLorentzVector partA = part1 + part2;
  TLorentzVector output = partA + part3;
  return output;
}

Double_t AlphaBetaComputation::fit2DError(TF2* fit,Int_t opt) {
  //Need to divide by NDF
  Double_t MinX, MinY;
  fit->GetMinimumXY(MinX,MinY);
  MinX *= 1/fSF;

  Double_t p1 = (fit->GetParameter(1))*(fSF);
  Double_t p2 = (fit->GetParameter(2));
  Double_t p3 = (fit->GetParameter(3))*(fSF*fSF);
  Double_t p4 = (fit->GetParameter(4));
  Double_t p5 = (fit->GetParameter(5))*(fSF);
  Double_t a(0),b(0),c(-2.3/fNDF);  //2.3(1.0) for 2D(1D) chi2 distribution corresponds to 1 sigma
                                    //Divide by fNDF for correct errors as we fit chi2/ndf instead
  if(opt == 0){
    a = p3;
    b = p1 + 2.0*p3*MinX + p5*MinY;
  }
  else if(opt == 1){
    a = p4;
    b = p2 + 2.0*p4*MinY + p5*MinX;
  }
  else return 0.0;

  Double_t Result = (-b + sqrt(b*b - 4.0*a*c))/(2.0*a);
  return Result;
}

void AlphaBetaComputation::determineBestAlphaBetaPerJob() {
  //!=============================================================================================================
  //! This function determines alpha and beta.
  //! We open an alpha/beta/momentum bin loop, and extract the info from hInvMass to create slices; each slice
  //! corresponding to an a/b/p combination; build a chisquare for the eighteen p combinations for each a/b pair
  //! and store the info in a 2D chisquare plot. We then fit this plot using a 2D Polynomial;
  //! the minimum of this plot is the best/alpha beta
  //!
  //! Also save mass/vs/momentum for each a/b pair to find the best (only lowest chisquare though...)
  //! Also save the 2D grid with the fit information; these two plots will be used in the PDF report

  Double_t fNBC[18] = {10.0,12.0,14.0,16.0,18.0,20.0,22.0,24.0,26.0,28.0,30.0,32.0,34.0,36.0,38.0,40.0,42.0,44.0};

  TH2I* hInvMass;
  if(!fReadingData) hInvMass = fHMass;
  else              hInvMass = static_cast<TH2I*>(fHisto.GetTH2("hM3pi"));

  TH2F* hChiGrid = new TH2F("hCHISQ","hCHISQ;#alpha;#beta",
			    fABins,(fAMin-0.5*fAStep)*fSF,(fAMax+0.5*fAStep)*fSF,
			    fBBins,(fBMin-0.5*fBStep),(fBMax+0.5*fBStep));
  
  TH2F* hNDF     = new TH2F("hNDF","hNDF;#alpha;#beta",
                            fABins,(fAMin-0.5*fAStep)*fSF,(fAMax+0.5*fAStep)*fSF,
                            fBBins,(fBMin-0.5*fBStep),(fBMax+0.5*fBStep));

  if (fNumberK3PI < 500) { // if no statistics, skip
    cout << user_normal() << "Little or no statistics, analysis skipping" << endl;
    fAlpha = 0.0;
    fBeta  = 0.0;
    fAlphaError = 0.0;
    fBetaError  = 0.0;
    return;
  }

  //!======================================================================
  //! save each test alpha and beta, with the value of the chisquared
  //! also save each mass vs momentum TGraph, just for PDF purposes.

  std::vector<Double_t> tAlpha, tBeta, tChi2;
  std::vector<TGraphErrors*> tMassVsMomentum;

  gErrorIgnoreLevel = 5000; // suppress messages generated   
  for(Int_t a = 0; a < fABins; a++){
    Double_t alpha = fAMin + double(a)*fAStep;
    for(Int_t b = 0; b < fBBins; b++){
      Double_t beta = fBMin + double(b)*fBStep;
      Double_t CHI2(0.),NDF(0);
      TGraphErrors* gr1 = new TGraphErrors();
      for(Int_t k = 0; k < fMBins; k++){
	Int_t Index3D = (a*fBBins*fMBins) + (b*fMBins) + k;
	TH1F* mSpectrum = new TH1F(Form("mHa%db%dk%d",a,b,k),"",fIMBins,490,497);
	for(Int_t i = 0; i < fIMBins; i++){
	  Double_t BinContent = hInvMass->GetBinContent(i+1,Index3D + 1);
	  mSpectrum->SetBinContent(i+1,BinContent);
	}       
	Int_t MaxBinLoc = mSpectrum->GetMaximumBin();           //Find maximum bin
	Double_t BinCen = mSpectrum->GetBinCenter(MaxBinLoc);   //get the location in X
	Double_t rgMin  = BinCen - 3.6;                         //define a range +/- 4sigma from peak
        Double_t rgMax  = BinCen + 3.6;                         //sigma roughly 0.9 MeV     

	TF1* massfit = new TF1("f1", "gaus", rgMin, rgMax);
	mSpectrum->Fit(massfit,"QR");

	Double_t GausCHI2 = massfit->GetChisquare();
	Double_t GausNDOF = massfit->GetNDF();
	Double_t mean  = massfit->GetParameter(1);
	Double_t merr  = massfit->GetParError(1);
	Double_t diff  = MKCH - mean;
	Double_t lChi2 = (diff*diff)/(merr*merr);  //Local Chi2 calculation

	gr1->SetPoint(k,fNBC[k],mean);
	gr1->SetPointError(k,0,merr);

	//Skip last bin due to low contents.
	if((mean != 0) && (merr != 0) && (mean >= 490) && (mean <= 497)){
	  if(GausCHI2 <= 0.01 || GausNDOF <= 0) continue;
	  if(k == fMBins -1)                    continue;
	  CHI2 += lChi2; 
	  NDF  += 1.0;
	} 
	delete mSpectrum;
	delete massfit;	
      }
      hChiGrid->SetBinContent(a+1,b+1,CHI2);
      hNDF->SetBinContent(a+1,b+1,NDF);
      fNDF += NDF; 
      tAlpha.push_back(alpha);
      tBeta.push_back(beta);
      tChi2.push_back(CHI2);
      tMassVsMomentum.push_back(gr1);
    }
  }

  //!=============================================================================
  //! This is the 2D Polynomial fit to the (a,b) vs Chi^2 plot. Firstly, we remove
  //! bins with very large Chi^2 values (>50 times the min)to ensure that the QWR
  //! fit performs well at the towards the (a,b) minimum.
  //! We have to use a scale-factor fSF to multiply the alpha terms; this ensures
  //! that the fit works first time (due to very different alpha/beta magnitudes);

  hChiGrid->Divide(hNDF); //Divide the chi2 grid by the NDF
  TH2F* hChiGridClone   = (TH2F*)hChiGrid->Clone(); //for monitoring plots.

  //! Improve fit performace by removing bins with large #chi2
  Double_t MinValue     = hChiGrid->GetBinContent(hChiGrid->GetMinimumBin());
  for(Int_t a = 0; a < fABins; a++){
    for(Int_t b = 0; b < fBBins; b++){
      Double_t BinCont        = hChiGrid->GetBinContent(a+1,b+1);
      if(BinCont > 50*MinValue) hChiGrid->SetBinContent(a+1,b+1,0);
    }
  }

  //!Perform the 2D polynomial fit. Need fSF scale factor so fit works first time
  //!due to the very different magnitudes between (a,b)
  TF2* fit2D = new TF2("Fit2D","[0] + [1]*x + [2]*y + [3]*x*x + [4]*y*y + [5]*x*y",fAMin*fSF,fAMax*fSF,fBMin,fBMax);
  hChiGrid->Fit(fit2D,"QWR");

  fit2D->GetMinimumXY(fAlpha,fBeta);
  fAlpha *= 1.0/fSF;
  fChiSq  = fit2D->GetChisquare();
  fChiSq *= 1.0/fit2D->GetNDF();
  fNDF   *= 1.0/(fABins*fBBins);

  fAlphaError = fit2DError(fit2D,0);
  fBetaError  = fit2DError(fit2D,1);
  if (fReadingData) FillTree("DataModeAlphaBeta");

  //!============================================================================
  //! Plot management. Below we save some useful plots for the PDF report.
  //! Save Mass vs Momentum plots; 
  //! Save (a,b) vs Chi^2 full grid.    

  Int_t iDefault(-1),iAlphaZero(-1),iBetaZero(-1), iMinimum(-1);
  for(Int_t a = 0; a < fABins; a++){
    Double_t aEff = fAMin + double(a)*fAStep;
    for(Int_t b = 0; b < fBBins; b++){
      Double_t bEff = fBMin + double(b)*fBStep;
      Int_t Index2D = (fBBins*a) + b;
      if((fabs(aEff) < 1e-20) && (fabs(bEff) < 1e-20))            iDefault   = Index2D;
      if((fabs(aEff-fAlpha) <= 0.5*fAStep && fabs(bEff) <1e-20 )) iAlphaZero = Index2D;
      if((fabs(bEff-fBeta)  <= 0.5*fBStep && fabs(aEff) <1e-20 )) iBetaZero  = Index2D;
      if(fabs(aEff-fAlpha) > 0.5*fAStep)  continue;          //find the correct step where alpha resides
      if(fabs(bEff-fBeta)  > 0.5*fBStep)  continue;          //find the correct step where beta resides
      iMinimum = Index2D;
    }
  }

  //Save all relevant graphs
  fVMassMom.push_back(tMassVsMomentum[iDefault]);         //MvsP graph where (a,b) = (0,0);
  fVMassMom.push_back(tMassVsMomentum[iAlphaZero]);       //MvsP graph where (a,b) = (amin,0);
  fVMassMom.push_back(tMassVsMomentum[iBetaZero]);        //MvsP graph where (a,b) = (0,bmin);
  fVMassMom.push_back(tMassVsMomentum[iMinimum]);         //MvsP graph where (a,b) = (amin,bmin)
  fVMassMom.push_back(tMassVsMomentum[iMinimum+fBBins]);  //MvsP graph where (a,b) = (amin+1step, bmin)
  fVMassMom.push_back(tMassVsMomentum[iMinimum-fBBins]);  //MvsP graph where (a,b) = (amin-1step, bmin)

  fVChiGrid.push_back(hChiGrid);
  fVChiGrid.push_back(hChiGridClone);
}

void AlphaBetaComputation::ReadDataModeTree(){
  //!======================================================================================================
  //! Function to read the input file data mode TTree and save the information from each subjob in a struct
  //! The struct is used to make the stability plots and also determines the RunID/NK3PI for histomode.

  TTree *DataTree = static_cast<TTree*>(GetCurrentFile()->Get("DataModeAlphaBeta"));
  if(!DataTree) return;
  Int_t nSubJobs = DataTree->GetEntries();
  DataTree->SetBranchAddress("RunID",&fRunID);
  DataTree->SetBranchAddress("BurstID",&fBurstID);
  DataTree->SetBranchAddress("NK3PI",&nK3PI);
  DataTree->SetBranchAddress("Alpha",&fAlpha);
  DataTree->SetBranchAddress("Beta",&fBeta);
  DataTree->SetBranchAddress("AlphaErr",&fAlphaError);
  DataTree->SetBranchAddress("BetaErr",&fBetaError);

  for(Int_t sj = 0; sj < nSubJobs; sj++){
    treeInfo SubJob;
    DataTree->GetEntry(sj);
    fNumberK3PI += nK3PI;
    SubJob.RunID       = fRunID;
    SubJob.BurstID     = fBurstID;
    SubJob.NK3PI       = nK3PI;
    SubJob.Alpha       = fAlpha;
    SubJob.Beta        = fBeta;
    SubJob.AlphaError  = fAlphaError;
    SubJob.BetaError   = fBetaError;
    SubJobVector.push_back(SubJob);
  }
}

void AlphaBetaComputation::saveBurstStabilityPlots(){
  //!===========================================================================================
  //! Function to to read data-mode tree struct and make the alpha/beta vs burst stability plots

  if(fNumberK3PI < 500) return;
  TGraphErrors* AlphaVsBurst = new TGraphErrors();
  TGraphErrors* BetaVsBurst  = new TGraphErrors();

  Int_t nIndex(0);
  //! Sort vector using BurstID so that plots are independent of file order.  
  std::sort(SubJobVector.begin(), SubJobVector.end(),
	    [](const treeInfo &left, const treeInfo &right)
	    {return(left.BurstID < right.BurstID);});

  for(UInt_t sj = 0; sj < SubJobVector.size(); sj++){
    treeInfo SubJob = SubJobVector[sj];
    AlphaVsBurst->SetPoint(nIndex,SubJob.BurstID,SubJob.Alpha);
    AlphaVsBurst->SetPointError(nIndex,0,SubJob.AlphaError);
    BetaVsBurst->SetPoint(nIndex,SubJob.BurstID,SubJob.Beta);
    BetaVsBurst->SetPointError(nIndex,0,SubJob.BetaError);
    nIndex++;
  }
  AlphaVsBurst->SetLineColorAlpha(kBlue,0.5);
  BetaVsBurst->SetLineColorAlpha(kRed,0.5);
  AlphaVsBurst->SetTitle("#alpha as determined by each subjob (#alpha vs burstID);BurstID;#alpha[MeV^{-1}]");
  BetaVsBurst->SetTitle("#beta as determined by each subjob (#beta vs burstID);BurstID;#beta");
  fVRunAlphaStability.push_back(AlphaVsBurst);
  fVRunBetaStability.push_back(BetaVsBurst);
}

void AlphaBetaComputation::makePDFReport() {
  //!===========================================================================================
  //! Function to make a PDF report from histogram mode, including alpha/beta vs Chi2
  //! a comparison of Mass vs Momentum before and after correction and the stability plots
  //! produced in saveBurstStabilityPlots().

  if(fNumberK3PI < 500){
    cout << user_normal() << "Little or no statistics, no .pdf produced" << endl;
    return;
  }

  //!The two chi^2 grids...
  TH2F* ReducedChiGrid = (TH2F*)fVChiGrid[0]->Clone();
  TH2F* FullChiGrid    = (TH2F*)fVChiGrid[1]->Clone();

  TBox *SignalBox = new TBox();
  SignalBox->SetLineColor(kRed);
  SignalBox->SetFillStyle(0);
  TMarker* ResultMark = new TMarker();
  ResultMark->SetMarkerStyle(29);
  ResultMark->SetMarkerSize(1);

  TCanvas* PDF = new TCanvas("er","",1500,1500);
  TString nPDF  = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000;
  PDF->Print(nPDF+"[");
  PDF->Divide(1,3);
  PDF->cd(1);gPad->Divide(3,1);
  PDF->cd(2);gPad->Divide(2,1);
  PDF->cd(3);gPad->Divide(3,1);
  //!========================================
  //!Top row, Pad 1: Run ID and Result.
  //!========================================
  PDF->cd(1);gPad->cd(1);
  TString RunID = Form("Run %d",fRunID);
  TString ARes  = Form("#alpha = (%6.2f #pm %0.2f ) #times 10^{-8} MeV^{-1}" ,fAlpha/(1.0e-8), fAlphaError/(1.0e-8));
  TString BRes  = Form("#beta = (%6.2f #pm %0.2f ) #times 10^{-3}",           fBeta/(1.0e-3),  fBetaError/(1.0e-3));
  TString CHsq  = Form("#frac{#chi^{2}}{NDF} = %4.2f",                        fChiSq);

  TLatex  tR(0.1,0.7,RunID);
  TLatex  tA(0.1,0.55,ARes);
  TLatex  tB(0.1,0.45,BRes);
  TLatex  tC(0.1,0.35,CHsq);
  tR.SetTextSize(0.120);tA.SetTextSize(0.06);tB.SetTextSize(0.06);tC.SetTextSize(0.06);
  tR.Draw();tA.Draw();tB.Draw();tC.Draw();
  //!=========================================
  //!Top Row, Pad 2: (a,b) vs Chi^2 plot w.fit
  //!=========================================
  PDF->cd(1);gPad->cd(2);
  ReducedChiGrid->SetTitle("Full #alpha,#beta vs #chi^{2};#alpha[MeV^{-1}] #times 10^{4};#beta;#chi^{2}/ndf");
  ReducedChiGrid->SetContour(1000);
  ReducedChiGrid->Draw("COLZ");
  ReducedChiGrid->GetXaxis()->SetTitleOffset(1.4); ReducedChiGrid->GetYaxis()->SetTitleOffset(1.4);
  ReducedChiGrid->SetStats(kFALSE);
  ResultMark->DrawMarker(fAlpha*fSF,fBeta);
  PDF->cd(1);gPad->cd(2)->SetRightMargin(0.13);
  //!=========================================
  //!Top Row, Pad3: Zoomed (a,b) vs Chi^2 plot
  //!=========================================
  PDF->cd(1);gPad->cd(3);
  TH2F* ZoomChiGrid   = (TH2F*)ReducedChiGrid->Clone();
  TF2*  Fit2D         = (TF2*)ZoomChiGrid->GetFunction("Fit2D");
  ZoomChiGrid->GetXaxis()->SetRangeUser(fSF*(fAlpha-5.0*fAStep),fSF*(fAlpha+5.0*fAStep));
  ZoomChiGrid->GetYaxis()->SetRangeUser((fBeta-5.0*fBStep),(fBeta+5.0*fBStep));
  ZoomChiGrid->SetTitle("Zoomed #alpha,#beta vs #chi^{2};#alpha[MeV^{-1}] #times 10^{4};#beta;#chi^{2}/ndf");
  ZoomChiGrid->SetContour(100);
  ZoomChiGrid->Draw("COLZ");
  ZoomChiGrid->GetXaxis()->SetTitleOffset(1.4); ZoomChiGrid->GetYaxis()->SetTitleOffset(1.4);
  ZoomChiGrid->SetStats(kFALSE);
  ResultMark->DrawMarker(fAlpha*fSF,fBeta);
  PDF->cd(1);gPad->cd(3)->SetRightMargin(0.13);
  //!=============================================
  //!Middle Row, Pad4: Mass vs Momentum Multigraph
  //!=============================================
  PDF->cd(2);gPad->cd(1);
  gPad->SetPad(0.0,0.0,0.67,1.0);
  std::vector<TString> fGraphNames;
  fGraphNames.push_back("(#alpha,#beta) = (0, 0)");
  fGraphNames.push_back(Form("(#alpha,#beta) = (%.2e, 0)",fAlpha));
  fGraphNames.push_back(Form("(#alpha,#beta) = (0, %.2e)",fBeta));
  fGraphNames.push_back("(#alpha,#beta) min");
  fGraphNames.push_back(Form("(#alpha,#beta) min with #alpha + %.0e",fAStep));
  fGraphNames.push_back(Form("(#alpha,#beta) min with #alpha - %.0e",fAStep));
  std::vector<Int_t> fGraphColors ={kBlue,kPink-3,kYellow,kMagenta+2,kOrange-1,kGreen-2};
  TLegend *fLegend = new TLegend(0.15,0.65,0.4,0.9);
  TMultiGraph* fMG = new TMultiGraph();
  for(UInt_t p = 0; p < fVMassMom.size(); p++){
    if(!fVMassMom[p]) continue;  //Skip if null graph (outside range)
    fVMassMom[p]->SetLineColorAlpha(fGraphColors[p],0.75);
    fVMassMom[p]->SetMarkerColorAlpha(fGraphColors[p],0.75);
    fMG->Add(fVMassMom[p]);
    fLegend->AddEntry(fVMassMom[p],fGraphNames[p],"lp");
  }
  fMG->SetTitle("M3#pi vs P(#pi^{-});P(GeV);M3#pi(MeV)");
  gPad->SetGrid();
  fMG->Draw("ALP*");
  if(fMG->GetHistogram()->GetMaximum() > 494.5) fMG->SetMaximum(494.5); //if error bars are big!
  if(fMG->GetHistogram()->GetMinimum() < 492.5) fMG->SetMinimum(492.5);
  fLegend->Draw();
  SignalBox->DrawBox(9,493.651,45.0,493.683);
  //!================================================================
  //!Middle Row, Pad5: Alpha Projection. Uses THStack projection
  //!to extract the distribution at the bin where beta fit resides
  //!Also have to 'project' the 2D fit to 1D to fit show performance.
  //!================================================================
  PDF->cd(2);gPad->cd(2);
  gPad->SetPad(0.67,0.0,1.0,1.0);

  Int_t MinABin(0),MinBBin(0),MinCBin(0);
  FullChiGrid->GetMinimumBin(MinABin,MinBBin,MinCBin);
  TH1F* AlphaClone = (TH1F*)FullChiGrid->Clone();
  AlphaClone->SetFillColorAlpha(kBlue,0.6);
  THStack* AlphaProjection = new THStack
    (AlphaClone,"x","alphaprojection",
     "#alpha projection along minimum bin;#alpha[MeV^{-1}] #times 10^{4};#chi^{2}/ndf",
     MinBBin,MinBBin);

  TF1* fAP = new TF1("fAP","[0] + [1]*x + [2]*[6] + [3]*x*x +[4]*[6]*[6] + [5]*[6]*x",-0.002,0.002);
  fAP->SetParameter(0,Fit2D->GetParameter(0));
  fAP->SetParameter(1,Fit2D->GetParameter(1));
  fAP->SetParameter(2,Fit2D->GetParameter(2));
  fAP->SetParameter(3,Fit2D->GetParameter(3));
  fAP->SetParameter(4,Fit2D->GetParameter(4));
  fAP->SetParameter(5,Fit2D->GetParameter(5));
  fAP->SetParameter(6,fBeta);
  fAP->SetNpx(10000);
  AlphaProjection->Draw();
  fAP->Draw("SAME");
  gPad->SetLogy(); gPad->SetGrid();
  //!================================================================
  //!Bottom Row, Pad8: Beta Projection. Uses THStack projection
  //!to extract the distribution at the bin where alpha fit resides
  //!Also have to 'project' the 2D fit to 1D to show fit performance.
  //!================================================================
  PDF->cd(3);gPad->cd(3);

  TH1F* BetaClone = (TH1F*)FullChiGrid->Clone();
  BetaClone->SetFillColorAlpha(kRed,0.6);
  THStack* BetaProjection = new THStack
    (BetaClone,"y","betaprojection",
     "#beta projection along minimum bin;#beta;#chi^{2}/ndf",MinABin,MinABin);

  TF1* fBP   = new TF1("fBP","[0] + [1]*[6] + [2]*x + [3]*[6]*[6] +[4]*x*x + [5]*[6]*x",fBMin,fBMax);
  fBP->SetParameter(0,Fit2D->GetParameter(0));
  fBP->SetParameter(1,Fit2D->GetParameter(1));
  fBP->SetParameter(2,Fit2D->GetParameter(2));
  fBP->SetParameter(3,Fit2D->GetParameter(3));
  fBP->SetParameter(4,Fit2D->GetParameter(4));
  fBP->SetParameter(5,Fit2D->GetParameter(5));
  fBP->SetParameter(6,fAlpha*fSF);
  fBP->SetNpx(1000);
  BetaProjection->Draw();
  fBP->Draw("SAME");
  gPad->SetLogy(); gPad->SetGrid();
  //!=============================================
  //!Bottom Row, Pad6&7: Alpha,Beta vs Burst;
  //!=============================================
  PDF->cd(3);gPad->cd(1);
  if(fVRunAlphaStability[0]) fVRunAlphaStability[0]->Draw("ALP*");
  gPad->SetGrid();
  PDF->cd(3);gPad->cd(2);
  if(fVRunBetaStability[0])  fVRunBetaStability[0]->Draw("ALP*");
  gPad->SetGrid();
  PDF->Print(nPDF);
  PDF->Print(nPDF+"]");  
}
