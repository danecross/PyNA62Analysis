// -------------------------------------------------------------
// History:
//
// Created by Viacheslav Duk (viacheslav.duk@cern.ch)
// and Riccardo Lollini (riccardo.lollini@cern.ch) 2017-04
//
// -------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHODSlewingCorrectionsCalculation.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "ConfigSettings.hh"
#include "NA62ConditionsService.hh"

#include <TF1.h>
#include <TProfile.h>
#include <TLine.h>
#include <TPaveLabel.h>
#include <TPDF.h>
#include <TStyle.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CHODSlewingCorrectionsCalculation
/// \Brief
/// Slewing corrections calculation.
/// \EndBrief
///
/// \Detailed
/// Two-step analyzer.\n
///   -1: Run the analyzer on a list of bursts. A root file will be produced.\n
///   -2: Read its own output (using --histo command line option) to compute
/// slewing corrections, light velocities, TOT and T0 at PM. A PDF report
/// is created, along with .dat files containing all these parameters.\n
///
/// An example of use:\n
/// \code/
// ./CHODSlewingCorrectionsCalculationExec -i NA62Reco_1474051559-02-006291-0001.root -o output1.root
/// ./CHODSlewingCorrectionsCalculationExec --histo -i output1.root -o output2.root
/// \endcode
/// where CHODSlewingCorrectionsCalculationExec is the name of the executable obtained compiling the analyzer.
/// \author Created by Viacheslav Duk (viacheslav.duk@cern.ch)
/// and Riccardo Lollini (riccardo.lollini@cern.ch) 2017-04
/// \EndDetailed

CHODSlewingCorrectionsCalculation::CHODSlewingCorrectionsCalculation(Core::BaseAnalysis *ba) : Analyzer(ba, "CHODSlewingCorrectionsCalculation")
{
  RequestTree("Cedar",new TRecoCedarEvent);
  RequestTree("CHOD",new TRecoCHODEvent);

  fReadingData = kTRUE;

  for (Int_t i=0; i<128; i++) {
    if (i<16) fSlabCenter[i] = (fSlabLimitsX[16-i-1] + fSlabLimitsX[16-i])/2.;
    else if (i<48) fSlabCenter[i] = (fSlabLimitsX[i-16] + fSlabLimitsX[i-16+1])/2.;
    else if (i<64) fSlabCenter[i] = (fSlabLimitsX[80-i] + fSlabLimitsX[80-i-1])/2.;
    else if (i<96) fSlabCenter[i] = (fSlabLimitsY[96-i] + fSlabLimitsY[96-i-1])/2.;
    else fSlabCenter[i] = (fSlabLimitsY[i-96] + fSlabLimitsY[i-96+1])/2.;
  }
}

void CHODSlewingCorrectionsCalculation::InitOutput(){}

void CHODSlewingCorrectionsCalculation::InitHist(){

  fReadingData = GetIsTree();

  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;
    BookHisto("hTimeVsIP", new TH2F("hTimeVsIP", "time vs IP", 2048, -0.5, 2047.5, 500, -25., 25.) );
    for(Int_t i=0; i<2048; i++)
      BookHisto(Form("hTOTVsIP%d",i), new TH2F(Form("hTOTVsIP%d",i), Form("time vs TOT for IP%d",i), 250, 5., 30., 500, -25., 25.));

    BookHisto("hTimeVsIPUncorr", new TH2F("hTimeVsIPUncorr", "time vs IP, no corrections", 2048, -0.5, 2047.5, 500, -25., 25.) );
    BookHisto("hTimeVsIPT0Corr", new TH2F("hTimeVsIPT0corr", "time vs IP, T0 corrections", 2048, -0.5, 2047.5, 500, -25., 25.) );
    BookHisto("hDtUncorr", new TH1F("hDtUncorr", "tV - tH, before corrections", 100, -10.*TdcCalib, 10.*TdcCalib) );
    BookHisto("hDtT0Corr", new TH1F("hDtT0Corr", "tV - tH, after T0 correction",100, -6.*TdcCalib, 6.*TdcCalib) );
  }
  else {
    cout << user_normal() << "Reading my own output" << endl;
    fHTime = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hTimeVsIPUncorr", true));
    for (Int_t SlabNumber=0; SlabNumber<128; SlabNumber++) {
      for (Int_t iIP=0; iIP<16; iIP++) {
        fHTotIP[SlabNumber][iIP] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("hTOTVsIP%d",SlabNumber*16+iIP), true));
      }
    }
  }
}

void CHODSlewingCorrectionsCalculation::DefineMCSimple(){}

void CHODSlewingCorrectionsCalculation::StartOfRunUser(){
  TString Line;

  // Light Velocities
  TString LightVelocitiesFileName = "CHOD-LightVelocities.dat";
  if (NA62ConditionsService::GetInstance()->Open(LightVelocitiesFileName)==kSuccess){
    Int_t iSlab = 0;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(LightVelocitiesFileName))) {
      if (Line.BeginsWith("#")) continue;
      fLightVelocities[iSlab] = Line.Atof();
      iSlab++;
    }
    NA62ConditionsService::GetInstance()->Close(LightVelocitiesFileName);
  }

  TString T0FileName = "CHOD-T0.dat";
  if (NA62ConditionsService::GetInstance()->Open(T0FileName)!=kSuccess){
    cout << user_normal() << "Asked to open CHOD T0 file, but cannot find it" << endl;
    return;
  }
  // Read new 128 T0's and convert them into the old 2048 T0's
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(T0FileName))) {
    if (Line.BeginsWith("#")) continue;
    Int_t ROch, SlabNumber;
    Double_t NewT0;
    stringstream ss;
    ss << Line;
    ss >> ROch >> SlabNumber >> NewT0;

    Int_t IP=0;
    if (SlabNumber < 16) {
      for (Int_t iIntersectingSlab=64; iIntersectingSlab<80; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-64);
        fT0[IP] = NewT0 - fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10.*(121.-fPMCoordinate[SlabNumber]));
      }
    }
    else if (SlabNumber < 32) {
      for (Int_t iIntersectingSlab=80; iIntersectingSlab<96; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-80);
        fT0[IP] = NewT0 + fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10.*(-fPMCoordinate[SlabNumber]));
      }
    }
    else if (SlabNumber < 48) {
      for (Int_t iIntersectingSlab=96; iIntersectingSlab<112; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-96);
        fT0[IP] = NewT0 + fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10.*(-121.+fPMCoordinate[SlabNumber]));
      }
    }
    else if (SlabNumber < 64) {
      for (Int_t iIntersectingSlab=112; iIntersectingSlab<128; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-112);
        fT0[IP] = NewT0 - fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10.*fPMCoordinate[SlabNumber]);
      }
    }
    else if (SlabNumber < 80) {
      for (Int_t iIntersectingSlab=0; iIntersectingSlab<16; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab);
        fT0[IP] = NewT0 + fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10.*(-fPMCoordinate[SlabNumber]));
      }
    }
    else if (SlabNumber < 96) {
      for (Int_t iIntersectingSlab=16; iIntersectingSlab<32; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-16);
        fT0[IP] = NewT0 + fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10*(-121.+fPMCoordinate[SlabNumber]));
      }
    }
    else if (SlabNumber < 112) {
      for (Int_t iIntersectingSlab=32; iIntersectingSlab<48; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-32);
        fT0[IP] = NewT0 - fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10*fPMCoordinate[SlabNumber]);
      }
    }
    else {
      for (Int_t iIntersectingSlab=48; iIntersectingSlab<64; iIntersectingSlab++) {
        IP = SlabNumber*16 + (iIntersectingSlab-48);
        fT0[IP] = NewT0 - fLightVelocities[SlabNumber]*(fSlabCenter[iIntersectingSlab]-10*(121.-fPMCoordinate[SlabNumber]));
      }
    }
  }
  NA62ConditionsService::GetInstance()->Close(T0FileName);
}

void CHODSlewingCorrectionsCalculation::StartOfBurstUser(){}

void CHODSlewingCorrectionsCalculation::ProcessSpecialTriggerUser(Int_t, UInt_t){}

void CHODSlewingCorrectionsCalculation::Process(Int_t){

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  TRecoCedarEvent *CedarEvent = GetEvent<TRecoCedarEvent>();
  TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();

  Int_t NCandCedar = CedarEvent->GetNCandidates();

  Double_t TriggerTime = GetEventHeader()->GetFineTime()*TdcCalib;

  Int_t iCedarBest = -1;
  Bool_t IsCedarCand = false;
  Double_t dTCedarTriggerBefore=100000.;
  TRecoCedarCandidate *CedarCandidate;
  Double_t CedarTime = -10000.;
  for(Int_t iCedar=0; iCedar<NCandCedar; iCedar++){
    CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedar));
    CedarTime = CedarCandidate->GetTime();
    Double_t dTCedarTrigger = fabs(CedarTime - TriggerTime);
    if (dTCedarTrigger < dTCedarTriggerBefore){
      iCedarBest = iCedar;
      dTCedarTriggerBefore = dTCedarTrigger;
    }
  }
  if(iCedarBest!=-1) {
    CedarCandidate = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCedarBest));
    CedarTime = CedarCandidate->GetTime();
    IsCedarCand = true;
  }

  if(IsCedarCand && CedarCandidate->GetNSectors()>=5 && CHODEvent->GetNHits()==2){

    // get first hit
    TRecoCHODHit* RecoHit1 = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(0));
    // get second hit
    TRecoCHODHit* RecoHit2 = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(1));


    // calculate IPs
    Int_t IP1 = (RecoHit2->GetChannelID()-64)%16;
    Int_t IP2 = RecoHit1->GetChannelID()%16;


    if(RecoHit1->GetChannelID()<64 && RecoHit2->GetChannelID()>=64) {
      if (fabs(RecoHit1->GetTime() - RecoHit2->GetTime()) <= 10.) { // uncorr time difference should be < 10 ns
	Int_t QuadrantV = RecoHit1->GetChannelID() / 16;
	Int_t QuadrantH = RecoHit2->GetChannelID() / 16;
	if ( (QuadrantH-QuadrantV) == 4) { // hits should be in the same quadrant (Q1 condition)
	    FillHisto("hTimeVsIPUncorr",RecoHit1->GetChannelID()*16+IP1, RecoHit1->GetTime()-CedarTime );
	    FillHisto("hTimeVsIPUncorr",RecoHit2->GetChannelID()*16+IP2, RecoHit2->GetTime()-CedarTime );


	    FillHisto(Form("hTOTVsIP%d",RecoHit1->GetChannelID()*16+IP1), RecoHit1->GetTimeWidth(), RecoHit1->GetTime()-CedarTime );
	    FillHisto(Form("hTOTVsIP%d",RecoHit2->GetChannelID()*16+IP2), RecoHit2->GetTimeWidth(), RecoHit2->GetTime()-CedarTime );

	    FillHisto("hDtUncorr", RecoHit1->GetTime()-RecoHit2->GetTime() );

	    // tV-tH after T0 corrections
	    Double_t t1Corr = RecoHit1->GetTime() - fT0[RecoHit1->GetChannelID()*16+IP1];
	    Double_t t2Corr = RecoHit2->GetTime() - fT0[RecoHit2->GetChannelID()*16+IP2];

	    FillHisto("hDtT0Corr", RecoHit1->GetTime()-fT0[RecoHit1->GetChannelID()*16+IP1] -
			                  RecoHit2->GetTime()+fT0[RecoHit2->GetChannelID()*16+IP2] );
	    FillHisto("hTimeVsIPT0Corr", RecoHit1->GetChannelID()*16+IP1, t1Corr - CedarTime );
	    FillHisto("hTimeVsIPT0Corr", RecoHit2->GetChannelID()*16+IP2, t2Corr - CedarTime );
	}
      }
    }

  }

}

void CHODSlewingCorrectionsCalculation::PostProcess(){

}

void CHODSlewingCorrectionsCalculation::EndOfBurstUser(){

}

void CHODSlewingCorrectionsCalculation::EndOfRunUser(){

}

void CHODSlewingCorrectionsCalculation::EndOfJobUser(){

  // Data mode: save output
  if (fReadingData) {
    SaveAllPlots();
    return;
  }

  // Histo mode: fit the histograms
  Bool_t HistoNotFound = false;
  if (!fHTime) HistoNotFound = true;
  for (Int_t SlabNumber=0; SlabNumber<128; SlabNumber++) {
    for (Int_t iIP=0; iIP<16; iIP++) {
      if (!fHTotIP[SlabNumber][iIP]) HistoNotFound = true;
    }
  }
  if (HistoNotFound) { // Histo mode required but no histograms found
    cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
    return;
  }

  Double_t LowVelocityLimit[128], HighVelocityLimit[128];
  Double_t TimeRangeForSlewingFit_LowLimit[128][16], TimeRangeForSlewingFit_HighLimit[128][16];

  // Create a PDF report with useful histograms for each slab
  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  TCanvas *Canvas = new TCanvas("CHODSlewingCorrectionsCalculation_Canvas","",1400,800);
  Canvas->Print(Form(OutputPDFFileName + "["), "pdf"); // open file
  gStyle->SetFuncWidth(1);

  Int_t runID = GetRunID();

  // output files
  ofstream SlewCorrFile, VelocityFile, T0AtPMFile, TOTFile;
  SlewCorrFile.open("CHOD-SlewCorr.run" + to_string(runID) + ".dat"); // slewing slope and slewing const
  VelocityFile.open("CHOD-LightVelocities.run" + to_string(runID) + ".dat"); // light velocities
  T0AtPMFile.open("CHOD-T0AtPM.run" + to_string(runID) + ".dat"); // T0 at PM
  TOTFile.open("CHOD-TOT.run" + to_string(runID) + ".dat"); // Time over threshold

  for (Int_t SlabNumber=0; SlabNumber<128; SlabNumber++) {

    // calculate quadrant number of the second plane (for the calculation of IP widths)
    Int_t QuadrantForIP;
    // our slab is in the vertical plane
    if(SlabNumber/16<4) QuadrantForIP = SlabNumber/16 + 4;
    // our slab is in the horizontal plane
    if(SlabNumber/16>=4) QuadrantForIP = SlabNumber/16 - 4;

    // define constants
    Double_t SlewingSlope[16], SlewingConst[16];
    //Double_t TimeRangeForSlewingFit_LowLimit[16], TimeRangeForSlewingFit_HighLimit[16];
    for(Int_t iIP=0; iIP<16; iIP++){
      SlewingSlope[iIP] = 0.;
      SlewingConst[iIP] = 0.;
      TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP] = -10000.;
      TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP] = -10000.;
    }

    // HISTOGRAMS
    // time histogram for T0 calculation and for further limits on time
    // time for each IP, for T0 fit
    for(Int_t iIP=0; iIP<16; iIP++){
      fHTimeIP[SlabNumber][iIP] = fHTime->ProjectionY(Form("time for IP %d",iIP), SlabNumber*16+iIP+1, SlabNumber*16+iIP+1, "");
      fHTimeIP[SlabNumber][iIP]->SetName(Form("HTimeIP%d",SlabNumber*16+iIP));
      fHTimeIP[SlabNumber][iIP]->Rebin(2);
    }
    // histogram for counting bins
    TH1D *hTemp;
    hTemp = fHTotIP[SlabNumber][8]->ProjectionY();

    // make histograms with bins of different width
    Double_t IPBins[17];
    IPBins[0]=0.;
    Double_t Sum = 0.;
    for(Int_t iIP=0; iIP<16; iIP++){
      Sum = Sum + fSlabWidth[QuadrantForIP*16+iIP];
      IPBins[iIP+1] = Sum;
    }
    fHVelocity[SlabNumber] = new TH1F(Form("TimeVsIPSlab%d",SlabNumber),"TimeVsIP", 16, IPBins);
    fHTotVsIP[SlabNumber] = new TH1F(Form("TOTVsIPSlab%d",SlabNumber),"TOTVsIP", 16, IPBins);

    // data analysis
    for(Int_t iIP=0; iIP<16; iIP++){
      // flag for the central hole
      Int_t FlagCentralHole = 0;
      Int_t IP1 = SlabNumber*16+iIP;
      if(IP1==15 || IP1==496 || IP1==527 || IP1==1008) FlagCentralHole = 1;

      // time distributions (for Nevents>140 in the histogram)
      Int_t Nevents = 140.;
      if(fHTimeIP[SlabNumber][iIP]->Integral()>Nevents && FlagCentralHole==0){
	// limits for T0 fits
	Double_t LowLimitForT0Fit = fHTimeIP[SlabNumber][iIP]->GetBinCenter(fHTimeIP[SlabNumber][iIP]->GetMaximumBin()) - 1.4;
	Double_t HighLimitForT0Fit = fHTimeIP[SlabNumber][iIP]->GetBinCenter(fHTimeIP[SlabNumber][iIP]->GetMaximumBin()) + 1.4;

	// time fit for T0 correction
	fHTimeIP[SlabNumber][iIP]->Fit("gaus","","", LowLimitForT0Fit, HighLimitForT0Fit);
	TF1 * fFGaus = fHTimeIP[SlabNumber][iIP]->GetFunction("gaus");
	// set time limits for future slewing fit
	TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP] = fFGaus->GetParameter(1) - 3.*fFGaus->GetParameter(2);
	TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP] = fFGaus->GetParameter(1) + 3.*fFGaus->GetParameter(2);
	// fill the histogram (time vs coordinate)
	fHVelocity[SlabNumber]->SetBinContent(iIP+1, fFGaus->GetParameter(1));
	fHVelocity[SlabNumber]->SetBinError(iIP+1, fFGaus->GetParameter(2));
      }
      // (Tchod-Tcedar) vs TOT profile in a good time range (defined above)
      fHTotProfile[SlabNumber][iIP] = fHTotIP[SlabNumber][iIP]->ProfileX(Form("TOT profile IP%d",iIP),
								       hTemp->FindBin(TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP]),
								       hTemp->FindBin(TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP]));
      fHTotProfile[SlabNumber][iIP]->SetName(Form("HTotProfile%d",SlabNumber*16+iIP));

      // TOT distribution in a good time range (defined above)
      fHTotProjection[SlabNumber][iIP] = fHTotIP[SlabNumber][iIP]->ProjectionX(Form("TOT projection IP%d",iIP),
								      hTemp->FindBin(TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP]),
								      hTemp->FindBin(TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP]));
      fHTotProjection[SlabNumber][iIP]->Rebin(2);
      fHTotProjection[SlabNumber][iIP]->SetName(Form("HTotProjection%d",SlabNumber*16+iIP));

      if(fHTimeIP[SlabNumber][iIP]->Integral()>Nevents && FlagCentralHole==0){
	// limits for TOT fits (slewing corrections)
	Double_t LowLimitForSlewingFit = fHTotProjection[SlabNumber][iIP]->GetBinCenter(
									      fHTotProjection[SlabNumber][iIP]->GetMaximumBin())-1.6;
	Double_t HighLimitForSlewingFit = fHTotProjection[SlabNumber][iIP]->GetBinCenter(
									       fHTotProjection[SlabNumber][iIP]->GetMaximumBin())+1.6;

	// limits are forced to select the first peak in TOT distribution when there is more than one:
	Double_t TOTHighLimitMax;
	if (SlabNumber == 79) TOTHighLimitMax = 23.;
	else TOTHighLimitMax = 15.;
	Double_t LowLimitTOTProjection = fHTotProjection[SlabNumber][iIP]->GetXaxis()->GetXmin();
	Double_t HighLimitTOTProjection = fHTotProjection[SlabNumber][iIP]->GetXaxis()->GetXmax();
	if (fHTotProjection[SlabNumber][iIP]->GetBinCenter(fHTotProjection[SlabNumber][iIP]->GetMaximumBin()) > TOTHighLimitMax) {
	  fHTotProjection[SlabNumber][iIP]->GetXaxis()->SetRangeUser(5., TOTHighLimitMax);
	  LowLimitForSlewingFit =
	    fHTotProjection[SlabNumber][iIP]->GetBinCenter(fHTotProjection[SlabNumber][iIP]->GetMaximumBin())-1.6;
	  HighLimitForSlewingFit =
	    fHTotProjection[SlabNumber][iIP]->GetBinCenter(fHTotProjection[SlabNumber][iIP]->GetMaximumBin())+1.6;
	  fHTotProjection[SlabNumber][iIP]->GetXaxis()->SetRangeUser(LowLimitTOTProjection, HighLimitTOTProjection);
	}

	// slewing fit
	Double_t LowLimitForSlewingFitBest = -100., HighLimitForSlewingFitBest = 100.;
	Double_t Chi2RidMin = 100.;
	Double_t FitWidth = HighLimitForSlewingFit - LowLimitForSlewingFit;
	Double_t FitStep = 15.*FitWidth/100.;
	for (Int_t kl=-2; kl<3; kl++){
	  for (Int_t kh=-2; kh<3; kh++){
            fFitStatus = fHTotProfile[SlabNumber][iIP]->Fit("pol1","","",LowLimitForSlewingFit+kl*FitStep, HighLimitForSlewingFit+kh*FitStep);
            if (fFitStatus!=0) continue;
	    fHTotProfile[SlabNumber][iIP]->Fit("pol1","","",LowLimitForSlewingFit+kl*FitStep, HighLimitForSlewingFit+kh*FitStep);
	    TF1 * fFPol1 = fHTotProfile[SlabNumber][iIP]->GetFunction("pol1");
	    if (fFPol1->GetChisquare()/fFPol1->GetNDF() < Chi2RidMin){
	      Chi2RidMin = fFPol1->GetChisquare()/fFPol1->GetNDF();
	      LowLimitForSlewingFitBest = LowLimitForSlewingFit+kl*FitStep;
	      HighLimitForSlewingFitBest = HighLimitForSlewingFit+kh*FitStep;
	    }
	  }
	}
	fFitStatus = fHTotProfile[SlabNumber][iIP]->Fit("pol1","","",LowLimitForSlewingFitBest, HighLimitForSlewingFitBest);
	if (fFitStatus!=0) {
	  SlewCorrFile << "0 0" << endl;
	  continue;
	}
	fHTotProfile[SlabNumber][iIP]->Fit("pol1","","",LowLimitForSlewingFitBest, HighLimitForSlewingFitBest);
	TF1 * fFPol1 = fHTotProfile[SlabNumber][iIP]->GetFunction("pol1");

	if( fFPol1->GetParameter(1)<0.) SlewingSlope[iIP] = fFPol1->GetParameter(1);
	// TOT fits for calculating <TOT>
	fHTotProjection[SlabNumber][iIP]->Fit("gaus","","",LowLimitForSlewingFitBest, HighLimitForSlewingFitBest);
	TF1 * fFGaus1 = fHTotProjection[SlabNumber][iIP]->GetFunction("gaus");
	// optimization of fit limits in order to have the peak of the distribution in between the limits
	if (fFGaus1->GetParameter(1)-0.5 < LowLimitForSlewingFitBest) {
	  fHTotProjection[SlabNumber][iIP]->Fit("gaus","","",LowLimitForSlewingFitBest-0.6, HighLimitForSlewingFitBest);
	  fFGaus1 = fHTotProjection[SlabNumber][iIP]->GetFunction("gaus");
	}
	else if (fFGaus1->GetParameter(1)+0.5 > HighLimitForSlewingFitBest) {
	  fHTotProjection[SlabNumber][iIP]->Fit("gaus","","",LowLimitForSlewingFitBest, HighLimitForSlewingFitBest+0.6);
	  fFGaus1 = fHTotProjection[SlabNumber][iIP]->GetFunction("gaus");
	}
	// writing SlewingConst = (-1) * fit_const * <TOT>
	if( fFPol1->GetParameter(1)<0.) SlewingConst[iIP] = (-1.)*SlewingSlope[iIP]*fFGaus1->GetParameter(1);
	// filling (TOT vs IP) histogram
	fHTotVsIP[SlabNumber]->SetBinContent(iIP+1,  fFGaus1->GetParameter(1));
	fHTotVsIP[SlabNumber]->SetBinError(iIP+1,  fFGaus1->GetParameter(2));

	SlewCorrFile << SlewingSlope[iIP] << " " << SlewingConst[iIP] << endl;

      }
      else {
	SlewCorrFile << "0 0" << endl;
      }
    }

    fHTime->SetAxisRange(SlabNumber*16, SlabNumber*16+15, "X");
    for (Int_t iIP=0; iIP<16; iIP++){
      fHTotProfile[SlabNumber][iIP]->SetAxisRange(TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP]-1,
						 TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP]+1, "Y");
    }

    // time vs IP
    if(fPMCoordinate[SlabNumber]>fOtherEndCoordinate[SlabNumber]){
      LowVelocityLimit[SlabNumber] = fOtherEndCoordinate[SlabNumber];
      HighVelocityLimit[SlabNumber] = fPMCoordinate[SlabNumber];
    }
    else{
      LowVelocityLimit[SlabNumber] = fPMCoordinate[SlabNumber];
      HighVelocityLimit[SlabNumber] = fOtherEndCoordinate[SlabNumber];
    }

    Canvas->Clear();
    TPaveLabel *page_title = new TPaveLabel(0.1,0.96,0.9,0.88,Form("Slab %d",SlabNumber));
    page_title->Draw();
    TPad *pad = new TPad("pad","pad",0.01,0.05,0.95,0.85);
    pad->Draw();
    pad->cd();
    pad->Divide(2,2);
    pad->cd(1);
    TLine a;
    a.SetLineColor(kRed);
    a.SetLineWidth(2);
    // the whole slab: time vs IP before cuts
    fHTime->SetAxisRange(SlabNumber*16, SlabNumber*16+15, "X");
    fHTime->Draw("colz");
    a.DrawLine(SlabNumber*16, -25., SlabNumber*16, 25.);
    a.DrawLine(SlabNumber*16+15, -25., SlabNumber*16+15, 25.);
    for(Int_t iIP=0; iIP<16; iIP++){
      a.DrawLine(SlabNumber*16+iIP-0.5, TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP],
		 SlabNumber*16+iIP+0.5, TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP]);
      a.DrawLine(SlabNumber*16+iIP-0.5, TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP],
		 SlabNumber*16+iIP+0.5, TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP]);
    }

    pad->cd(2);
    fHVelocity[SlabNumber]->Fit("pol1","","", LowVelocityLimit[SlabNumber], HighVelocityLimit[SlabNumber]-1.);
    TF1 * fFPol1Vel = fHVelocity[SlabNumber]->GetFunction("pol1");
    fHVelocity[SlabNumber]->SetLineWidth(2);
    fHVelocity[SlabNumber]->SetLineColor(kBlue);
    fHVelocity[SlabNumber]->SetAxisRange(TimeRangeForSlewingFit_LowLimit[SlabNumber][7]-10,
					 TimeRangeForSlewingFit_HighLimit[SlabNumber][7]+14, "Y");
    fHVelocity[SlabNumber]->Draw();
    if (fFPol1Vel) {
      VelocityFile << fabs(fFPol1Vel->GetParameter(1)/10.) << endl;
      T0AtPMFile << fFPol1Vel->GetParameter(0) + fFPol1Vel->GetParameter(1)*fPMCoordinate[SlabNumber] << endl;
    }
    else {
      VelocityFile << "0" << endl;
      T0AtPMFile << "0" << endl;
    }

    pad->cd(3);
    fHTotVsIP[SlabNumber]->Fit("pol1","","", LowVelocityLimit[SlabNumber], HighVelocityLimit[SlabNumber]-1.);
    TF1 * fFPol1TOT = fHTotVsIP[SlabNumber]->GetFunction("pol1");
    fHTotVsIP[SlabNumber]->SetAxisRange(5., 25., "Y");
    fHTotVsIP[SlabNumber]->Draw();
    if (fFPol1TOT) TOTFile << fFPol1TOT->GetParameter(0) + fFPol1TOT->GetParameter(1)*fPMCoordinate[SlabNumber]
			   << " " << fabs(fFPol1TOT->GetParameter(1)/10.) << endl;
    else TOTFile << "0" << endl;
    Canvas->Print(OutputPDFFileName, "pdf");

    Canvas->Clear();
    Canvas->Divide(4,4);
    for(Int_t iIP=0; iIP<16; iIP++){
      Canvas->cd(iIP+1);
      fHTimeIP[SlabNumber][iIP]->SetAxisRange(-15., 15., "X");
      fHTimeIP[SlabNumber][iIP]->Draw("colz");
    }
    Canvas->Print(OutputPDFFileName, "pdf");

    Canvas->Clear();
    Canvas->Divide(4,4);
    for(Int_t iIP=0; iIP<16; iIP++){
      Canvas->cd(iIP+1);
      fHTotProfile[SlabNumber][iIP]->SetAxisRange(TimeRangeForSlewingFit_LowLimit[SlabNumber][iIP]-1,
						 TimeRangeForSlewingFit_HighLimit[SlabNumber][iIP]+1, "Y");
      fHTotProfile[SlabNumber][iIP]->Draw("colz");
    }
    Canvas->Print(OutputPDFFileName, "pdf");

    Canvas->Clear();
    Canvas->Divide(4,4);
    for(Int_t iIP=0; iIP<16; iIP++){
      Canvas->cd(iIP+1);
      fHTotProjection[SlabNumber][iIP]->Draw("colz");
    }
    Canvas->Print(OutputPDFFileName, "pdf");

    Canvas->Clear();

  }
  SlewCorrFile.close();
  VelocityFile.close();
  T0AtPMFile.close();
  TOTFile.close();
  Canvas->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  delete Canvas;
  gErrorIgnoreLevel = -1; // restore the default
}

void CHODSlewingCorrectionsCalculation::ExportPlot(){

  if (!fReadingData) {

    fHTime->Write();

    for (Int_t SlabNumber=0; SlabNumber<128; SlabNumber++) {
      fHVelocity[SlabNumber]->Write();
      fHTotVsIP[SlabNumber]->Write();

      for (Int_t iIP=0; iIP<16; iIP++) {
	fHTimeIP[SlabNumber][iIP]->Write();
	fHTotProfile[SlabNumber][iIP]->Write();
	fHTotProjection[SlabNumber][iIP]->Write();
      }
    }
  }
}


void CHODSlewingCorrectionsCalculation::DrawPlot(){
}

CHODSlewingCorrectionsCalculation::~CHODSlewingCorrectionsCalculation(){

}
