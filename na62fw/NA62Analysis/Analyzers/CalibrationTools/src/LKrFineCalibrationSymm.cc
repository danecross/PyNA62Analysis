// ----------------------
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// April 2019
// ----------------------
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
// ------------------------------------------------------------

#include <iostream>
#include <TChain.h>
#include <TLine.h>
#include "LKrFineCalibrationSymm.hh"
#include "Event.hh"
#include "ConfigSettings.hh"
#include "GeometricAcceptance.hh"
#include "EnergyCluster.hh"
#include "K2piSelection.hh"
#include "Pi0Selection.hh"
#include <TProfile.h>
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

LKrFineCalibrationSymm::LKrFineCalibrationSymm(Core::BaseAnalysis *ba) : Analyzer(ba, "LKrFineCalibrationSymm"), fHRsVsE(nullptr), fGRsVsE(nullptr), fHRsNewVsE(nullptr), fGRsNewVsE(nullptr), fHRsOldVsE(nullptr), fGRsOldVsE(nullptr){

  RequestL0Data();
  AddParam("TriggerMask", &fTriggerMask, 0xFF);
  AddParam("FitRangeMin", &fNewFitRange[0], 10000.0); // MeV
  AddParam("FitRangeMax", &fNewFitRange[1], 30000.0); // MeV
  AddParam("FitEdge", &fFitEdge, 5000.0); // MeV
  AddParam("NIter", &fNIter, 1);
  AddParam("LKrPi0CalibFileName", &fLKrPi0CalibFileName, "");

}

void LKrFineCalibrationSymm::InitOutput() {
  if(fLKrPi0CalibFileName=="") { // no external pi0 file provided, use "smart" default names
    if(fNIter>1) fLKrPi0CalibFileName = Form("LKr-Pi0CalibrationSymm%d.dat",fNIter-1);
    // no input for NIter = 1
  }
  fOutPDFFileName = fAnalyzerName + Form("%d.pdf",fNIter);
}

LKrFineCalibrationSymm::~LKrFineCalibrationSymm() {
}

void LKrFineCalibrationSymm::StartOfRunUser() {
  // default fit parameters (= identity)
  fPreviousFitPar[0] = 1.;
  for(Int_t j=1; j<8; j++) fPreviousFitPar[j] = 0.;
  fPreviousFitRange[0] = 0.;
  fPreviousFitRange[1] = 999999999.9;

  if(fNIter<=1) return; // no input file reading

  Bool_t ParametersFound = false;
  if (NA62ConditionsService::GetInstance()->Open(fLKrPi0CalibFileName)==kSuccess) {
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fLKrPi0CalibFileName))) {
      if (Line.BeginsWith("#")) continue;
      TObjArray *l = Line.Tokenize(" ");
      fPreviousFitRange[0] = ((TObjString*)(l->At(0)))->GetString().Atof();
      fPreviousFitRange[1] = ((TObjString*)(l->At(1)))->GetString().Atof();
      for(Int_t j=0;j<8;j++){
        fPreviousFitPar[j] = ((TObjString*)(l->At(j+2)))->GetString().Atof();
      }
      delete l;
      ParametersFound = true;
    }
    NA62ConditionsService::GetInstance()->Close(fLKrPi0CalibFileName);
  }

  if(!ParametersFound)
    cout << user_normal() << "LKrPi0Calibration file needed for the procedure not found!" << endl;
}

void LKrFineCalibrationSymm::InitHist(){
  fReadingData = GetIsTree();

  if (fReadingData){
    BookHisto("hRsOldVsEnergy", new TH2D("hRsOldVsEnergy", "RsOld VS Energy; Energy [MeV];RsOld", 40, -500, 39500, 1000, 0.5, 1.5));
    BookHisto("hRsNewVsEnergy", new TH2D("hRsNewVsEnergy", "RsNew VS Energy; Energy [MeV];RsNew", 40, -500, 39500, 1000, 0.5, 1.5));
    BookHisto("hRsVsEnergy", new TH2D("hRsVsEnergy", "Rs VS Energy; Energy [MeV];Rs", 40, -500, 39500, 1000, 0.5, 1.5));
  }
  else{ // histo mode
    fHRsOldVsE = static_cast<TH2D*>(RequestHistogram(fAnalyzerName, "hRsOldVsEnergy", true));
    fHRsNewVsE = static_cast<TH2D*>(RequestHistogram(fAnalyzerName, "hRsNewVsEnergy", true));
    fHRsVsE = static_cast<TH2D*>(RequestHistogram(fAnalyzerName, "hRsVsEnergy", true));
  }

}

void LKrFineCalibrationSymm::Process(Int_t) {

  if (!fReadingData) return; // no action if reading its own output in --histo mode

  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Int_t  L0TriggerWord = GetL0Data()->GetTriggerFlags();
  Bool_t PhysicsData   = L0DataType & 0x1;
  Bool_t CTRLTrigger   = L0DataType & 0x10;
  Bool_t TriggerOK     = (PhysicsData && (L0TriggerWord&0xFF)) || CTRLTrigger; //ALL MASKS + CTRL
  if (!TriggerOK) return; // process control triggers and selected MASKS only

  Bool_t K2piSelected = *GetOutput<Bool_t>("K2piSelection.EventSelected");
  if(!K2piSelected) return;
  auto Clusters = *GetOutput<vector<EnergyCluster>>("EnergyClusterBuilder.Output");
  Pi0SelectionOutput pi0SelectionOutput = *GetOutput<Pi0SelectionOutput>("K2piSelection.K2piPi0SelectionOutput");
  Int_t ClusterID1 = pi0SelectionOutput.fClustersID.first;
  Int_t ClusterID2 = pi0SelectionOutput.fClustersID.second;
  auto Cluster1 = Clusters.at(ClusterID1);
  auto Cluster2 = Clusters.at(ClusterID2);
  Double_t Energy1 = Cluster1.GetClusterEnergy();
  Double_t Energy2 = Cluster2.GetClusterEnergy();

  // geometrical cut
  if (Cluster1.GetLKrCandidate()->GetClusterDDeadCell()<30.) return; // 3 cm from dead cell
  if (Cluster2.GetLKrCandidate()->GetClusterDDeadCell()<30.) return; // 3 cm from dead cell
  Double_t x1 = Cluster1.GetLKrCandidate()->GetClusterX();
  Double_t y1 = Cluster1.GetLKrCandidate()->GetClusterY();
  if ( (x1*x1 + y1*y1)<(180.0*180.0) ) return; // 18 cm (Beam pipe)
  if ( (x1*x1 + y1*y1)>(1070.0*1070.0) ) return; // 107 cm  (LAV shadow)
  Double_t x2 = Cluster2.GetLKrCandidate()->GetClusterX();
  Double_t y2 = Cluster2.GetLKrCandidate()->GetClusterY();
  if ( (x2*x2 + y2*y2)<(180.0*180.0) ) return; // 18 cm (Beam pipe)
  if ( (x2*x2 + y2*y2)>(1070.0*1070.0) ) return; // 107 cm  (LAV shadow)

  Bool_t is1InSymmRegion = false;
  Bool_t is2InSymmRegion = false;
  if(Energy1>fPreviousFitRange[0] && Energy1<fPreviousFitRange[1]) is1InSymmRegion = true;
  if(Energy2>fPreviousFitRange[0] && Energy2<fPreviousFitRange[1]) is2InSymmRegion = true;
  if(!is1InSymmRegion || !is2InSymmRegion) return;

  // previous correction
  Double_t Rs1 = fPreviousFitPar[0];
  Double_t Rs2 = fPreviousFitPar[0];
  for(Int_t iPol=1;iPol<8;iPol++){ // pol7
    Rs1 += fPreviousFitPar[iPol]*pow(Energy1,iPol);
    Rs2 += fPreviousFitPar[iPol]*pow(Energy2,iPol);
  }
  Energy1 = Energy1/Rs1;
  Energy2 = Energy2/Rs2;

  Bool_t isSymm = false;
  if(fabs(Energy1-Energy2)<1000.0) isSymm = true; // 1 GeV
  if(!isSymm) return;

  // New Correction
  Double_t LKrZ = GeometricAcceptance::GetInstance()->GetZLKr();
  TVector3 Vertex = *GetOutput<TVector3>("K2piSelection.K2piVertexPosition");
  Double_t z = LKrZ - Vertex.Z();
  Double_t x = x1-x2;
  Double_t y = y1-y2;
  Double_t R2 = (x*x+y*y);
  Double_t Pi0Mass = sqrt(Energy1*Energy2*R2)/z; // Pi0Mass computed with "neutral vertex" formula

  Double_t Energy = 0.5*(Energy1+Energy2);

  Double_t RsOld = fPreviousFitPar[0];
  for(Int_t iPol=1;iPol<8;iPol++){ // pol7
    RsOld += fPreviousFitPar[iPol]*pow(Energy,iPol);
  }
  FillHisto("hRsOldVsEnergy",Energy,RsOld);

  Double_t RsNew = Pi0Mass/MPI0;
  FillHisto("hRsNewVsEnergy",Energy,RsNew);

  Double_t Rs = RsNew*RsOld;
  FillHisto("hRsVsEnergy",Energy,Rs);

} // end Process Function

void LKrFineCalibrationSymm::EndOfBurstUser() {
}

void LKrFineCalibrationSymm::EndOfJobUser() {
  if(!fReadingData){
    if (!fHRsVsE) { // Histo mode required but no histograms found
      cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
      return;
    }

    Int_t NBins = fHRsVsE->GetXaxis()->GetNbins();
    if(fGRsVsE) delete fGRsVsE;
    fGRsVsE = new TGraphErrors();
    fGRsVsE->SetName("fGRsVsE");
    fGRsVsE->Set(NBins);
    for(Int_t iBin=1;iBin<=NBins;iBin++){
      TH1D* hRs = fHRsVsE->ProjectionY(Form("hRs_Bin%0d",iBin),iBin,iBin);
      hRs->Rebin(10); // factor 10: from 1000 to 100 bins
      if(hRs->GetEntries()>=5) { // bin with at least 5 events
        Double_t maxcontent = hRs->GetBinContent(hRs->GetMaximumBin());
        Int_t iBeginOfPeak=1;
        Double_t PeakFraction = 0.5;
        for (Int_t i=hRs->GetMaximumBin(); i>1; i--) {
          if (hRs->GetBinContent(i)<PeakFraction*maxcontent) {
            iBeginOfPeak = i;
            break;
          }
        }
        Int_t iEndOfPeak=hRs->GetNbinsX();
        for (Int_t i=hRs->GetMaximumBin(); i<hRs->GetNbinsX(); i++) {
          if (hRs->GetBinContent(i)<PeakFraction*maxcontent) {
            iEndOfPeak = i;
            break;
          }
        }
        TF1 * func = new TF1("fgaus","gaus");
        hRs->Fit("fgaus","Q","",hRs->GetBinCenter(iBeginOfPeak),hRs->GetBinCenter(iEndOfPeak));

        double EBin = 1000.0*(iBin-1); // MeV
        fGRsVsE->SetPoint(iBin,EBin,func->GetParameter(1));
        fGRsVsE->SetPointError(iBin,0.,func->GetParError(1));
        delete func;
      }
      delete hRs;
    }

    TF1* FuncRsVsE = new TF1("FuncRsVsE","pol7");
    FuncRsVsE->SetParameter(0,1.);
    for(Int_t j=1; j<8; j++){
      FuncRsVsE->SetParameter(j,0.);
    }

    Int_t FitStatus = fGRsVsE->Fit(FuncRsVsE,"","",fNewFitRange[0],fNewFitRange[1]); //0: success; any other values: failure
    for(Int_t j=0; j<8; j++){
      fNewFitPar[j] = -999999999.9;
    }
    if(!FitStatus){
      for(Int_t j=0; j<8; j++){
        fNewFitPar[j] = FuncRsVsE->GetParameter(j);
      }
    }
    delete FuncRsVsE;

    NBins = fHRsNewVsE->GetXaxis()->GetNbins();
    if(fGRsNewVsE) delete fGRsNewVsE;
    fGRsNewVsE = new TGraphErrors();
    fGRsNewVsE->SetName("fGRsNewVsE");
    fGRsNewVsE->Set(NBins);
    for(Int_t iBin=1;iBin<=NBins;iBin++){
      TH1D* hRsNew = fHRsNewVsE->ProjectionY(Form("hRsNew_Bin%0d",iBin),iBin,iBin);
      hRsNew->Rebin(10); // factor 10: from 1000 to 100 bins
      if(hRsNew->GetEntries()>=5) { // bin with at least 5 events
        Double_t maxcontent = hRsNew->GetBinContent(hRsNew->GetMaximumBin());
        Int_t iBeginOfPeak=1;
        Double_t PeakFraction = 0.5;
        for (Int_t i=hRsNew->GetMaximumBin(); i>1; i--) {
          if (hRsNew->GetBinContent(i)<PeakFraction*maxcontent) {
            iBeginOfPeak = i;
            break;
          }
        }
        Int_t iEndOfPeak=hRsNew->GetNbinsX();
        for (Int_t i=hRsNew->GetMaximumBin(); i<hRsNew->GetNbinsX(); i++) {
          if (hRsNew->GetBinContent(i)<PeakFraction*maxcontent) {
            iEndOfPeak = i;
            break;
          }
        }
        TF1 * func = new TF1("fgaus","gaus");
        hRsNew->Fit("fgaus","Q","",hRsNew->GetBinCenter(iBeginOfPeak),hRsNew->GetBinCenter(iEndOfPeak));

        double EBin = 1000.0*(iBin-1); // MeV
        fGRsNewVsE->SetPoint(iBin,EBin,func->GetParameter(1));
        fGRsNewVsE->SetPointError(iBin,0.,func->GetParError(1));
        delete func;
      }
      delete hRsNew;
    }

    TF1* FuncRsNewVsE = new TF1("FuncRsNewVsE","pol7");
    FuncRsNewVsE->SetParameter(0,1.);
    for(Int_t j=1; j<8; j++){
      FuncRsNewVsE->SetParameter(j,0.);
    }

    fGRsNewVsE->Fit(FuncRsNewVsE,"","",fNewFitRange[0],fNewFitRange[1]); //0: success; any other values: failure
    delete FuncRsNewVsE;

    BuildPDFReport();

    // write fit parameters in .dat file
    ofstream LKrFineCalibrationParameters;
    LKrFineCalibrationParameters.open(Form("LKr-Pi0CalibrationSymm%d.run%06d_0000-run%06d_9999.dat",fNIter,GetRunID(),GetRunID()));
    LKrFineCalibrationParameters << "# FitRangeMin FitRangeMax FitPar[0] ... FitPar[7] " << std::endl;
    LKrFineCalibrationParameters << fNewFitRange[0]+fFitEdge << " "<< fNewFitRange[1]-fFitEdge <<" "<< fNewFitPar[0] <<" "<< fNewFitPar[1] <<" "<< fNewFitPar[2] <<" "<< fNewFitPar[3] <<" "<< fNewFitPar[4] <<" "<< fNewFitPar[5] <<" "<< fNewFitPar[6] <<" "<< fNewFitPar[7] << std::endl;
    LKrFineCalibrationParameters.close();

    Double_t AverageFitPar[8];
    for(Int_t j=0; j<8; j++){
      AverageFitPar[j] = 0.5*(fNewFitPar[j]+fPreviousFitPar[j]);
    }
    // write average fit parameters in .dat file
    ofstream LKrFineCalibrationAverageParameters;
    LKrFineCalibrationAverageParameters.open(Form("LKr-Pi0CalibrationSymm%dAvg.run%06d_0000-run%06d_9999.dat",fNIter,GetRunID(),GetRunID()));
    LKrFineCalibrationAverageParameters << "# FitRangeMin FitRangeMax FitPar[0] ... FitPar[7] " << std::endl;
    LKrFineCalibrationAverageParameters << fNewFitRange[0]+fFitEdge << " "<< fNewFitRange[1]-fFitEdge <<" "<< AverageFitPar[0] <<" "<< AverageFitPar[1] <<" "<< AverageFitPar[2] <<" "<< AverageFitPar[3] <<" "<< AverageFitPar[4] <<" "<< AverageFitPar[5] <<" "<< AverageFitPar[6] <<" "<< AverageFitPar[7] << std::endl;
    LKrFineCalibrationAverageParameters.close();

  }

  SaveAllPlots();
  return;
}

void LKrFineCalibrationSymm::BuildPDFReport() {
  TCanvas* Canvas = new TCanvas("Canvas");
  Canvas->Print(Form(fOutPDFFileName + "["), "pdf");

  fGRsVsE->SetTitle(Form("Rs vs E  for Run %d",GetRunID()));
  fGRsVsE->Draw("AP");
  fGRsVsE->SetMarkerStyle(20);
  fGRsVsE->SetMarkerSize(0.8);
  fGRsVsE->GetXaxis()->SetTitle("E [MeV]");
  fGRsVsE->GetYaxis()->SetTitle("Rs");
  fGRsVsE->GetXaxis()->SetRangeUser(5000,60000);
  fGRsVsE->GetYaxis()->SetRangeUser(0.98,1.02);

  TF1* PreviousFuncRVsE = new TF1("PreviousFuncRVsE","pol7",fPreviousFitRange[0],fPreviousFitRange[1]);
  for(Int_t j=0; j<8; j++){
    PreviousFuncRVsE->SetParameter(j,fPreviousFitPar[j]);
  }
  PreviousFuncRVsE->SetLineColor(kBlue);
  PreviousFuncRVsE->Draw("same");

  Canvas->Print(fOutPDFFileName, "pdf");

  fGRsNewVsE->SetTitle(Form("RsNew (Residual) vs E  for Run %d",GetRunID()));
  fGRsNewVsE->Draw("AP");
  fGRsNewVsE->SetMarkerStyle(20);
  fGRsNewVsE->SetMarkerSize(0.8);
  fGRsNewVsE->GetXaxis()->SetTitle("E [MeV]");
  fGRsNewVsE->GetYaxis()->SetTitle("RsNew");
  fGRsNewVsE->GetXaxis()->SetRangeUser(5000,60000);
  fGRsNewVsE->GetYaxis()->SetRangeUser(0.98,1.02);

  TF1* Constant1 = new TF1("Constant1","1",fPreviousFitRange[0],fPreviousFitRange[1]);
  Constant1->SetLineColor(kBlue);
  Constant1->Draw("same");

  Canvas->Print(fOutPDFFileName, "pdf");

  Canvas->Print(Form(fOutPDFFileName + "]"), "pdf");

  delete PreviousFuncRVsE;
  delete Constant1;
  delete Canvas;

  return;

}
