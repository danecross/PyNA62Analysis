    // ----------------------
// Created by Francesco Brizioli (francesco.brizioli@cern.ch)
// April 2019
// ----------------------
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
// ------------------------------------------------------------

#include <iostream>
#include <TChain.h>
#include <TLine.h>
#include "LKrFineCalibrationAsymm.hh"
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

LKrFineCalibrationAsymm::LKrFineCalibrationAsymm(Core::BaseAnalysis *ba) : Analyzer(ba, "LKrFineCalibrationAsymm"), fHRaVsE(nullptr), fGRaVsE(nullptr){

  RequestL0Data();

  // Configuration::ConfigSettings::SetNoSkipBadBurst(true); // do not skip bad bursts

  AddParam("TriggerMask", &fTriggerMask, 0xFF);
  AddParam("FitRangeMin", &fNewFitRange[0], 2000.0); // MeV
  AddParam("FitRangeMax", &fNewFitRange[1], 50000.0); // MeV
  AddParam("FitEdge", &fFitEdge, -1.); // MeV
  AddParam("MakeExtrapolationOutOfRange", &fMakeExtrapolationOutOfRange, false);
  AddParam("NIter", &fNIter, 1);
  AddParam("LKrPi0CalibFileName", &fLKrPi0CalibFileName,"");
}

void LKrFineCalibrationAsymm::InitOutput(){
  if(fLKrPi0CalibFileName=="") { // no external pi0 file provided, use "smart" default names
    if(fNIter>1) fLKrPi0CalibFileName = Form("LKr-Pi0CalibrationAsymm%d.dat",fNIter-1);
    else         fLKrPi0CalibFileName = "LKr-Pi0CalibrationSymm1.dat";
  }
  if(fFitEdge==-1) { // no external FitEdge provided, use "smart" default values
    if(fNIter>1) fFitEdge = 0.;
    else         fFitEdge = 7000.; // MeV
  }
  fOutPDFFileName = fAnalyzerName + Form("%d.pdf",fNIter);
}


LKrFineCalibrationAsymm::~LKrFineCalibrationAsymm() {
}

void LKrFineCalibrationAsymm::StartOfRunUser() {
  for(Int_t j=0; j<8; j++){
    fPreviousFitPar[j] = -999999999.9;
  }
  fPreviousFitRange[0] = -999999999.9;
  fPreviousFitRange[1] = -999999999.9;
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

void LKrFineCalibrationAsymm::StartOfBurstUser() {
}

void LKrFineCalibrationAsymm::InitHist(){
  fReadingData = GetIsTree();

  if (fReadingData){
    BookHisto("hLKrPositionExp-Reco", new TH2D("hLKrPositionExp-Reco", "; (X_exp-X_reco) [mm]; (Y_exp-Y_reco) [mm]", 200, -100, 100, 200, -100, 100));
    BookHisto("hRaVsEnergyNV", new TH2D("hRaVsEnergyNV", "Ra VS Energy (neutral vertex); Energy [MeV];Ra", 70, -500, 69500, 1000, 0.5, 1.5));
  }

  else{ // histo mode
    fHRaVsE = static_cast<TH2D*>(RequestHistogram(fAnalyzerName, "hRaVsEnergyNV", true));
  }
}

void LKrFineCalibrationAsymm::Process(Int_t iEvent) {

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
  Double_t Cluster_X = Cluster1.GetLKrCandidate()->GetClusterX();
  Double_t Cluster_Y = Cluster1.GetLKrCandidate()->GetClusterY();
  if ( (Cluster_X*Cluster_X + Cluster_Y*Cluster_Y)<(180.0*180.0) ) return; // 18 cm (Beam pipe)
  if ( (Cluster_X*Cluster_X + Cluster_Y*Cluster_Y)>(1070.0*1070.0) ) return; // 107 cm  (LAV shadow)
  Cluster_X = Cluster2.GetLKrCandidate()->GetClusterX();
  Cluster_Y = Cluster2.GetLKrCandidate()->GetClusterY();
  if ( (Cluster_X*Cluster_X + Cluster_Y*Cluster_Y)<(180.0*180.0) ) return; // 18 cm (Beam pipe)
  if ( (Cluster_X*Cluster_X + Cluster_Y*Cluster_Y)>(1070.0*1070.0) ) return; // 107 cm  (LAV shadow)

  // previous correction
  Double_t Rs1 = fPreviousFitPar[0];
  Double_t Rs2 = fPreviousFitPar[0];
  Double_t x2 = fPreviousFitRange[1];
  Double_t x1 = x2 - 5000.0; // pol1 extrapolation done in last 5 GeV
  Double_t y2 = fPreviousFitPar[0];
  Double_t y1 = fPreviousFitPar[0];
  for(Int_t iPol=1;iPol<8;iPol++){ // pol7
    Rs1 += fPreviousFitPar[iPol]*pow(Energy1,iPol);
    Rs2 += fPreviousFitPar[iPol]*pow(Energy2,iPol);
    y1 += fPreviousFitPar[iPol]*pow(x1,iPol);
    y2 += fPreviousFitPar[iPol]*pow(x2,iPol);
  }
  Double_t Pol1[2]; // y = q + mx
  Pol1[1] = (y1-y2)/(x1-x2); // m
  Pol1[0] = y1 - Pol1[1]*x1; // q
  Double_t Rs1_Pol1 = Pol1[0] + Energy1*Pol1[1];
  Double_t Rs2_Pol1 = Pol1[0] + Energy2*Pol1[1];

  Double_t ReferenceEnergy = Energy1;
  Double_t ToBeCorrectedEnergy = Energy2;
  auto ReferenceCluster = Cluster1;
  auto ToBeCorrectedCluster = Cluster2;
  Bool_t GoodEventForAsymm = true;

  Bool_t is1InSymmRegion = false;
  Bool_t is2InSymmRegion = false;
  if(Energy1>fPreviousFitRange[0] && Energy1<fPreviousFitRange[1]) is1InSymmRegion = true;
  if(Energy2>fPreviousFitRange[0] && Energy2<fPreviousFitRange[1]) is2InSymmRegion = true;

  if(is2InSymmRegion && !is1InSymmRegion){ // inside - outside
    ReferenceEnergy = Energy2/Rs2;
    ReferenceCluster = Cluster2;
    ToBeCorrectedEnergy = Energy1;
    ToBeCorrectedCluster = Cluster1;
  }
  else if(is1InSymmRegion && !is2InSymmRegion){ // inside - outside
    ReferenceEnergy = Energy1/Rs1;
    ReferenceCluster = Cluster1;
    ToBeCorrectedEnergy = Energy2;
    ToBeCorrectedCluster = Cluster2;
  }
  else if(is1InSymmRegion && is2InSymmRegion){ // both inside
    if(iEvent%2){ // random choice
      ReferenceEnergy = Energy2/Rs2;
      ReferenceCluster = Cluster2;
      ToBeCorrectedEnergy = Energy1;
      ToBeCorrectedCluster = Cluster1;
    }
    else{
      ReferenceEnergy = Energy1/Rs1;
      ReferenceCluster = Cluster1;
      ToBeCorrectedEnergy = Energy2;
      ToBeCorrectedCluster = Cluster2;
    }
  }
  else if(fMakeExtrapolationOutOfRange && Energy1>fPreviousFitRange[1] && Energy2<fPreviousFitRange[0]){ // both outside - 1 overflow: pol1
    ReferenceEnergy = Energy1/Rs1_Pol1;
    ReferenceCluster = Cluster1;
    ToBeCorrectedEnergy = Energy2;
    ToBeCorrectedCluster = Cluster2;
  }
  else if(fMakeExtrapolationOutOfRange && Energy2>fPreviousFitRange[1] && Energy1<fPreviousFitRange[0]){ // both outside - 1 overflow: pol1
    ReferenceEnergy = Energy2/Rs2_Pol1;
    ReferenceCluster = Cluster2;
    ToBeCorrectedEnergy = Energy1;
    ToBeCorrectedCluster = Cluster1;
  }
  else{
    GoodEventForAsymm = false;
  }

  // new correction (asymmetric - "Neutral Vertex" formula)
  if(!GoodEventForAsymm) return;

  TLorentzVector Kaon = *GetOutput<TLorentzVector>("K2piSelection.K2piKaonFourMomentum");
  TLorentzVector Pion = *GetOutput<TLorentzVector>("K2piSelection.K2piPionFourMomentum");
  TLorentzVector Pi0 = Kaon-Pion;
  TVector3 Vertex = *GetOutput<TVector3>("K2piSelection.K2piVertexPosition");

  Double_t LKrZ = GeometricAcceptance::GetInstance()->GetZLKr();

  TLorentzVector ReferencePhoton;
  TVector3 ReferencePhotonMomentum;
  ReferencePhotonMomentum.SetXYZ(ReferenceCluster.GetClusterX(), ReferenceCluster.GetClusterY(), LKrZ);
  ReferencePhotonMomentum = ReferencePhotonMomentum - Vertex;
  ReferencePhotonMomentum.SetMag(ReferenceEnergy);
  ReferencePhoton.SetVect(ReferencePhotonMomentum);
  ReferencePhoton.SetE(ReferenceEnergy);

  Double_t ToBeCorrectedClusterRecoX = ToBeCorrectedCluster.GetClusterX();
  Double_t ToBeCorrectedClusterRecoY = ToBeCorrectedCluster.GetClusterY();

  Double_t x = ReferenceCluster.GetClusterX()-ToBeCorrectedClusterRecoX;
  Double_t y = ReferenceCluster.GetClusterY()-ToBeCorrectedClusterRecoY;
  Double_t ClustersDistance2 = (x*x+y*y);

  TLorentzVector ToBeCorrectedPhoton = Pi0-ReferencePhoton;
  Double_t ToBeCorrectedPhotonPosAtLKr_X = Vertex.X()+(LKrZ-Vertex.Z())*ToBeCorrectedPhoton.Px()/ToBeCorrectedPhoton.Pz();
  Double_t ToBeCorrectedPhotonPosAtLKr_Y = Vertex.Y()+(LKrZ-Vertex.Z())*ToBeCorrectedPhoton.Py()/ToBeCorrectedPhoton.Pz();

  Bool_t MatchedPosition = false;
  x = ToBeCorrectedPhotonPosAtLKr_X-ToBeCorrectedClusterRecoX;
  y = ToBeCorrectedPhotonPosAtLKr_Y-ToBeCorrectedClusterRecoY;
  if ( (x*x+y*y)<(60.*60.) ) MatchedPosition = true; // 60 mm for spacial coincidence
  FillHisto("hLKrPositionExp-Reco",(ToBeCorrectedPhotonPosAtLKr_X-ToBeCorrectedClusterRecoX),(ToBeCorrectedPhotonPosAtLKr_Y-ToBeCorrectedClusterRecoY));

  if(MatchedPosition){
    Double_t z = LKrZ - Vertex.Z();
    Double_t ExpectedEnergy = MPI0*MPI0*z*z/(ReferenceEnergy*ClustersDistance2); // "neutral vertex" formula
    Double_t RaNV = ToBeCorrectedEnergy/ExpectedEnergy;
    FillHisto("hRaVsEnergyNV",ToBeCorrectedEnergy,RaNV);
  }

} // end Process Function

void LKrFineCalibrationAsymm::EndOfBurstUser() {
}

void LKrFineCalibrationAsymm::EndOfJobUser() {
  if(!fReadingData){

    if (!fHRaVsE) { // Histo mode required but no histograms found
      cout << user_normal() << "Asked to read my own output but cannot find it" << endl;
      return;
    }

    Int_t NBins = fHRaVsE->GetXaxis()->GetNbins();
    if(fGRaVsE) delete fGRaVsE;
    fGRaVsE = new TGraphErrors();
    fGRaVsE->SetName("fGRaVsE");
    fGRaVsE->Set(NBins);

    for(Int_t iBin=1;iBin<=NBins;iBin++){
      TH1D* hRa = fHRaVsE->ProjectionY(Form("hRa_Bin%0d",iBin),iBin,iBin);
      hRa->Rebin(10); // factor 10: from 1000 to 100 bins
      if(hRa->GetEntries()>=10) { // bin with at least 10 events
        Double_t maxcontent = hRa->GetBinContent(hRa->GetMaximumBin());
        Int_t iBeginOfPeak=1;
        Double_t PeakFraction = 0.5;
        for (Int_t i=hRa->GetMaximumBin(); i>1; i--) {
          if (hRa->GetBinContent(i)<PeakFraction*maxcontent) {
            iBeginOfPeak = i;
            break;
          }
        }
        Int_t iEndOfPeak=hRa->GetNbinsX();
        for (Int_t i=hRa->GetMaximumBin(); i<hRa->GetNbinsX(); i++) {
          if (hRa->GetBinContent(i)<PeakFraction*maxcontent) {
            iEndOfPeak = i;
            break;
          }
        }
        TF1 * func = new TF1("fgaus","gaus");
        hRa->Fit("fgaus","Q","",hRa->GetBinCenter(iBeginOfPeak),hRa->GetBinCenter(iEndOfPeak));

        double EBin = 1000.0*(iBin-1); // MeV
        fGRaVsE->SetPoint(iBin,EBin,func->GetParameter(1));
        fGRaVsE->SetPointError(iBin,0.,func->GetParError(1));

        delete func;
      }
      delete hRa;
    }

    TF1* FuncRaVsENV = new TF1("FuncRaVsENV","pol7");
    FuncRaVsENV->SetParameter(0,1.);
    for(Int_t j=1; j<8; j++){
      FuncRaVsENV->SetParameter(j,0.);
    }

    Int_t FitStatus = fGRaVsE->Fit(FuncRaVsENV,"","",fNewFitRange[0],fNewFitRange[1]); //0: success; any other values: failure
    for(Int_t j=0; j<8; j++){
      fNewFitPar[j] = -999999999.9;
    }
    if(!FitStatus){
      for(Int_t j=0; j<8; j++){
        fNewFitPar[j] = FuncRaVsENV->GetParameter(j);
      }
    }

    delete FuncRaVsENV;

    BuildPDFReport();

    // write fit parameters in .dat file
    ofstream LKrFineCalibrationParameters;
    LKrFineCalibrationParameters.open(Form("LKr-Pi0CalibrationAsymm%d.run%06d_0000-run%06d_9999.dat",fNIter,GetRunID(),GetRunID()));
    LKrFineCalibrationParameters << "#  FitRangeMin FitRangeMax FitPar[0] ... FitPar[7] " << std::endl;
    LKrFineCalibrationParameters << fNewFitRange[0]+fFitEdge << " "<< fNewFitRange[1]-fFitEdge <<" "<< fNewFitPar[0] <<" "<< fNewFitPar[1] <<" "<< fNewFitPar[2] <<" "<< fNewFitPar[3] <<" "<< fNewFitPar[4] <<" "<< fNewFitPar[5] <<" "<< fNewFitPar[6] <<" "<< fNewFitPar[7] << std::endl;
    LKrFineCalibrationParameters.close();

    Double_t AverageFitPar[8];
    for(Int_t j=0; j<8; j++){
      AverageFitPar[j] = 0.5*(fNewFitPar[j]+fPreviousFitPar[j]);
    }

    // write average fit parameters in .dat file
    ofstream LKrFineCalibrationAverageParameters;
    LKrFineCalibrationAverageParameters.open(Form("LKr-Pi0CalibrationAsymm%dAvg.run%06d_0000-run%06d_9999.dat",fNIter,GetRunID(),GetRunID()));
    LKrFineCalibrationAverageParameters << "#  FitRangeMin FitRangeMax FitPar[0] ... FitPar[7] " << std::endl;
    LKrFineCalibrationAverageParameters << fNewFitRange[0]+fFitEdge << " "<< fNewFitRange[1]-fFitEdge <<" "<< AverageFitPar[0] <<" "<< AverageFitPar[1] <<" "<< AverageFitPar[2] <<" "<< AverageFitPar[3] <<" "<< AverageFitPar[4] <<" "<< AverageFitPar[5] <<" "<< AverageFitPar[6] <<" "<< AverageFitPar[7] << std::endl;
    LKrFineCalibrationAverageParameters.close();

  }

  SaveAllPlots();

  return;

}

void LKrFineCalibrationAsymm::BuildPDFReport() {
  TCanvas* Canvas = new TCanvas("Canvas");
  Canvas->Print(Form(fOutPDFFileName + "["), "pdf");

  fGRaVsE->SetTitle(Form("Ra vs E  for Run %d",GetRunID()));
  fGRaVsE->Draw("AP");
  fGRaVsE->SetMarkerStyle(20);
  fGRaVsE->SetMarkerSize(0.8);
  fGRaVsE->GetXaxis()->SetTitle("E [MeV]");
  fGRaVsE->GetYaxis()->SetTitle("Ra");
  fGRaVsE->GetXaxis()->SetRangeUser(0,65000);
  fGRaVsE->GetYaxis()->SetRangeUser(0.95,1.1);

  TF1* PreviousFuncRVsE = new TF1("PreviousFuncRVsE","pol7",fPreviousFitRange[0],fPreviousFitRange[1]);
  for(Int_t j=0; j<8; j++){
    PreviousFuncRVsE->SetParameter(j,fPreviousFitPar[j]);
  }
  PreviousFuncRVsE->SetLineColor(kBlue);
  PreviousFuncRVsE->Draw("same");

  Canvas->Print(fOutPDFFileName, "pdf");

  Canvas->Print(Form(fOutPDFFileName + "]"), "pdf");

  delete PreviousFuncRVsE;
  delete Canvas;

  return;

}
