// ---------------------------------------------------------------
// History:
//
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-08-17
// Created by Karim Massri (karim.massri@cern.ch) 2016-04-14
//
// ---------------------------------------------------------------
#include "NA62VOnlineMonitorCanvas.hh"
#include "NA62VRawDecoder.hh"
#include "TDCBRawDecoder.hh"
#include "SpectrometerReconstruction.hh"

#include "TRootBrowser.h"
#include "TGFrame.h"
#include "TRootEmbeddedCanvas.h"
#include "TGLayout.h"
#include "TPad.h"
#include "TCanvas.h"
#include "TPaveText.h"
#include "TKey.h"

#include <sys/stat.h>

NA62VOnlineMonitorCanvas::NA62VOnlineMonitorCanvas(TCanvas * Canvas, TString Name) : NA62VNamedModule(Name), fiCurrentFrame(0){
  fCanvas = Canvas;
  fCanvas->SetName(Name);
  f1DHistos.clear();
  f1DHistosReference.clear();
  f2DHistos.clear();
  fFrames.clear();
  fOnlineMonitorName="";
  fReferenceFileName="";
}

NA62VOnlineMonitorCanvas::NA62VOnlineMonitorCanvas(TString Name) : NA62VNamedModule(Name), fiCurrentFrame(0){
  fCanvas = new TCanvas(Name, Name, 800, 600);
  NA62VOnlineMonitorCanvas(fCanvas,Name);
}

NA62VOnlineMonitorCanvas::~NA62VOnlineMonitorCanvas(){
  for(UInt_t iFrame=0;iFrame<fFrames.size();iFrame++) {
    delete fFrames[iFrame];
    fFrames[iFrame] = 0;
  }
  fFrames.clear();
  delete fCanvas;
  fCanvas = 0;
}

void NA62VOnlineMonitorCanvas::Update() {
  for(UInt_t iFrame = 0; iFrame < fFrames.size(); iFrame++){ // update subtabs
    fFrames[iFrame]->Update();
  }
  if(!fFrames.size()){ // update only deepest canvas 
    //look at all the stored histos
    Double_t ymaxHisto = 0.;
    for(UInt_t iHisto=0; iHisto<f1DHistos.size();iHisto++){
      if(f1DHistos[iHisto] && f1DHistosReference[iHisto]) {
        TString refHistoName = f1DHistosReference[iHisto]->GetName();
        if(f1DHistos[iHisto]->Integral()){
          if(fOnlineMonitorName=="Spectrometer" && refHistoName.Contains("DigiTimeRaw")){
            f1DHistosReference[iHisto]->Scale((f1DHistos[iHisto]->Integral())*(SpectrometerReconstruction::GetRebinFactor_DigiTimeRaw())/f1DHistosReference[iHisto]->Integral());
          }else{
            f1DHistosReference[iHisto]->Scale(f1DHistos[iHisto]->Integral()/f1DHistosReference[iHisto]->Integral());
          };
        };
        if(ymaxHisto<f1DHistosReference[iHisto]->GetBinContent(f1DHistosReference[iHisto]->GetMaximumBin())){
          ymaxHisto = f1DHistosReference[iHisto]->GetBinContent(f1DHistosReference[iHisto]->GetMaximumBin());
        }
      }
      if(f1DHistos[iHisto]) {
        if(ymaxHisto<f1DHistos[iHisto]->GetBinContent(f1DHistos[iHisto]->GetMaximumBin())){
          ymaxHisto = f1DHistos[iHisto]->GetBinContent(f1DHistos[iHisto]->GetMaximumBin());
        }
      }
    }
    ymaxHisto *= 1.1;
    for(UInt_t iHisto=0; iHisto<f1DHistos.size();iHisto++){
      f1DHistos[iHisto]->SetMaximum(ymaxHisto);
    }
    fCanvas->Modified();
    fCanvas->Update();
  }
}

void NA62VOnlineMonitorCanvas::Reset() {
  for(UInt_t iFrame = 0; iFrame < fFrames.size(); iFrame++){ // reset subtabs
    fFrames[iFrame]->Reset();
  }
  if(!fFrames.size()){ // reset only deepest canvas 
    for(UInt_t iHisto=0; iHisto<f1DHistos.size();iHisto++){
      f1DHistos[iHisto]->Reset("M");
    }
    for(UInt_t iHisto=0; iHisto<f2DHistos.size();iHisto++){
      f2DHistos[iHisto]->Reset("M");
    }
  }
}

void NA62VOnlineMonitorCanvas::DrawHisto(TH1* Histo,TH1* RefHisto,TString Option){
  if(!Option.Contains("same")) {
    f1DHistos.clear();
    f1DHistosReference.clear();
  }
  f1DHistos.push_back(Histo);
  if(f1DHistos.back()) {
    f1DHistos.back()->SetStats(0);
    f1DHistos.back()->SetLineWidth(1);
    f1DHistos.back()->SetFillStyle(1001);
    if(f1DHistos.size()==1){
      f1DHistos.back()->SetLineColor(kBlue);
      f1DHistos.back()->SetFillColor(kCyan-9);
    }
    else if(f1DHistos.size()==2){
      f1DHistos.back()->SetLineColor(kRed);
      f1DHistos.back()->SetFillColor(kRed-9);
    }
    else if(f1DHistos.size()==3){
      f1DHistos.back()->SetLineColor(kGreen+2);
      f1DHistos.back()->SetFillColor(kGreen-9);
    }
    else {
      f1DHistos.back()->SetLineColor(2+f1DHistos.size());
      f1DHistos.back()->SetFillColor(2+f1DHistos.size());
    }
    f1DHistos.back()->Draw(Option);
  }
  f1DHistosReference.push_back(RefHisto);
  if(f1DHistosReference.back()) f1DHistosReference.back()->SetDirectory(0); // "detach" the histogram from the file
  if(f1DHistosReference.back()) {
    f1DHistosReference.back()->SetLineWidth(2);
    f1DHistosReference.back()->SetLineColor(kBlack);
    f1DHistosReference.back()->Draw("HISTOsame");
  }
}

void NA62VOnlineMonitorCanvas::DrawHisto(TH1* Histo,TString Option){
  TH1* ReferenceHisto = 0;
  TFile * ReferenceFile = TFile::Open(fReferenceFileName);
  if(ReferenceFile){
    TString RefHistoName = FindReferenceHistoPath(ReferenceFile,TString(Histo->GetName()));
    if(!RefHistoName.IsNull()) ReferenceHisto = (TH1*)ReferenceFile->Get(RefHistoName);
  }
  DrawHisto(Histo,ReferenceHisto,Option);
  if(ReferenceFile) ReferenceFile->Close();
}

void NA62VOnlineMonitorCanvas::DrawHisto(TH2* Histo,TString Option){
  //for the time being just draw the histo
  if(!Option.Contains("same")) {
    f2DHistos.clear();
  }
  f2DHistos.push_back(Histo);
  Histo->SetStats(0);
  Histo->Draw(Option);
}

void NA62VOnlineMonitorCanvas::DrawHisto(TGraph* Histo,TString Option){
  //for the time being just draw the graph
  Histo->Draw(Option);
}

void NA62VOnlineMonitorCanvas::DrawHisto(TGraphErrors* Histo,TString Option){
  //for the time being just draw the graph
  Histo->Draw(Option);
}

TH1* NA62VOnlineMonitorCanvas::DrawProjectionX(TH2* Histo,TString ProjectionName,Int_t FirstYBin,Int_t LastYBin,TString Option){
  TH1* HistoProjection = Histo->ProjectionX(ProjectionName,FirstYBin,LastYBin); //must be created before opening the reference file
  TH1* ReferenceHisto = 0;
  TFile * ReferenceFile = TFile::Open(fReferenceFileName);
  if(ReferenceFile){
    TString RefHistoName = FindReferenceHistoPath(ReferenceFile,TString(Histo->GetName()));
    if(!RefHistoName.IsNull()) ReferenceHisto = ((TH2F*)ReferenceFile->Get(RefHistoName))->ProjectionX(ProjectionName+"_ref",FirstYBin,LastYBin);
  }
  DrawHisto(HistoProjection,ReferenceHisto,Option);
  if(ReferenceFile) ReferenceFile->Close();
  return f1DHistos.back();
}

TH1* NA62VOnlineMonitorCanvas::DrawProjectionY(TH2* Histo,TString ProjectionName,Int_t FirstXBin,Int_t LastXBin,TString Option){
  TH1* HistoProjection = Histo->ProjectionY(ProjectionName,FirstXBin,LastXBin); //must be created before opening the reference file
  TH1* ReferenceHisto = 0;
  TFile * ReferenceFile = TFile::Open(fReferenceFileName);
  if(ReferenceFile){
    TString RefHistoName = FindReferenceHistoPath(ReferenceFile,TString(Histo->GetName()));
    if(!RefHistoName.IsNull()) ReferenceHisto = ((TH2F*)ReferenceFile->Get(RefHistoName))->ProjectionY(ProjectionName+"_ref",FirstXBin,LastXBin);
  }
  DrawHisto(HistoProjection,ReferenceHisto,Option);
  if(ReferenceFile) ReferenceFile->Close();
  return f1DHistos.back();
}

void NA62VOnlineMonitorCanvas::Divide(Int_t Nx,Int_t Ny){
  fCanvas->Divide(Nx,Ny);
  UInt_t NFrames = fCanvas->GetListOfPrimitives()->GetEntries();
  for(UInt_t iFrame = 0; iFrame < NFrames; iFrame++){
    TCanvas* Frame = (TCanvas*)fCanvas->GetListOfPrimitives()->At(iFrame);
    fFrames.push_back(new NA62VOnlineMonitorCanvas(Frame,Form("%s_%02d",fCanvas->GetName(),iFrame)));
    fFrames.back()->SetOnlineMonitorName(fOnlineMonitorName);
    fFrames.back()->SetReferenceFileName(fReferenceFileName);
  }
}

TVirtualPad * NA62VOnlineMonitorCanvas::cd(Int_t iCanvas){
  fiCurrentFrame = iCanvas-1;
  return fCanvas->cd(iCanvas);
}

TString NA62VOnlineMonitorCanvas::FindReferenceHistoPath(TFile * ReferenceFile,TString HistoName){

  if(!ReferenceFile) {
    std::cerr << "["+fOnlineMonitorName+"OnlineMonitor] WARNING: reference file not found!" << std::endl;
    return "";
  }

  TString ReferenceDirName = fOnlineMonitorName+"Monitor";
  if(fOnlineMonitorName=="NA62")  ReferenceDirName = "/";
  TKey * Key = ReferenceFile->GetDirectory(ReferenceDirName)->FindKeyAny(HistoName);
  TString Path = "";
  if(Key){
    TDirectory * Dir = Key->GetMotherDir();
    while(Dir->GetMotherDir()){
      Path = TString(Dir->GetName())+"/"+Path;
      TDirectory * MotherDir = Dir->GetMotherDir();
      Dir = MotherDir;
    }
  }
  else {
    std::cerr << "["+fOnlineMonitorName+"OnlineMonitor] WARNING: reference histo '" << HistoName << "' not found!" << std::endl;
    return "";
  }
  return TString(Path+HistoName);
}
