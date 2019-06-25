#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "JittersCheck.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "TFitResult.h"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
// -------------------------------------
// History:
//
// Created by Michele Corvino 2019-03-12
//
// -------------------------------------

/// \class JittersCheck
/// \Brief
//	Check for jitters in LKr using a list of reconstructed burst running with --histo option
/// \EndBrief
///

JittersCheck::JittersCheck(Core::BaseAnalysis *ba) : Analyzer(ba, "JittersCheck")
{
  RequestTree("LKr",new TRecoLKrEvent);
}
void JittersCheck::InitOutput(){
}

void JittersCheck::InitHist(){
  DTLeft    = static_cast<TH2F *> (RequestHistogram("LKrMonitor", "DeltaTLeft", true));
  DTRight   = static_cast<TH2F *> (RequestHistogram("LKrMonitor", "DeltaTRight", true));
  NEntriesL = static_cast<TH2F *> (RequestHistogram("LKrMonitor", "CREAMNEntriesLeft", true));
  NEntriesR = static_cast<TH2F *> (RequestHistogram("LKrMonitor", "CREAMNEntriesRight", true));
  BookHisto(new TH2F("JitterAsymmetries", "", 200, -20, 20, 200, -20, 20));
  BookHisto(new TH2F("CheckedCREAMs", "", 32, -0.5, 31.5, 18, 2.5, 20.5));
  BookHisto(new TH2F("CREAMsWithJitters", "", 32, -0.5, 31.5, 18, 2.5, 20.5));
  BookHisto(new TH1F("TimeJitter","", 120, -30,30));
  for(int i=0; i<3; i++){
    for(int j=0; j<3; j++){
      if(i*j==1) continue;
      fHNEntriesNegTail[i][j]    = static_cast<TH2F*> (RequestHistogram("LKrMonitor", Form("HistoNegative_%d_%d", i,j),true));   
      fHNEntriesPosTail[i][j]    = static_cast<TH2F*> (RequestHistogram("LKrMonitor", Form("HistoPositive_%d_%d", i,j),true));
      fHDeltaT[i][j]             = static_cast<TH2F*> (RequestHistogram("LKrMonitor", Form("DeltaT_%d_%d",i,j),true));
      fHNPipEntries[i][j]        = static_cast<TH2F*> (RequestHistogram("LKrMonitor", Form("NPipsPerCREAM_%d_%d_",i,j),true));
    }
  }
}

void JittersCheck::DefineMCSimple(){
}

void JittersCheck::StartOfRunUser(){
}

void JittersCheck::StartOfBurstUser(){
}

void JittersCheck::ProcessSpecialTriggerUser(int, unsigned int){
}

void JittersCheck::Process(int iEvent){
  if(fMCSimple.fStatus == MCSimple::kMissing){printIncompleteMCWarning(iEvent);return;}
}
void JittersCheck::PostProcess(){
}

void JittersCheck::EndOfBurstUser(){
}

void JittersCheck::EndOfRunUser(){

}

void JittersCheck::EndOfJobUser(){

  for(int i=0; i<32; i++){
    for(int j=0; j<16; j++){
      Int_t slot = j<8 ? j+3 : j+5;
      if(CREAMHasJitter(i,j)) {
        FillHisto("CREAMsWithJitters",i,slot);
        CheckCREAM(i,slot);
      }
    }
  }
  SaveAllPlots();
}

void JittersCheck::DrawPlot(){	/// \MemberDescr
}

JittersCheck::~JittersCheck(){
}

Bool_t JittersCheck::CREAMHasJitter(Int_t crate, Int_t slot){
  Int_t Threshold = 5;
  Int_t NCREAMsUsed = 0;
  Int_t NPos = 0;
  Int_t NNeg = 0;
  Double_t MeanPos =  0;
  Double_t MeanNeg	= 0;
  Double_t JittTime;
  Double_t MinTailFraction = 0.01;
  Int_t PhysicalSlot = slot < 8 ? slot+3 : slot+5;
  for(Int_t i=0; i<3;i++){
    for(Int_t j=0; j<3; j++){
      if(i==kCREAMSeed && j==kCREAMSeed) continue;
      TH1D *histo = fHDeltaT[i][j]->ProjectionY(Form("CREAM%d-%d_%d_%d",crate,PhysicalSlot,i,j),crate*16+slot+1,crate*16+slot+1,"");
      Int_t NBins = histo->GetNbinsX();
      Int_t NegTailEnd = histo->FindBin(-15);
      Int_t PosTailStart = histo->FindBin(15);
      Int_t NEventsInCore = histo->Integral(NegTailEnd,PosTailStart);
      Int_t NEventsInTails = histo->Integral(1,NegTailEnd)+histo->Integral(PosTailStart,NBins); 
      if(fHNPipEntries[i][j]->GetBinContent(crate+1,slot+1) < 30) continue;
      NCREAMsUsed++; 
      if(GetAsymmetry(crate, slot,i,j) > Threshold) {
        if(NEventsInTails<MinTailFraction*NEventsInCore) continue;
        NPos++;
        histo->GetXaxis()->SetRange(PosTailStart,NBins);
        MeanPos += histo->GetMean();
      }
      if(GetAsymmetry(crate,slot,i,j) < -1*Threshold){
        if(NEventsInTails<MinTailFraction*NEventsInCore) continue;
        NNeg++;
        histo->GetXaxis()->SetRange(1,NegTailEnd);
        MeanNeg += histo->GetMean();
      }
    } 
  }
  if(NCREAMsUsed) FillHisto("CheckedCREAMs", crate, PhysicalSlot);
  if(NPos > NCREAMsUsed/2 ){
    JittTime = MeanPos/NCREAMsUsed;
    FillHisto("TimeJitter", JittTime);
    return true;
  }
  else if(NNeg > NCREAMsUsed/2){
    JittTime = MeanNeg/NCREAMsUsed;
    FillHisto("TimeJitter", JittTime);
    return true;
  }
  return false;
}



Double_t JittersCheck::GetAsymmetry(Int_t Crate, Int_t Slot,Int_t i, Int_t j){
  Double_t Asym = -9999.99;
  TH2F *Neg, *Pos; 
  Neg = fHNEntriesNegTail[i][j];
  Pos = fHNEntriesPosTail[i][j];
  Double_t n = Neg->GetBinContent(Crate+1,Slot+1);
  Double_t p = Pos->GetBinContent(Crate+1,Slot+1);
  if(n && p) Asym = (p-n)/TMath::Sqrt(n+p);
  else if(n) Asym = (p-n)/TMath::Sqrt(1+n);
  else Asym = (p-n)/TMath::Sqrt(1+p);
  return Asym;
}

void JittersCheck::CheckCREAM(Int_t Crate, Int_t Slot){
  TCanvas *plot = new TCanvas(Form("DebugCREAM%d-%d", Crate, Slot));
  plot->Divide(3,3);
  Int_t progslot = Slot>10 ? Slot-5 : Slot-3;
  for(Int_t i=0; i<3;i++){
    for(Int_t j=0; j<3; j++){
      plot->cd(i*3+j+1);
      if(i==kCREAMSeed && j==kCREAMSeed) continue;
      TH1D *histo = fHDeltaT[i][j]->ProjectionY(Form("Deltat_%d_%d",i,j),Crate*16+progslot+1,Crate*16+progslot+1,"");
      histo->Draw();
    }
  }
  plot->Write();
  delete plot;
}
