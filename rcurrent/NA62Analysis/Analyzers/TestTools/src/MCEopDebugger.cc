// ---------------------------------------------------------
//
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2019-03-25
//
// ---------------------------------------------------------

/// \class MCEopDebugger
/// \Brief
/// MC Eop debugger
/// \EndBrief
/// \Detailed
/// This is an MC Eop debugger
/// \author Karim Massri (karim.massri@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "MCEopDebugger.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;


MCEopDebugger::MCEopDebugger(Core::BaseAnalysis *ba) : Analyzer(ba, "MCEopDebugger") {
  RequestAllMCTrees();
  RequestTree("LKr", new TLKrEvent, "MC");
}

void MCEopDebugger::InitOutput(){
}

void MCEopDebugger::InitHist(){

  BookHisto(new TH2I("LKrXYNum", "", 128,-0.5,127.5,128,-0.5,127.5));
  BookHisto(new TH2I("LKrXYDen", "", 128,-0.5,127.5,128,-0.5,127.5));
  BookHisto(new TH2F("LKrXYEff", "", 128,-0.5,127.5,128,-0.5,127.5));
  BookHisto(new TH2F("LKrHitTime", "", 128,-0.5,127.5,128,-0.5,127.5));
  BookHisto(new TH1F("LKrMCEoP", "", 150, 0, 1.5));
  BookHisto(new TH1F("LKrMCEoP_inTime", "", 150, 0, 1.5));
  BookHisto(new TH1F("LKrMCDeltaE", "", 2000, -20e3, 20e3));
}

void MCEopDebugger::DefineMCSimple(){
}

void MCEopDebugger::StartOfRunUser(){
}

void MCEopDebugger::StartOfBurstUser(){
}

void MCEopDebugger::ProcessSpecialTriggerUser(int, unsigned int){
}

void MCEopDebugger::Process(int iEvent){
  Bool_t DebugEvent = false;
  Event* MCTruthEvent = GetMCEvent();
  TLKrEvent* LKrEvent = static_cast<TLKrEvent*>(GetEvent("LKr"));

  Double_t MCTruthEnergy=0.;
  if(MCTruthEvent){ //MC
    for(Int_t iKinePart=0; iKinePart<MCTruthEvent->GetNKineParts(); iKinePart++){
      KinePart * pKinePart = static_cast<KinePart*>(MCTruthEvent->GetKineParts()->At(iKinePart));
      if (iKinePart && pKinePart->GetPDGcode() == -11 && pKinePart->GetParentIndex()>=0){ //secondary positron
        KinePart * pKineParent = static_cast<KinePart*>(MCTruthEvent->GetKineParts()->At(pKinePart->GetParentIndex()));
        if ((!pKinePart->GetParentIndex() || !pKineParent->GetParentIndex()) && pKineParent->GetEndProcessName().BeginsWith("Decay")){
          //primary particle or particle produced by primary particle decay
          MCTruthEnergy = pKinePart->GetInitialMomentum().Mag();
        }
      }
    }
  }

  for(Int_t iHit=0; iHit<LKrEvent->GetNHits(); iHit++){
    TLKrHit* LKrHit = static_cast<TLKrHit*>(LKrEvent->GetHit(iHit));
    if(LKrHit->GetEnergy()<1000.) continue;
    if(LKrHit->GetTime()<900.) continue;
    DebugEvent = true;
  }
  Double_t TotalEnergy=0.;
  Double_t TotalEnergyInTime=0.;
  for(Int_t iHit=0; iHit<LKrEvent->GetNHits(); iHit++){
    TLKrHit* LKrHit = static_cast<TLKrHit*>(LKrEvent->GetHit(iHit));
    if(LKrHit->GetEnergy()<50.) continue;
    fHisto.GetTH2("LKrHitTime")->SetBinContent(LKrHit->GetXCellID()+1,LKrHit->GetYCellID()+1,fabs(LKrHit->GetTime()-806));
    if(DebugEvent){
      TotalEnergy+=LKrHit->GetEnergy();
      if(abs(LKrHit->GetTime()-806.)<10.) TotalEnergyInTime+=LKrHit->GetEnergy();
      else FillHisto("LKrXYNum",LKrHit->GetXCellID(),LKrHit->GetYCellID());
      std::cout << "Event " << iEvent <<  " " << MCTruthEvent->GetEventNumber() << " " << iHit << " test " << LKrHit->GetTime() << " " << LKrHit->GetEnergy() << " " << LKrHit->GetXCellID() << " " << LKrHit->GetYCellID() << " " << LKrHit->GetPosition().X() << " " << LKrHit->GetPosition().Y() << " " << LKrHit->GetEnergy() << std::endl;
    }
    FillHisto("LKrXYDen",LKrHit->GetXCellID(),LKrHit->GetYCellID());
  }
  if(DebugEvent){
    std::cout << "Event " << iEvent << " " << MCTruthEvent->GetEventNumber() << " " << MCTruthEvent->GetRandomDecayState()->GetSeed() << " [" <<  MCTruthEvent->GetRanecuState()[0] << "," <<MCTruthEvent->GetRanecuState()[1] << "] TotalE: " << TotalEnergy << " InTime: " << TotalEnergyInTime <<  " MCTruth: " << MCTruthEnergy << std::endl;
    FillHisto("LKrMCEoP", TotalEnergy/MCTruthEnergy);
    FillHisto("LKrMCEoP_inTime", TotalEnergyInTime/MCTruthEnergy);
    FillHisto("LKrMCDeltaE", TotalEnergyInTime-MCTruthEnergy);
    FilterAccept();
  }
}

void MCEopDebugger::PostProcess(){
}

void MCEopDebugger::EndOfBurstUser(){
}

void MCEopDebugger::EndOfRunUser(){
}

void MCEopDebugger::EndOfJobUser(){

  TH2F* hLKrEff = static_cast<TH2F*>(fHisto.GetTH2("LKrXYEff"));
  TH2I* hLKrNum = static_cast<TH2I*>(fHisto.GetTH2("LKrXYNum"));
  TH2I* hLKrDen = static_cast<TH2I*>(fHisto.GetTH2("LKrXYDen"));
  hLKrEff->Divide(hLKrNum,hLKrDen, 1., 1., "B");
  SaveAllPlots();
}

void MCEopDebugger::DrawPlot(){
}

MCEopDebugger::~MCEopDebugger(){
}
