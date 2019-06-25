#include "CheckTrigger.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "TriggerConditions.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;


CheckTrigger::CheckTrigger(Core::BaseAnalysis *ba) : Analyzer(ba, "CheckTrigger")
{
  RequestL0Data();

  AddParam("WhichTrigger", "int", &fWhichTrigger, 1); //1=control, 2=pnn
  AddParam("Verbosity", "bool", &verb, false);
}

void CheckTrigger::InitOutput()
{
  RegisterOutput("EventPassedTrigger", &fEventPassedTrigger);
  RegisterOutput("TriggerTime", &fTriggerTime);
}

void CheckTrigger::InitHist()
{
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hIsControlTrigger", "hIsControlTrigger", 2, 0, 2));
    BookHisto(new TH1I("hIsPNNTrigger", "hIsPNNTrigger", 2, 0, 2));
    BookHisto(new TH1D("hTriggerTime", "hTriggerTime", 512, 0., 50.));
    BookHisto(new TH1D("hFineTime", "hFineTime", 300, 0., 300.));
  };
}

void CheckTrigger::DefineMCSimple(){

}

void CheckTrigger::StartOfRunUser(){
}

void CheckTrigger::StartOfBurstUser(){
}

void CheckTrigger::ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType)
{
  // suppress unused variable warnings
  (void)iEvent;
  (void)triggerType;
}

void CheckTrigger::Process(int iEvent){
  if(!fReadingData) return;
  if(verb){
    cout<<endl;
    cout<<"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"<<endl;
    cout<<"                                    Event = "<<iEvent<<"                               "<<endl;
    cout<<"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"<<endl;
    cout<<endl;
    cout<<"Run ID = "<<GetRunID()<<endl;
    cout<<"Burst ID = "<<GetBurstID()<<endl;
    cout<<"Event number = "<<GetEventHeader()->GetEventNumber()<<endl;
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"CheckTrigger"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  (void)iEvent;

  PrepareOutputs();

  //trigger time
  double fineTime = 0.;
  bool fControlTriggerOK = false;
  bool fPNNTriggerOK = false;
  fControlTriggerOK = TriggerConditions::GetInstance()->IsControlTrigger(GetL0Data());
  fPNNTriggerOK = TriggerConditions::GetInstance()->L0TriggerBitOn(GetL0Data(), 1);

  if(verb) cout<<"Wanted trigger "<<(fWhichTrigger==1?"Control":(fWhichTrigger==2?"PNN":""))<<endl;
  if(verb) cout<<"Control ? "<<fControlTriggerOK<<" Pnn? "<<fPNNTriggerOK<<endl;
  if(fWhichTrigger==1){ //control
    if(fControlTriggerOK){
      fineTime = GetL0Data()->GetReferenceFineTime();
      fTriggerTime = fineTime*TdcCalib;
      if(!fPNNTriggerOK) fTriggerTime+=1.5;
      fEventPassedTrigger = true;
    };
  }else if(fWhichTrigger==2){ //pnn
    if(fPNNTriggerOK){
      fineTime = GetL0Data()->GetReferenceFineTime();
      fTriggerTime = fineTime*TdcCalib;
      fEventPassedTrigger = true;
    };
  };
  FillHisto("hIsControlTrigger", (int)fControlTriggerOK);
  FillHisto("hIsPNNTrigger", (int)fPNNTriggerOK);
  if(fEventPassedTrigger){
    FillHisto("hTriggerTime", fTriggerTime);
    FillHisto("hFineTime", fineTime);
  };
  if(verb){
    cout<<"trigger time = "<<fTriggerTime<<endl;
    cout<<"event passed trigger? "<<fEventPassedTrigger<<endl;
  };
}

void CheckTrigger::PostProcess(){

}

void CheckTrigger::EndOfBurstUser(){
}

void CheckTrigger::EndOfRunUser(){
}

void CheckTrigger::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void CheckTrigger::DrawPlot(){
}

CheckTrigger::~CheckTrigger(){
}

void CheckTrigger::PrepareOutputs(){
  fTriggerTime = 0.;
  fEventPassedTrigger = false;
  SetOutputState("TriggerTime", kOValid);
  SetOutputState("EventPassedTrigger", kOValid);
}
