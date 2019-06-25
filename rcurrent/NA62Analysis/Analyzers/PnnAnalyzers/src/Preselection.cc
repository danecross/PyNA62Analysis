#include "Preselection.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;


Preselection::Preselection(Core::BaseAnalysis *ba) : Analyzer(ba, "Preselection")
{
  RequestTree(new TRecoCHODEvent);
  RequestTree(new TRecoLKrEvent);
  RequestTree(new TRecoSpectrometerEvent);

  AddParam("Verbosity", "bool", &verb, false);
  AddParam("MinCHODHits", "int", &fMinCHODHits, 1);
  AddParam("MinLKrHits", "int", &fMinLKrHits, 1);
  AddParam("MaxLKrHits", "int", &fMaxLKrHits, 2000);
  AddParam("MinSTRAWCandidates", "int", &fMinSTRAWCandidates, 1);
  AddParam("MaxSTRAWCandidates", "int", &fMaxSTRAWCandidates, 10);
}

void Preselection::InitOutput(){
  RegisterOutput("PreselectedEvent", &fPreselectedEvent);
}

void Preselection::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 20, 1, 21));
    BookHisto(new TH1I("hNCHODHits", "hNCHODHits", 2000, 0, 2000));
    BookHisto(new TH1I("hNLKrHits", "hNLKrHits", 2000, 0, 2000));
    BookHisto(new TH1I("hNSTRAWCandidates", "hNSTRAWCandidates", 20, 0, 20));
  };
}

void Preselection::DefineMCSimple(){}

void Preselection::StartOfRunUser(){}

void Preselection::StartOfBurstUser(){}

void Preselection::ProcessSpecialTriggerUser(int, unsigned int){}

void Preselection::Process(int){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"Preselection"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  PrepareOutputs();
  ValidateOutputs();

  OutputState state;
  auto eventPassedTrigger =
    *(bool*)GetOutput("CheckTrigger.EventPassedTrigger", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  if(verb) cout<<"Did event pass trigger? "<<eventPassedTrigger<<endl;
  if(!eventPassedTrigger){
    if(verb) cout<<"Event did not pass the trigger"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  int qualityMask = GetEventHeader()->GetEventQualityMask();
  if(verb) cout<<"Event quality mask "<<qualityMask<<" = 0 "<<endl;
  if(qualityMask!=0) return;
  FillHisto("hCut", cutID);
  cutID++;

  TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();
  FillHisto("hNCHODHits", CHODEvent->GetNHits());
  if(verb) cout<<"N CHOD hits: "<<CHODEvent->GetNHits()<<" >= "<<fMinCHODHits<<endl;
  if(CHODEvent->GetNHits()<fMinCHODHits) return;
  FillHisto("hCut", cutID);
  cutID++;

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  FillHisto("hNLKrHits", LKrEvent->GetNHits());
  if(verb) cout<<"N LKr hits: "<<fMinLKrHits<<" <= "<<LKrEvent->GetNHits()<<" <= "<<fMaxLKrHits<<endl;
  if(LKrEvent->GetNHits()>fMaxLKrHits || LKrEvent->GetNHits()<fMinLKrHits) return;
  FillHisto("hCut", cutID);
  cutID++;

  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  FillHisto("hNSTRAWCandidates", STRAWEvent->GetNCandidates());
  if(verb) cout<<"N STRAW candidates: "<<fMinSTRAWCandidates<<" <= "<<STRAWEvent->GetNCandidates()<<" <= "<<fMaxSTRAWCandidates<<endl;
  if(STRAWEvent->GetNCandidates()<fMinSTRAWCandidates || STRAWEvent->GetNCandidates()>fMaxSTRAWCandidates) return;
  FillHisto("hCut", cutID);

  fPreselectedEvent = true;
  if(verb) cout<<"Preselected event: "<<(int)fPreselectedEvent<<endl;
}

void Preselection::PostProcess(){}

void Preselection::EndOfBurstUser(){}

void Preselection::EndOfRunUser(){}

void Preselection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void Preselection::DrawPlot(){}

Preselection::~Preselection(){}

void Preselection::PrepareOutputs(){
  fPreselectedEvent = false;
  SetOutputState("PreselectedEvent", kOInvalid);
}

void Preselection::ValidateOutputs(){
  SetOutputState("PreselectedEvent", kOValid);
}
