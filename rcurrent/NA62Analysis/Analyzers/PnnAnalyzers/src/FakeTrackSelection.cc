#include "FakeTrackSelection.hh"

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

FakeTrackSelection::FakeTrackSelection(Core::BaseAnalysis *ba) : Analyzer(ba, "FakeTrackSelection")
{
  RequestTree(new TRecoSpectrometerEvent);

  AddParam("CutMinNSTRAWChambers", "int", &fCutMinNSTRAWChambers, 4);
  AddParam("CutChi2", "double", &fCutChi2, 30.);
  AddParam("Verbosity", "bool", &verb, false);
}

void FakeTrackSelection::InitOutput(){
  RegisterOutput("FakeTracks", &fFakeTracks);
}

void FakeTrackSelection::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hNChambers", "hNChambers", 4, 1, 5));
    BookHisto(new TH1D("hChi2", "hChi2", 50, 0., 50.));
    BookHisto(new TH1I("hCommonHit", "hCommonHit", 2, 0, 2));
    BookHisto(new TH1I("hNFakeTracks", "hNFakeTracks", 10, 0, 10));
    BookHisto(new TH1I("hNNonFakeTracks", "hNNonFakeTracks", 10, 0, 10));
  };
}

void FakeTrackSelection::DefineMCSimple(){}

void FakeTrackSelection::StartOfRunUser(){}

void FakeTrackSelection::StartOfBurstUser(){}

void FakeTrackSelection::ProcessSpecialTriggerUser(int, unsigned int){}

void FakeTrackSelection::Process(int){
  if(!fReadingData) return;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"FakeTrackSelection"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  PrepareOutputs(STRAWEvent->GetNCandidates());
  ValidateOutputs();

  if(verb){
    cout<<"N STRAW candidates "<<STRAWEvent->GetNCandidates()<<endl;
  };
  for(int j=0; j<STRAWEvent->GetNCandidates(); j++){
    if(verb){
      cout<<endl;
      cout<<"candidate "<<j<<endl;
    };
    TRecoSpectrometerCandidate* STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(j));
    FillHisto("hNChambers", STRAWCand->GetNChambers());
    if(verb) cout<<"N STRAW chambers: "<<STRAWCand->GetNChambers()<<" >= "<<fCutMinNSTRAWChambers<<endl;
    if(STRAWCand->GetNChambers()>=fCutMinNSTRAWChambers){
      if(verb) cout<<"Not fake track"<<endl;
      continue;
    }else{
      FillHisto("hChi2", STRAWCand->GetChi2());
      if(verb) cout<<"track Chi2: "<<STRAWCand->GetChi2()<<" <= "<<fCutChi2<<endl;
      if(STRAWCand->GetChi2()>fCutChi2){
    	fFakeTracks.at(j) = true;
	if(verb) cout<<"Is fake track"<<endl;
    	continue;
      };
      bool commonHit = has_common_hit(j);
      FillHisto("hCommonHit", (int)commonHit);
      if(verb) cout<<"Has common hit? "<<commonHit<<endl;
      if(commonHit){
	fFakeTracks.at(j) = true;
	if(verb) cout<<"Is fake track"<<endl;
    	continue;
      };
      if(verb) cout<<"Not fake track"<<endl;
    };
  };

  if(verb) cout<<endl;
  if(verb) cout<<"N fake tracks = "<<std::count(fFakeTracks.begin(), fFakeTracks.end(), true)<<endl;
  FillHisto("hNFakeTracks", std::count(fFakeTracks.begin(), fFakeTracks.end(), true));
  FillHisto("hNNonFakeTracks", std::count(fFakeTracks.begin(), fFakeTracks.end(), false));
}

void FakeTrackSelection::PostProcess(){}

void FakeTrackSelection::EndOfBurstUser(){}

void FakeTrackSelection::EndOfRunUser(){}

void FakeTrackSelection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void FakeTrackSelection::DrawPlot(){}

FakeTrackSelection::~FakeTrackSelection(){}

void FakeTrackSelection::PrepareOutputs(int n){
  fFakeTracks.clear();
  for(int i=0; i<n; i++){
    fFakeTracks.push_back(false);
  };

  SetOutputState("FakeTracks", kOInvalid);
}

void FakeTrackSelection::ValidateOutputs(){
  SetOutputState("FakeTracks", kOValid);
}

bool FakeTrackSelection::has_common_hit(int i){
  TRecoSpectrometerCandidate* STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(i));
  int* hitIDs = STRAWCand->GetHitsIndexes();
  std::sort(hitIDs, hitIDs+STRAWCand->GetNHits());
  TRecoSpectrometerCandidate* sc;
  int* hID;
  int ncommon = 0;
  for(int j=0; j<STRAWEvent->GetNCandidates(); j++){
    if(j==i) continue;
    sc = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(j));
    hID = sc->GetHitsIndexes();
    std::sort(hID, hID+sc->GetNHits());
    int* n1 = hitIDs+STRAWCand->GetNHits();
    int* n2 = hID+sc->GetNHits();
    while((hitIDs!=n1) && (hID!=n2)){
      if(*hitIDs<*hID){
    	++hitIDs;
      }else if(*hID<*hitIDs){
    	++hID;
      }else{
        ncommon++;
	++hitIDs;
	++hID;
      };
    };
  };

  if(ncommon>1){
    return true;
  }else{
    return false;
  };
}
