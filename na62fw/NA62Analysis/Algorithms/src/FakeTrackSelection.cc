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

FakeTrackSelection::FakeTrackSelection(BaseAnalysis *ba, Analyzer* ana, const std::string &name) : Algorithm(ba, ana, name) 
{
  fCutMinNSTRAWChambers = 4;
  fCutChi2 = 30.;

  EnablePrefix(false);
}

void FakeTrackSelection::Init(){
  BookHisto(new TH1I("hNChambers", "hNChambers", 4, 1, 5));
  BookHisto(new TH1D("hChi2", "hChi2", 50, 0., 50.));
  BookHisto(new TH1I("hCommonHit", "hCommonHit", 2, 0, 2));
  BookHisto(new TH1I("hNFakeTracks", "hNFakeTracks", 10, 0, 10));
  BookHisto(new TH1I("hNNonFakeTracks", "hNNonFakeTracks", 10, 0, 10));
}

void FakeTrackSelection::Process(TRecoSpectrometerEvent *STRAWEvent){
  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"FakeTrackSelection"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  PrepareOutputs(STRAWEvent->GetNCandidates());

  cout<<user()<<"N STRAW candidates "<<STRAWEvent->GetNCandidates()<<endl;
  for(int j=0; j<STRAWEvent->GetNCandidates(); j++){
    if(TestLevel(Verbosity::kUser)){
      cout<<endl;
      cout<<"candidate "<<j<<endl;
    };
    TRecoSpectrometerCandidate* STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(j));
    FillHisto("hNChambers", STRAWCand->GetNChambers());
    cout<<user()<<"N STRAW chambers: "<<STRAWCand->GetNChambers()<<" >= "<<fCutMinNSTRAWChambers<<endl;
    if(STRAWCand->GetNChambers()>=fCutMinNSTRAWChambers){
      cout<<user()<<"Not fake track"<<endl;
      continue;
    }else{
      FillHisto("hChi2", STRAWCand->GetChi2());
      cout<<user()<<"track Chi2: "<<STRAWCand->GetChi2()<<" <= "<<fCutChi2<<endl;
      if(STRAWCand->GetChi2()>fCutChi2){
    	fFakeTracks.at(j) = true;
	cout<<user()<<"Is fake track"<<endl;
    	continue;
      };
      bool commonHit = has_common_hit(STRAWEvent, j);
      FillHisto("hCommonHit", (int)commonHit);
      cout<<user()<<"Has common hit? "<<commonHit<<endl;
      if(commonHit){
	fFakeTracks.at(j) = true;
	cout<<user()<<"Is fake track"<<endl;
    	continue;
      };
      cout<<user()<<"Not fake track"<<endl;
    };
  };
  
  cout<<user()<<endl;
  cout<<user()<<"N fake tracks = "<<std::count(fFakeTracks.begin(), fFakeTracks.end(), true)<<endl;
  FillHisto("hNFakeTracks", std::count(fFakeTracks.begin(), fFakeTracks.end(), true));
  FillHisto("hNNonFakeTracks", std::count(fFakeTracks.begin(), fFakeTracks.end(), false));
}
 
FakeTrackSelection::~FakeTrackSelection(){}

void FakeTrackSelection::PrepareOutputs(int n){
  fFakeTracks.clear();
  for(int i=0; i<n; i++){
    fFakeTracks.push_back(false);
  };
}

 bool FakeTrackSelection::has_common_hit(TRecoSpectrometerEvent *STRAWEvent, int i){
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
