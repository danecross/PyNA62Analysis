#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "SingleTrackEventSelection.hh"
#include "SpectrometerRICHAssociationSingleRing.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "PnnKinematicTailsFunctions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;


SingleTrackEventSelection::SingleTrackEventSelection(Core::BaseAnalysis *ba) : Analyzer(ba, "SingleTrackEventSelection")
{
  RequestTree(new TRecoSpectrometerEvent);
  RequestTree(new TRecoGigaTrackerEvent);

  AddParam("UseGTK", "bool", &UseGTK, false);
  AddParam("CutMaxNTracks", "int", &fCutMaxNTracks, 2);
  AddParam("CutMaxGTKAssoc", "int", &fCutMaxGTKAssoc, 1000);
  AddParam("CutMinCDA", "double", &fCutMinCDA, 30.);

  fMatchingRG = new MatchingRG(ba, this, "MatchingRG");

  fGTKReco = new GigaTrackerRecoAlgorithm(ba, this, "GTKRecoAlgo");
  fGTKReco->SetRedoXYCorr(1);
  fGTKReco->SetFillHistograms(true);
  EnablePrefix(false);
}

void SingleTrackEventSelection::InitOutput(){
  RegisterOutput("EventSelected", &fSelected);
  RegisterOutput("TrackID", &fTrackID);
}

void SingleTrackEventSelection::InitHist(){
  fReadingData = GetIsTree();
  fMatchingRG->SetVerbosity(GetCoreVerbosityLevel(), GetAnalyzerVerbosityLevel());

  if(fReadingData){
    ReconfigureAnalyzer("BestTrackSelection", "Verbose", GetVerbosityLevel());
    ReconfigureAnalyzer("CheckTrigger", "Verbose", GetVerbosityLevel());
    ReconfigureAnalyzer("Preselection", "Verbose", GetVerbosityLevel());

    BookHisto(new TH1I("hCut", "hCut", 30, 1, 31));
    BookHisto(new TH1I("hNTracks", "hNTracks", 20, 0, 20));
    BookHisto(new TH1I("hNBestTracks", "hNBestTracks", 20, 0, 20));
    BookHisto(new TH1I("hOneTrackOutGTKIDSize", "hOneTrackOutGTKIDSize", 10, 0, 10));
    BookHisto(new TH1D("hCDA", "hCDA", 100, 0., 1000.));
    BookHisto(new TH1I("hSingleTrackSelected", "hSingleTrackSelected", 2, 0, 2));
    BookHisto(new TH1I("hNSTRAWChambers_singleTracks", "hNSTRAWChambers_singleTracks", 5, 0, 5));
    BookHisto(new TH1D("hTrackMomentum", "hTrackMomentum", 85, 0., 85000.));
  };
}

void SingleTrackEventSelection::DefineMCSimple(){
}

void SingleTrackEventSelection::StartOfRunUser(){
}

void SingleTrackEventSelection::StartOfBurstUser(){
}

void SingleTrackEventSelection::ProcessSpecialTriggerUser(int, unsigned int){
}

void SingleTrackEventSelection::Process(int){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"SingleTrackEventSelection"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  //prepare output
  PrepareOutputs();
  ValidateOutputs();

  //request output
  OutputState state;
  auto preselectedEvent =
    *(bool*)GetOutput("Preselection.PreselectedEvent", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  cout<<user()<<"Is event preselected? "<<preselectedEvent<<endl;
  if(!preselectedEvent){
    cout<<user()<<"Event is not preselected"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto tTrigger =
    *(double*)GetOutput("CheckTrigger.TriggerTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  cout<<user()<<"Trigger time read = "<<tTrigger<<endl;
  FillHisto("hCut", cutID);
  cutID++; //5

  auto bestTracks =
    *(std::vector<bool>*)GetOutput("BestTrackSelection.BestTracks", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hNBestTracks", std::count(bestTracks.begin(), bestTracks.end(), true));
  cout<<user()<<"N best tracks read = "<<std::count(bestTracks.begin(), bestTracks.end(), true)<<endl;
  FillHisto("hCut", cutID);
  cutID++; //7

  if(std::count(bestTracks.begin(), bestTracks.end(), true)==0) return;
  FillHisto("hCut", cutID);
  cutID++;

  auto bestTrackCHODTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackCHODTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto bestTrackRICHTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackRICHTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto bestTrackKTAGTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackKTAGTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto bestTrackGTKTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackGTKTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto bestTrackLKrTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackLKrTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto GTKNAssocs =
    *(std::vector<int>*)GetOutput("BestTrackSelection.GTKNAssocs", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  cout<<user()<<"all requested outputs are valid"<<endl;

  //reco events
  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  int NTracks = STRAWEvent->GetNCandidates();
  FillHisto("hNTracks", NTracks);
  TRecoSpectrometerCandidate *STRAWCand1;
  TRecoSpectrometerCandidate *STRAWCand2;

  //single track event topology
  cout<<user()<<"N STRAW tracks: "<<" 0 < "<<NTracks<<" <= "<<fCutMaxNTracks<<endl;
  if(NTracks>fCutMaxNTracks) return;
  FillHisto("hCut", cutID);
  cutID++;

  int NbestTracks = std::count(bestTracks.begin(), bestTracks.end(), true);
  cout<<user()<<"N best tracks = "<<NbestTracks<<" > 0 "<<endl;
  if(NTracks==0 || NbestTracks==0) return;
  FillHisto("hCut", cutID);
  cutID++;

  if(NTracks==1){
    cout<<user()<<"1 STRAW track"<<endl;
    if(UseGTK) FillHisto("hOneTrackOutGTKIDSize", GTKNAssocs.at(0));
    if(bestTracks.at(0)==true && ((UseGTK && GTKNAssocs.at(0)<=fCutMaxGTKAssoc) || !UseGTK)){
      fTrackID = 0;
      cout<<user()<<"saving track "<<fTrackID<<endl;
      fSelected = true;
    }else{
      cout<<user()<<"track not saved"<<endl;
      return;
    };
  }else if(NTracks==2){
    cout<<user()<<"2 STRAW tracks"<<endl;
    STRAWCand1 = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(0));
    STRAWCand2 = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(1));
    if((STRAWCand1->GetCharge()!=1) || (STRAWCand2->GetCharge()!=1)) return;
    if(NbestTracks==1 && ((UseGTK && GTKNAssocs.at(find(bestTracks.begin(), bestTracks.end(), true)-bestTracks.begin())<=fCutMaxGTKAssoc) || !UseGTK)){
      if(TestLevel(Verbosity::kUser)){
	cout<<"1 best track"<<endl;
	cout<<"saving track "<<find(bestTracks.begin(), bestTracks.end(), true)-bestTracks.begin()<<endl;
      };
      fTrackID = find(bestTracks.begin(), bestTracks.end(), true)-bestTracks.begin();
      fSelected = true;
    }else if(NbestTracks==2){
      cout<<user()<<"2 best tracks"<<endl;
      double CDA = GetCDA(STRAWCand1, STRAWCand2);
      FillHisto("hCDA", CDA);
      cout<<user()<<"CDA between these two tracks = "<<CDA<<" > "<<fCutMinCDA<<endl;
      if(CDA<=fCutMinCDA) return;
      cout<<user()<<"CDA is large enough"<<endl;
      cout<<user()<<"tTrigger = "<<tTrigger<<endl;

      int closeKTAG = -1;
      if(fabs(bestTrackKTAGTime.at(0)-tTrigger)<fabs(bestTrackKTAGTime.at(1)-tTrigger)){
	closeKTAG = 0;
      }else if(fabs(bestTrackKTAGTime.at(0)-tTrigger)>fabs(bestTrackKTAGTime.at(1)-tTrigger)){
	closeKTAG = 1;
      };
      cout<<user()<<"KTAG times = "<<bestTrackKTAGTime.at(0)<<" "<<bestTrackKTAGTime.at(1)<<endl;
      cout<<user()<<"closer KTAG for track "<<closeKTAG<<endl;

      int closeGTK = -1;
      if(UseGTK){
	double time1 = bestTrackGTKTime.at(0);
	double time2 = bestTrackGTKTime.at(1);
	if(fabs(time1-tTrigger)<fabs(time2-tTrigger)){
	  closeGTK = 0;
	}else if(fabs(time1-tTrigger)>fabs(time2-tTrigger)){
	  closeGTK = 1;
	};
	cout<<user()<<"GTK times = "<<time1<<" "<<time2<<endl;
	cout<<user()<<"closer GTK for track "<<closeGTK<<endl;
      };

      int closeRICH = -1;
      if(fabs(bestTrackRICHTime.at(0)-tTrigger)<fabs(bestTrackRICHTime.at(1)-tTrigger)){
	closeRICH = 0;
      }else if(fabs(bestTrackRICHTime.at(0)-tTrigger)>fabs(bestTrackRICHTime.at(1)-tTrigger)){
	closeRICH = 1;
      };
      cout<<user()<<"RICH times = "<<bestTrackRICHTime.at(0)<<" "<<bestTrackRICHTime.at(1)<<endl;
      cout<<user()<<"closer RICH for track "<<closeRICH<<endl;

      int closeCHOD = -1;
      double tCHOD1 = bestTrackCHODTime.at(0);
      double tCHOD2 = bestTrackCHODTime.at(1);
      if(fabs(tCHOD1-tTrigger)<fabs(tCHOD2-tTrigger)){
      	closeCHOD = 0;
      }else if(fabs(tCHOD1-tTrigger)>fabs(tCHOD2-tTrigger)){
      	closeCHOD = 1;
      };
      cout<<user()<<"CHOD times = "<<bestTrackCHODTime.at(0)<<" "<<bestTrackCHODTime.at(1)<<endl;
      cout<<user()<<"closer CHOD for track "<<closeCHOD<<endl;

      int closeLKr = -1;
      double t1 = bestTrackLKrTime.at(0);
      double t2 = bestTrackLKrTime.at(1);
      if(fabs(t1-tTrigger)<fabs(t2-tTrigger)){
	closeLKr = 0;
      }else if(fabs(t1-tTrigger)>fabs(t2-tTrigger)){
	closeLKr = 1;
      };
      cout<<user()<<"LKr times = "<<bestTrackLKrTime.at(0)<<" "<<bestTrackLKrTime.at(1)<<endl;
      cout<<user()<<"closer LKr for track "<<closeLKr<<endl;

      int counter = 0;
      if((closeCHOD+closeRICH+closeLKr+closeKTAG)==4){
	if(!UseGTK || closeGTK==1){ // (!UseGTK || (UseGTK && closeGTK==1))
	  fTrackID = 1;
	  cout<<user()<<"saving track "<<fTrackID<<endl;
	  fSelected = true;
	  counter++;
	};
      };
      if((fabs(closeCHOD)+fabs(closeRICH)+fabs(closeLKr)+fabs(closeKTAG))==0){
	if(!UseGTK || closeGTK==0){ // (!UseGTK || (UseGTK && closeGTK==0))
	  fTrackID = 0;
	  cout<<user()<<"saving track "<<fTrackID<<endl;
	  fSelected = true;
	  counter++;
	};
      };
      if(counter!=1) return;
    }else{
      return;
    };
  }else{
    return;
  };

  //chosen STRAW candidate
  STRAWCand1 = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(fTrackID));

  if(TestLevel(Verbosity::kUser)){
    cout<<"Single track ID = "<<fTrackID<<endl;
    cout<<"Track momentum "<<STRAWCand1->GetThreeMomentumBeforeMagnet().X()<<" "<<STRAWCand1->GetThreeMomentumBeforeMagnet().Y()<<" "<<STRAWCand1->GetThreeMomentumBeforeMagnet().Z()<<endl;
  };

  //prepare correct GTKreco and fill StrawGTKMatching histograms
  if(fSelected && UseGTK){
    TRecoGigaTrackerEvent* GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
    cout<<user()<<"prepare GTK candidates with ref time "<<bestTrackKTAGTime.at(fTrackID)<<endl;
    fGTKReco->Process(GTKEvent, bestTrackKTAGTime.at(fTrackID));
    cout<<user()<<"prepared"<<endl;

    cout<<user()<<"prepare correct matching"<<endl;
    fMatchingRG->Process(GTKEvent, STRAWCand1, bestTrackKTAGTime.at(fTrackID), bestTrackKTAGTime.at(fTrackID), bestTrackRICHTime.at(fTrackID), 0, "");
    cout<<user()<<"prepared"<<endl;
  };
  if(fSelected) {
      FillHisto("hNSTRAWChambers_singleTracks", STRAWCand1->GetNChambers());
      FillHisto("hTrackMomentum", STRAWCand1->GetMomentum());
  }
  FillHisto("hSingleTrackSelected", (int)fSelected);
  FillHisto("hCut", cutID);
}

void SingleTrackEventSelection::PostProcess(){
}

void SingleTrackEventSelection::EndOfBurstUser(){
}

void SingleTrackEventSelection::EndOfRunUser(){
}

void SingleTrackEventSelection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
    fGTKReco->SaveAllPlots();
  };
}

void SingleTrackEventSelection::DrawPlot(){
}

SingleTrackEventSelection::~SingleTrackEventSelection(){
  delete fGTKReco;
  fGTKReco=nullptr;
  delete fMatchingRG;
  fMatchingRG=nullptr;
}

void SingleTrackEventSelection::PrepareOutputs(){
  fSelected = false;
  fTrackID = -1;
  SetOutputState("EventSelected", kOInvalid);
  SetOutputState("TrackID", kOInvalid);
}

void SingleTrackEventSelection::ValidateOutputs(){
  SetOutputState("EventSelected", kOValid);
  SetOutputState("TrackID", kOValid);
}
