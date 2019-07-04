#include "EventCleaning.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GeometricAcceptance.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;


EventCleaning::EventCleaning(Core::BaseAnalysis *ba) : Analyzer(ba, "EventCleaning")
{
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");

  AddParam("SpecificCharge", "int", &fSpecificCharge, 1);
  AddParam("CutSlopeDiffX", "double", &fCutSlopeDiffX, 0.0003);
  AddParam("CutSlopeDiffY", "double", &fCutSlopeDiffY, 0.001);
  AddParam("CutPatternRecognQuality", "double", &fCutPatternRecognQuality, 4.);
  AddParam("CutMinNHits", "int", &fCutMinNHits, 15);
  AddParam("CutMaxNHits", "int", &fCutMaxNHits, 42);
  AddParam("kMinX", "double", &fkMinX, 0.0012);
  AddParam("qMinX", "double", &fqMinX, -93.9);
  AddParam("kMaxX", "double", &fkMaxX, 0.00122);
  AddParam("qmaxX", "double", &fqMaxX, -148.53);
  AddParam("kMinY", "double", &fkMinY, 0.000112);
  AddParam("qMinY", "double", &fqMinY, 3.6);
  AddParam("kMaxY", "double", &fkMaxY, -0.00009464);
  AddParam("qmaxY", "double", &fqMaxY, -5.4);
  AddParam("CutTimeDiffCHODGTK", "double", &fCutTimeDiffCHODGTK, 1.1);
  AddParam("CutTimeDiffKTAGGTK", "double", &fCutTimeDiffKTAGGTK, 1.1);

  EnablePrefix(false);
}

void EventCleaning::InitOutput(){
  RegisterOutput("GoodEvent", &fGoodEvent);
}

void EventCleaning::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 20, 1, 21));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 20, 1, 21, 85, 0., 85000.));
    BookHisto(new TH1I("hTrackCharge", "hTrackCharge", 3, -1, 2));
    BookHisto(new TH2D("hSlopeDiffYvsX", "hSlopeDiffYvsX", 100, 0., 0.001, 100, 0., 0.01));
    BookHisto(new TH1D("hCombinationTotalQuality", "hCombinationTotalQuality", 100, 0., 10.));
    BookHisto(new TH1I("hNHitsInCandidate", "hNHitsInCandidate", 100, 0, 100));
    BookHisto(new TH1D("hVertexZ", "hVertexZ", 200, 100000., 200000.));
    BookHisto(new TH2D("hVertexZvsX", "hVertexZvsX", 200, -500., 500., 200, 100000., 200000.));
    BookHisto(new TH2D("hVertexZvsY", "hVertexZvsY", 200, -500., 500., 200, 100000., 200000.));
    BookHisto(new TH1D("hTimeDiffCHODGTK", "hTimeDiffCHODGTK", 200, -10., 10.));
    BookHisto(new TH1D("hTimeDiffKTAGGTK", "hTimeDiffKTAGGTK", 200, -10., 10.));
  };
}

void EventCleaning::DefineMCSimple(){}

void EventCleaning::StartOfRunUser(){}

void EventCleaning::StartOfBurstUser(){}

void EventCleaning::ProcessSpecialTriggerUser(int, unsigned int){}

void EventCleaning::Process(int){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"EventCleaning"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

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

  auto isSingleTrack =
    *(bool*)GetOutput("SingleTrackEventSelection.EventSelected", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  cout<<user()<<"Is single track event? "<<isSingleTrack<<endl;
  if(!isSingleTrack) return;
  FillHisto("hCut", cutID);
  cutID++;

  auto trackID =
    *(int*)GetOutput("SingleTrackEventSelection.TrackID", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  cout<<user()<<"Track ID read = "<<trackID<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  auto trackCHODTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackCHODTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double tCHOD = trackCHODTime.at(trackID);
  cout<<user()<<"CHOD time = "<<tCHOD<<endl;

  auto trackKTAGTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackKTAGTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double tKTAG = trackKTAGTime.at(trackID);
  cout<<user()<<"KTAG time = "<<tKTAG<<endl;

  auto trackGTKTime =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackGTKTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double tGTK = trackGTKTime.at(trackID);
  cout<<user()<<"GTK time = "<<tGTK<<endl;

  auto Vertex =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocVertex", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  TVector3 vertex = Vertex.at(trackID);
  cout<<user()<<"vertex = ("<<vertex.X()<<","<<vertex.Y()<<","<<vertex.Z()<<")"<<endl;
  FillHisto("hVertexZ", vertex.Z());

  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate* STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(trackID));
  FillHisto("hTrackMomentumVsCut", cutID-1, STRAWCand->GetMomentum());

  //track charge
  FillHisto("hTrackCharge", STRAWCand->GetCharge());
  cout<<user()<<"Track charge: "<<STRAWCand->GetCharge()<<" = "<<fSpecificCharge<<endl;
  if(STRAWCand->GetCharge()!=fSpecificCharge) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //slopes before magnet before and after fit
  TVector3 momA = STRAWCand->GetThreeMomentumBeforeMagnet();
  double slopeXB = STRAWCand->GetSlopeXBeforeFit();
  double slopeYB = STRAWCand->GetSlopeYBeforeFit();
  double slopeXA = momA.X()/momA.Z();
  double slopeYA = momA.Y()/momA.Z();
  FillHisto("hSlopeDiffYvsX", fabs(slopeXB - slopeXA), fabs(slopeYB - slopeYA));
  cout<<user()<<"Slope X difference: "<<fabs(slopeXB - slopeXA)<<" <= "<<fCutSlopeDiffX<<endl;
  cout<<user()<<"Slope Y difference: "<<fabs(slopeYB - slopeYA)<<" <= "<<fCutSlopeDiffY<<endl;
  if(fabs(slopeXB - slopeXA)>fCutSlopeDiffX || fabs(slopeYB - slopeYA)>fCutSlopeDiffY) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //Combination total quality of STRAW candidate
  FillHisto("hCombinationTotalQuality", STRAWCand->GetCombinationTotalQuality());
  cout<<user()<<"Track total quality: "<<STRAWCand->GetCombinationTotalQuality()<<" <= "<<fCutPatternRecognQuality<<endl;
  if(STRAWCand->GetCombinationTotalQuality()>fCutPatternRecognQuality) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //N hits in STRAW candidate
  FillHisto("hNHitsInCandidate", STRAWCand->GetNHits());
  cout<<user()<<"N STRAW hits: "<<fCutMinNHits<<" < "<<STRAWCand->GetNHits()<<" < "<<fCutMaxNHits<<endl;
  if(STRAWCand->GetNHits()<=fCutMinNHits || STRAWCand->GetNHits()>=fCutMaxNHits) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //vertex X vs Z
  double x = vertex.X();
  double y = vertex.Y();
  double z = vertex.Z();
  FillHisto("hVertexZvsX", x, z);
  cout<<user()<<"Vertex XvsZ: "<<(fkMaxX*z+fqMaxX)<<" < "<<x<<" < "<<(fkMinX*z+fqMinX)<<endl;
  if(x>(fkMinX*z+fqMinX) || x<(fkMaxX*z+fqMaxX)) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //vertex Y vs Z
  FillHisto("hVertexZvsY", y, z);
  cout<<user()<<"Vertex YvsZ: "<<(fkMaxY*z+fqMaxY)<<" < "<<y<<" < "<<(fkMinY*z+fqMinY)<<endl;
  if(y>(fkMinY*z+fqMinY) || y<(fkMaxY*z+fqMaxY)) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  // time diff CHOD-GTK
  FillHisto("hTimeDiffCHODGTK", tCHOD - tGTK);
  cout<<user()<<"time diff CHOD GTK: "<<fabs(tCHOD - tGTK)<<" < "<<fCutTimeDiffCHODGTK<<endl;
  if(fabs(tCHOD - tGTK)>=fCutTimeDiffCHODGTK) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

 // time diff KTAG-GTK
  FillHisto("hTimeDiffKTAGGTK", tKTAG - tGTK);
  cout<<user()<<"time diff KTAG GTK: "<<fabs(tKTAG - tGTK)<<" < "<<fCutTimeDiffKTAGGTK<<endl;
  if(fabs(tKTAG - tGTK)>=fCutTimeDiffKTAGGTK) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);

  fGoodEvent = true;
  cout<<user()<<"good event"<<endl;
}

void EventCleaning::PostProcess(){}

void EventCleaning::EndOfBurstUser(){}

void EventCleaning::EndOfRunUser(){}

void EventCleaning::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void EventCleaning::DrawPlot(){}

EventCleaning::~EventCleaning(){}

void EventCleaning::PrepareOutputs(){
  fGoodEvent = false;
  SetOutputState("GoodEvent", kOInvalid);
}

void EventCleaning::ValidateOutputs(){
  SetOutputState("GoodEvent", kOValid);
}
