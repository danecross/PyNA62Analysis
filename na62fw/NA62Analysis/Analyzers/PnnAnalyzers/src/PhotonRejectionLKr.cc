#include "PhotonRejectionLKr.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "PnnFunctions.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

PhotonRejectionLKr::PhotonRejectionLKr(Core::BaseAnalysis *ba) : Analyzer(ba, "PhotonRejectionLKr")
{
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");

  AddParam("Verbosity", "bool", &verb, false);
  AddParam("CutMinDistLKrClusterTrack", "double", &fCutMinDistLKrClusterTrack, 100.);
  AddParam("OffsetLKrStandardCandidate", "double", &fOffsetLKrStandardCandidate, -1.62); //[ns]
  AddParam("OffsetLKrAuxCandidate", "double", &fOffsetLKrAuxCandidate, 0.); //[ns]
  AddParam("LKrSigmaA", "double", &fLKrSigmaA, 0.56);
  AddParam("LKrSigmaB", "double", &fLKrSigmaB, 1.53);
  AddParam("LKrSigmaC", "double", &fLKrSigmaC, 0.233);
  AddParam("LKrClusterEnergy1", "double", &fLKrClusterEnergy1, 1000.);
  AddParam("LKrClusterEnergy2", "double", &fLKrClusterEnergy2, 2000.);
  AddParam("LKrClusterEnergy3", "double", &fLKrClusterEnergy3, 10000.);
  AddParam("LKrClusterEnergy4", "double", &fLKrClusterEnergy4, 15000.);
  AddParam("LKrClusterTimeDiff1", "double", &fLKrClusterTimeDiff1, 5.);
  AddParam("LKrClusterTimeDiff2", "double", &fLKrClusterTimeDiff2, 2.5);
  AddParam("LKrClusterTimeDiffSigma1", "double", &fLKrClusterTimeDiffSigma1, 5.);
  AddParam("LKrClusterTimeDiffSigma2", "double", &fLKrClusterTimeDiffSigma2, 15.);
  AddParam("LKrClusterTimeDiffSigma3", "double", &fLKrClusterTimeDiffSigma3, 70.);
  AddParam("LKrClusterTimeDiffSigma4", "double", &fLKrClusterTimeDiffSigma4, 3.);
  AddParam("LKrClusterMinPosX", "double", &fLKrClusterMinPosX, 0.);
  AddParam("LKrClusterMaxPosX", "double", &fLKrClusterMaxPosX, 600.);
  AddParam("LKrClusterMaxPosY", "double", &fLKrClusterMaxPosY, 300.);
  AddParam("LKrClusterTimeDiff2", "double", &fLKrClusterTimeDiff2, 2.5);
  fZLKr = GeometricAcceptance::GetInstance()->GetZLKr();
  fLKrAuxClusterReco = new LKrAuxClusterReco();
}

void PhotonRejectionLKr::InitOutput(){
  RegisterOutput("Photons", &fPhotons);
}

void PhotonRejectionLKr::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 30, 1, 31));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 20, 1, 21, 85, 0., 85000.));
    BookHisto(new TH1D("hPosDiffTrackLKr", "hPosDiffTrackLKr", 500, 0., 500.));
    BookHisto(new TH2D("hTimeDiffLKrTrackVSClusterEnergy", "hTimeDiffLKrTrackVSClusterEnergy", 800, 0., 80000., 200, -130., 70.));
    BookHisto(new TH2D("hClusPosXvsClusterEnergy", "hClusPosXvsClusterEnergy", 800, 0., 80000., 400, -2000., 2000.));
    BookHisto(new TH2D("hClusPosYvsClusterEnergy", "hClusPosYvsClusterEnergy", 800, 0., 80000., 400, -2000., 2000.));
    BookHisto(new TH2D("hTimeDiff36LKrTrackVSClusterEnergy", "hTimeDiff36LKrTrackVSClusterEnergy", 800, 0., 80000., 200, -130., 70.));
    BookHisto(new TH1I("hPhotonInLKr", "hPhotonInLKr", 2, 0, 2));
    BookHisto(new TH2D("hTimeDiffAuxLKrTrackVSClusterEnergy", "hTimeDiffAuxLKrTrackVSClusterEnergy", 800, 0., 80000., 200, -130., 70.));
    BookHisto(new TH2D("hAuxClusPosXvsClusterEnergy", "hClusPosXvsClusterEnergy", 800, 0., 80000., 400, -2000., 2000.));
    BookHisto(new TH2D("hAuxClusPosYvsClusterEnergy", "hClusPosYvsClusterEnergy", 800, 0., 80000., 400, -2000., 2000.));
    BookHisto(new TH2D("hTimeDiffAux36LKrTrackVSClusterEnergy", "hTimeDiff36LKrTrackVSClusterEnergy", 800, 0., 80000., 200, -130., 70.));
    BookHisto(new TH1I("hPhotonInAuxLKr", "hPhotonInAuxLKr", 2, 0, 2));
  };
}

void PhotonRejectionLKr::DefineMCSimple(){
}

void PhotonRejectionLKr::StartOfRunUser(){
}

void PhotonRejectionLKr::StartOfBurstUser(){
}

void PhotonRejectionLKr::ProcessSpecialTriggerUser(int, unsigned int){
}

void PhotonRejectionLKr::Process(int iEvent){
  if(!GetIsTree()) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"PhotonRejectionLKr"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };
  (void)iEvent;

  PrepareOutputs();

  //request output
  OutputState state;
  auto preselectedEvent =
    *(bool*)GetOutput("Preselection.PreselectedEvent", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  if(verb) cout<<"Is event preselected? "<<preselectedEvent<<endl;
  if(!preselectedEvent){
    if(verb) cout<<"Event is not preselected"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto isSingleTrack =
    *(bool*)GetOutput("SingleTrackEventSelection.EventSelected", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  if(verb) cout<<"Is single track event? "<<isSingleTrack<<endl;
  if(!isSingleTrack) return;
  FillHisto("hCut", cutID);
  cutID++;

  int trackID =
    *(int*)GetOutput("SingleTrackEventSelection.TrackID", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  if(verb) cout<<"Track ID read = "<<trackID<<endl;
  if(trackID==-1) return;
  FillHisto("hCut", cutID);
  cutID++;

  auto trackTimes =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double trackTime = trackTimes.at(trackID);
  if(verb) cout<<"track time read = "<<trackTime<<endl;

  auto lkrAssocPositions =
    *(std::vector<TVector2>*)GetOutput("BestTrackSelection.LKrAssocPosition", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  TVector2 lkrAssocPos = lkrAssocPositions.at(trackID);

  ValidateOutputs();

  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate *STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(trackID));
  FillHisto("hTrackMomentumVsCut", cutID-1, STRAWCand->GetMomentum());

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  TVector2 trackAtLKr(STRAWCand->xAt(fZLKr), STRAWCand->yAt(fZLKr));
  if(verb) cout<<"track position at LKr: "<<"("<<trackAtLKr.X()<<", "<<trackAtLKr.Y()<<")"<<endl;
  bool photonInLKr = false;
  if(verb) cout<<"Standard reco: going over all "<<LKrEvent->GetNCandidates()<<" LKr candidates"<<endl;
  for(int i=0; i<LKrEvent->GetNCandidates(); i++){
    if(verb) cout<<"candidate "<<i<<endl;
    TRecoLKrCandidate *LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
    TVector2 clusPos(LKrCand->GetClusterX(), LKrCand->GetClusterY());
    FillHisto("hPosDiffTrackLKr", (trackAtLKr-clusPos).Mod());
    if(verb){
      cout<<"cluster position: "<<"("<<clusPos.X()<<", "<<clusPos.Y()<<")"<<endl;
      cout<<"track-cluster distance: "<<(trackAtLKr-clusPos).Mod()<<" >= "<<fCutMinDistLKrClusterTrack<<endl;
    };
    if((lkrAssocPos-clusPos).Mod()<fCutMinDistLKrClusterTrack || (trackAtLKr-clusPos).Mod()<fCutMinDistLKrClusterTrack){
      if(verb) cout<<"cluster "<<i<<" corresponds to track"<<endl;
      continue;
    };
    double Eclus = LKrCand->GetClusterEnergy();
    double deltaT = (LKrCand->GetTime() - trackTime + fOffsetLKrStandardCandidate);
    double sigma = fLKrSigmaA + fLKrSigmaB/(Eclus/1000.) - fLKrSigmaC/sqrt(Eclus/1000.);
    FillHisto("hTimeDiffLKrTrackVSClusterEnergy", Eclus, deltaT);
    FillHisto("hClusPosXvsClusterEnergy", Eclus, clusPos.X());
    FillHisto("hClusPosYvsClusterEnergy", Eclus, clusPos.Y());
    FillHisto("hTimeDiff36LKrTrackVSClusterEnergy", Eclus, deltaT+36.);
    if(GetWithMC()){
      photonInLKr = true;
      break;
    };
    photonInLKr = EvaluateLKrCluster(Eclus, deltaT, sigma, fLKrClusterEnergy1, fLKrClusterEnergy2, fLKrClusterEnergy3, fLKrClusterEnergy4, fLKrClusterTimeDiff1, fLKrClusterTimeDiff2, fLKrClusterTimeDiffSigma1, fLKrClusterTimeDiffSigma2, fLKrClusterTimeDiffSigma3, verb);
    if(photonInLKr) break;
  };
  FillHisto("hPhotonInLKr", (int)photonInLKr);
  if(verb) cout<<"Found photon in LKr? "<<photonInLKr<<endl;
  if(photonInLKr) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //auxiliary
  fLKrAuxClusterReco->SetfCutCellDistance(5.);
  fLKrAuxClusterReco->FindClusters(trackTime, LKrEvent);
  if(verb){
    cout<<endl;
    cout<<"Auxuliary Cluster Reconstruction: going over all aux clusters"<<endl;
  };
  for(int i=0; i<fLKrAuxClusterReco->GetNClusters(); i++){
    if(GetWithMC()) continue;
    TVector2 clusPos(fLKrAuxClusterReco->GetCandidate(i)->GetClusterX(), fLKrAuxClusterReco->GetCandidate(i)->GetClusterY());
    if(verb){
      cout<<"cluster "<<i<<endl;
      cout<<"cluster position: "<<"("<<clusPos.X()<<", "<<clusPos.Y()<<")"<<endl;
      cout<<"track-cluster distance: "<<(trackAtLKr-clusPos).Mod()<<" >= "<<fCutMinDistLKrClusterTrack<<endl;
    };
    double Eclus = fLKrAuxClusterReco->GetCandidate(i)->GetClusterEnergy();
    double deltaT = (fLKrAuxClusterReco->GetCandidate(i)->GetTime() - trackTime + fOffsetLKrAuxCandidate);
    double sigma = fLKrSigmaA + fLKrSigmaB/(Eclus/1000.) - fLKrSigmaC/sqrt(Eclus/1000.);
    FillHisto("hTimeDiffAuxLKrTrackVSClusterEnergy", Eclus, deltaT);
    FillHisto("hAuxClusPosXvsClusterEnergy", Eclus, clusPos.X());
    FillHisto("hAuxClusPosYvsClusterEnergy", Eclus, clusPos.Y());
    FillHisto("hTimeDiffAux36LKrTrackVSClusterEnergy", Eclus, deltaT+36.);
    if((lkrAssocPos-clusPos).Mod()<fCutMinDistLKrClusterTrack || (trackAtLKr-clusPos).Mod()<fCutMinDistLKrClusterTrack){
      if(verb) cout<<"cluster corresponds to track"<<endl;
      continue;
    };
    if(Eclus<fLKrClusterEnergy1){
      if(verb) cout<<"cluster energy too low"<<endl;
      continue;
    };
    photonInLKr = EvaluateLKrCluster(Eclus, deltaT, sigma, fLKrClusterEnergy1, fLKrClusterEnergy2, fLKrClusterEnergy3, fLKrClusterEnergy4, fLKrClusterTimeDiff1, fLKrClusterTimeDiff2, fLKrClusterTimeDiffSigma1, fLKrClusterTimeDiffSigma2, fLKrClusterTimeDiffSigma3, verb);
    if(photonInLKr) break;
  };
  FillHisto("hPhotonInAuxLKr", (int)photonInLKr);
  if(verb) cout<<"Found photon in LKr using AuxReco? "<<photonInLKr<<endl;
  if(photonInLKr) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  fPhotons = false;
}

void PhotonRejectionLKr::PostProcess(){
}

void PhotonRejectionLKr::EndOfBurstUser(){
}

void PhotonRejectionLKr::EndOfRunUser(){
}

void PhotonRejectionLKr::EndOfJobUser(){
  SaveAllPlots();
}

void PhotonRejectionLKr::DrawPlot(){
}

PhotonRejectionLKr::~PhotonRejectionLKr(){
}

void PhotonRejectionLKr::PrepareOutputs(){
  fPhotons = true;
  SetOutputState("Photons", kOInvalid);
}

void PhotonRejectionLKr::ValidateOutputs(){
  SetOutputState("Photons", kOValid);
}
