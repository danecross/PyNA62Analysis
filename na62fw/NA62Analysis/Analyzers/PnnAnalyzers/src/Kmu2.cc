#include "Kmu2.hh"

#include <stdlib.h>
#include <iostream>
#include <bitset>
#include <TChain.h>
#include "BeamParameters.hh"
#include "RICHParameters.hh"
#include "GeometricAcceptance.hh"
#include "TriggerConditions.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "PnnFunctions.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;


Kmu2::Kmu2(Core::BaseAnalysis *ba) : Analyzer(ba, "Kmu2")
{
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");

  AddParam("UseGTK", "bool", &UseGTK, false);
  AddParam("CutTrackMomMin", "double", &fCutTrackMomMin, 15000.);
  AddParam("CutTrackMomMax", "double", &fCutTrackMomMax, 35000.);
  AddParam("CutMatchedGTKQuality", "double", &fCutMatchedGTKQuality, 20.);
  AddParam("CutEoP", "double", &fCutEoP, 0.8);
  AddParam("CutTimeDiffMUV3", "double", &fCutTimeDiffMUV3, 7.);
  AddParam("CutMaxTotalExtraEnergy", "double", &fMaxTotalExtraEnergy, 5000.);
  AddParam("CutMuonProbability", "double", &fCutMuonProbability, 0.99);
  AddParam("CutDMIP", "double", &fCutDMIP, 1.);

  fZMUV3 = GeometricAcceptance::GetInstance()->GetZMUV3();
  fSegmentsAlgo = make_unique<StrawSegmentAlgorithm>(ba, this, "StrawSegmentAlgorithm");

  EnablePrefix(false);
}

void Kmu2::InitOutput(){
  RegisterOutput("Kmu2EventSelected", &fEventSelected);
  RegisterOutput("Kmu2NomTrackMomentum", &fKmu2NomTrackMom);
  RegisterOutput("Kmu2NomKaonMomentum", &fKmu2NomKaonMom);
  RegisterOutput("Kmu2TrackMomentum", &fKmu2TrackMom);
  RegisterOutput("Kmu2KaonMomentum", &fKmu2KaonMom);
  RegisterOutput("Kmu2KaonID", &fKmu2KaonID);
  RegisterOutput("Kmu2TrackID", &fKmu2TrackID);
  RegisterOutput("Kmu2RICHMomentum", &fKmu2RICHMomentum);
}

void Kmu2::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 40, 1, 41));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 40, 1, 41, 85, 0., 85000.));
    BookHisto(new TH2D("hMissM2VsCut", "hMissM2VsCut", 40, 1, 41, 280, -0.5, 0.2));
    BookHisto(new TH1D("hTrackMomentum", "hTrackMomentum", 70, 0., 70000.));
    BookHisto(new TH1D("hTrackTime", "hTrackTime", 100, -50., 50.));
    BookHisto(new TH1D("hTimeDiffTrackTrigger", "hTimeDiffTrackTrigger", 400, -20., 20.));
    BookHisto(new TH1D("hKaonMom", "hKaonMom", 300, 60000., 90000.));
    BookHisto(new TH1D("hGTKMatchedQuality1", "hGTKMatchedQuality1", 500, 0., 1.));
    BookHisto(new TH2D("hMissM2VSvertexZ", "hMissM2VSvertexZ", 120, 80000., 200000., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSGTKMatchedQuality1", "hMissM2VSGTKMatchedQuality1", 500, 0., 1., 500, -0.2, 0.2));
    BookHisto(new TH1D("hMissM2", "hMissM2", 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSmomentum", "hMissM2VSmomentum", 700, 0., 70000., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSNtracks", "hMissM2VSNtracks", 5, 0, 5, 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSMatchedGTKChi2", "hMissM2VSMatchedGTKChi2", 100, 0., 50., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSTrackChi2", "hMissM2VSTrackChi2", 100, 0., 50., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSTrackLeadingTime", "hMissM2VSTrackLeadingTime", 100, -50., 50., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSTrackTotalQuality", "hMissM2VSTrackTotalQuality", 100, 0., 5, 500, -0.2, 0.2));
    BookHisto(new TH2D("hGTKMatchedTimeGTK3VSMeanTimeGTK12", "hGTKMatchedTimeGTK3VSMeanTimeGTK12", 750, -25., 50., 750, -25., 50.));
    BookHisto(new TH2D("hMissM2VSTimeGTKDiff312", "hMissM2VSTimeGTKDiff312", 100, -5., 5., 500, -0.2, 0.2));
  };
}

void Kmu2::DefineMCSimple(){}

void Kmu2::StartOfRunUser(){}

void Kmu2::StartOfBurstUser(){}

void Kmu2::Process(int iEvent){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"Kmu2"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  (void)iEvent;

  PrepareOutputs();
  ValidateOutputs();

  //candidates
  TRecoSpectrometerEvent* SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate *STRAWCand;
  TRecoGigaTrackerEvent *GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
  TRecoGigaTrackerCandidate *GTKCand;
  fMUV3Event = GetEvent<TRecoMUV3Event>();

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
  cutID++;//5

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

  int trackID =
    *(int*)GetOutput("SingleTrackEventSelection.TrackID", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  cout<<user()<<"Track ID read = "<<trackID<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  auto trackTimes =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double trackTime = trackTimes.at(trackID);
  cout<<user()<<"track time = "<<trackTime<<endl;

  auto dMIP =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.DMIP", state);
  fDMIP = dMIP.at(trackID);
  cout<<user()<<"dMIP = "<<fDMIP<<endl;

  auto muonProb =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.MuonProbability", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fMuonProb = muonProb.at(trackID);
  cout<<user()<<"muonProb = "<<fMuonProb<<endl;

  auto lkrAssocEnergy =
    *(std::vector<double>*)GetOutput("BestTrackSelection.LKrAssocEnergy", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  fLKrEnergy = lkrAssocEnergy.at(trackID);

  auto specRICHsr =
    *(std::vector<SpectrometerRICHAssociationOutputSingleRing>*)GetOutput("SpectrometerRICHAssociationSingleRing.Output", state);
  if(!specRICHsr.at(trackID).isAssociated()){
    cout<<user()<<"No Association found"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fSpecRICHsr = &(specRICHsr.at(trackID));

  auto specMUV3 =
    *(std::vector<SpectrometerMUV3AssociationOutput>*)GetOutput("SpectrometerMUV3Association.Output", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fSpecMUV3 = &(specMUV3.at(trackID));

  auto GTKAssocID =
    *(std::vector<int>*)GetOutput("BestTrackSelection.GTKAssocID", state);
  auto Vertex =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocVertex", state);
  auto quality1 =
    *(std::vector<double>*)GetOutput("BestTrackSelection.GTKAssocQuality1", state);
  auto GTKMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocMomentum", state);
  auto TrackMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocTrackMomentum", state);
  if(state==kOInvalid || state==kOUninit){
    cout<<user()<<"Uninit/Invalid"<<endl;
    return;
  };
  cout<<user()<<"GTK ID "<<GTKAssocID.at(trackID)<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  auto NomVertex =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.NomVertex", state);
  auto NomKaonMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.NomKaonMomentum", state);
  auto NomTrackMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.BestTrackNomMomentum", state);
  if(state==kOInvalid || state==kOUninit){
    cout<<user()<<"Uninit/Invalid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  //vertex
  TVector3 vertex;
  if(!UseGTK){
    vertex = NomVertex.at(trackID);
  }else{
    vertex = Vertex.at(trackID);
  };

  bool goodEvent =
    *(bool*)GetOutput("EventCleaning.GoodEvent", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool decaySelected =
  *(bool*)GetOutput("KaonDecaySelection.DecaySelected", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool multiplicity =
    *(bool*)GetOutput("MultiplicitySelection.Multiplicity", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  fSegmentsAlgo->Process(SpectrometerEvent, trackID);
  bool segments = fSegmentsAlgo->ReconstructSegments(&vertex, tTrigger);
  FillHisto("hCut", cutID);
  cutID++;

  bool photonsLAVSAV =
    *(bool*)GetOutput("PhotonRejection.Photons", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool photonsLKr =
    *(bool*)GetOutput("PhotonRejectionLKr.Photons", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  cout<<user()<<"all requested outputs are valid"<<endl;

  STRAWCand = static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(trackID));

  //muon
  TLorentzVector muon;
  if(!UseGTK){
    muon.SetVectM(NomTrackMomentum.at(trackID), MMU); //only for histogram purposes
  }else{
    muon.SetVectM(TrackMomentum.at(trackID), MMU);
  };

  //kaon
  TLorentzVector kaon;
  if(!UseGTK){
    kaon.SetVectM(NomKaonMomentum.at(trackID), MKCH);
  }else{
    kaon.SetVectM(GTKMomentum.at(trackID), MKCH);
  };
  FillHisto("hCut", cutID);
  cutID++; //25

  //missM2
  double missM2 = (kaon - muon).M2()/1000000.;
  FillHisto("hMissM2VsCut", cutID-1, missM2); //cutID-1 = 24
  FillHisto("hTrackMomentumVsCut", cutID-1, STRAWCand->GetMomentum());

  /////////////////////////////////////////////////////////////////////////////
  //Kmu2 selection

  // event cleaning
  cout<<user()<<"Is good event? "<<goodEvent<<endl;
  if(!goodEvent){
    cout<<user()<<"Event is rejected after Event Cleaning"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //kaon decay
  cout<<user()<<"Is selected kaon decay? "<<decaySelected<<endl;
  if(!decaySelected){
    cout<<user()<<"Event is rejected due to KaonDecaySelection"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //photon veto LAV, SAV
  cout<<user()<<"Has photon in LAV or SAV? "<<photonsLAVSAV<<endl;
  if(photonsLAVSAV){
    cout<<user()<<"Event is rejected due to photons in LAV, SAV"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //photon veto LKr
  cout<<user()<<"Has photon in LKr? "<<photonsLKr<<endl;
  if(photonsLKr){
    cout<<user()<<"Event is rejected due to photons in LKr"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  // multiplicity rejection
  cout<<user()<<"Is multiplicity true? "<<multiplicity<<endl;
  if(multiplicity){
    cout<<user()<<"Event is rejected due to multiplicity"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  // segment rejection
  cout<<user()<<"Is segments true? "<<segments<<endl;
  if(segments){
    cout<<user()<<"Event is rejected due to segments"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //muon
  cout<<user()<<"Is muon? "<<endl;
  if(!isMuon(STRAWCand->GetMomentum(), trackTime)) return;
  cout<<user()<<"Is muon."<<endl;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //momentum cut
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hTrackMomentum", STRAWCand->GetMomentum());
  cout<<user()<<"STRAW momentum: "<<fCutTrackMomMin<<" < "<<STRAWCand->GetMomentum()<<" < "<<fCutTrackMomMax<<endl;
  if((STRAWCand->GetMomentum()<fCutTrackMomMin) || (STRAWCand->GetMomentum()>fCutTrackMomMax)) return;
  FillHisto("hTrackTime", trackTime);
  FillHisto("hTimeDiffTrackTrigger", trackTime - tTrigger);
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);

  GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(GTKAssocID.at(trackID)));
  FillHisto("hKaonMom", kaon.Vect().Mag());
  FillHisto("hGTKMatchedQuality1", quality1.at(trackID));
  FillHisto("hMissM2VSvertexZ", vertex.Z(), missM2);
  FillHisto("hMissM2VSGTKMatchedQuality1", quality1.at(trackID), missM2);
  FillHisto("hMissM2", missM2);
  FillHisto("hMissM2VSmomentum", STRAWCand->GetMomentum(), missM2);
  FillHisto("hMissM2VSNtracks", SpectrometerEvent->GetNCandidates(), missM2);
  FillHisto("hMissM2VSMatchedGTKChi2", GTKCand->GetChi2(), missM2);
  FillHisto("hMissM2VSTrackChi2", STRAWCand->GetChi2(), missM2);
  FillHisto("hMissM2VSTrackLeadingTime", STRAWCand->GetLeadingTime(), missM2);
  FillHisto("hMissM2VSTrackTotalQuality", STRAWCand->GetCombinationTotalQuality(), missM2);
  FillHisto("hGTKMatchedTimeGTK3VSMeanTimeGTK12", GTKCand->GetTimeStation(2), (GTKCand->GetTimeStation(0)+GTKCand->GetTimeStation(1))/2.);
  FillHisto("hMissM2VSTimeGTKDiff312", GTKCand->GetTimeStation(2) - ((GTKCand->GetTimeStation(0)+GTKCand->GetTimeStation(1))/2.), missM2);

  fEventSelected = true;
  fKmu2NomKaonMom = NomKaonMomentum.at(trackID);
  fKmu2NomTrackMom = NomTrackMomentum.at(trackID);
  fKmu2TrackMom = TrackMomentum.at(trackID);
  fKmu2KaonMom = GTKMomentum.at(trackID);
  fKmu2KaonID = GTKAssocID.at(trackID);
  fKmu2TrackID = trackID;
  Int_t  RunNumber = GetRunID();
  time_t BurstTime = GetEventHeader()->GetBurstTime();
  fKmu2RICHMomentum = momRICH(fSpecRICHsr, RunNumber, BurstTime);
  cout<<user()<<"Track ID  "<<fKmu2TrackID<<" GTK ID "<<fKmu2KaonID<<" GTKMom ("<<fKmu2KaonMom.X()<<","<<fKmu2KaonMom.Y()<<","<<fKmu2KaonMom.Z()<<")"<<endl;
  cout<<user()<<"Selecting Kmu2 event"<<endl;
}

void Kmu2::PostProcess(){}

void Kmu2::EndOfBurstUser(){}

void Kmu2::EndOfRunUser(){}

void Kmu2::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void Kmu2::DrawPlot(){}

Kmu2::~Kmu2(){}

void Kmu2::PrepareOutputs(){
  fEventSelected = false;
  fKmu2NomKaonMom.SetXYZ(0., 0., 0.);
  fKmu2NomTrackMom.SetXYZ(0., 0., 0.);
  fKmu2KaonMom.SetXYZ(0., 0., 0.);
  fKmu2TrackMom.SetXYZ(0., 0., 0.);
  fKmu2KaonID = -1;
  fKmu2TrackID = -1;
  fKmu2RICHMomentum = 0.;
  SetOutputState("Kmu2EventSelected", kOInvalid);
  SetOutputState("Kmu2NomKaonMomentum", kOInvalid);
  SetOutputState("Kmu2NomTrackMomentum", kOInvalid);
  SetOutputState("Kmu2TrackMomentum", kOInvalid);
  SetOutputState("Kmu2KaonMomentum", kOInvalid);
  SetOutputState("Kmu2KaonID", kOInvalid);
  SetOutputState("Kmu2TrackID", kOInvalid);
  SetOutputState("Kmu2RICHMomentum", kOInvalid);
}

void Kmu2::ValidateOutputs(){
  SetOutputState("Kmu2EventSelected", kOValid);
  SetOutputState("Kmu2NomKaonMomentum", kOValid);
  SetOutputState("Kmu2NomTrackMomentum", kOValid);
  SetOutputState("Kmu2TrackMomentum", kOValid);
  SetOutputState("Kmu2KaonMomentum", kOValid);
  SetOutputState("Kmu2KaonID", kOValid);
  SetOutputState("Kmu2TrackID", kOValid);
  SetOutputState("Kmu2RICHMomentum", kOValid);
}

bool Kmu2::isMuon(double mom, double trackTime){
  bool verb = TestLevel(Verbosity::kUser);
  bool isMuon1 = isMuonMUV3Candidates(fMUV3Event, trackTime, fCutTimeDiffMUV3, verb);
  bool isMuon2 = isMuonMUV3Associations(fSpecMUV3, trackTime, fCutTimeDiffMUV3, verb);
  bool isMuon3 = isMuonProbability(fMuonProb, fCutMuonProbability, verb);
  bool isMuon4 = isMuonMIP(fDMIP, fCutDMIP, verb);
  bool isPositron = isPositronEoP(mom, fLKrEnergy, fCutEoP, verb);

  cout<<user()<<"Is "<<((!isMuon1)?"not":"")<<" muon by MUV3 candidates."<<endl;
  cout<<user()<<"Is "<<((!isMuon2)?"not":"")<<" muon by MUV3 associations."<<endl;
  cout<<user()<<"Is "<<((!isMuon3)?"not":"")<<" muon by probability."<<endl;
  cout<<user()<<"Is "<<((!isMuon4)?"not":"")<<" muon by MIP."<<endl;
  cout<<user()<<"Is "<<((!isPositron)?"not":"")<<" positron."<<endl;
  bool is = ((isMuon1 || isMuon2) && (isMuon4) && (isMuon3) && (!isPositron));
  if(verb && is) cout<<"Is muon."<<endl;
  return is;
}
