#include "K2pi.hh"

#include <stdlib.h>
#include <iostream>
#include <bitset>
#include <TChain.h>
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"
#include "RICHParameters.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "DownstreamTrack.hh"
#include "Pi0Selection.hh"
#include "TriggerConditions.hh"
#include "MCSimple.hh"
#include "BlueTubeTracker.hh"
#include "functions.hh"
#include "Event.hh"

#include "BestTrackSelection.hh"
#include "PhotonRejection.hh"
#include "PnnKinematicTailsFunctions.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

K2pi::K2pi(Core::BaseAnalysis *ba) : Analyzer(ba, "K2pi")
{
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");

  RequestL0Data();

  AddParam("UseGTK", "bool", &UseGTK, false);
  AddParam("Verbosity", "bool", &verb, false);
  AddParam("CutTrackMomMin", "double", &fCutTrackMomMin, 15000.);
  AddParam("CutTrackMomMax", "double", &fCutTrackMomMax, 35000.);
  AddParam("CutMatchedGTKQuality", "double", &fCutMatchedGTKQuality, 20.);
  AddParam("CutMinDiPionMass", "double", &fCutMinDiPionMass, 483.);
  AddParam("CutMaxDiPionMass", "double", &fCutMaxDiPionMass, 503.);
  AddParam("OffsetLKrCandidateToExpPiPlus", "double", &fOffsetLKrCandidateToExpPiPlus, -1.1); //[ns]
  AddParam("OffsetLKrHitToExpPiPlus", "double", &fOffsetLKrHitToExpPiPlus, -2.); //[ns]
  AddParam("CutTimeDiffPi0CHOD", "double", &fCutTimeDiffPi0CHOD, 5.);
  AddParam("CutEoP", "double", &fCutEoP, 0.8);
  AddParam("OffsetLKrCandidateExtraPhotons", "double", &fOffsetLKrCandidateExtraPhotons, -1.62); //[ns]
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
  AddParam("LKrClusterTimeDiff2", "double", &fLKrClusterTimeDiff2, 3.);
  AddParam("CutTimeDiffMUV3", "double", &fCutTimeDiffMUV3, 7.);
  AddParam("CutMaxTotalExtraEnergy", "double", &fMaxTotalExtraEnergy, 5000.);
  AddParam("CutRICHMaxLikelihood", "double", &fCutRICHMaxLikelihood, 0.05);
  AddParam("CutMinRICHMass", "double", &fCutMinRICHMass, 118.);
  AddParam("CutMaxRICHMass", "double", &fCutMaxRICHMass, 200.);
  AddParam("CutMinNeutralVertexZ", "double", &fCutMinNeutralVertexZ, 115000.);
  AddParam("CutMaxNeutralVertexZ", "double", &fCutMaxNeutralVertexZ, 165000.);

  fZMUV3 = GeometricAcceptance::GetInstance()->GetZMUV3();
  fZLKr = GeometricAcceptance::GetInstance()->GetZLKr();
  fZRICHfront = GeometricAcceptance::GetInstance()->GetZRICHFrontPlane();
  fZRICHback = GeometricAcceptance::GetInstance()->GetZRICHBackPlane();
}

void K2pi::InitOutput(){
  RegisterOutput("K2piEventSelected", &fEventSelected);
  RegisterOutput("K2piNomKaonMomentum", &fK2piNomKaonMom);
  RegisterOutput("K2piNomTrackMomentum", &fK2piNomTrackMom);
  RegisterOutput("K2piKaonMomentum", &fK2piKaonMom);
  RegisterOutput("K2piTrackMomentum", &fK2piTrackMom);
  RegisterOutput("K2piKaonID", &fK2piKaonID);
  RegisterOutput("K2piTrackID", &fK2piTrackID);
  RegisterOutput("K2piRICHMomentum", &fK2piRICHMomentum);
}

void K2pi::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){

    ReconfigureAnalyzer("Pi0Selection", "CutMaxTimeDiffClusterTrigger", 9999.);
    ReconfigureAnalyzer("Pi0Selection", "CutMinVertexZ", 105000.);
    ReconfigureAnalyzer("Pi0Selection", "CutMaxVertexZ", 180000.);
    ReconfigureAnalyzer("Pi0Selection", "CutMinClusterEnergy", 3000.);
    ReconfigureAnalyzer("Pi0Selection", "CutMinTotalEnergy", 6000.);
    ReconfigureAnalyzer("Pi0Selection", "CutMaxClusterTimeDiff", 3.);
    ReconfigureAnalyzer("Pi0Selection", "UseGTK", false); //was false, RG use nominal kaon, GTK would affect selection
    ReconfigureAnalyzer("Pi0Selection", "CheckIsolation", true);
    ReconfigureAnalyzer("Pi0Selection", "CheckSpectrometerAssociation", false);
    ReconfigureAnalyzer("Pi0Selection", "CheckLKrAcceptance", true); 
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutK1", 0.00093);
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutQ1", 2.21);
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutK2", 0.002046); //0.00186
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutQ2", 2.21);
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutSR1", 0.2); //0.23
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutSR2", 0.44); //0.46
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutMinDist", 150.);
    ReconfigureAnalyzer("EnergyClusterBuilder", "CutMaxTimeDiff", 999999.); //no cut on time diff between clusters

    BookHisto(new TH1I("hCut", "hCut", 50, 1, 51));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 50, 1, 51, 85, 0., 85000.));
    BookHisto(new TH2D("hMissM2VsCut", "hMissM2VsCut", 50, 1, 51, 280, -0.5, 0.2));
    BookHisto(new TH1D("hTimeDiffPi0CHOD", "hTimeDiffPi0CHOD", 50, -25., 25.));
    BookHisto(new TH2D("hTimeDiffLKrTrackVSClusterEnergy", "hTimeDiffLKrTrackVSClusterEnergy", 800, 0., 80000., 200, -130., 70.));
    BookHisto(new TH2D("hClusPosXvsClusterEnergy", "hClusPosXvsClusterEnergy", 800, 0., 80000., 400, -2000., 2000.));
    BookHisto(new TH2D("hClusPosYvsClusterEnergy", "hClusPosYvsClusterEnergy", 800, 0., 80000., 400, -2000., 2000.));
    BookHisto(new TH2D("hTimeDiff36LKrTrackVSClusterEnergy", "hTimeDiff36LKrTrackVSClusterEnergy", 800, 0., 80000., 200, -130., 70.));
    BookHisto(new TH1I("hPhotonInLKr", "hPhotonInLKr", 2, 0, 2));
    BookHisto(new TH1D("hTrackMomentum", "hTrackMomentum", 70, 0., 70000.));
    BookHisto(new TH1D("hTrackTime", "hTrackTime", 100, -50., 50.));
    BookHisto(new TH1D("hTimeDiffTrackTrigger", "hTimeDiffTrackTrigger", 400, -20., 20.));
    BookHisto(new TH1D("hKaonMom", "hKaonMom", 500, 40000., 90000.));
    BookHisto(new TH1D("hGTKMatchedQuality1", "hGTKMatchedQuality1", 1000, 0., 1.));
    BookHisto(new TH1D("hVtxZ", "hVtxZ", 120, 80000., 200000.));
    BookHisto(new TH2D("hMissM2VSvertexZ", "hMissM2VSvertexZ", 120, 80000., 200000., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSGTKMatchedQuality1", "hMissM2VSGTKMatchedQuality1", 500, 0., 1., 500, -0.2, 0.2));
    BookHisto(new TH1D("hMissM2", "hMissM2", 200, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSmomentum", "hMissM2VSmomentum", 700, 0., 70000., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSNtracks", "hMissM2VSNtracks", 5, 0, 5, 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSMatchedGTKChi2", "hMissM2VSMatchedGTKChi2", 100, 0., 50., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSTrackChi2", "hMissM2VSTrackChi2", 100, 0., 50., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSTrackLeadingTime", "hMissM2VSTrackLeadingTime", 100, -50., 50., 500, -0.2, 0.2));
    BookHisto(new TH2D("hMissM2VSTrackTotalQuality", "hMissM2VSTrackTotalQuality", 100, 0., 1., 500, -0.2, 0.2));
    BookHisto(new TH2D("hGTKMatchedTimeGTK3VSMeanTimeGTK12", "hGTKMatchedTimeGTK3VSMeanTimeGTK12", 750, -25., 50., 750, -25., 50.));
    BookHisto(new TH2D("hMissM2VSTimeGTKDiff312", "hMissM2VSTimeGTKDiff312", 100, -5., 5., 500, -0.2, 0.2));
    BookHisto(new TH1I("hIsGoodExpectedPiPlus", "hIsGoodExpectedPiPlus", 10, 1, 11));
    BookHisto(new TH1D("hExpPiPlusMass2", "hExpPiPlusMass2", 250, 0., 50000.));
  };
}

void K2pi::DefineMCSimple(){}

void K2pi::StartOfRunUser(){}

void K2pi::StartOfBurstUser(){}

void K2pi::Process(int iEvent){
  if(!fReadingData) return;

  (void)iEvent;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"K2pi"<<endl;
    cout<<"-------------------"<<endl;
    cout<<endl;
  };

  (void)iEvent;
  PrepareOutputs();
  ValidateOutputs();

  //events and candidates
  fLKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoSpectrometerEvent* SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate *STRAWCand;
  fMUV3Event = GetEvent<TRecoMUV3Event>();
  TRecoGigaTrackerEvent *GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
  TRecoGigaTrackerCandidate *GTKCand;

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

  double tTrigger =
    *(double*)GetOutput("CheckTrigger.TriggerTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  if(verb) cout<<"Trigger time read = "<<tTrigger<<endl;
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
  if(verb) cout<<"Track ID read = "<<trackID<<endl;
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
  if(verb) cout<<"track time = "<<trackTime<<endl;

  auto trackCHODTimes =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackCHODTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double trackCHODTime = trackCHODTimes.at(trackID);

  if(verb) cout<<"track CHOD time = "<<trackCHODTime<<endl;
  auto pionProb =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.PionProbability", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fPionProb = pionProb.at(trackID);
  if(verb) cout<<"pionProb = "<<fPionProb<<endl;

  auto muv1Energy =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.MUV1Energy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fMUV1Energy = muv1Energy.at(trackID);
  if(verb) cout<<"MUV1 energy = "<<fMUV1Energy<<endl;

  auto muv2Energy =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.MUV2Energy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fMUV2Energy = muv2Energy.at(trackID);
  if(verb) cout<<"MUV2 energy = "<<fMUV2Energy<<endl;

  auto extraMUV1Energy =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.ExtraMUV1Energy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fExtraMUV1Energy = extraMUV1Energy.at(trackID);
  if(verb) cout<<"Extra MUV1 energy = "<<fExtraMUV1Energy<<endl;

  auto extraMUV2Energy =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.ExtraMUV2Energy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fExtraMUV2Energy = extraMUV2Energy.at(trackID);
  if(verb) cout<<"Extra MUV2 energy = "<<fExtraMUV2Energy<<endl;

  auto totalEnergy =
    *(std::vector<double>*)GetOutput("TrackCalorimetricEnergyAssociation.TotalEnergy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fTotalEnergy = totalEnergy.at(trackID);
  if(verb) cout<<"Total energy = "<<fTotalEnergy<<endl;

  auto lkrStandardAssoc =
    *(std::vector<bool>*)GetOutput("BestTrackSelection.LKrStandardAssoc", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  auto lkrChargedAssoc =
    *(std::vector<bool>*)GetOutput("BestTrackSelection.LKrChargedAssoc", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };

  auto lkrAssocSeedEnergy =
      *(std::vector<double>*)GetOutput("BestTrackSelection.LKrAssocSeedEnergy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  fLKrSeedEnergy = lkrAssocSeedEnergy.at(trackID);

  auto lkrAssocNCells =
    *(std::vector<int>*)GetOutput("BestTrackSelection.LKrAssocNCells", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  fLKrNCells = lkrAssocNCells.at(trackID);

  auto lkrAssocEnergy =
    *(std::vector<double>*)GetOutput("BestTrackSelection.LKrAssocEnergy", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  fLKrEnergy = lkrAssocEnergy.at(trackID);

  auto lkrAssocPosition =
    *(std::vector<TVector2>*)GetOutput("BestTrackSelection.LKrAssocPosition", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  fLKrAssocPos = lkrAssocPosition.at(trackID);

  if(verb) cout<<"Which assoc? "<<(lkrStandardAssoc.at(trackID) ? 1 : (lkrChargedAssoc.at(trackID) ? 2 : 0))<<endl;
  if(verb) cout<<"LKr assoc energy = "<<fLKrEnergy<<endl;
  if(verb) cout<<"LKr assoc seed energy = "<<fLKrSeedEnergy<<endl;
  if(verb) cout<<"LKr assoc N cells = "<<fLKrNCells<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  auto specRICH =
    *(std::vector<SpectrometerRICHAssociationOutput>*)GetOutput("SpectrometerRICHAssociation.Output", state);
  if(!specRICH.at(trackID).isValid()){
    if(verb) cout<<"Requested output is not valid";
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fSpecRICH = &(specRICH.at(trackID));

  auto specRICHsr =
    *(std::vector<SpectrometerRICHAssociationOutputSingleRing>*)GetOutput("SpectrometerRICHAssociationSingleRing.Output", state);
  if(!specRICHsr.at(trackID).isAssociated()){
    if(verb) cout<<"No Association found"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fSpecRICHsr = &(specRICHsr.at(trackID));

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
    if(verb) cout<<"Uninit/Invalid"<<endl;
    return;
  };
  if(verb) cout<<"GTK ID = "<<GTKAssocID.at(trackID)<<endl;
  if(verb) cout<<"GTK quality1 = "<<quality1.at(trackID)<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  auto NomTrackMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.BestTrackNomMomentum", state);
  auto NomKaonMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.NomKaonMomentum", state);
  auto NomVertex =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.NomVertex", state);
  if(state==kOInvalid || state==kOUninit){
    if(verb) cout<<"Uninit/Invalid"<<endl;
    return;
  };

  auto pi0selected =
    *(bool*)GetOutput("Pi0Selection.EventSelected", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool goodEvent =
    *(bool*)GetOutput("EventCleaning.GoodEvent", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool decaySelected =
  *(bool*)GetOutput("KaonDecaySelection.DecaySelected", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool segments =
    *(bool*)GetOutput("SegmentRejection.Segments", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  bool photonsLAVSAV =
    *(bool*)GetOutput("PhotonRejection.Photons", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++; //27

  if(verb) cout<<"all requested outputs are valid"<<endl;

  //candidates
  STRAWCand = static_cast<TRecoSpectrometerCandidate*>(SpectrometerEvent->GetCandidate(trackID));

  //vertex
  TVector3 vertex;
  if(!UseGTK){
    vertex = NomVertex.at(trackID);
  }else{
    vertex = Vertex.at(trackID);
  };

  //pion
  TLorentzVector pion;
  if(!UseGTK){
    pion.SetVectM(NomTrackMomentum.at(trackID), MPI);
  }else{
    pion.SetVectM(TrackMomentum.at(trackID), MPI);
  };

  //kaon
  TLorentzVector kaon;
  if(!UseGTK){
    kaon.SetVectM(NomKaonMomentum.at(trackID), MKCH);
  }else{
    kaon.SetVectM(GTKMomentum.at(trackID), MKCH);
  };
  FillHisto("hCut", cutID);
  cutID++; //28

  //nominal kaon
  TVector3 nomBeamMom = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  fKaonNom.SetXYZM(nomBeamMom.X(), nomBeamMom.Y(), nomBeamMom.Z(), MKCH);

  //missM2
  double missM2 = (kaon - pion).M2()/1000000.;
  FillHisto("hMissM2VsCut", cutID-1, missM2); //cutID-1 = 27

  /////////////////////////////////////////////////////////////////////////////
  //K2pi selection

  //Pi0 selected
  if(verb) cout<<"Pi0 selected? "<<pi0selected<<endl;
  if(!pi0selected) return;
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //photons in RICH acceptance
  std::vector<Pi0SelectionOutput> pi0Selected =
   *(std::vector<Pi0SelectionOutput>*)GetOutput("Pi0Selection.SelectedPi0");
  Pi0SelectionOutput pi0 = pi0Selected.at(0);
  bool photonsInAccRICH = ArePhotonsInRICHAcceptance(pi0.fClustersID, pi0.fPosition);
  if(verb) cout<<"Photons in RICH acceptance? "<<photonsInAccRICH<<" = 1"<<endl;
  if(!photonsInAccRICH) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //N selected Pi+
  if(verb) cout<<"N pi0 selected: "<<pi0Selected.size()<<" = 1"<<endl;
  if(pi0Selected.size()!=1) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //neutral vertex position
  double pi0VtxZ = pi0.fPosition.Z();
  if(verb) cout<<"Neutral vertex position = "<<pi0.fPosition.X()<<" "<<pi0.fPosition.Y()<<" "<<pi0.fPosition.Z()<<endl;
  if(verb) cout<<"Neutral vertex Z position "<<fCutMinNeutralVertexZ<<" < "<<pi0VtxZ<<" < "<<fCutMaxNeutralVertexZ<<endl;
  if((fCutMinNeutralVertexZ>pi0VtxZ) || (fCutMaxNeutralVertexZ<pi0VtxZ)) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //check expected Pi+
  if(verb) cout<<"Would selected Pi0 have a good Pi+? "<<endl;
  bool isgoodexp = isGoodExpectedPiPlus(pi0.fMomentum, pi0.fPosition, pi0.fClustersID);
  if(!isgoodexp) return;
  if(verb) cout<<"Selected Pi0 has good Pi+."<<endl;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //LKr cluster to expected pi+
  bool found = false;
  TLorentzVector expPiPlus = fKaonNom - pi0.fMomentum;
  TVector3 expPos = GetPositionAtZ(expPiPlus.Vect(), pi0.fPosition, fZLKr);
  TVector2 clusPos;
  if(verb) cout<<"Look for standard cluster for expected pi+."<<endl;
  for(int j=0; j<fLKrEvent->GetNCandidates(); j++){
    if(verb) cout<<"LKr cluster "<<j<<endl;
    TRecoLKrCandidate *c = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(j));
    double tLKrCand = c->GetTime() + fOffsetLKrCandidateToExpPiPlus;
    clusPos.Set(c->GetClusterX(), c->GetClusterY());
    if(verb) cout<<"Cluster position - expected Pi+ position = "<<(clusPos-expPos.XYvector()).Mod()<<" < 200."<<endl;
    if(verb) cout<<"Cluster time - expected Pi+ time = |"<<tLKrCand<<" - "<<pi0.fTime<<"| = "<<fabs(tLKrCand-pi0.fTime)<<" < 5"<<endl;
    if((clusPos-expPos.XYvector()).Mod()>200.) continue;
    if(fabs(tLKrCand-pi0.fTime)>5.) continue;
    found = true;
    if(verb) cout<<"Cluster "<<j<<" should correspond to expected Pi+."<<endl;
    break;
  };
  if(verb) cout<<"Is there standard LKr cluster to expected Pi+? "<<found<<endl;
  if(!found){
    //LKr charged cluster
    if(verb) cout<<"Look for charged LKr cluster for expected pi+."<<endl;
    double tCluster = 9999999.;
    double maxEnergy = 0.;
    double ECluster = 0.;
    int ncells = 0;
    if(verb) cout<<"N LKr hits: "<<fLKrEvent->GetNHits()<<endl;
    for(int k=0; k<fLKrEvent->GetNHits(); k++){
      if(verb) cout<<"LKr hit "<<k<<endl;
      TRecoLKrHit *LKrHit = static_cast<TRecoLKrHit*>(fLKrEvent->GetHit(k));
      double tHit = LKrHit->GetTime() + fOffsetLKrHitToExpPiPlus;
      if(verb){
        cout<<"Hit time = "<<tHit<<endl;
        cout<<"Hit energy = "<<LKrHit->GetEnergy()<<endl;
        cout<<"Hit position = "<<LKrHit->GetPosition().X()<<" "<<LKrHit->GetPosition().Y()<<" "<<LKrHit->GetPosition().Z()<<endl;
        cout<<"Expected Pi+ position = "<<expPos.X()<<" "<<expPos.Y()<<" "<<expPos.Z()<<endl;
        cout<<"Hit position - expected Pi+ position = "<<(LKrHit->GetPosition().XYvector()-expPos.XYvector()).Mod()<<" < 100."<<endl;
        cout<<"Hit time - expected Pi+ time = |"<<tHit<<" - "<<pi0.fTime<<"| = "<<fabs(tHit-pi0.fTime)<<" < 20"<<endl;
      };
      if((LKrHit->GetPosition().XYvector()-expPos.XYvector()).Mod()<100. && fabs(tHit-pi0.fTime)<20.){
        ncells++;
        ECluster+=LKrHit->GetEnergy();
        if(verb){
          cout<<"N cells = "<<ncells<<endl;
          cout<<"Total cluster energy = "<<ECluster<<endl;
        };
        if(LKrHit->GetEnergy()>maxEnergy){
          maxEnergy = LKrHit->GetEnergy();
          tCluster = tHit;
        };
      };
    };
    if(verb){
      cout<<"Found charged association?"<<endl;
      cout<<"Max energy = "<<maxEnergy<<" > 40."<<endl;
      cout<<"Cluster time - expected Pi+ time "<<fabs(tCluster-pi0.fTime)<<" <= 8."<<endl;
    };
    if((maxEnergy>40.) && (fabs(tCluster-pi0.fTime)<=8.)){
      found = true;
      if(verb) cout<<"Charged association found."<<endl;
    };
    maxEnergy = CorrectLKrCellCluster(ncells, maxEnergy);
  };
  if(!found) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //Pi0 close to CHOD
  FillHisto("hTimeDiffPi0CHOD", pi0.fTime-trackCHODTime);
  if(verb) cout<<"Is Pi0 in time with CHOD? "<<fabs(pi0.fTime-trackCHODTime)<<" < "<<fCutTimeDiffPi0CHOD<<endl;
  if(fabs(pi0.fTime-trackCHODTime)>fCutTimeDiffPi0CHOD) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //event cleaning
  if(verb) cout<<"Is good event? "<<goodEvent<<endl;
  if(!goodEvent){
    if(verb) cout<<"Event is rejected after Event Cleaning"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //kaon decay
  if(verb) cout<<"Is selected kaon decay? "<<decaySelected<<endl;
  if(!decaySelected){
    if(verb) cout<<"Event is rejected due to KaonDecaySelection"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //photon veto LAV, SAV
  if(verb) cout<<"Has photon in LAV or SAV? "<<photonsLAVSAV<<endl;
  if(photonsLAVSAV){
    if(verb) cout<<"Event is rejected due to photons in LAV, SAV"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //LKr extra activity
  int id1 = pi0.fClustersID.first;
  int id2 = pi0.fClustersID.second;
  bool photonInLKr = ExtraPhotonsInLKr(STRAWCand, id1, id2, trackTime);
  FillHisto("hPhotonInLKr", (int)photonInLKr);
  if(verb) cout<<"Any extra LKr activity corresponding to additional photon? "<<photonInLKr<<endl;
  if(photonInLKr) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  // segment rejection
  if(verb) cout<<"Is segments true? "<<segments<<endl;
  if(segments){
    if(verb) cout<<"Event is rejected due to segments"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //pion with RICH + calorimeters
  if(verb) cout<<"Is Pion? "<<endl;
  if(!isPion(STRAWCand->GetMomentum(), trackTime)) return;
  if(verb) cout<<"Is pion."<<endl;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++;

  //momentum cut
  FillHisto("hTrackMomentum", STRAWCand->GetMomentum());
  if(verb) cout<<"STRAW momentum: "<<fCutTrackMomMin<<" < "<<STRAWCand->GetMomentum()<<" < "<<fCutTrackMomMax<<endl;
  if((STRAWCand->GetMomentum()<fCutTrackMomMin) || (STRAWCand->GetMomentum()>fCutTrackMomMax)) return;
  FillHisto("hTrackTime", trackTime);
  FillHisto("hTimeDiffTrackTrigger", trackTime - tTrigger);
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hMissM2VsCut", cutID, missM2);
  FillHisto("hCut", cutID);
  cutID++; //43

  //RICH momentum
  Int_t  RunNumber = GetRunID();
  time_t BurstTime = GetEventHeader()->GetBurstTime();
  double RICHMom = momRICH(fSpecRICHsr, RunNumber, BurstTime);

  GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(GTKAssocID.at(trackID)));
  FillHisto("hKaonMom", kaon.Vect().Mag());
  FillHisto("hGTKMatchedQuality1", quality1.at(trackID));
  FillHisto("hVtxZ", vertex.Z());
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
  fK2piNomKaonMom = NomKaonMomentum.at(trackID);
  fK2piNomTrackMom = NomTrackMomentum.at(trackID);
  fK2piTrackMom = TrackMomentum.at(trackID);
  fK2piKaonMom = GTKMomentum.at(trackID);
  fK2piKaonID = GTKAssocID.at(trackID);
  fK2piTrackID = trackID;
  fK2piRICHMomentum = RICHMom;
  if(verb) cout<<"Track ID "<<fK2piTrackID<<" GTK ID "<<fK2piKaonID<<" GTKMom ("<<fK2piKaonMom.X()<<","<<fK2piKaonMom.Y()<<","<<fK2piKaonMom.Z()<<")"<<endl;
  if(verb) cout<<"Selecting K2pi event"<<endl;
}

void K2pi::PostProcess(){}

void K2pi::EndOfBurstUser(){}

void K2pi::EndOfRunUser(){}

void K2pi::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void K2pi::DrawPlot(){}

K2pi::~K2pi(){}

void K2pi::PrepareOutputs(){
  fEventSelected = false;
  fK2piNomTrackMom.SetXYZ(0., 0., 0.);
  fK2piNomKaonMom.SetXYZ(0., 0., 0.);
  fK2piTrackMom.SetXYZ(0., 0., 0.);
  fK2piKaonMom.SetXYZ(0., 0., 0.);
  fK2piKaonID = -1;
  fK2piTrackID = -1;
  fK2piRICHMomentum = 0.;
  SetOutputState("EventSelected", kOInvalid);
  SetOutputState("K2piNomKaonMomentum", kOInvalid);
  SetOutputState("K2piNomTrackMomentum", kOInvalid);
  SetOutputState("K2piKaonMomentum", kOInvalid);
  SetOutputState("K2piTrackMomentum", kOInvalid);
  SetOutputState("K2piKaonID", kOInvalid);
  SetOutputState("K2piTrackID", kOInvalid);
  SetOutputState("K2piRICHMomentum", kOInvalid);
}

void K2pi::ValidateOutputs(){
  SetOutputState("EventSelected", kOValid);
  SetOutputState("K2piNomKaonMomentum", kOValid);
  SetOutputState("K2piNomTrackMomentum", kOValid);
  SetOutputState("K2piKaonMomentum", kOValid);
  SetOutputState("K2piTrackMomentum", kOValid);
  SetOutputState("K2piKaonID", kOValid);
  SetOutputState("K2piTrackID", kOValid);
  SetOutputState("K2piRICHMomentum", kOValid);
}

bool K2pi::isGoodExpectedPiPlus(TLorentzVector pi0, TVector3 vertex, const std::pair<Int_t, Int_t> &photonIDs){ //const & - reference to the object, only for reading
  int mmm=1;
  FillHisto("hIsGoodExpectedPiPlus", mmm);
  mmm++;

  TLorentzVector expPiPlus = fKaonNom - pi0;
  if(verb) cout<<"Expected momentum of Pi+ = "<<expPiPlus.Vect().X()<<" "<<expPiPlus.Vect().Y()<<" "<<expPiPlus.Vect().Z()<<endl;
  double momPiPlus = expPiPlus.Vect().Mag();
  if(verb) cout<<"Expected momentum of Pi+:"<<"5000. < "<<momPiPlus<<" < "<<80000.<<endl;
  if(momPiPlus<5000. || momPiPlus>80000.) return false;
  FillHisto("hIsGoodExpectedPiPlus", mmm);
  mmm++;

  if(verb) cout<<"Is in geometrical acceptance of Spectrometer, RICH, CHOD, MUV2, MUV3? ";
  TVector3 slopeBefMag = expPiPlus.Vect()*(1./expPiPlus.Vect().Mag());
  TVector3 posAtMagnet = GetPositionAtZ(expPiPlus.Vect(), vertex, 197600.);
  TVector3 slopeAftMag = MomAfterKick(expPiPlus.Vect(), 270.);
  TVector3 posAtFrontRICH = GetPositionAtZ(expPiPlus.Vect(), vertex, 219385.);
  double Rfront = sqrt(pow(posAtFrontRICH.X() - 34., 2) + pow(posAtFrontRICH.Y(), 2));
  TVector3 posAtBackRICH = GetPositionAtZ(expPiPlus.Vect(), vertex, 237326.);
  double Rback = sqrt(pow(posAtBackRICH.X() - 2., 2) + pow(posAtBackRICH.Y(), 2));
  if(!GeometricAcceptance::GetInstance()->InAcceptance(vertex, slopeBefMag, kSpectrometer, 0) ||
     !GeometricAcceptance::GetInstance()->InAcceptance(vertex, slopeBefMag, kSpectrometer, 1) ||
     !GeometricAcceptance::GetInstance()->InAcceptance(posAtMagnet, slopeAftMag, kSpectrometer, 2) ||
     !GeometricAcceptance::GetInstance()->InAcceptance(posAtMagnet, slopeAftMag, kSpectrometer, 3) ||
     !GeometricAcceptance::GetInstance()->InAcceptance(posAtMagnet, slopeAftMag, kCHOD) ||
     !GeometricAcceptance::GetInstance()->InAcceptance(posAtMagnet, slopeAftMag, kMUV2) ||
     !GeometricAcceptance::GetInstance()->InAcceptance(posAtMagnet, slopeAftMag, kMUV3, 0, 130., 1100.) ||
     !(Rfront>90. && Rfront<1100. && Rback>90. && Rback<1100.)) return false;
  if(verb) cout<<"yes"<<endl;
  FillHisto("hIsGoodExpectedPiPlus", mmm);
  mmm++;

  FillHisto("hExpPiPlusMass2", expPiPlus.M2());
  if(verb) cout<<"Expected mass^2 of Pi+: "<<"8000. < "<<expPiPlus.M2()<<" < 31000."<<endl;
  if(expPiPlus.M2()<8000. || expPiPlus.M2()>31000.) return false;
  FillHisto("hIsGoodExpectedPiPlus", mmm);

  TRecoLKrCandidate *c1 = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(photonIDs.first));
  TRecoLKrCandidate *c2 = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(photonIDs.second));
  TVector2 expPos = GetPositionAtZ(expPiPlus.Vect(), vertex, fZLKr).XYvector();
  TVector2 c1pos(c1->GetClusterX(), c1->GetClusterY());
  TVector2 c2pos(c2->GetClusterX(), c2->GetClusterY());
  if(verb) cout<<"Expected Pi+ close to photons? "<<"photon1: "<<(c1pos-expPos).Mod()<<" < 150  "<<"photon2: "<<(c2pos-expPos).Mod()<<" < 150"<<endl;
  if((c1pos-expPos).Mod()<150. || (c2pos-expPos).Mod()<150.) return false;

  if(verb) cout<<"Is good expected Pi+"<<endl;
  return true;
}

bool K2pi::isPion(double mom, double trackTime){
  bool isPion1 = isPionProbability(mom, fPionProb, verb);
  bool isPion2 = isPionCaloEnergy(fTotalEnergy, mom, verb);
  bool isPion3 = isPionCellSeed(fMUV1Energy, fMUV2Energy, fTotalEnergy, fLKrSeedEnergy, fLKrEnergy, fLKrNCells, verb);
  bool isMuon = isMuonMUV3Candidates(fMUV3Event, trackTime, fCutTimeDiffMUV3, verb);
  bool isExtra = isExtraCaloEnergy(fExtraMUV1Energy, fExtraMUV2Energy, fMaxTotalExtraEnergy, verb);
  bool isPositron = isPositronEoP(mom, fLKrEnergy, fCutEoP, verb);
  bool isPionRich = isPionRICH(fSpecRICH, fSpecRICHsr, fCutMinRICHMass, fCutMaxRICHMass, fCutRICHMaxLikelihood, verb);

  if(verb && isPionRich) cout<<"Could be pion (RICH)."<<endl;
  bool ispion = (!isMuon && isPion1 && isPion2 && isPion3 && !isExtra && !isPositron);
  if(verb) cout<<"Could "<<(ispion?"":"not")<<" be pion."<<endl;

  bool is = (ispion && isPionRich);
  if(verb && is) cout<<"Is pion."<<endl;
  return is;
}

bool K2pi::ExtraPhotonsInLKr(TRecoSpectrometerCandidate *STRAWCand, int id1, int id2, double trackTime){
  bool isExtraPhoton = false;
  TVector2 trackAtLKr(STRAWCand->xAt(fZLKr), STRAWCand->yAt(fZLKr));
  if(verb){
    cout<<endl;
    cout<<"Extra activity study"<<endl;
    cout<<"Track position at LKr: "<<trackAtLKr.X()<<" "<<trackAtLKr.Y()<<endl;
    cout<<"N LKr candidates: "<<fLKrEvent->GetNCandidates()<<endl;
    cout<<"LKr candidates "<<id1<<" and "<<id2<<" correspond to photons from pi0."<<endl;
  };
  for(int l=0; l<fLKrEvent->GetNCandidates(); l++){
    if(verb) cout<<"candidate "<<l<<endl;
    if(l==id1 || l==id2){
      if(verb) cout<<"LKr candidate "<<l<<" corresponds to photon from pi0."<<endl;
      continue;
    };
    TRecoLKrCandidate *LKrCand = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(l));
    TVector2 candPos(LKrCand->GetClusterX(), LKrCand->GetClusterY());
    if((trackAtLKr-candPos).Mod()<100. || (fLKrAssocPos-candPos).Mod()<100.){
      continue;
    };
    double Eclus = LKrCand->GetClusterEnergy();
    double deltaT = LKrCand->GetTime() - trackTime + fOffsetLKrCandidateExtraPhotons;
    double sigma = fLKrSigmaA + fLKrSigmaB/(Eclus/1000.) - fLKrSigmaC/sqrt((Eclus/1000.));
    if(verb) cout<<"Position "<<candPos.X()<<" "<<candPos.Y()<<endl;
    if(verb) cout<<"Energy "<<Eclus<<endl;
    FillHisto("hTimeDiffLKrTrackVSClusterEnergy", Eclus, deltaT);
    FillHisto("hClusPosXvsClusterEnergy", Eclus, candPos.X());
    FillHisto("hClusPosYvsClusterEnergy", Eclus, candPos.Y());
    FillHisto("hTimeDiff36LKrTrackVSClusterEnergy", Eclus, deltaT+36.);
    isExtraPhoton = EvaluateLKrCluster(Eclus, deltaT, sigma, fLKrClusterEnergy1, fLKrClusterEnergy2, fLKrClusterEnergy3, fLKrClusterEnergy4, fLKrClusterTimeDiff1, fLKrClusterTimeDiff2, fLKrClusterTimeDiffSigma1, fLKrClusterTimeDiffSigma2, fLKrClusterTimeDiffSigma3, verb);
    if(isExtraPhoton) break;
  };
  return isExtraPhoton;
}

bool K2pi::ArePhotonsInRICHAcceptance(const std::pair<Int_t, Int_t> &photonIDs, TVector3 vertex){
  bool are = true;
  TRecoLKrCandidate *c1 = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(photonIDs.first));
  TRecoLKrCandidate *c2 = static_cast<TRecoLKrCandidate*>(fLKrEvent->GetCandidate(photonIDs.second));

  TVector3 pos1(c1->GetClusterX(), c1->GetClusterY(), fZLKr);
  TVector3 pos2(c2->GetClusterX(), c2->GetClusterY(), fZLKr);

  TVector3 dir1 = (pos1-vertex).Unit();
  TVector3 dir2 = (pos2-vertex).Unit();

  TVector3 pos1AtRICHfront(pos1.X()+dir1.X()*(fZRICHfront-pos1.Z())/dir1.Z(), pos1.Y()+dir1.Y()*(fZRICHfront-pos1.Z())/dir1.Z(), fZRICHfront);
  TVector3 pos2AtRICHfront(pos2.X()+dir2.X()*(fZRICHfront-pos2.Z())/dir2.Z(), pos2.Y()+dir2.Y()*(fZRICHfront-pos2.Z())/dir2.Z(), fZRICHfront);

  TVector3 pos1AtRICHback(pos1.X()+dir1.X()*(fZRICHback-pos1.Z())/dir1.Z(), pos1.Y()+dir1.Y()*(fZRICHback-pos1.Z())/dir1.Z(), fZRICHback);
  TVector3 pos2AtRICHback(pos2.X()+dir2.X()*(fZRICHback-pos2.Z())/dir2.Z(), pos2.Y()+dir2.Y()*(fZRICHback-pos2.Z())/dir2.Z(), fZRICHback);

  double r1front = sqrt(pow(pos1AtRICHfront.X() - 34., 2) + pow(pos1AtRICHfront.Y(), 2));
  double r2front = sqrt(pow(pos2AtRICHfront.X() - 34., 2) + pow(pos2AtRICHfront.Y(), 2));

  double r1back = sqrt(pow(pos1AtRICHback.X(), 2) + pow(pos1AtRICHback.Y(), 2));
  double r2back = sqrt(pow(pos2AtRICHback.X(), 2) + pow(pos2AtRICHback.Y(), 2));

  if((r1front<100.) || (r2front<100.) || (r1back<120.) || (r2back<120.)) are = false;
  return are;
}
