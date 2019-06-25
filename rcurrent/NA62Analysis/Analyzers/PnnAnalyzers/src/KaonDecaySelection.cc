#include "KaonDecaySelection.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GeometricAcceptance.hh"
#include "BlueTubeTracker.hh"
#include "BeamParameters.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "PnnKinematicTailsFunctions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

KaonDecaySelection::KaonDecaySelection(Core::BaseAnalysis *ba) : Analyzer(ba, "KaonDecaySelection")
{
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("CHANTI", new TRecoCHANTIEvent, "Reco");
  RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco");

  AddParam("Verbosity", "bool", &verb, false);
  AddParam("CutMinGTKMomentum", "double", &fCutMinGTKMomentum, 72700.);
  AddParam("CutMaxGTKMomentum", "double", &fCutMaxGTKMomentum, 77200.);
  AddParam("MinSlopeX", "double", &fMinSlopeX, 0.0009);
  AddParam("MaxSlopeX", "double", &fMaxSlopeX, 0.0016);
  AddParam("MinSlopeY", "double", &fMinSlopeY, -0.0003);
  AddParam("MaxSlopeY", "double", &fMaxSlopeY, 0.0004);
  AddParam("MaxThetaCorr", "double", &fMaxThetaCorr, 0.00035);
  AddParam("CutMinXTrackAtGTK3", "double", &fCutMinXTrackAtGTK3, 30.);
  AddParam("CutMinYTrackAtGTK3", "double", &fCutMinYTrackAtGTK3, 15.);
  AddParam("CutMinDistTrackHitGTK3", "double", &fCutMinDistTrackHitGTK3, 24.);
  AddParam("STRAW1CenterX", "double", &fSTRAW1CenterX, 101.2);
  AddParam("kMax1Rstraw1", "double", &fkMax1Rstraw1, -0.00436); //-0.004
  AddParam("qMax1Rstraw1", "double", &fqMax1Rstraw1, 830.); //735.
  AddParam("kMax2Rstraw1", "double", &fkMax2Rstraw1, -0.0625);
  AddParam("qMax2Rstraw1", "double", &fqMax2Rstraw1, 7462.5);
  AddParam("kMinRstraw1", "double", &fkMinRstraw1, -0.0098333);
  AddParam("qMinRstraw1", "double", &fqMinRstraw1, 1812.5);
  AddParam("CutMaxVertexZ", "double", &fCutMaxVertexZ, 165000.);
  AddParam("CutToT", "double", &fCutToT, 23.);
  AddParam("CutTimeDiffTrackCHANTI", "double", &fCutTimeDiffTrackCHANTI, 3.);
  AddParam("CutAdditionalVertexMinZ", "double", &fCutAdditionalVertexMinZ, 100000.);
  AddParam("CutAdditionalVertexMaxZ", "double", &fCutAdditionalVertexMaxZ, 105000.);
  AddParam("CutMinXTrim5", "double", &fCutMaxXTrim5, 100.);
  AddParam("CutMinYTrim5", "double", &fCutMaxYTrim5, 500.);
  AddParam("kMax3Rstraw1", "double", &fkMax3Rstraw1, -0.00436);
  AddParam("qMax3Rstraw1", "double", &fqMax3Rstraw1, 830.);
  AddParam("UseCutRStraw1VsVertexZ", "bool", &fUseCutRStraw1VsVertexZ, true);
  AddParam("UseCutTrim5", "bool", &fUseCutTrim5, false);
  AddParam("UseCutToT", "bool", &fUseCutToT, true);

  fZGTK3 = GeometricAcceptance::GetInstance()->GetZGTK3();
  fZSTRAW1 = GeometricAcceptance::GetInstance()->GetZStraw(1);
  fZTRIM5 = GeometricAcceptance::GetInstance()->GetZTrim5(); //101800mm
  fZB6end = 99460.;
  fZB6start = 96960.;
  fZB5end = 95860.;
  fZB5start = 93360;
  fB = 1.6678;
  fTRIM5kick = 91.3;
  fTRIM5halfwidth = 200.;
}

void KaonDecaySelection::InitOutput(){
  RegisterOutput("DecaySelected", &fDecaySelected);
}

void KaonDecaySelection::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 20, 1, 21));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 20, 1, 21, 85, 0., 85000.));
    BookHisto(new TH1F("hGTKMom", "hGTKMom", 100, 70000., 80000.));
    BookHisto(new TH2F("hGTKSlopeYvsSlopeX", "hGTKSlopeYvsSlopeX", 100, 0.0005, 0.002, 100, -0.0005, 0.0005));
    BookHisto(new TH1F("hThetaCorr", "hThetaCorr", 100, 0., 0.001));
    BookHisto(new TH1I("hGTKstation", "hGTKstation", 3, 0, 3));
    BookHisto(new TH1D("hPosDiffGTK3HitTrack", "hPosDiffGTK3HitTrack", 1000, 0., 1000.));
    BookHisto(new TH1I("hNCloseHitsInGTK3", "hNCloseHitsInGTK3", 100, 0, 100));
    BookHisto(new TH1D("hVertexZ", "hVertexZ", 100, 100000., 200000.));
    BookHisto(new TH2D("hRstraw1vsVertexZ", "hRstraw1vsVertexZ", 100, 100000., 200000., 100, 0., 1000.));
    BookHisto(new TH1D("hGTKToT", "hGTKToT", 100, 0., 100.));
    BookHisto(new TH1I("hNHitsHighToT", "hNHitsHighToT", 10, 0, 10));
    BookHisto(new TH1D("hTimeDiffCHANTITrack", "hTimeDiffCHANTITrack", 50, -25., 25.));
  };
}

void KaonDecaySelection::DefineMCSimple(){}

void KaonDecaySelection::StartOfRunUser(){}

void KaonDecaySelection::StartOfBurstUser(){}

void KaonDecaySelection::ProcessSpecialTriggerUser(int, unsigned int){}

void KaonDecaySelection::Process(int){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"KaonDecaySelection"<<endl;
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

  auto trackID =
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
  if(verb) cout<<"track time read = "<<trackTime<<endl;

  auto trackKTAGTimes =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackKTAGTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double trackKTAGTime = trackKTAGTimes.at(trackID);
  if(verb) cout<<"track KTAG time read = "<<trackKTAGTime<<endl;

  auto trackGTKTimes =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackGTKTime", state);
  if(state!=kOValid){
    if(verb) cout<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  double trackGTKTime = trackGTKTimes.at(trackID);
  if(verb) cout<<"track GTK time read = "<<trackGTKTime<<endl;

  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate* STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(trackID));
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());

  auto GTKAssocID =
    *(std::vector<int>*)GetOutput("BestTrackSelection.GTKAssocID", state);
  auto AllVertices =
    *(std::vector<std::vector<TVector3>>*)GetOutput("BestTrackSelection.GTKAllAssocVertices", state);
  auto trackMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocTrackMomentum", state);
  auto GTKMomentum =
    *(std::vector<TVector3>*)GetOutput("BestTrackSelection.GTKAssocMomentum", state);
  if(state==kOInvalid || state==kOUninit){
    if(verb) cout<<"Uninit/Invalid"<<endl;
    return;
  };
  if(verb) cout<<"all requested outputs are valid"<<endl;
  if(verb) cout<<"GTK ID = "<<GTKAssocID.at(trackID)<<endl;
  FillHisto("hCut", cutID);
  cutID++;
  TVector3 momGTKatV = GTKMomentum.at(trackID);
  TVector3 momTRACKatV = trackMomentum.at(trackID);
  TVector3 vertex = AllVertices.at(trackID).at(0);

  TRecoGigaTrackerEvent* GTKEvent = GetEvent<TRecoGigaTrackerEvent>();
  TRecoGigaTrackerCandidate* GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(GTKAssocID.at(trackID)));
  TRecoGigaTrackerHit* GTKHit;

  TVector3 posTRACKatGTK3;
  TVector3 momTRACKatGTK3;
  ApplyBlueTube(1, vertex, momTRACKatV, fZGTK3, &posTRACKatGTK3, &momTRACKatGTK3);

  //track position at GTK3
  if(verb) cout<<"track position at GTK3: x = "<<fabs(posTRACKatGTK3.X())<<" > "<<fCutMinXTrackAtGTK3<<" y = "<<fabs(posTRACKatGTK3.Y())<<" > "<<fCutMinYTrackAtGTK3<<endl;
  if(fabs(posTRACKatGTK3.X())<fCutMinXTrackAtGTK3 && fabs(posTRACKatGTK3.Y())<fCutMinYTrackAtGTK3) return; //against inefficiency in GTK3
  FillHisto("hCut", cutID);
  cutID++;

  //interaction of track at GTK3
  int count = 0;
  if(verb) cout<<"N hits in GTK = "<<GTKEvent->GetNHits()<<endl;
  for(int i=0; i<GTKEvent->GetNHits(); i++){
    GTKHit = static_cast<TRecoGigaTrackerHit*>(GTKEvent->GetHit(i));
    FillHisto("hGTKstation", GTKHit->GetStationNo());
    if(GTKHit->GetStationNo()!=2) continue;
    if(verb) cout<<"hit in GTK3  = "<<i<<endl;
    TVector3 pos = GTKHit->GetPosition();
    FillHisto("hPosDiffGTK3HitTrack", (pos-posTRACKatGTK3).Mag());
    if(verb) cout<<"distance from track at GTK3 = "<<(pos-posTRACKatGTK3).Mag()<<" < "<<fCutMinDistTrackHitGTK3<<" to be close hit"<<endl;
    if((pos-posTRACKatGTK3).Mag()<fCutMinDistTrackHitGTK3) count++;
  };
  FillHisto("hNCloseHitsInGTK3", count);
  if(verb) cout<<"N Close hits in GTK3: "<<count<<" = 0"<<endl;
  if(count>0) return;
  FillHisto("hCut", cutID);
  cutID++;

  //good GTK candidate
  bool goodGTK = IsGoodCandidate(GTKCand);
  if(verb) cout<<"Is good candidate? "<<goodGTK<<endl;
  if(!goodGTK) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //vertex Z - added lower cut because both K2pi and Kmu2 selection require it
  FillHisto("hVertexZ", vertex.Z());
  if(verb) cout<<"vertex Z: "<<"115000. < "<<vertex.Z()<<" < 165000."<<endl;
  if(vertex.Z()<115000. || vertex.Z()>165000.) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //Rstraw1 vs vertexZ
  TVector3 posTRACKatSTRAW1 = GetPositionAtZ(STRAWCand->GetThreeMomentumBeforeMagnet(), STRAWCand->GetPositionBeforeMagnet(), fZSTRAW1);
  double Rstraw1 = sqrt(pow(posTRACKatSTRAW1.X() - fSTRAW1CenterX, 2) + pow(posTRACKatSTRAW1.Y(), 2));
  if(fUseCutRStraw1VsVertexZ){
    FillHisto("hRstraw1vsVertexZ", vertex.Z(), Rstraw1);
    FillHisto("hVertexZ", vertex.Z());
    if(verb) cout<<"track position at STRAW1: "<<posTRACKatSTRAW1.X()<<" "<<posTRACKatSTRAW1.Y()<<" "<<posTRACKatSTRAW1.Z()<<endl;
    if(verb) cout<<"Rstraw1: "<<Rstraw1<<" > "<<fqMax1Rstraw1 + fkMax1Rstraw1*vertex.Z()<<endl;
    if(Rstraw1<=(fqMax1Rstraw1 + fkMax1Rstraw1*vertex.Z())) return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //N GTK hits with high ToT
  if(fUseCutToT){
    count = 0;
    std::vector<int> countInStation{0, 0, 0};
    if(verb) cout<<"N GTK hits = "<<GTKEvent->GetNHits()<<endl;
    for(int i=0; i<GTKEvent->GetNHits(); i++){
      GTKHit = static_cast<TRecoGigaTrackerHit*>(GTKEvent->GetHit(i));
      double deltaT = GTKHit->GetTime() - trackKTAGTime;
      if(verb){
	cout<<"hit "<<i<<" with T = "<<GTKHit->GetTime()<<" and deltaT = |"<<deltaT<<"| <= 1.2 to be in-time"<<endl;
	cout<<"hit "<<i<<" with ToT = "<<GTKHit->GetToT()<<" >= "<<fCutToT<<" to be high ToT"<<endl;
      };
      if(fabs(deltaT)>1.2) continue;
      if(verb) cout<<"in-time hit in station "<<GTKHit->GetStationNo()<<endl;
      countInStation.at(GTKHit->GetStationNo())++;
      FillHisto("hGTKToT", GTKHit->GetToT());
      if(GTKHit->GetToT()>=fCutToT){
	if(verb) cout<<"high ToT hit"<<endl;
	count++;
      };
    };
    FillHisto("hNHitsHighToT", count);
    if(verb) cout<<"N GTK hits with high ToT: "<<count<<" = 0"<<endl;
    if(count>0) return;
    if(verb) cout<<"N GTK hits in-time in stations: "<<countInStation.at(0)<<" < 50 "<<countInStation.at(1)<<" < 50 "<<countInStation.at(2)<<" < 50"<<endl;
    if(countInStation.at(0)>50 || countInStation.at(1)>50 || countInStation.at(2)>50) return; //not physical cut from RG (to not overflow array)
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  // no matched candidate in CHANTI
  TRecoCHANTIEvent *CHANTIEvent = GetEvent<TRecoCHANTIEvent>();
  bool hasCHANTI = false;
  for(int l=0; l<CHANTIEvent->GetNCandidates(); l++){
    TRecoCHANTICandidate *CHANTICand = static_cast<TRecoCHANTICandidate*>(CHANTIEvent->GetCandidate(l));
    double dT = CHANTICand->GetTime() - trackTime;
    double dTktag = CHANTICand->GetTime() - trackKTAGTime;
    double dTgtk = CHANTICand->GetTime() - trackGTKTime;
    FillHisto("hTimeDiffCHANTITrack", dT);
    if(!CHANTICand->GetXYMult()) continue;
    if(fabs(dT)<fCutTimeDiffTrackCHANTI || fabs(dTktag)<fCutTimeDiffTrackCHANTI || fabs(dTgtk)<fCutTimeDiffTrackCHANTI){
      hasCHANTI = true;
      break;
    };
  };
  if(verb) cout<<"Matched CHANTI: "<<hasCHANTI<<" = 0"<<endl;
  if(hasCHANTI) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //additional vertex upstream of GTK3 (100<z<105m)
  std::vector<TVector3> trackVertices = AllVertices.at(trackID);
  bool hasAdditVertex = false;
  for(unsigned int k=0; k<trackVertices.size(); k++){
    double z = trackVertices.at(k).Z();
    if(z<fCutAdditionalVertexMaxZ && z>fCutAdditionalVertexMinZ){
      hasAdditVertex = true;
      break;
    };
  };
  if(verb) cout<<"Found additional vertex? "<<hasAdditVertex<<endl;
  if(hasAdditVertex) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  fDecaySelected = true;
  if(verb) cout<<"Decay selected"<<endl;
}

void KaonDecaySelection::PostProcess(){}

void KaonDecaySelection::EndOfBurstUser(){}

void KaonDecaySelection::EndOfRunUser(){}

void KaonDecaySelection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void KaonDecaySelection::DrawPlot(){}

KaonDecaySelection::~KaonDecaySelection(){}

void KaonDecaySelection::PrepareOutputs(){
  fDecaySelected = false;
  SetOutputState("DecaySelected", kOInvalid);
}

void KaonDecaySelection::ValidateOutputs(){
  SetOutputState("DecaySelected", kOValid);
}

bool KaonDecaySelection::IsGoodCandidate(TRecoGigaTrackerCandidate *GTKCand){
  TVector3 mom = GTKCand->GetMomentum();
  if(verb) cout<<"GTK momentum: "<<fCutMinGTKMomentum<<" < "<<mom.Mag()<<" < "<<fCutMaxGTKMomentum<<endl;
  FillHisto("hGTKMom", mom.Mag());
  if((mom.Mag()<fCutMinGTKMomentum) || (mom.Mag()>fCutMaxGTKMomentum)){
    if(verb) cout<<"Not good candidate (wrong momentum), try other candidates."<<endl;
    return 0;
  };

  double slopeX = mom.X()/mom.Z();
  double slopeY = mom.Y()/mom.Z();
  if(verb) cout<<"SlopeX: "<<fMinSlopeX<<" < "<<slopeX<<" < "<<fMaxSlopeX<<endl;
  if(verb) cout<<"SlopeY: "<<fMinSlopeY<<" < "<<slopeY<<" < "<<fMaxSlopeY<<endl;
  FillHisto("hGTKSlopeYvsSlopeX", slopeX, slopeY);
  if((slopeX<fMinSlopeX) || (slopeX>fMaxSlopeX) || (slopeY<fMinSlopeY) || (slopeY>fMaxSlopeY)){
    if(verb) cout<<"Not good candidate (wrong slopes), try other candidates."<<endl;
    return 0;
  };

  //TVector3 nomKaonMom = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  //TVector3 nomMomBefTrim5 = MomAfterKick(nomKaonMom, 91.3);
  double meanSlopeX = 0.00002; //before: nomMomBefTrim5.X()/nomMomBefTrim5.Z()
  double meanSlopeY = 0.00002; //before: nomMomBefTrim5.Y()/nomMomBefTrim5.Z()
  double thetaXtrim5 = 0.0012; //before: 0.0012
  double trimKick = 91.3;
  thetaXtrim5 = trimKick/mom.Mag();
  if(verb){
    cout<<"Px = "<<mom.X()<<endl;
    cout<<"Py = "<<mom.Y()<<endl;
    cout<<"Pz = "<<mom.Z()<<endl;
    cout<<"slopeX = "<<slopeX<<endl;
    cout<<"thetaXtrim5 = "<<thetaXtrim5<<endl;
    cout<<"mean slopeX = "<<meanSlopeX<<endl;
    cout<<"slopeY = "<<slopeY<<endl;
    cout<<"mean slopeY = "<<meanSlopeY<<endl;
    cout<<"slopeX - thetaXtrim5 - meanSlopeX = "<<slopeX - thetaXtrim5 - meanSlopeX<<endl;
    cout<<"slopeY - meanSlopeY = "<<slopeY - meanSlopeY<<endl;
  };
  double thetaCorr = sqrt(pow(slopeX - thetaXtrim5 - meanSlopeX, 2) + pow(slopeY - meanSlopeY, 2));
  if(verb) cout<<"ThetaCorr: "<<thetaCorr<<" < "<<fMaxThetaCorr<<endl;
  FillHisto("hThetaCorr", thetaCorr);
  if(thetaCorr>fMaxThetaCorr){
    if(verb) cout<<"Not good candidate (wrong thetaCorr), try other candidates."<<endl;
    return 0;
  };

  return 1;
}

