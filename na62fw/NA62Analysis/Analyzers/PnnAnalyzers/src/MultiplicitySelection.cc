#include "MultiplicitySelection.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "SpectrometerNewCHODAssociationOutput.hh"
#include "GeometricAcceptance.hh"
#include "MCSimple.hh"
#include "PnnFunctions.hh"
#include "functions.hh"
#include "Event.hh"
#include "NA62ConditionsService.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
MultiplicitySelection::MultiplicitySelection(Core::BaseAnalysis *ba) : Analyzer(ba, "MultiplicitySelection")
{
  RequestTree("HAC", new TRecoHACEvent, "Reco");
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("CHOD", new TRecoCHODEvent, "Reco");
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestTree("MUV0", new TRecoMUV0Event, "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");

  AddParam("MaxNCHODSlabs", "int", &fMaxNCHODSlabs, 3);
  AddParam("CutTimeDiffTrackCHODSlab", "double", &fCutTimeDiffTrackCHODSlab, 7.);
  AddParam("CutMaxDistanceCHODLKrCoincidence", "double", &fCutMaxDistanceCHODLKrCoincidence, 130.);
  AddParam("CutTimeDiffCHODLKrCoincidence", "double", &fCutTimeDiffCHODLKrCoincidence, 15.);
  AddParam("CHODSlabHalfWidth", "double", &fCHODSlabHalfWidth, 32.515);
  AddParam("CutMaxYDistanceCHODNewCHODCoincidence", "double", &fCutMaxYDistanceCHODNewCHODCoincidence, 140.);
  AddParam("CutMaxXDistanceCHODNewCHODCoincidence", "double", &fCutMaxXDistanceCHODNewCHODCoincidence, 250.);
  AddParam("CutTimeDiffCHODNewCHODCoincidence", "double", &fCutTimeDiffCHODNewCHODCoincidence, 15.);
  AddParam("CutMaxYDistanceNewCHODLKrCoincidence", "double", &fCutMaxYDistanceNewCHODLKrCoincidence, 140.);
  AddParam("CutMaxXDistanceNewCHODLKrCoincidence", "double", &fCutMaxXDistanceNewCHODLKrCoincidence, 250.);
  AddParam("CutTimeDiffTrackHAC", "double", &fCutTimeDiffTrackHAC, 3.);
  AddParam("CutMaxTimeDiffTrackMUV0", "double", &fCutMaxTimeDiffTrackMUV0, 8.);
  AddParam("CutMInTimeDiffTrackMUV0", "double", &fCutMinTimeDiffTrackMUV0, -10.);
  AddParam("CutMinDistanceLKrTrackMergedLikeLKrCluster", "double", &fCutMinDistanceLKrTrackMergedLikeLKrCluster, 40.);
  AddParam("CutMaxDistanceLKrTrackMergedLikeLKrCluster", "double", &fCutMaxDistanceLKrTrackMergedLikeLKrCluster, 100.);
  AddParam("CutLKrEnergy", "double", &fCutLKrEnergy, 2000.);
  AddParam("CutTimeDiffTrackLKrMergedLikeLKrCluster", "double", &fCutTimeDiffTrackLKrMergedLikeLKrCluster, 6.); //Rado suggested to change this from 10 to 6
  AddParam("CutEnergyLKrExtra", "double", &fCutEnergyLKrExtra, 50.);
  AddParam("CutLowEnergyLKrExtra", "double", &fCutLowEnergyLKrExtra, 300.);
  AddParam("CutHighEnergyLKrExtra", "double", &fCutHighEnergyLKrExtra, 2000.);
  AddParam("CutTimeDiffForLowEnergyLKrExtra", "double", &fCutTimeDiffForLowEnergyLKrExtra, 4.);
  AddParam("CutMinTimeDiffForMiddleEnergyLKrExtra", "double", &fCutMinTimeDiffForMiddleEnergyLKrExtra, -7.);
  AddParam("CutMaxTimeDiffForMiddleEnergyLKrExtra", "double", &fCutMaxTimeDiffForMiddleEnergyLKrExtra, 10.);
  AddParam("CutTimeDiffForHighEnergyLKrExtra", "double", &fCutTimeDiffForHighEnergyLKrExtra, 10.);
  AddParam("CutMinDistanceLKrExtra", "double", &fCutMinDistanceLKrExtra, 100.);
  AddParam("CutMinDistanceNewCHODExtra", "double", &fCutMinDistanceNewCHODExtra, 100.);
  AddParam("CutTimeDiffNewCHODExtra", "double", &fCutTimeDiffNewCHODExtra, 5.);

  fZLKr = GeometricAcceptance::GetInstance()->GetZLKr();
  fZNewCHOD = GeometricAcceptance::GetInstance()->GetZNewCHOD();
  fZCHODV = GeometricAcceptance::GetInstance()->GetZCHODVPlane();
  fZCHODH = GeometricAcceptance::GetInstance()->GetZCHODHPlane();

  fOffsetX = 0.;
  fOffsetY = 0.;

  EnablePrefix(false);
}

void MultiplicitySelection::InitOutput(){
  RegisterOutput("Multiplicity", &fMultiplicity);
}

void MultiplicitySelection::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 30, 1, 31));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 20, 1, 21, 85, 0., 85000.));
    BookHisto(new TH1I("hNCHODSlabsInTime", "hNCHODSlabsInTime", 10, 0, 10));
    BookHisto(new TH2D("hPosDiffCHODLKrYvsX", "hPosDiffCHODLKrYvsX", 500, 0., 1000., 500, 0., 1000.));
    BookHisto(new TH1D("hTimeDiffCHODLKr", "hTimeDiffCHODLKr", 50, -25., 25.));
    BookHisto(new TH1D("hPosDiffCHODLKr", "hPosDiffCHODLKr", 250, 0., 2000.));
    BookHisto(new TH2D("hPosDiffLKrCHODvsTimeDiffLKrCHOD", "hPosDiffLKrCHODvsTimeDiffLKrCHOD", 50, -25., 25., 1000, 0., 1000.));
    BookHisto(new TH2D("hLKrCHODCoincPosDiffTrackCHODYvsX", "hPosDiffTrackCHODYvsX", 500, 0., 500., 500, 0., 500.));
    BookHisto(new TH1I("hLKrCHODCoincidence", "hLKrCHODCoincidence", 2, 0, 2));
    BookHisto(new TH2D("hPosDiffCHODNewCHODYvsX", "hPosDiffCHODNewCHODYvsX", 500, 0., 1000., 500, 0., 1000.));
    BookHisto(new TH1D("hTimeDiffCHODNewCHOD", "hTimeDiffCHODNewCHOD", 50, -25., 25.));
    BookHisto(new TH1D("hPosDiffCHODNewCHOD", "hPosDiffCHODNewCHOD", 250, 0., 1000.));
    BookHisto(new TH2D("hPosDiffCHODNewCHODvsTimeDiffCHODNewCHOD", "hPosDiffCHODNewCHODvsTimeDiffCHODNewCHOD", 50, -25., 25., 1000, 0., 1000.));
    BookHisto(new TH2D("hCHODNewCHODCoincPosDiffTrackCHODYvsX", "hPosDiffTrackCHODYvsX", 500, 0., 500., 500, 0., 500.));
    BookHisto(new TH1I("hNewCHODCHODCoincidence", "hNewCHODCHODCoincidence", 2, 0, 2));
    BookHisto(new TH2D("hPosDiffLKrNewCHODYvsX", "hPosDiffLKrNewCHODYvsX", 500, 0., 2000., 500, 0., 2000.));
    BookHisto(new TH1I("hLKrNewCHODCoincidence", "hLKrNewCHODCoincidence", 2, 0, 2));
    BookHisto(new TH1D("hTimeDiffHACTrack", "hTimeDiffHACTrack", 50, -25., 25.));
    BookHisto(new TH1I("hHACInTime", "hHACInTime", 10, 0, 10));
    BookHisto(new TH1D("hTimeDiffMUV0Track", "hTimeDiffMUV0Track", 50, -25., 25.));
    BookHisto(new TH1I("hMUV0InTime", "hMUV0InTime", 10, 0, 10));
    BookHisto(new TH1I("hMergedLikeLKrCluster", "hMergedLikeLKrCluster", 2, 0, 2));
  };
}

void MultiplicitySelection::DefineMCSimple(){
}

void MultiplicitySelection::StartOfRunUser(){
  if(!GetWithMC()) ReadOffsets(fOffsetX, fOffsetY);
  ReadSlabWidths(fSlabWidth);
  ReadSlabCenters(fSlabCenter);
  ReadLightVelocities(fLV);
  ReadSlewCorrections(fSlewSlope, fSlewConst, GetWithMC());
}

void MultiplicitySelection::StartOfBurstUser(){
}

void MultiplicitySelection::ProcessSpecialTriggerUser(int, unsigned int){
}

void MultiplicitySelection::Process(int iEvent){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"MultiplicitySelection"<<endl;
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

  int trackID =
    *(int*)GetOutput("SingleTrackEventSelection.TrackID", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  cout<<user()<<"Track ID read = "<<trackID<<endl;
  FillHisto("hCut", cutID);
  cutID++;

  auto trackCHODTimes =
    *(std::vector<double>*)GetOutput("BestTrackSelection.BestTrackCHODTime", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;

  double trackTime = trackCHODTimes.at(trackID);
  cout<<user()<<"track time = "<<trackTime<<endl;

  auto CHODAssocID =
    *(std::vector<int>*)GetOutput("BestTrackSelection.CHODAssocID", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  cout<<user()<<"CHOD assoc ID = "<<CHODAssocID.at(trackID)<<endl;

  auto NewCHODAssocID =
    *(std::vector<int>*)GetOutput("BestTrackSelection.NewCHODAssocID", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  cout<<user()<<"New CHOD assoc ID = "<<NewCHODAssocID.at(trackID)<<endl;

  auto LKrAssocID =
    *(std::vector<int>*)GetOutput("BestTrackSelection.LKrAssocID", state);
  if(state!=kOValid){
    cout<<user()<<"Requested output is not valid"<<endl;
    return;
  };
  FillHisto("hCut", cutID);
  cutID++;
  fLKrID = LKrAssocID.at(trackID);
  cout<<user()<<"LKr assoc ID = "<<fLKrID<<endl;

  ValidateOutputs();

  TRecoHACEvent* HACEvent = GetEvent<TRecoHACEvent>();
  TRecoMUV0Event* MUV0Event = GetEvent<TRecoMUV0Event>();
  fCHODEvent = GetEvent<TRecoCHODEvent>();
  TRecoNewCHODEvent* NewCHODEvent = GetEvent<TRecoNewCHODEvent>();
  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate *STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(trackID));

  //find extra activity in LKr
  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"----Find LKr extra activity----"<<endl;
  };
  std::vector<int> LKrExtra;
  TVector3 posAtLKr(STRAWCand->xAt(fZLKr), STRAWCand->yAt(fZLKr), fZLKr);
  LKrExtraActivity(LKrExtra, trackTime, posAtLKr);
  cout<<user()<<"------------found------------"<<endl;

  //find extra activity in NewCHOD
  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"----Find NewCHOD extra activity----"<<endl;
  };
  std::vector<int> NewCHODExtra;
  auto SpecNewCHOD =
    *(std::vector<SpectrometerNewCHODAssociationOutput>*)GetOutput("SpectrometerNewCHODAssociation.Output", state);
  int tileID = SpecNewCHOD[trackID].GetAssociationRecord(NewCHODAssocID.at(trackID))->GetTileID();
  TVector3 posAtNewCHOD(STRAWCand->xAt(fZNewCHOD), STRAWCand->yAt(fZNewCHOD), fZNewCHOD);
  NewCHODExtraActivity(NewCHODExtra, trackTime, tileID);
  cout<<user()<<"------------found------------"<<endl;
  cout<<user()<<endl;

  //HAC
  int cHAC = 0;
  cout<<user()<<"N HAC hits: "<<HACEvent->GetNHits()<<endl;
  for(int j=0; j<HACEvent->GetNHits(); j++){
    TRecoHACHit* HACHit = static_cast<TRecoHACHit*>(HACEvent->GetHit(j));
    FillHisto("hTimeDiffHACTrack", HACHit->GetTime() - trackTime);
    if(fabs(HACHit->GetTime() - trackTime)<fCutTimeDiffTrackHAC){
      cHAC++;
    };
  };
  FillHisto("hHACInTime", cHAC);
  cout<<user()<<"HAC in time? "<<(cHAC>0?1:0)<<endl;
  if(cHAC>0 || (GetWithMC() && HACEvent->GetNHits()>0)){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //MUV0
  int cMUV0 = 0;
  cout<<user()<<"N MUV0 hits: "<<MUV0Event->GetNHits()<<endl;
  for(int j=0; j<MUV0Event->GetNHits(); j++){
    TRecoMUV0Hit* MUV0Hit = static_cast<TRecoMUV0Hit*>(MUV0Event->GetHit(j));
    FillHisto("hTimeDiffMUV0Track", MUV0Hit->GetTime() - trackTime);
    double dt = trackTime-MUV0Hit->GetTime();
    if(dt<fCutMaxTimeDiffTrackMUV0 && dt>fCutMinTimeDiffTrackMUV0){
      cMUV0++;
    };
  };
  FillHisto("hMUV0InTime", cMUV0);
  cout<<user()<<"MUV0 in time? "<<(cMUV0>0 ? 1 : 0)<<endl;
  if(cMUV0>0 || (GetWithMC() && MUV0Event->GetNHits()>0)){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //***HIT MULTIPLICITY***
  if(TestLevel(Verbosity::kUser)){
    cout<<endl;
    cout<<"----Hit multiplicity:----"<<endl;
  };

  //Find CHOD slabs (hits actually) in time
  int NSlabsInTime = NCHODSlabs(fCHODEvent, trackTime, CHODAssocID.at(trackID), fCutTimeDiffTrackCHODSlab);
  FillHisto("hNCHODSlabsInTime", NSlabsInTime);
  cout<<user()<<"N CHOD hits in time: "<<NSlabsInTime<<" <= "<<fMaxNCHODSlabs<<endl;
  if(NSlabsInTime>fMaxNCHODSlabs){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //preparation
  std::array<std::vector<int>, 4> Hhits;
  std::array<std::vector<int>, 4> Vhits;
  SortCHODHits(fCHODEvent, Hhits, Vhits);
  TRecoCHODHit *CHODHitH;
  TRecoCHODHit *CHODHitV;
  double widthV;
  double widthH;
  TVector2 posTrackCHOD(STRAWCand->xAt(fZCHODV) - fOffsetX, STRAWCand->yAt(fZCHODH) - fOffsetY);

  //no extra activity in coincidence LKr - CHOD
  cout<<user()<<endl;
  cout<<user()<<"Look for LKr-CHOD coincidence:"<<endl;
  bool LKrCHODCoincidence = false;
  cout<<user()<<"How many LKr extra? "<<LKrExtra.size()<<endl;
  for(unsigned int j=0; j<LKrExtra.size(); j++){
    cout<<user()<<endl;
    cout<<user()<<"LKr hit "<<LKrExtra.at(j)<<" at position "<<j<<endl;
    TRecoLKrHit *LKrHit = static_cast<TRecoLKrHit*>(LKrEvent->GetHit(LKrExtra.at(j)));
    TVector2 posLKr = LKrHit->GetPosition().XYvector();
    double D = 1000000000.;
    int hH = -1;
    int hV = -1;
    int hPlane = -1;
    TVector2 posCHOD;
    double timeHV = 0.;
    double tHV = 0.;
    for(int m=0; m<4; m++){
      for(unsigned int k=0; k<Hhits[m].size(); k++){
	CHODHitH = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(Hhits[m].at(k)));
	for(unsigned int l=0; l<Vhits[m].size(); l++){
	  CHODHitV = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(Vhits[m].at(l)));
	  cout<<user()<<"test CHOD H hit = "<<Hhits[m].at(k)<<" V hit = "<<Vhits[m].at(l)<<endl;
	  FillHisto("hPosDiffCHODLKrYvsX", fabs(CHODHitV->GetPosition().X() - posLKr.X()), fabs(CHODHitH->GetPosition().Y() - posLKr.Y()));
	  posCHOD.Set(CHODHitV->GetPosition().X(), CHODHitH->GetPosition().Y());
	  double timeHcorr = 0.;
	  double timeVcorr = 0.;
	  CorrectCHODHitsTime(fCHODEvent, Vhits[m].at(l), Hhits[m].at(k), fLV, fSlabCenter, fSlewSlope, fSlewConst, GetWithMC(), timeVcorr, timeHcorr);
	  tHV = (timeVcorr + timeHcorr)/2.;
	  cout <<user()<<"Test this coincidence: LKr extra " << j << " CHODpos=(" << posCHOD.X() << ", " << posCHOD.Y() << "), LKrpos=(" << posLKr.X() << ", " << posLKr.Y() << "), CHODtime = "<< tHV << ", LKr time = " << LKrHit->GetTime() << endl;
	  FillHisto("hTimeDiffCHODLKr", tHV - LKrHit->GetTime());
	  FillHisto("hPosDiffCHODLKr", (posCHOD-posLKr).Mod());
	  double d = pow((posCHOD-posLKr).Mod()/(2.*13.), 2) + pow((tHV-LKrHit->GetTime())/(3.*5.6), 2) + pow((timeVcorr-timeHcorr)/(3.*3.), 2);
	  cout<<user()<<"d = "<<d<<endl;
	  if(d<D){
	    cout<<user()<<"is min d"<<endl;
	    D=d;
	    hH=Hhits[m].at(k);
	    hV=Vhits[m].at(l);
	    hPlane=m;
	    timeHV = tHV;;
	  };
	};
      };
    };
    if(hH==-1 || hV==-1 || hPlane==-1) continue;
    cout<<user()<<"Pair with min d: H = "<<hH<<" V = "<<hV<<endl;
    CHODHitH = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(hH));
    CHODHitV = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(hV));
    widthV = fSlabWidth[CHODHitV->GetChannelID()];
    widthH = fSlabWidth[CHODHitH->GetChannelID()];
    posCHOD.Set(CHODHitV->GetPosition().X(), CHODHitH->GetPosition().Y());
    FillHisto("hPosDiffLKrCHODvsTimeDiffLKrCHOD", timeHV - LKrHit->GetTime(), (posCHOD-posLKr).Mod());
    cout<<user()<<"Distance = "<<(posCHOD-posLKr).Mod()<<" < "<<fCutMaxDistanceCHODLKrCoincidence<<" and deltaT = "<<fabs(timeHV - LKrHit->GetTime())<<" < "<<fCutTimeDiffCHODLKrCoincidence<<" to be coincidence."<<endl;
    if((posCHOD-posLKr).Mod()>=fCutMaxDistanceCHODLKrCoincidence || fabs(timeHV - LKrHit->GetTime())>=fCutTimeDiffCHODLKrCoincidence){
      cout<<user()<<"LKr hit too far from CHOD pair or not close in time. Try another hit."<<endl;
      continue;
    };
    cout<<user()<<"LKr hit close to CHOD pair and close in time. Could be coincidence."<<endl;
    FillHisto("hLKrCHODCoincPosDiffTrackCHODYvsX", fabs(posTrackCHOD.X()-CHODHitV->GetPosition().X()), fabs(posTrackCHOD.Y()-CHODHitH->GetPosition().Y()));
    if(fabs(posTrackCHOD.X()-CHODHitV->GetPosition().X())<widthV/2. && fabs(posTrackCHOD.Y()-CHODHitH->GetPosition().Y())<widthH/2.){
      cout<<user()<<"CHOD pair corresponds to track. Try another hit."<<endl;
      continue;
    };
    cout<<user()<<"CHOD pair does not correspond to track. Found LKr-CHOD coincidence."<<endl;
    LKrCHODCoincidence = true;
    break;
  };
  FillHisto("hLKrCHODCoincidence", (int)LKrCHODCoincidence);
  cout<<user()<<"LKr CHOD coincidence? "<<LKrCHODCoincidence<<endl;
  if(LKrCHODCoincidence){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;


  //no extra activity in coincidence NewCHOD  - CHOD
  cout<<user()<<"Look for NewCHOD-CHOD coincidence:"<<endl;
  bool NewCHODCHODCoincidence = false;
  cout<<user()<<"How many NewCHOD extra? "<<NewCHODExtra.size()<<endl;
  for(unsigned int j=0; j<NewCHODExtra.size(); j++){
    cout<<user()<<endl;
    cout<<user()<<"NewCHOD hit "<<NewCHODExtra.at(j)<<" at position "<<j<<endl;
    TRecoNewCHODHit *NewCHODHit = static_cast<TRecoNewCHODHit*>(NewCHODEvent->GetHit(NewCHODExtra.at(j)));
    TVector2 posNewCHOD = NewCHODHit->GetPosition().XYvector();
    double D = 1000000000.;
    int hH = -1;
    int hV = -1;
    int hPlane = -1;
    TVector2 posCHOD;
    double timeHV = 0.;
    double tHV = 0.;
    for(int m=0; m<4; m++){
      for(unsigned int k=0; k<Hhits[m].size(); k++){
	CHODHitH = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(Hhits[m].at(k)));
	for(unsigned int l=0; l<Vhits[m].size(); l++){
	  CHODHitV = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(Vhits[m].at(l)));
	  cout<<user()<<"test CHOD H hit = "<<Hhits[m].at(k)<<" V hit = "<<Vhits[m].at(l)<<endl;
	  FillHisto("hPosDiffCHODNewCHODYvsX", fabs(CHODHitV->GetPosition().X() - posNewCHOD.X()), fabs(CHODHitH->GetPosition().Y() - posNewCHOD.Y()));
	  posCHOD.Set(CHODHitV->GetPosition().X(), CHODHitH->GetPosition().Y());
	  double timeHcorr = 0.;
	  double timeVcorr = 0.;
	  CorrectCHODHitsTime(fCHODEvent, Vhits[m].at(l), Hhits[m].at(k), fLV, fSlabCenter, fSlewSlope, fSlewConst, GetWithMC(), timeVcorr, timeHcorr);
	  tHV = (timeVcorr + timeHcorr)/2.;
	  cout << user() << "Test this coincidence: NewCHOD extra " << j << " CHODpos=(" << posCHOD.X() << ", " << posCHOD.Y() << "), NewCHODpos=(" << posNewCHOD.X() << ", " << posNewCHOD.Y() << "), CHODtime = "<< tHV << ", NewCHOD time = " << NewCHODHit->GetTime() << endl;
	  FillHisto("hTimeDiffCHODNewCHOD", tHV - NewCHODHit->GetTime());
	  FillHisto("hPosDiffCHODNewCHOD", (posCHOD-posNewCHOD).Mod());
	  double d = pow((posCHOD-posNewCHOD).Mod()/(sqrt(6.)*16.), 2) + pow((tHV-NewCHODHit->GetTime())/(sqrt(3.)*7.), 2) + pow((timeVcorr-timeHcorr)/(3.*6.), 2);
	  cout<<user()<<"d = "<<d<<endl;
	  if(d<D){
	    cout<<user()<<"is min d"<<endl;
	    D=d;
	    hH=Hhits[m].at(k);
	    hV=Vhits[m].at(l);
	    hPlane=m;
	    timeHV = tHV;;
	  };
	};
      };
    };
    if(hH==-1 || hV==-1 || hPlane==-1) continue;
    cout<<user()<<"Pair with min d: H = "<<hH<<" V = "<<hV<<endl;
    CHODHitH = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(hH));
    CHODHitV = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(hV));
    widthV = fSlabWidth[CHODHitV->GetChannelID()];
    widthH = fSlabWidth[CHODHitH->GetChannelID()];
    posCHOD.Set(CHODHitV->GetPosition().X(), CHODHitH->GetPosition().Y());
    FillHisto("hPosDiffCHODNewCHODvsTimeDiffCHODNewCHOD", timeHV - NewCHODHit->GetTime(), (posCHOD-posNewCHOD).Mod());
    cout<<user()<<"DistanceX = "<<fabs(posCHOD.X()-posNewCHOD.X())<<" < "<<fCutMaxXDistanceCHODNewCHODCoincidence<<" and distanceY = "<<fabs(posCHOD.Y()-posNewCHOD.Y())<<" < "<<fCutMaxYDistanceCHODNewCHODCoincidence<<" and deltaT = "<<fabs(timeHV - NewCHODHit->GetTime())<<" < "<<fCutTimeDiffCHODNewCHODCoincidence<<" to be coincidence."<<endl;
    if(fabs(timeHV - NewCHODHit->GetTime())>=fCutTimeDiffCHODNewCHODCoincidence || fabs(posCHOD.X()-posNewCHOD.X())>fCutMaxXDistanceCHODNewCHODCoincidence || fabs(posCHOD.Y()-posNewCHOD.Y())>fCutMaxYDistanceCHODNewCHODCoincidence){
      cout<<user()<<"NewCHOD hit too far from CHOD pair or not close in time. Try another hit."<<endl;
      continue;
    };
    cout<<user()<<"NewCHOD hit close to CHOD pair and close in time. Could be coincidence."<<endl;
    FillHisto("hCHODNewCHODCoincPosDiffTrackCHODYvsX", fabs(posTrackCHOD.X()-CHODHitV->GetPosition().X()), fabs(posTrackCHOD.Y()-CHODHitH->GetPosition().Y()));
    if(fabs(posTrackCHOD.X()-CHODHitV->GetPosition().X())<widthV/2. && fabs(posTrackCHOD.Y()-CHODHitH->GetPosition().Y())<widthH/2.){
      cout<<user()<<"CHOD pair corresponds to track. Try another hit."<<endl;
      continue;
    };
    cout<<user()<<"CHOD pair does not correspond to track. Found NewCHOD-CHOD coincidence."<<endl;
    NewCHODCHODCoincidence = true;
    break;
  };
  FillHisto("hNewCHODCHODCoincidence", (int)NewCHODCHODCoincidence);
  cout<<user()<<"NewCHOD CHOD coincidence? "<<NewCHODCHODCoincidence<<endl;
  if(NewCHODCHODCoincidence){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

 //no extra activity in coincidence LKr - NewCHOD
  cout<<user()<<"Look for LKr-NewCHOD coincidence:"<<endl;
  bool LKrNewCHODCoincidence = false;
  for(unsigned int j=0; j<LKrExtra.size(); j++){
    cout<<user()<<endl;
    cout<<user()<<"LKr hit "<<LKrExtra.at(j)<<" at position "<<j<<endl;
    TRecoLKrHit *LKrHit = static_cast<TRecoLKrHit*>(LKrEvent->GetHit(LKrExtra.at(j)));
    TVector2 posLKr = LKrHit->GetPosition().XYvector();
    for(unsigned int k=0; k<NewCHODExtra.size(); k++){
      cout<<user()<<"NewCHOD hit "<<NewCHODExtra.at(k)<<" at position "<<k<<endl;
      TRecoNewCHODHit *NewCHODHit = static_cast<TRecoNewCHODHit*>(NewCHODEvent->GetHit(NewCHODExtra.at(k)));
      TVector2 posNewCHOD = NewCHODHit->GetPosition().XYvector();
      FillHisto("hPosDiffLKrNewCHODYvsX", fabs(posLKr.X() - posNewCHOD.X()), fabs(posLKr.Y() - posNewCHOD.Y()));
      if(fabs(posLKr.Y() - posNewCHOD.Y())<fCutMaxYDistanceNewCHODLKrCoincidence && fabs(posLKr.X() - posNewCHOD.X())<fCutMaxXDistanceNewCHODLKrCoincidence){
	cout<<user()<<"Hits are close. Found coincidence."<<endl;
	LKrNewCHODCoincidence = true;
	break;
      };
      cout<<user()<<"Hits too distant. Try other hits."<<endl;
    };
    if(LKrNewCHODCoincidence) break;
  };
  FillHisto("hLKrNewCHODCoincidence", (int)LKrNewCHODCoincidence);
  cout<<user()<<"LKr NewCHOD coincidence? "<<LKrNewCHODCoincidence<<endl;
  if(LKrNewCHODCoincidence){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  //merged-like LKr cluster
  bool mergedLKr = MergedLikeLKrCluster(trackTime, posAtLKr);
  FillHisto("hMergedLikeLKrCluster", (int)mergedLKr);
  cout<<user()<<"Merged-like LKr cluster? "<<mergedLKr<<endl;
  if(mergedLKr){
    fMultiplicity = true;
    cout<<user()<<"Multiplicity is true"<<endl;
    return;
  };
  cout<<user()<<"Multiplicity is false"<<endl;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;
}

void MultiplicitySelection::PostProcess(){
}

void MultiplicitySelection::EndOfBurstUser(){
}

void MultiplicitySelection::EndOfRunUser(){
}

void MultiplicitySelection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void MultiplicitySelection::DrawPlot(){
}

MultiplicitySelection::~MultiplicitySelection(){
}

void MultiplicitySelection::PrepareOutputs(){
  fMultiplicity = false;
  SetOutputState("Multiplicity", kOInvalid);
}

void MultiplicitySelection::ValidateOutputs(){
  SetOutputState("Multiplicity", kOValid);
}

bool MultiplicitySelection::MergedLikeLKrCluster(double tTrack, TVector3 v){
  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  bool isClose = false;
  bool isMatchedCluster = false;
  TVector2 posTrack(v.X(), v.Y());
  for(int i=0; i<LKrEvent->GetNCandidates(); i++){
    if(i==fLKrID){
      isMatchedCluster = true;
      continue;
    };
    TRecoLKrCandidate* LKrCand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
    TVector2 pos(LKrCand->GetClusterX(), LKrCand->GetClusterY());
    if((pos-posTrack).Mod()<fCutMinDistanceLKrTrackMergedLikeLKrCluster || (pos-posTrack).Mod()>fCutMaxDistanceLKrTrackMergedLikeLKrCluster) continue;
    if(fabs(LKrCand->GetTime()-tTrack)>fCutTimeDiffTrackLKrMergedLikeLKrCluster) continue;
    isClose = true;
    break;
  };
  if(isMatchedCluster && isClose){
    return true;
  }else{
    return false;
  };
}

void MultiplicitySelection::LKrExtraActivity(std::vector<int> &LKrExtra, double tTrack, TVector3 v){
  cout<<user()<<endl;
  cout<<user()<<"find LKr extra activity: "<<endl;
  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  for(int i=0; i<LKrEvent->GetNHits(); i++){
    cout<<user()<<"hit "<<i<<endl;
    TRecoLKrHit *LKrHit = static_cast<TRecoLKrHit*>(LKrEvent->GetHit(i));
    double E = LKrHit->GetEnergy();
    cout<<user()<<"hit energy = "<<E<<endl;
    cout<<user()<<"hit time = "<<LKrHit->GetTime()<<endl;
    cout<<user()<<"track time = "<<tTrack<<endl;
    if(E<=fCutEnergyLKrExtra) continue;
    if(E<fCutLowEnergyLKrExtra){
      if(fabs(tTrack - LKrHit->GetTime())>=fCutTimeDiffForLowEnergyLKrExtra) continue;
    }else if(E>=fCutLowEnergyLKrExtra && E<fCutHighEnergyLKrExtra){
      if((tTrack - LKrHit->GetTime())<=fCutMinTimeDiffForMiddleEnergyLKrExtra || (tTrack - LKrHit->GetTime()>=fCutMaxTimeDiffForMiddleEnergyLKrExtra)) continue;
    }else if(E>=fCutHighEnergyLKrExtra){
      if(fabs(tTrack - LKrHit->GetTime())>=fCutTimeDiffForHighEnergyLKrExtra) continue;
    };
    TVector3 posHitLKr = LKrHit->GetPosition();
    cout<<user()<<"pos LKr = "<<posHitLKr.X()<<" "<<posHitLKr.Y()<<" "<<posHitLKr.Z()<<endl;
    cout<<user()<<"pos track at LKr = "<<v.X()<<" "<<v.Y()<<" "<<v.Z()<<endl;
    cout<<user()<<"distance = "<<(posHitLKr.XYvector() - v.XYvector()).Mod()<<" > "<<fCutMinDistanceLKrExtra<<" to be extra activity."<<endl;
    if((posHitLKr.XYvector() - v.XYvector()).Mod()<=fCutMinDistanceLKrExtra) continue;
    cout<<user()<<"is extra activity"<<endl;
    LKrExtra.push_back(i);
  };
}

void MultiplicitySelection::NewCHODExtraActivity(std::vector<int> &NewCHODExtra, double tTrack, int tileID){
  cout<<user()<<"find NewCHOD extra activity: "<<endl;
  TRecoNewCHODEvent* NewCHODEvent = GetEvent<TRecoNewCHODEvent>();
  for(int i=0; i<NewCHODEvent->GetNHits(); i++){
    cout<<user()<<"hit "<<i<<endl;
    TRecoNewCHODHit *NewCHODHit = static_cast<TRecoNewCHODHit*>(NewCHODEvent->GetHit(i));
    cout<<user()<<"hit tileID = "<<NewCHODHit->GetTileID()<<" != "<<tileID<<" = track tileID to be extra activity."<<endl;
    cout<<user()<<"hit time = "<<NewCHODHit->GetTime()<<endl;
    cout<<user()<<"track time = "<<tTrack<<endl;
    cout<<user()<<"time diff = "<<fabs(NewCHODHit->GetTime() - tTrack)<<" < "<<fCutTimeDiffNewCHODExtra<<" to be extra activity."<<endl;
    if(NewCHODHit->GetTileID()==tileID) continue;
    if(fabs(NewCHODHit->GetTime() - tTrack)>=fCutTimeDiffNewCHODExtra) continue;
    cout<<user()<<"is extra activity"<<endl;
    NewCHODExtra.push_back(i);
  };
}

