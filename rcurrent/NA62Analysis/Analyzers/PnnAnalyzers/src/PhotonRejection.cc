#include "PhotonRejection.hh"

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "GeometricAcceptance.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

PhotonRejection::PhotonRejection(Core::BaseAnalysis *ba) : Analyzer(ba, "PhotonRejection")
{
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("LAV", new TRecoLAVEvent, "Reco");
  RequestTree("IRC", new TRecoIRCEvent, "Reco");
  RequestTree("SAV", new TRecoSAVEvent, "Reco");
  RequestTree("SAC", new TRecoSACEvent, "Reco");

  AddParam("Verbosity", "bool", &verb, false);
  AddParam("LAVMinTimeCut", "double", &fLAVMinTimeCut, 3.);
  AddParam("LAVMaxTimeCut", "double", &fLAVMaxTimeCut, 3.);
  AddParam("IRCMinTimeCut", "double", &fIRCMinTimeCut, 7.);
  AddParam("IRCMaxTimeCut", "double", &fIRCMaxTimeCut, 7.);
  AddParam("SACMinTimeCut", "double", &fSACMinTimeCut, 7.);
  AddParam("SACMaxTimeCut", "double", &fSACMaxTimeCut, 7.);
  AddParam("IRCMinToT", "double", &fIRCMinToT, 2.);
  AddParam("IRCMaxToT", "double", &fIRCMaxToT, 25.);
  AddParam("fCutTimeDiffIRCTrack", "double", &fCutTimeDiffIRCTrack, 7.);
  AddParam("SACMinToT", "double", &fSACMinToT, 2.);
  AddParam("SACMaxToT", "double", &fSACMaxToT, 25.);
  AddParam("CutTimeDiffSACTrack1", "double", &fCutTimeDiffSACTrack1, 4.);
  AddParam("CutTimeDiffSACTrack2", "double", &fCutTimeDiffSACTrack2, 7.);
  AddParam("CutTimeDiffSACTrack3", "double", &fCutTimeDiffSACTrack3, -7.);
  AddParam("CutTimeDiffSACTrackMin", "double", &fCutTimeDiffSACTrackMin, -7.);
  AddParam("CutTimeDiffSACTrackMax", "double", &fCutTimeDiffSACTrackMax, 10.);
  AddParam("CutMinEnergySAVFADCHit", "double", &fCutMinEnergySAVFADCHit, 1000.);
  AddParam("CutTimeDiffSAVFADCHit", "double", &fCutTimeDiffSAVFADCHit, 7.);
  fZLKr = GeometricAcceptance::GetInstance()->GetZLKr();
}

void PhotonRejection::InitOutput(){
  RegisterOutput("Photons", &fPhotons);
}

void PhotonRejection::InitHist(){
  fReadingData = GetIsTree();

  if(fReadingData){
    BookHisto(new TH1I("hCut", "hCut", 30, 1, 31));
    BookHisto(new TH2D("hTrackMomentumVsCut", "hTrackMomentumVsCut", 30, 1, 31, 85, 0., 85000.));
    BookHisto(new TH1I("hLAVHasTimeMatching", "hLAVHasTimeMatching", 2, 0, 2));
    BookHisto(new TH1I("hSAVHasTimeMatching", "hSAVHasTimeMatching", 2, 0, 2));
    BookHisto(new TH1D("hIRCToT", "hIRCToT", 100, 0., 100.));
    BookHisto(new TH2D("hToTvsTimeDiffIRC", "hToTvsTimeDiffIRC", 50, -25., 25., 100, 0., 100.));
    BookHisto(new TH1I("hPhotonInIRC", "hPhotonInIRC", 2, 0, 2));
    BookHisto(new TH2D("hToTvsTimeDiffSAC", "hToTvsTimeDiffSAC", 50, -100., 100., 100, 0., 200.));
    BookHisto(new TH1I("hPhotonInSAC", "hPhotonInSAC", 2, 0, 2));
    BookHisto(new TH1D("hTimeSAVHit", "hTimeSAVHit", 50, -50., 50.));
    BookHisto(new TH1I("hWhichSAVHit", "hWhichSAVHit", 2, 0, 2));
    BookHisto(new TH2D("hWhichSAVHitVsTimeSAVHit", "hWhichSAVHitVsTimeSAVHit", 50, -50., 50., 2, 0., 2.));
    BookHisto(new TH2D("hSACFADCEnergyVsTimeDiff", "hSACFADCEnergyVsTimeDiff", 50, -50., 50., 100, 0., 10000.));
    BookHisto(new TH2D("hIRCFADCEnergyVsTimeDiff", "hIRCFADCEnergyVsTimeDiff", 50, -50., 50., 100, 0., 10000.));
    BookHisto(new TH1I("hPhotonInSAVFADC", "hPhotonInSAVFADC", 3, -1, 2));
  };
}

void PhotonRejection::DefineMCSimple(){
}

void PhotonRejection::StartOfRunUser(){
}

void PhotonRejection::StartOfBurstUser(){
}

void PhotonRejection::ProcessSpecialTriggerUser(int, unsigned int){
}

void PhotonRejection::Process(int iEvent){
  if(!fReadingData) return;

  int cutID = 1;
  FillHisto("hCut", cutID);
  cutID++;

  if(verb){
    cout<<endl;
    cout<<"-------------------"<<endl;
    cout<<"PhotonRejection"<<endl;
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

  ValidateOutputs();

  TRecoSpectrometerEvent* STRAWEvent = GetEvent<TRecoSpectrometerEvent>();
  TRecoSpectrometerCandidate *STRAWCand = static_cast<TRecoSpectrometerCandidate*>(STRAWEvent->GetCandidate(trackID));
  FillHisto("hTrackMomentumVsCut", cutID-1, STRAWCand->GetMomentum());

  TRecoLAVEvent *LAVEvent = GetEvent<TRecoLAVEvent>();
  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(trackTime);
  pLAVMatching->SetTimeCuts(fLAVMinTimeCut, fLAVMaxTimeCut);
  if(verb) cout<<"LAVMatching "<<"minTime = "<<trackTime-fLAVMinTimeCut<<"  maxTime = "<<trackTime+fLAVMaxTimeCut<<endl;
  if(verb) cout<<"LAV has matching? "<<pLAVMatching->LAVHasTimeMatching(LAVEvent)<<endl;
  if(verb) cout<<"Number of matched blocks: "<<pLAVMatching->GetNumberOfMatchedBlocks()<<endl;
  if(verb) pLAVMatching->Print();
  FillHisto("hLAVHasTimeMatching", (int)pLAVMatching->LAVHasTimeMatching(LAVEvent));
  if(pLAVMatching->LAVHasTimeMatching(LAVEvent)) return;
  if(pLAVMatching->GetNumberOfMatchedBlocks()!=0) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  TRecoIRCEvent *IRCEvent = GetEvent<TRecoIRCEvent>();
  TRecoSACEvent *SACEvent = GetEvent<TRecoSACEvent>();
  SAVMatching* pSAVMatching = *(SAVMatching**)GetOutput("PhotonVetoHandler.SAVMatching");
  pSAVMatching->SetReferenceTime(trackTime);
  pSAVMatching->SetIRCTimeCuts(fIRCMinTimeCut, fIRCMaxTimeCut); // half time window; default = 5ns
  pSAVMatching->SetSACTimeCuts(fSACMinTimeCut, fSACMaxTimeCut); // half time window; default = 5ns
  Bool_t SAVmatched = pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent, 0);
  if(verb) cout<<"IRCMatching "<<"minTime = "<<trackTime-fIRCMinTimeCut<<"  maxTime = "<<trackTime+fIRCMaxTimeCut<<endl;
  if(verb) cout<<"SACMatching "<<"minTime = "<<trackTime-fSACMinTimeCut<<"  maxTime = "<<trackTime+fSACMaxTimeCut<<endl;
  if(verb) cout<<"SAV has matching? "<<pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent, 0)<<endl;
  if(verb) pSAVMatching->Print();
  FillHisto("hSAVHasTimeMatching", (int)SAVmatched);
  if(SAVmatched) return;
  if(pSAVMatching->GetNumberOfIRCMatchedBlocks()!=0) return;
  if(pSAVMatching->GetNumberOfSACMatchedBlocks()!=0) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  bool photonInIRC = false;
  if(verb) cout<<"Photons in IRC? "<<endl;
  double fIRCPriorityMask[16];
  fIRCPriorityMask[0] = 0; // --SNH--
  fIRCPriorityMask[1] = 4; // LL __ __ __
  fIRCPriorityMask[2] = 5; // __ LH __ __
  fIRCPriorityMask[3] =10; // LL LH __ __
  fIRCPriorityMask[4] = 2; // __ __ TH __
  fIRCPriorityMask[5] = 7; // LL __ TH __
  fIRCPriorityMask[6] =11; // __ LH TH __
  fIRCPriorityMask[7] =13; // LL LH TH __
  fIRCPriorityMask[8] = 1; // __ __ __ TL
  fIRCPriorityMask[9] =12; // LL __ __ TL
  fIRCPriorityMask[10]= 6; // __ LH __ TL
  fIRCPriorityMask[11]=14; // LL LH __ TL
  fIRCPriorityMask[12]= 3; // __ __ TH TL
  fIRCPriorityMask[13]= 8; // LL __ TH TL
  fIRCPriorityMask[14]= 9; // __ LH TH TL
  fIRCPriorityMask[15]=15; // LL LH TH TL
  Double_t totest[4] = {-1.3,-1.3,-1.3,-1.3};
  if(!GetWithMC()){
    totest[0] += -0.65+1.;
    totest[1] += 0.6-0.25;
    totest[2] += 0.+0.26;
    totest[3] += 0.36;
  }else{
    totest[0] = -2.5;
    totest[1] = -2.5;
    totest[2] = -2.5;
    totest[3] = -2.5;
  };
  Int_t bestIRCHitType = 0;
  for(Int_t jHit=0; jHit<IRCEvent->GetNHits(); jHit++){
    TRecoIRCHit *hit = static_cast<TRecoIRCHit*>(IRCEvent->GetHit(jHit));
    Int_t edge = hit->GetEdgeMask();
    if(fIRCPriorityMask[edge]>bestIRCHitType){
      bestIRCHitType = fIRCPriorityMask[edge]; //removed conditions on 10 ns, not needed and dangerous
    };
  };
  if(bestIRCHitType>7 || bestIRCHitType==4){
    Int_t nirc = 0;
    Double_t eToT = 0;
    Double_t eToT2 = 0;
    Double_t maxToT = 0;
    Double_t mintime = 999999.;
    Double_t mintime2 = 999999.;
    Double_t mintime3 = 999999.;
    for(int i=0; i<IRCEvent->GetNHits(); i++){
      if(verb) cout<<"hit "<<i<<endl;
      TRecoIRCHit *IRCHit = static_cast<TRecoIRCHit*>(IRCEvent->GetHit(i));
      Int_t chid = IRCHit->GetChannelID();
      Int_t edge = IRCHit->GetEdgeMask();
      if(fIRCPriorityMask[edge]<7 && fIRCPriorityMask[edge]!=4) continue;
      double ToT = (IRCHit->GetTrailingEdgeLow() && IRCHit->GetLeadingEdgeLow()) ? IRCHit->GetTrailingEdgeLow()-IRCHit->GetLeadingEdgeLow() : 0.;
      Double_t totTH_LH = (IRCHit->GetTrailingEdgeHigh() && IRCHit->GetLeadingEdgeHigh()) ? IRCHit->GetTrailingEdgeHigh()-IRCHit->GetLeadingEdgeLow() : 0.;
      Double_t totTL_TH = (IRCHit->GetTrailingEdgeHigh() && IRCHit->GetTrailingEdgeLow()) ? IRCHit->GetTrailingEdgeLow()-IRCHit->GetTrailingEdgeHigh() : 0.;
      if((IRCHit->GetLeadingEdgeLow() - trackTime) < 0. && (IRCHit->GetTrailingEdgeLow() - trackTime) > 0.){
	if(totTH_LH > 30. || totTL_TH > 30.){
	  photonInIRC = true;
	  break;
	}; //flag against double pulses in the IRC
      };
      if(verb) cout<<"TOT "<<ToT<<endl;
      FillHisto("hIRCToT", ToT);
      //slewing corrections
      double deltaT = 0.;
      if(ToT>2. && ToT<40.) deltaT = 6.38 - 0.303*ToT + 0.003578*ToT*ToT;
      if(ToT>=40. && ToT<60.) deltaT = 6.38 - 0.303*40. + 0.003578*40.*40.;
      if(ToT>2. && ToT<15.) deltaT += 1.2;
      double Tirc = IRCHit->GetTime()-deltaT;
      if(verb) cout<<"time IRC = "<<Tirc<<endl;

      double dT = Tirc - trackTime + 0.52 - totest[chid];
      FillHisto("hToTvsTimeDiffIRC", dT, ToT);
      if(ToT>=2. && fabs(dT)<fabs(mintime)){
	mintime = dT;
	eToT = ToT; // save tot of this hit
      };
      if(ToT<2. && fabs(dT)<fabs(mintime2)){
	mintime2 = dT;
	eToT2 = ToT; // save tot of this hit
      };
      if(ToT<2. && fabs(dT-7.)<fabs(mintime3-7.)){ // double peak at tot = 0
	mintime3 = dT;
	eToT2 = ToT; // save tot of this hit
      };             // hit with max tot
      if(ToT>=maxToT){
	maxToT = ToT;
      };
      nirc++;
    };
    if(nirc){ // Improved treatment of hits with missing slewing
      if(verb) cout<<"condition 1: "<<fIRCMinToT<<" < "<<eToT<<" < 999999."<<" &&  "<<" -7. < "<<mintime<<" < 4."<<endl;
      if(verb) cout<<"condition 2: "<<eToT2<<" < "<<fIRCMinToT<<" && "<<fabs(mintime2)<<" < "<<fCutTimeDiffIRCTrack<<endl;
      if(verb) cout<<"condition 3: "<<eToT2<<" < "<<fIRCMinToT<<" && "<<fabs(mintime3 - 7.)<<" < "<<fCutTimeDiffIRCTrack<<endl;
      if(eToT>=fIRCMinToT && eToT<999999. && (mintime<4.) && (mintime>-7.)) photonInIRC = true;
      if(eToT2<fIRCMinToT && fabs(mintime2)<fCutTimeDiffIRCTrack) photonInIRC = true;
      if(eToT2<fIRCMinToT && fabs(mintime3-7.)<fCutTimeDiffIRCTrack) photonInIRC = true; // double peak check at tot = 0
    };
  };
  FillHisto("hPhotonInIRC", (int)photonInIRC);
  if(verb) cout<<"Found photon in IRC? "<<photonInIRC<<endl;
  if(photonInIRC) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  bool photonInSAC = false;
  if(verb) cout<<"Photons in SAC? "<<endl;
  // // SAC priority definition
  // double fSACPriorityMask[16];
  // fSACPriorityMask[0] = 0; // --SNH--
  // fSACPriorityMask[1] = 4; // LL __ __ __
  // fSACPriorityMask[2] = 5; // __ LH __ __
  // fSACPriorityMask[3] =10; // LL LH __ __
  // fSACPriorityMask[4] = 2; // __ __ TH __
  // fSACPriorityMask[5] = 7; // LL __ TH __
  // fSACPriorityMask[6] =11; // __ LH TH __
  // fSACPriorityMask[7] =13; // LL LH TH __
  // fSACPriorityMask[8] = 1; // __ __ __ TL
  // fSACPriorityMask[9] =12; // LL __ __ TL
  // fSACPriorityMask[10]= 6; // __ LH __ TL
  // fSACPriorityMask[11]=14; // LL LH __ TL
  // fSACPriorityMask[12]= 3; // __ __ TH TL
  // fSACPriorityMask[13]= 8; // LL __ TH TL
  // fSACPriorityMask[14]= 9; // __ LH TH TL
  // fSACPriorityMask[15]=15; // LL LH TH TL

  double minDT = 999999.;
  double eToT = 0.;
  double maxtot = 0.;
  int nsac = 0;
  for(int i=0; i<SACEvent->GetNHits(); i++){
    TRecoSACHit *SACHit = static_cast<TRecoSACHit*>(SACEvent->GetHit(i));
    if(verb) cout<<"hit "<<i<<endl;
    double ToT = (SACHit->GetTrailingEdgeLow() && SACHit->GetLeadingEdgeLow()) ? (SACHit->GetTrailingEdgeLow()-SACHit->GetLeadingEdgeLow()) : 0.;
    if(verb) cout<<"SAC TOT = "<<ToT<<endl;
    Double_t totTH_LH = (SACHit->GetTrailingEdgeHigh() && SACHit->GetLeadingEdgeHigh()) ? SACHit->GetTrailingEdgeHigh()-SACHit->GetLeadingEdgeLow() : 0.;
    Double_t totTL_TH = (SACHit->GetTrailingEdgeHigh() && SACHit->GetTrailingEdgeLow()) ? SACHit->GetTrailingEdgeLow()-SACHit->GetTrailingEdgeHigh() : 0.;
    //slewing corrections
    double deltaT = 0.;
    double Tsac = SACHit->GetTime()-deltaT;
    if(verb) cout<<"time SAC = "<<Tsac<<endl;
    if((SACHit->GetLeadingEdgeLow()-trackTime)<0. && (SACHit->GetTrailingEdgeLow()-trackTime)>0.){
      if(totTH_LH>45. || totTL_TH>45.){ //flag against double pulses in the SAC
	photonInSAC = true;
	break;
      };
    };
    if(ToT>=2.){
      if(fabs(Tsac-trackTime)<fabs(minDT)){ // hit closest in time to ref
	minDT = Tsac-trackTime;
      };
    }else{
      if(fabs(Tsac-trackTime+3.)<fabs(minDT+3.)){ // hit closest in time to ref
	minDT = Tsac-trackTime;
      };
    };
    if(ToT>maxtot){
      maxtot = ToT;
    };
    eToT += ToT;
    nsac++;
  };
  if(nsac>0){
    FillHisto("hToTvsTimeDiffSAC", minDT, eToT);
    if(verb) cout<<"condition 1: "<<eToT<<" < "<<fSACMinToT<<" && "<<fCutTimeDiffSACTrackMin<<" < "<<minDT<<" < "<<fCutTimeDiffSACTrackMax<<endl;
    if(verb) cout<<"condition 2: "<<fSACMinToT<<" < "<<eToT<<" < "<<fSACMaxToT<<" && ( "<<fabs(minDT)<<" < "<<fCutTimeDiffSACTrack2<<" || "<<fabs(minDT-(-11.3524+0.2105*eToT))<<" < 3. )"<<endl;
    if(verb) cout<<"condition 3: "<<fSACMaxToT<<" < "<<eToT<<" && "<<fCutTimeDiffSACTrack3<<" < "<<minDT<<" < "<<fCutTimeDiffSACTrack1<<endl;
    if(eToT<fSACMinToT && minDT>fCutTimeDiffSACTrackMin && minDT<fCutTimeDiffSACTrackMax) photonInSAC = true;
    if(eToT>=fSACMinToT && eToT<fSACMaxToT && (fabs(minDT)<fCutTimeDiffSACTrack2 || fabs(minDT-(-11.3524+0.2105*eToT))<3.)) photonInSAC = true;
    if(eToT>fSACMaxToT && minDT<fCutTimeDiffSACTrack1 && minDT>fCutTimeDiffSACTrack3) photonInSAC = true;
  };
  FillHisto("hPhotonInSAC", (int)photonInSAC);
  if(verb) cout<<"Found photon in SAC? "<<photonInSAC<<endl;
  if(photonInSAC) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  double minT0 = 999999.;
  double minT1 = 999999.;
  double minE0 = 0.;
  double minE1 = 0.;
  bool hasMin0 = false;
  bool hasMin1 = false;
  TRecoSAVEvent *SAVEvent = GetEvent<TRecoSAVEvent>();
  if(verb) cout<<"SAV event hits"<<endl;
  for(int i=0; i<SAVEvent->GetNHits(); i++){
    if(verb) cout<<"hit "<<i<<endl;
    TRecoSAVHit *SAVHit = static_cast<TRecoSAVHit*>(SAVEvent->GetHit(i));
    double t = SAVHit->GetTime();
    if(verb) cout<<"hit time = "<<t<<endl;
    if(verb) cout<<"track time = "<<trackTime<<endl;
    FillHisto("hTimeSAVHit", t);
    FillHisto("hWhichSAVHit", SAVHit->GetDetector());
    FillHisto("hWhichSAVHitVsTimeSAVHit", t, SAVHit->GetDetector());
    if(SAVHit->GetDetector()==0){
      if(verb) cout<<"SAV detector 0"<<endl;
      if(verb) cout<<"hit time - track time = "<<fabs(t-trackTime)<<" < "<<fabs(minT0)<<endl;
      if(fabs(t-trackTime)<fabs(minT0) && SAVHit->GetEnergy()>1000.){
	if(verb) cout<<"is min0 hit"<<endl;
	minT0 = t-trackTime;
	minE0 = SAVHit->GetEnergy();
	hasMin0 = true;
      };
    };
    if(SAVHit->GetDetector()==1){
      if(verb) cout<<"SAV detector 1"<<endl;
      if(verb) cout<<"hit time - track time = "<<fabs(t-trackTime)<<" < "<<fabs(minT1)<<endl;
      if(fabs(t-trackTime)<fabs(minT1) && SAVHit->GetEnergy()>1000.){
	if(verb) cout<<"is min1 hit"<<endl;
	minT1 = t-trackTime;
	minE1 = SAVHit->GetEnergy();
	hasMin1 = true;
      };
    };
  };
  if(hasMin0){
    if(verb) cout<<"has Min0: "<<fCutMinEnergySAVFADCHit<<" < "<<minE0<<" && "<<fabs(minT0)<<" < "<<fCutTimeDiffSAVFADCHit<<endl;
    FillHisto("hSACFADCEnergyVsTimeDiff", minT0, minE0);
    if(minE0>=fCutMinEnergySAVFADCHit && fabs(minT0)<fCutTimeDiffSAVFADCHit) photonInSAC = true;
  };
  if(hasMin1){
    if(verb) cout<<"has Min1: "<<fCutMinEnergySAVFADCHit<<" < "<<minE1<<" && "<<fabs(minT1)<<" < "<<fCutTimeDiffSAVFADCHit<<endl;
    FillHisto("hIRCFADCEnergyVsTimeDiff", minT1, minE1);
    if(minE1>=fCutMinEnergySAVFADCHit && fabs(minT1)<fCutTimeDiffSAVFADCHit) photonInIRC = true;
  };
  if(photonInSAC){
    FillHisto("hPhotonInSAVFADC", 0);
  }else if(photonInIRC){
    FillHisto("hPhotonInSAVFADC", 1);
  }else{
    FillHisto("hPhotonInSAVFADC", -1);
  };
  if(verb) cout<<"Checking IRC and SAC hits"<<endl;
  if(verb) cout<<"Found photon in IRC? "<<photonInIRC<<endl;
  if(verb) cout<<"Found photon in SAC? "<<photonInSAC<<endl;
  if(photonInIRC || photonInSAC) return;
  FillHisto("hTrackMomentumVsCut", cutID, STRAWCand->GetMomentum());
  FillHisto("hCut", cutID);
  cutID++;

  fPhotons = false;
}

void PhotonRejection::PostProcess(){
}

void PhotonRejection::EndOfBurstUser(){
}

void PhotonRejection::EndOfRunUser(){
}

void PhotonRejection::EndOfJobUser(){
  if(fReadingData){
    SaveAllPlots();
  };
}

void PhotonRejection::DrawPlot(){
}

PhotonRejection::~PhotonRejection(){
}

void PhotonRejection::PrepareOutputs(){
  fPhotons = true;
  SetOutputState("Photons", kOInvalid);
}

void PhotonRejection::ValidateOutputs(){
  SetOutputState("Photons", kOValid);
}
