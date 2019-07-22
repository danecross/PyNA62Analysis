#include "FilterPnn.hh"
#include <stdlib.h>
#include <iostream>
#include <numeric>
#include <TChain.h>
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TMath.h"
#include "TVector2.h"
#include "NA62Exceptions.hh"
#include "TSystem.h"
#include "NA62ConditionsService.hh"

#include "GeometricAcceptance.hh"
#include "SpectrometerMUV3AssociationOutput.hh"
#include "LAVMatching.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class FilterPnn 
/// \Brief
/// Filter events for pinunu or control trigger
/// \EndBrief
/// \Detailed
/// Filtering condition controlled by the parameter FilterControl.<br>
/// 0: no trigger condition applied for filtering;<br>
/// 1: control trigger events only;<br>
/// 2: pnn mask trigger only;<br>
/// 3: control trigger and pnn mask.<br>
/// The filtering uses GigaTrackerEvtReco as preanalyzer, storing the
/// fully corrected hit times and well reconstructed GTK candidates
/// in the GigaTracker tree.
/// \author Giuseppe Ruggiero (giuseppe.ruggiero@cern.ch) 
/// \EndDetailed

FilterPnn::FilterPnn(Core::BaseAnalysis *ba) : Analyzer(ba, "FilterPnn")
{
  RequestAllMCTrees();
  RequestAllRecoTrees();
  RequestL0Data();

  // Filtering Parameters
  AddParam("FilterControl",&fFilterControl,3); // 0 pnn filter without trigger selection, 1 filter control, 2 filter pnn, 3 filter control and pnn 
  AddParam("DWControl0TRK",&fCDW,10); // Downscaling for control trigger events without tracks reconstructed 

  // Geometrical instance
  fGeo = GeometricAcceptance::GetInstance();

  for (Int_t i=0; i<128; i++) {
    if (i<16) fSlabCenter[i] = (fSlabLimitsX[16-i-1] + fSlabLimitsX[16-i])/2.;
    else if (i<48) fSlabCenter[i] = (fSlabLimitsX[i-16] + fSlabLimitsX[i-16+1])/2.;
    else if (i<64) fSlabCenter[i] = (fSlabLimitsX[80-i] + fSlabLimitsX[80-i-1])/2.;
    else if (i<96) fSlabCenter[i] = (fSlabLimitsY[96-i] + fSlabLimitsY[96-i-1])/2.;
    else fSlabCenter[i] = (fSlabLimitsY[i-96] + fSlabLimitsY[i-96+1])/2.;
  }
}

void FilterPnn::InitOutput(){
}

void FilterPnn::InitHist(){

  // Histograms
  BookHisto(new TH2F("StrawAnalysis_CDAZVertex","",250,50000,300000,200,0,100));
  BookHisto(new TH1F("Track_chodminchi2","",200,0,50));
  BookHisto(new TH2F("Track_chodtrackmatch","",200,-100,100,400,0,2000));
  BookHisto(new TH2F("CHODAnalysis_dtime","",2048,0,2048,200,-10,10));
  BookHisto(new TH1F("KTAGAnalysis_alltime","",1000,-50,50));
  BookHisto(new TH1F("KTAGAnalysis_dtime","",1000,-50,50));
  BookHisto(new TH2F("MUV3Analysis_timedist","",400,-20,20,150,0,600));
  BookHisto(new TH2F("LKrAnalysis_xy","",128,-126.35,126.35,128,-126.35,126.35));
  BookHisto(new TH2F("LKrAnalysis_photon","",200,0,2000,600,-150,150));
  BookHisto(new TH1F("GTKAnalysis_dtime","",500,-100,100));
  BookHisto(new TH1F("LAVAnalysis_time_broad","",300,-15,15));
  BookHisto(new TH1F("LAVAnalysis_time_tight","",300,-15,15));
  BookHisto(new TH1F("Track_eovp","",220,0,1.1));

  // Counters
  BookCounter("NTotal");
  BookCounter("NPnn");
  BookCounter("NControl_Filter");
  BookCounter("NControl_0trk");
  BookCounter("NControl_NoSel");
  NewEventFraction("PnnSelected");
  AddCounterToEventFraction("PnnSelected","NTotal");
  AddCounterToEventFraction("PnnSelected","NPnn");
  AddCounterToEventFraction("PnnSelected","NControl_Filter");
  AddCounterToEventFraction("PnnSelected","NControl_0trk");
  AddCounterToEventFraction("PnnSelected","NControl_NoSel");
  DefineSampleSizeCounter("PnnSelected", "NTotal");
}

void FilterPnn::StartOfRunUser(){
}

void FilterPnn::StartOfBurstUser(){
  
  // Read CHOD parameters 
  //Int_t runID = GetEventHeader()->GetRunID();
  TString CHODConfigFile = Form("%s/config/CHOD.conf",getenv("NA62RECOSOURCE"));
  TString CHODLightVelocitiesFile = "CHOD-LightVelocities.dat";
  TString CHODSlewingFile = "CHOD-SlewCorr.dat";

  // Read Geometry
  TString line;
  ifstream CHODConfig(CHODConfigFile.Data());
  if(CHODConfig.is_open()){
    cout << user_normal() << " --->  Reading " << CHODConfigFile.Data() << endl;
    while (line.ReadLine(CHODConfig)) {
      if (line.BeginsWith("#")) continue;
      TObjArray * l = line.Tokenize(" ");
      if (line.BeginsWith("SC_PositionV")) {
        Double_t pos[16];
        for (Int_t jl(0); jl<16; jl++) pos[jl] = ((TObjString*)(l->At(jl+1)))->GetString().Atof();
        for (Int_t j=0; j<16; j++)  fCHODPosV[j] = -pos[j];
        for (Int_t j=16; j<32; j++) fCHODPosV[j] = -pos[31-j];
        for (Int_t j=32; j<48; j++) fCHODPosV[j] = pos[j-32];
        for (Int_t j=48; j<64; j++) fCHODPosV[j] = pos[63-j];
        for (Int_t j=0; j<16; j++)  fCHODPosH[j] = pos[15-j];
        for (Int_t j=16; j<32; j++) fCHODPosH[j] = -pos[j-16];
        for (Int_t j=32; j<48; j++) fCHODPosH[j] = -pos[47-j];
        for (Int_t j=48; j<64; j++) fCHODPosH[j] = pos[j-48];
      }  
      delete l;
    } 
    CHODConfig.close();
  }
  cout << user_normal() << "---> File " << CHODConfigFile.Data() << " read " << endl;

  // Light Velocities
  if(NA62ConditionsService::GetInstance()->Open(CHODLightVelocitiesFile)==kSuccess){
    Int_t iSlab = 0;
    while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(CHODLightVelocitiesFile))) {
      if (line.BeginsWith("#")) continue;
      fCHODLightVelocities[iSlab] = line.Atof();
      //Int_t IP=0;
      if (iSlab >= 0 && iSlab < 16) {
	for (Int_t iIntersectingSlab=64; iIntersectingSlab<80; iIntersectingSlab++) {
	  Double_t lightvelCorr = - fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10.*(121.-fPMCoordinate[iSlab]));
	  //IP = iSlab*16 + (iIntersectingSlab-64);
	  fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-64] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 16 && iSlab < 32) {
	for (Int_t iIntersectingSlab=80; iIntersectingSlab<96; iIntersectingSlab++) {
	  Double_t lightvelCorr = + fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10.*(-fPMCoordinate[iSlab]));
	  //IP = iSlab*16 + (iIntersectingSlab-80);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-80] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 32 && iSlab < 48) {
	for (Int_t iIntersectingSlab=96; iIntersectingSlab<112; iIntersectingSlab++) {
	  Double_t lightvelCorr = + fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10.*(-121.+fPMCoordinate[iSlab]));
	  //IP = iSlab*16 + (iIntersectingSlab-96);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-96] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 48 && iSlab < 64) {
	for (Int_t iIntersectingSlab=112; iIntersectingSlab<128; iIntersectingSlab++) {
	  Double_t lightvelCorr = - fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10.*fPMCoordinate[iSlab]);
	  //IP = iSlab*16 + (iIntersectingSlab-112);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-112] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 64 && iSlab < 80) {
	for (Int_t iIntersectingSlab=0; iIntersectingSlab<16; iIntersectingSlab++) {
	  Double_t lightvelCorr = + fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10.*(-fPMCoordinate[iSlab]));
	  //IP = iSlab*16 + (iIntersectingSlab);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 80 && iSlab < 96) {
	for (Int_t iIntersectingSlab=16; iIntersectingSlab<32; iIntersectingSlab++) {
	  Double_t lightvelCorr = + fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10*(-121.+fPMCoordinate[iSlab]));
	  //IP = iSlab*16 + (iIntersectingSlab-16);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-16] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 96 && iSlab < 112) {
	for (Int_t iIntersectingSlab=32; iIntersectingSlab<48; iIntersectingSlab++) {
	  Double_t lightvelCorr = - fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10*fPMCoordinate[iSlab]);
	  //IP = iSlab*16 + (iIntersectingSlab-32);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-32] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      else if (iSlab >= 112 && iSlab < 128) {
	for (Int_t iIntersectingSlab=48; iIntersectingSlab<64; iIntersectingSlab++) {
	  Double_t lightvelCorr = - fCHODLightVelocities[iSlab]*(fSlabCenter[iIntersectingSlab]-10*(121.-fPMCoordinate[iSlab]));
	  //IP = iSlab*16 + (iIntersectingSlab-48);
          fCHODLightVelocitiesCorr[iSlab][iIntersectingSlab-48] = fabs(lightvelCorr)>=99 ? 0.0 : lightvelCorr;
	}
      }
      iSlab++;
    }
    NA62ConditionsService::GetInstance()->Close(CHODLightVelocitiesFile);
  }

  // Slewings
  Int_t nRL = 0;
  if(NA62ConditionsService::GetInstance()->Open(CHODSlewingFile)==kSuccess){
    while (line.ReadLine(NA62ConditionsService::GetInstance()->Get(CHODSlewingFile))) {
      if (line.BeginsWith("#")) continue;
      TObjArray * l = line.Tokenize(" ");
      fCHODAllSlewSlope[(Int_t)(nRL/16)][(Int_t)(nRL%16)] = ((TObjString*)(l->At(0)))->GetString().Atof();
      fCHODAllSlewConst[(Int_t)(nRL/16)][(Int_t)(nRL%16)] = ((TObjString*)(l->At(1)))->GetString().Atof();
      delete l;
      nRL++;
    } 
    NA62ConditionsService::GetInstance()->Close(CHODSlewingFile);
  }
  cout << user_normal() << "---> File " << CHODSlewingFile.Data() << " read " << nRL << " lines" << endl;

  // Fine T0 
  for (Int_t jP(0); jP<2048; jP++) {
    fCHODAllFineT0[(Int_t)(jP/16)][(Int_t)(jP%16)] = 0.;
  }

  //// LAV Noisy channels
  //vector<Int_t> vNoisyCh;
  //if (runID==6291)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 113002 << 123083 << 123121;
  //if (runID==6342)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 113002 << 123083;
  //if (runID==6420)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 112042 << 113002 << 123083;
  //if (runID==6483)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 113002 << 123083 << 1123083;
  //if (runID==6610)  vNoisyCh = make_vector<Int_t>() << 44013 << 63081 << 90083 << 103040 << 113002 << 123083;
  //if (runID==6614)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 113002 << 123083;
  //if (runID==6625)  vNoisyCh = make_vector<Int_t>() << 34070 << 63081 << 90083 << 103040 << 113002 << 121081 << 123083 << 123121;
  //if (runID==6632)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 113002 << 123083;
  //if (runID==6670)  vNoisyCh = make_vector<Int_t>() << 40070 << 63081 << 90083 << 103040 << 113002 << 121081 << 123083;
  //if (runID==6683)  vNoisyCh = make_vector<Int_t>() << 63081 << 90083 << 103040 << 113002 << 121081 << 123083;
  //fLAVMatching->ClearNoisyChannels();
  //for (auto &j : vNoisyCh) fLAVMatching->SetNoisyChannel(j); 
  //vNoisyCh.clear();
}

void FilterPnn::ProcessSpecialTriggerUser(int, unsigned int){
}

void FilterPnn::Process(int){
  fCedarEvent = GetEvent<TRecoCedarEvent>();
  fGigaTrackerEvent = GetEvent<TRecoGigaTrackerEvent>();
  fSpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  fCHODEvent = GetEvent<TRecoCHODEvent>();
  fLKrEvent = GetEvent<TRecoLKrEvent>();
  fLAVEvent = GetEvent<TRecoLAVEvent>();
  fMUV3Event = GetEvent<TRecoMUV3Event>();
  fL0Data = GetL0Data();
  fRawHeader = GetEventHeader();

  // Clear
  vector<Int_t> fvCHODQ[8];
  fMUV3Candidate.clear();
////  fLAVMatching->Clear();

  // Load analyzers
  fMUV3Candidate = *GetOutput<vector<SpectrometerMUV3AssociationOutput>>("SpectrometerMUV3Association.Output");

  // Count events
  IncrementCounter("NTotal");

  // General quality  
  if (fRawHeader->GetEventQualityMask()>0)  return;

  // Skip periodics 
  if (fL0Data->GetDataType()&0x2) return;

  // Filtering conditions
  if (!FilteringCondition()) return;

  // Preliminary conditions for pinunu 
  if (fGigaTrackerEvent->GetErrorMask()) return;
  if (!fSpectrometerEvent->GetNCandidates()) return;
  if (fSpectrometerEvent->GetNCandidates()>10) return;
  if (fCHODEvent->GetNHits()<2) return;
  if (!CHODQ1(fvCHODQ)) return;
      
  // Loop on tracks  
  Bool_t isToFilter = false;
  vector<Double_t> chodPar;
  Double_t ttimeoff = -2.1;
  for (Int_t jT(0); jT<fSpectrometerEvent->GetNCandidates(); jT++) {
    chodPar.clear();
    TRecoSpectrometerCandidate *track = static_cast<TRecoSpectrometerCandidate *>(fSpectrometerEvent->GetCandidate(jT));

    // Track selection
    if (track->GetChi2()>30) continue;
    //Double_t trackP = 0.9987*(1-track->GetCharge()*0.3e-05*track->GetMomentum()*0.001)*track->GetMomentum();
    Double_t trackP = track->GetMomentum();
    if (fabs(fabs(track->GetMomentumBeforeFit())-trackP)>25000.) continue;
    if (!fGeo->InAcceptance(track,kNewCHOD,0,-1,-1)) continue;
    if (!fGeo->InAcceptance(track,kCHOD,0,125.,-1)) continue;
    if (!fGeo->InAcceptance(track,kLKr,0,145.,-1)) continue;
    if (!fGeo->InAcceptance(track,kMUV3,0,-1,-1)) continue;
    if (MultiTrack(jT,trackP)) continue;
    if (trackP>50000.) continue;
    if (trackP<8000.) continue;

    // CHOD matching
    Double_t ttime = track->GetTime()+ttimeoff;
    chodPar = CHODTrackMatching(track->xAtAfterMagnet(fGeo->GetZCHODVPlane()),track->yAtAfterMagnet(fGeo->GetZCHODHPlane()),ttime,fvCHODQ);
    FillHisto("Track_chodminchi2",chodPar[0]);
    if (chodPar[0]>20) continue; // discriminant
    FillHisto("Track_chodtrackmatch",chodPar[2],sqrt(chodPar[1]));
    if (fabs(chodPar[2])>30) continue; // CHOD time - Track time
    Double_t chodTime = chodPar[2]+ttime;

    // KTAG matching
    Double_t ktagTime = 999999.;
    if (!KTAGTrackMatching(chodTime,&ktagTime)) continue; 
    FillHisto("CHODAnalysis_dtime",16*chodPar[3]+((Int_t)(chodPar[4]-64)%16),ktagTime-chodPar[5]);  
    FillHisto("CHODAnalysis_dtime",16*chodPar[4]+((Int_t)chodPar[3]%16),ktagTime-chodPar[6]);  
    Double_t dtime = ktagTime-chodTime;
    FillHisto("KTAGAnalysis_dtime",dtime);
    if (fabs(dtime)>5) continue;

    // GTK condition
    Int_t nhits[3] = {0,0,0};
    for (Int_t jHit(0);jHit<fGigaTrackerEvent->GetNHits();jHit++) {
      TRecoGigaTrackerHit *hit = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(jHit));
      FillHisto("GTKAnalysis_dtime",hit->GetTime()-ktagTime);
      if (fabs(hit->GetTime()-ktagTime)<2.) nhits[hit->GetStationNo()]++; 
    }
    if (!nhits[0]||!nhits[1]||!nhits[2]) continue;

    // Muon rejection
    if (MUV3TrackMatching(jT,track->xAtAfterMagnet(fGeo->GetZMUV3()),track->yAtAfterMagnet(fGeo->GetZMUV3()),chodTime)) continue;  

    // Photon rejection 
    Double_t trackELKr = -1.;
    if (LKrPhotons(track->xAtAfterMagnet(fGeo->GetZLKr()),track->yAtAfterMagnet(fGeo->GetZLKr()),chodTime,&trackELKr)) continue;
    if (LAVPhotons(chodTime)) continue;

    // Reject positrons 
    Double_t eovp = trackELKr>=0 ? trackELKr/trackP : -1.; 
    FillHisto("Track_eovp",eovp);

    // an event is filtered if at least a track exists, matching CHOD and KTAG and not matching MUV3, LKr and LAV signals
    isToFilter = true; 
  }

  // Filter pnn
  if (isToFilter) {
    IncrementCounter("NPnn");
    FilterAccept();
  }
}

void FilterPnn::PostProcess(){
}

void FilterPnn::EndOfBurstUser(){
}

void FilterPnn::EndOfRunUser(){
}

void FilterPnn::EndOfJobUser(){
//  SaveAllPlots();
  fHisto.GetTH1("Track_chodminchi2")->Write();
  fHisto.GetTH2("Track_chodtrackmatch")->Write();
  fHisto.GetTH2("CHODAnalysis_dtime")->Write();
  fHisto.GetTH1("KTAGAnalysis_dtime")->Write();
  fHisto.GetTH1("GTKAnalysis_dtime")->Write();
  fHisto.GetTH2("MUV3Analysis_timedist")->Write();
  fHisto.GetTH2("LKrAnalysis_photon")->Write();
}

void FilterPnn::DrawPlot(){
}

FilterPnn::~FilterPnn(){
}

//##################################
//#######                   ########
//####### Analysis Routines ########
//#######                   ########
//##################################

Bool_t FilterPnn::MultiTrack(Int_t iT, Double_t pTrack) {
  Bool_t isMultiTrack = false;
  if (fSpectrometerEvent->GetNCandidates()<2) return isMultiTrack;

  // Select good candidates to pair with
  vector<Int_t> idTrack(fSpectrometerEvent->GetNCandidates());
  iota(begin(idTrack),end(idTrack),0);
  TrackQualityCondition tq(fSpectrometerEvent,iT); 
  Int_t nT = distance(idTrack.begin(),partition(idTrack.begin(),idTrack.end(),tq));
  if (!nT) return isMultiTrack;
  vector<Int_t> vTrack;
  vTrack.assign(idTrack.begin(),idTrack.begin()+nT); 

  // Find the multi vertex
  TLorentzVector ptracks[2];
  TVector3 postracks[2];
  TRecoSpectrometerCandidate *piT = static_cast<TRecoSpectrometerCandidate *>(fSpectrometerEvent->GetCandidate(iT));
  ptracks[0] = Get4Momentum(pTrack,piT->GetSlopeXBeforeMagnet(),piT->GetSlopeYBeforeMagnet(),MPI);
  postracks[0] = piT->GetPositionBeforeMagnet();
  for (auto &jT : vTrack) {
    TRecoSpectrometerCandidate *pjT = static_cast<TRecoSpectrometerCandidate *>(fSpectrometerEvent->GetCandidate(jT));
    //Double_t trackP = 0.9987*(1-pjT->GetCharge()*0.3e-05*pjT->GetMomentum()*0.001)*pjT->GetMomentum();
    Double_t trackP = pjT->GetMomentum();
    ptracks[1] = Get4Momentum(trackP,pjT->GetSlopeXBeforeMagnet(),pjT->GetSlopeYBeforeMagnet(),MPI);
    postracks[1] = pjT->GetPositionBeforeMagnet();
    Double_t cda = 9999.;
    TVector3 vertex = MultiTrackVertex(2,ptracks,postracks,&cda);
    if (iT>jT) FillHisto("StrawAnalysis_CDAZVertex",vertex.Z(),cda);
    Double_t dtime = piT->GetTime()-pjT->GetTime();
    if (cda<10 && vertex.Z()>80000. && vertex.Z()<180000. && fabs(dtime)<30.) isMultiTrack = true;
  }

  return isMultiTrack;
}

TLorentzVector FilterPnn::Get4Momentum(Double_t pmag, Double_t thx, Double_t thy, Double_t mass) {
  TLorentzVector pmom;
  Double_t pmomz = pmag/sqrt(1.+thx*thx+thy*thy);
  Double_t pmomx = pmomz*thx;
  Double_t pmomy = pmomz*thy;
  pmom.SetXYZM(pmomx,pmomy,pmomz,mass);
  return pmom;
}

TVector3 FilterPnn::MultiTrackVertex(Int_t nTracks, TLorentzVector *ptracks, TVector3 *postracks, Double_t *cda) {
  TVector3 avPosition(0,0,0);
  TVector3 avSlope(0,0,0);
  TVector3 avSlope2(0,0,0);
  TVector3 avMixed(0,0,0);

  // Compute Z as the position of minimum apporach between tracks
  Double_t z0 = 0;
  for (Int_t j=0; j<nTracks; j++) {
    TVector3 position = postracks[j];
    TLorentzVector momentum = ptracks[j]; 
    avPosition += position;
    TVector3 ddz = momentum.Vect()*(1./momentum.Vect().Z());
    avSlope += ddz;
    avSlope2 += TVector3(ddz.X()*ddz.X(),ddz.Y()*ddz.Y(),ddz.Z()*ddz.Z());
    avMixed += TVector3(position.X()*ddz.X(),position.Y()*ddz.Y(),position.Z()*ddz.Z());
    z0 = position.Z();
  }
  avPosition = (1./nTracks)*avPosition;
  avSlope = (1./nTracks)*avSlope;
  avSlope2 = (1./nTracks)*avSlope2;
  avMixed = (1./nTracks)*avMixed;
  Double_t num = nTracks*(avMixed.X()+avMixed.Y())-nTracks*(avPosition.X()*avSlope.X()+avPosition.Y()*avSlope.Y());
  Double_t den = nTracks*(avSlope2.X()+avSlope2.Y())-nTracks*(avSlope.X()*avSlope.X()+avSlope.Y()*avSlope.Y());
  Double_t zvertex = z0-num/den;

  // Compute the trasnverse position and the cda
  TVector3 avPosVtx(0,0,0);
  TVector3 avPosVtx2(0,0,0);
  for (Int_t j=0; j<nTracks; j++) {
    TVector3 position = postracks[j];
    TLorentzVector momentum = ptracks[j]; 
    TVector3 posvtx = position+momentum.Vect()*(1./momentum.Vect().Z())*(zvertex-position.Z());
    avPosVtx += posvtx;
    avPosVtx2 += TVector3(posvtx.X()*posvtx.X(),posvtx.Y()*posvtx.Y(),posvtx.Z()*posvtx.Z());
  }
  avPosVtx = (1./nTracks)*avPosVtx;
  avPosVtx2 = (1./nTracks)*avPosVtx2;
  *cda = sqrt(avPosVtx2.X()+avPosVtx2.Y()-avPosVtx.X()*avPosVtx.X()-avPosVtx.Y()*avPosVtx.Y());

  return TVector3(avPosVtx.X(),avPosVtx.Y(),zvertex);
}

Bool_t FilterPnn::CHODQ1(vector<Int_t> *fvCHODQ) {
  Bool_t isQ1 = false;

  // Sort hits per increasing channel number
  vector<Int_t> idHit(fCHODEvent->GetNHits());
  iota(begin(idHit),end(idHit),0);
  ChannelOrder co(fCHODEvent); 
  sort(idHit.begin(),idHit.end(),co);

  // Sort hits per plane 
  PlaneCondition pc(fCHODEvent);
  auto from = idHit.begin();
  Int_t nV = distance(from,partition(from,idHit.end(),pc));
  if (!nV) return isQ1;
  if (!(fCHODEvent->GetNHits()-nV)) return isQ1;
  vector<Int_t> vP[2];
  vP[0].assign(from,from+nV); 
  vP[1].assign(from+nV,idHit.end()); 

  // Sort hits per quadrant 
  vector<Int_t>::iterator fromP[2];
  fromP[0] = vP[0].begin();
  fromP[1] = vP[1].begin();
  Int_t jQ = 0;
  while ((fromP[0]!=vP[0].end() || fromP[1]!=vP[1].end()) && jQ<4) { 
    for (Int_t jP(0); jP<2; jP++) {
      QuadrantCondition qc(fCHODEvent,jQ,jP);
      Int_t nQ = distance(fromP[jP],partition(fromP[jP],vP[jP].end(),qc));
      if (!nQ) continue;
      fvCHODQ[jQ+4*jP].assign(fromP[jP],fromP[jP]+nQ); 
      fromP[jP] += nQ;
    }
    jQ++;
  }

  // Select events with hits in at least a corresponding H-V quadrant
  for (Int_t jq(0); jq<4; jq++) {
    if (fvCHODQ[jq].size()&&fvCHODQ[jq+4].size()) isQ1 = true;
  }

  return isQ1;
}

vector<Double_t> FilterPnn::CHODTrackMatching(Double_t xPos, Double_t yPos, Double_t ttime, vector<Int_t> *fvCHODQ) {
  vector<Double_t> par(7);

  // Loop over the quadrant pairs
  Double_t minchi2chod = 99999999.; 
  for (Int_t jQ(0); jQ<4; jQ++) {
    Double_t minpar[7];
    Double_t minchi2 = 99999999.;
    if (!fvCHODQ[jQ].size() || !fvCHODQ[jQ+4].size()) continue;
    for (auto &iV : fvCHODQ[jQ]) {
      TRecoCHODHit *hV = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(iV));
      Int_t cV = hV->GetChannelID();
      for (auto &iH : fvCHODQ[jQ+4]) {
        TRecoCHODHit *hH = static_cast<TRecoCHODHit*>(fCHODEvent->GetHit(iH));
        Int_t cH = hH->GetChannelID();
        Double_t tV = hV->GetTime()-CHODTimeCorrection(cV,(cH-64)%16,hV->GetTimeWidth(),true);
        Double_t tH = hH->GetTime()-CHODTimeCorrection(cH,cV%16,hH->GetTimeWidth(),true);
        Double_t dtime = 0.5*(tV+tH)-ttime; 
        Double_t dist2 = (fCHODPosV[cV]-xPos)*(fCHODPosV[cV]-xPos)+(fCHODPosH[cH-64]-yPos)*(fCHODPosH[cH-64]-yPos);
        Double_t chi2chod = (tV-tH)*(tV-tH)/(9*3*3)+dtime*dtime/(9*7*7)+dist2/(4*13*13);
        if (chi2chod<minchi2) {
          minchi2 = chi2chod;
          minpar[0] = minchi2;
          minpar[1] = dist2;
          minpar[2] = dtime;
          minpar[3] = cV;
          minpar[4] = cH;
          minpar[5] = tV;
          minpar[6] = tH;
        }
      }
    }
    if (minchi2<minchi2chod) {
      minchi2chod = minchi2;
      par.clear();
      for (Int_t k(0); k<7; k++) par.push_back(minpar[k]);
    } 
  } 

  return par;  
}

Double_t FilterPnn::CHODTimeCorrection(Int_t iS, Int_t iP, Double_t tot, Bool_t enslew) {
  Double_t t0 = fCHODLightVelocitiesCorr[iS][iP]<99. ? fCHODLightVelocitiesCorr[iS][iP]-fCHODAllFineT0[iS][iP] : 0.;
  Double_t efftot = tot<15. ? tot : 15.;
  Double_t sC = (enslew && tot>0.) ? fCHODAllSlewSlope[iS][iP]*efftot+fCHODAllSlewConst[iS][iP] : 0.;
  return t0+sC; 
}

Bool_t FilterPnn::KTAGTrackMatching(Double_t timeref, Double_t *ktagtime) {
  Double_t mintime = 9999999.;
  Bool_t isCand = false;
  for (Int_t jktag=0; jktag<fCedarEvent->GetNCandidates(); jktag++) {
    TRecoCedarCandidate *ktag = static_cast<TRecoCedarCandidate *>(fCedarEvent->GetCandidate(jktag));
    if (ktag->GetNSectors()<4) continue;
    Double_t dt = ktag->GetTime()-timeref;
    FillHisto("KTAGAnalysis_alltime",dt);
    if (fabs(dt)<fabs(mintime)) {
      mintime = dt;
      isCand = true;
    }
  }
  if (!isCand) return false;
  *ktagtime = mintime+timeref; 
  return true; 
}

Bool_t FilterPnn::MUV3TrackMatching(Int_t jTrack, Double_t xPos, Double_t yPos, Double_t timeref) {
  Bool_t isMUV3 = false;
  if (!fMUV3Candidate[jTrack].GetNAssociationRecords()) return isMUV3; 
  TVector2 pTrack(xPos,yPos);
  for (Int_t k(0); k<fMUV3Candidate[jTrack].GetNAssociationRecords(); k++) {
    Int_t iMUV3 = fMUV3Candidate[jTrack].GetAssociationRecord(k)->GetMuonID();
    TRecoMUV3Candidate *cMUV3 = static_cast<TRecoMUV3Candidate *>(fMUV3Event->GetCandidate(iMUV3));
    Double_t dtime = cMUV3->GetTime()-timeref;
    TVector2 pMUV3(cMUV3->GetX(),cMUV3->GetY());
    Double_t dist = (pMUV3-pTrack).Mod();
    FillHisto("MUV3Analysis_timedist",dtime,dist);
    if (fabs(dtime)<4.) isMUV3 = true;   
  }
  return isMUV3; 
}

Bool_t FilterPnn::LKrPhotons(Double_t xPos, Double_t yPos, Double_t timeref, Double_t *energy) {
  Bool_t isPhoton = false; 
  Double_t mindist = 9999999.;
  Double_t minenergy = -1.;
  for (Int_t jC(0); jC<fLKrEvent->GetNCandidates(); jC++) {
    TRecoLKrCandidate *pC = static_cast<TRecoLKrCandidate *>(fLKrEvent->GetCandidate(jC));
    Double_t dist = sqrt(((pC->GetClusterX()+1.3)-xPos)*((pC->GetClusterX()+1.3)-xPos)+((pC->GetClusterY()+0.8)-yPos)*((pC->GetClusterY()+0.8)-yPos));
    Double_t dtime = pC->GetTime()-timeref;
    if (dtime>50) FillHisto("LKrAnalysis_xy",pC->GetClusterX()/10.,pC->GetClusterY()/10.);
    FillHisto("LKrAnalysis_photon",dist,dtime);
    if (dist<150. && fabs(dtime)<10.) { // track associated if less than 15 cm and +-10 ns in time with reference time
      if (dist<mindist) {
        mindist = dist;
        minenergy = LKrCorrectedEnergy(pC);  
      }  
    }
    if (dist<=180.) continue; // do not look for clusters around 180 mm from the track impact point
    if (fabs(dtime)<5.) isPhoton = true;
  } 
  *energy = minenergy; 
  return isPhoton;
}

Double_t FilterPnn::LKrCorrectedEnergy(TRecoLKrCandidate *pC) {
  Double_t ue = 0.001*pC->GetClusterEnergy();
  Double_t ce = ue; 
  if (pC->GetNCells()>5) {
    if (ue<22) ce = ue/(0.7666+0.0573489*log(ue));
    if (ue>=22 && ue<65) ce = ue/(0.828962+0.0369797*log(ue));
    if (ue>=65) ce = ue/(0.828962+0.0369797*log(65));
    ce = 0.99*ce/(0.94+0.0037*ce-9.4e-05*ce*ce+8.9e-07*ce*ce*ce);
  }
  ce *= 1.03;
  Double_t radius = 0.1*sqrt(pC->GetClusterX()*pC->GetClusterX()+pC->GetClusterY()*pC->GetClusterY());
  if (radius>=14 && radius<=18.5) ce = ce/(0.97249+0.0014692*radius)*0.9999;
  Double_t rcorr2 = 0;
  if (radius>=14 && radius<18) rcorr2 = 0.0042-3.7e-4*radius; 
  if (radius>=18 && radius<20) rcorr2 = -0.00211; 
  if (radius>=20 && radius<22) rcorr2 = -0.01694-7.769e-4*radius; 
  ce *= (1-rcorr2);
  if (ce<15) ce = (ce+0.015)/(15+0.015)*15*0.9999; 
  ce *= 1000.;
  return ce;
}

Bool_t FilterPnn::LAVPhotons(Double_t timeref) {

  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(timeref);
  pLAVMatching->SetTimeCuts(10,10);
  if (pLAVMatching->LAVHasTimeMatching(fLAVEvent)) {
    FillHisto("LAVAnalysis_time_broad",pLAVMatching->GetBestTimeOfMatchedBlocks());
    pLAVMatching->SetTimeCuts(2,2);
    if (pLAVMatching->LAVHasTimeMatching(fLAVEvent)) {
      FillHisto("LAVAnalysis_time_tight",pLAVMatching->GetBestTimeOfMatchedBlocks());
      return true;
    }
  }
  return false;
}

//////////////////////////////////////////////
// Filtering conditions for control trigger //
//////////////////////////////////////////////
Bool_t FilterPnn::FilteringCondition() {
  Bool_t isControl = (fL0Data->GetDataType()>>4)&1;  
  Bool_t isPhysics = (fL0Data->GetDataType()>>0)&1; // not exclusive with control
  Bool_t isPnnMask = isPhysics && (fL0Data->GetTriggerFlags()>>1)&1;

  switch (fFilterControl) {
    case 0: // always pass the pnn filter
    return true;
    break;

    case 1: // filter control only
    if (isControl) {
      FilterControl(); // pass through the control filter (also if control & physics)
      return false; // do not pass through the pnn filter
    } else return false; // do not pass neither through the control nor the pnn filter
    break;

    case 2: // filter pnn
    if (!isPnnMask) return false; // do not pass through the pnn filter 
    return true; // pass through the pnn filter (also if control & physics)
    break;  
 
    case 3:
    if (isControl) { // filter control and pnn
      FilterControl(); // pass through the control filter (also if control & physics)
      return false; // do not pass through the pnn filter
    } else {
      if (!isPnnMask) return false; // do not pass through the pnn filter
      return true; // pass through the pnn filter (physics only, not physics & control) 
    }
    break;

    default:
    return false; // do not pass neither through the control nor the pnn filter
    break;
  }

  return true; // pass through the pnn filter (should never reach here);
}

Bool_t FilterPnn::FilterControl() {

  // Filter 0 track events
  if (!fSpectrometerEvent->GetNCandidates()) {
    IncrementCounter("NControl_0trk");
    if (!(GetCounterValue("NControl_0trk")%(fCDW*10))) {
      IncrementCounter("NControl_Filter");
      FilterAccept();
    }
    return 0;
  }

  // Loose selection for events with at least 1 track 
  if (fGigaTrackerEvent->GetErrorMask()) return 1;
  vector<Int_t> fvCHODQ[8];
  vector<Double_t> chodPar;
  Bool_t isGood = false;
  Double_t ttimeoff = -2.1;
  if (!CHODQ1(fvCHODQ)) return 1;
  for (Int_t jT(0); jT<fSpectrometerEvent->GetNCandidates(); jT++) {
    chodPar.clear();
    TRecoSpectrometerCandidate *track = static_cast<TRecoSpectrometerCandidate *>(fSpectrometerEvent->GetCandidate(jT));
    if (track->GetNChambers()<4) continue;
    if (!fGeo->InAcceptance(track,kCHOD,0,125.,-1)) continue;
    if (!fGeo->InAcceptance(track,kLKr,0,140.,-1)) continue;
    if (!fGeo->InAcceptance(track,kMUV3,0,-1,-1)) continue;
    Double_t ttime = track->GetTime()+ttimeoff;
    chodPar = CHODTrackMatching(track->xAtAfterMagnet(fGeo->GetZCHODVPlane()),track->yAtAfterMagnet(fGeo->GetZCHODHPlane()),ttime,fvCHODQ);
    if (chodPar[0]>50) continue; // discriminant
    Double_t chodTime = chodPar[2]+ttime;

    // KTAG matching
    Double_t ktagTime = 999999.;
    Bool_t isKTAG = !KTAGTrackMatching(chodTime,&ktagTime); 

    // Reference time
    Double_t timeref = (isKTAG && fabs(ktagTime-chodTime)<=5) ? ktagTime : chodTime;

    // GTK condition
    Int_t nhits[3] = {0,0,0};
    for (Int_t jHit(0);jHit<fGigaTrackerEvent->GetNHits();jHit++) {
      TRecoGigaTrackerHit *hit = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(jHit));
      if (fabs(hit->GetTime()-timeref)<2.) nhits[hit->GetStationNo()]++; 
    }
    if (!nhits[0]||!nhits[1]||!nhits[2]) continue;
    isGood = true;
  }

  // Filter 1 track events
  if (!isGood) {
    IncrementCounter("NControl_NoSel");
    if (!(GetCounterValue("NControl_NoSel")%(5*fCDW))) {
      IncrementCounter("NControl_Filter");
      FilterAccept();
    }
    return 1;
  }
  IncrementCounter("NControl_Filter");
  FilterAccept();
  return 1;
}
