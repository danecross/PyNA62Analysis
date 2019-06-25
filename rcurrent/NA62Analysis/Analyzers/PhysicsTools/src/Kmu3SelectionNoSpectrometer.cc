// ---------------------------------------------------------------
//
// History:    
//
// Created by Zuzana Kucerova (zuzana.kucerova@cern.ch) 01.2019
//
// ---------------------------------------------------------------
/// \class Kmu3SelectionNoSpectrometer
/// \Brief
/// Kmu3 decay selection without STRAW spectrometer
/// \EndBrief
/// \Detailed
/// The analyzer is used by SpectrometerEfficiency analyzer. 
/// It has three outputs: EventSelected, TrackFourMomentum and TrackPosition. 
/// 
/// \author Zuzana Kucerova (zuzana.kucerova@cern.ch)
/// \EndDetailed 

#include <stdlib.h>
#include <iostream>
#include <bitset>
#include <TChain.h>
#include "BeamParameters.hh"
#include "GeometricAcceptance.hh"
#include "Pi0Selection.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "Kmu3SelectionNoSpectrometer.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

Kmu3SelectionNoSpectrometer::Kmu3SelectionNoSpectrometer(Core::BaseAnalysis *ba) : Analyzer(ba, "Kmu3SelectionNoSpectrometer")
{
  RequestTree("LAV", new TRecoLAVEvent, "Reco");
  RequestTree("IRC", new TRecoIRCEvent, "Reco");
  RequestTree("SAC", new TRecoSACEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("RICH", new TRecoRICHEvent, "Reco");
  RequestTree("MUV1", new TRecoMUV1Event, "Reco");
  RequestTree("MUV2", new TRecoMUV2Event, "Reco");

  RequestL0Data();

  fTriggerConditions = TriggerConditions::GetInstance();

  AddParam("CutTimeDiffRICH", "double", &fCutTimeDiffRICH, 10.);
  AddParam("CutTimeDiffMUV3", "double", &fCutTimeDiffMUV3, 10.);
  AddParam("CutTimeDiffMUV1Pi0", "double", &fCutTimeDiffMUV1Pi0, 5.);
  AddParam("CutMinEnergyOfMUV1Hit", "double", &fCutMinEnergyOfMUV1Hit, 100.);
  AddParam("CutTimeDiffMUV2Pi0", "double", &fCutTimeDiffMUV2Pi0, 5.);
  AddParam("CutMinEnergyOfMUV2Hit", "double", &fCutMinEnergyOfMUV2Hit, 100.);
  AddParam("CutMaxNHitsMUV12", "int", &fCutMaxNHitsMUV12, 10);
  AddParam("TimeWindowIRC", "double", &fTimeWindowIRC, 10.);
  AddParam("TimeWindowSAC", "double", &fTimeWindowSAC, 10.);
  AddParam("CutMissM2", "double", &fCutMissM2, 2000.);

  Lf = 17020.;
  refInd = 1.000062;
  fMNP33kick = 270.;
}

void Kmu3SelectionNoSpectrometer::InitOutput(){
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("Kmu3MuonFourMomentum", &fTrackFourMomentum);
  RegisterOutput("Kmu3MuonPosition", &fTrackPosition);
}

void Kmu3SelectionNoSpectrometer::InitHist(){
  BookHisto(new TH1I("NEvents", "N events; N events", 1, 1, 2));
  BookHisto(new TH1F("hVertexZ", "Vertex Z; Z_{vertex} [mm]", 500, 90000., 190000.));
  BookHisto(new TH1F("hTimePi0", "#pi^{0} time; T_{#pi^{0}} [ns]", 200, -100., 100.));
  BookHisto(new TH1F("hTimeDiffPI0_RICH", "#Delta T (PI0-RICH); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNInTimeRICH", "N RICH candidates in time with #pi^{0};N candidates; N events", 10, 0, 10));
  BookHisto(new TH2F("hRICHRingRadiusVsMom", "RICH ring radius VS momentum (muon hypothesis); P [MeV]; R [mm]", 800, 0., 80000., 300, 0., 300.));
  BookHisto(new TH1I("hRICHMomAboveThreshold", "Is the momentum above threshold? (muon hypothesis);N events", 2, 0, 2));
  BookHisto(new TH1F("hMuonMom", "Momentum (muon hypothesis); P [MeV]", 1000, 0., 100000.));
  BookHisto(new TH1F("hTimeDiffPI0_MUV3", "#Delta T (PI0-MUV3); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1I("hNInTimeMUV3CloseInPos", "N MUV3 candidates in time with #pi^{0} and close to the impact point in MUV3 plane;N candidates; N events", 10, 0, 10));
  BookHisto(new TH1D("hTimeDiffPI0_MUV1", "#Delta T (PI0-MUV1); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1F("hEofHit_MUV1", "MUV1 hit energy; E [MeV]", 200, 0., 40000.));
  BookHisto(new TH2F("hHitEvsTimeDiff_MUV1", "MUV1 hit energy VS #Delta T (PI0-MUV1); #Delta T [ns]; E [MeV]", 200, -100., 100., 200, 0., 40000.));
  BookHisto(new TH1F("hTimeDiffPI0_MUV2", "#Delta T (PI0-MUV2); #Delta T [ns]", 200, -100., 100.));
  BookHisto(new TH1F("hEofHit_MUV2", "MUV2 hit energy; E [MeV]", 200, 0., 40000.));
  BookHisto(new TH2F("hHitEvsTimeDiff_MUV2", "MUV2 hit energy VS #Delta T (PI0-MUV2); #Delta T [ns]; E [MeV]", 200, -100., 100., 200, 0., 40000.));
  BookHisto(new TH1I("hSumNHits_MUV12", "N hits with E>100MeV in MUV1+MUV2 in time with #pi^{0}", 50, 0, 50));
  BookHisto(new TH1I("hLAVHasTimeMatching", "LAV has time matching; N events", 2, 0, 2));
  BookHisto(new TH1I("hSAVHasTimeMatching", "SAV has time matching; N events", 2, 0, 2));
  BookHisto(new TH1I("hNGTKCandidates", "Number of GTK candidates", 50, 0, 50));
  BookHisto(new TH1F("hKaonMom", "Kaon momentum; P [MeV]", 1000, 0., 100000.));
  BookHisto(new TH1F("hMissM2", "Squared neutrino missing mass; M_{miss}^{2}(#nu) [MeV^{2}]", 200, -10000., 10000.));
  BookHisto(new TH1F("hMissM2_AS", "Squared neutrino missing mass (after full selection); M_{miss}^{2}(#nu) [MeV^{2}]", 60, -3000., 3000.));
}

void Kmu3SelectionNoSpectrometer::DefineMCSimple(){}

void Kmu3SelectionNoSpectrometer::StartOfRunUser(){}

void Kmu3SelectionNoSpectrometer::StartOfBurstUser(){}

void Kmu3SelectionNoSpectrometer::Process(Int_t){
  FillHisto("NEvents", 1);
  fEventSelected = false;
  SetOutputState("EventSelected", kOValid); 
  fTrackFourMomentum.SetXYZT(0., 0., 0., 0.);
  SetOutputState("Kmu3MuonFourMomentum", kOValid);
  fTrackPosition.SetXYZ(0., 0., 0.);
  SetOutputState("Kmu3MuonPosition", kOValid);

  //trigger
  Bool_t controlTrig = true;
  if(!GetWithMC()){ // data
    controlTrig = fTriggerConditions->IsControlTrigger(GetL0Data());
  };
  if(!controlTrig) return;

  //no K2pi
  OutputState state;
  bool k2piSelected = *(bool*)GetOutput("K2piSelection.EventSelected", state);
  if(state!=kOValid) return;
  if(k2piSelected) return;

  //pion
  TVector3 vertex;
  bool selected = *(bool*)GetOutput("Pi0Selection.EventSelected");
  if(!selected) return;
  std::vector<Pi0SelectionOutput> pi0output = *(std::vector<Pi0SelectionOutput>*)GetOutput("Pi0Selection.SelectedPi0");
  if(pi0output.size()!=1) return;

  TLorentzVector Pion;
  Pion = pi0output[0].fMomentum;
  ftime = pi0output[0].fTime;
  vertex = pi0output[0].fPosition;
  FillHisto("hVertexZ", vertex.Z());
  FillHisto("hTimePi0", ftime);

  //muon in RICH
  TRecoRICHEvent *RICHEvent = static_cast<TRecoRICHEvent*>(GetEvent("RICH"));
  int countRICHInTime = 0;
  int inTimeID = -1;
  for(int i=0; i<RICHEvent->GetNRingCandidates(); i++){
    TRecoRICHCandidate *RICHCand = static_cast<TRecoRICHCandidate*>(RICHEvent->GetRingCandidate(i));
    double ringTime = RICHCand->GetRingTime();
    FillHisto("hTimeDiffPI0_RICH", ringTime - ftime);
    if(fabs(ringTime - ftime)<fCutTimeDiffRICH){
      countRICHInTime++;
      inTimeID = i;
    };
  };
  FillHisto("hNInTimeRICH", countRICHInTime);
  if(countRICHInTime!=1) return;
  TRecoRICHCandidate *RICHCand = static_cast<TRecoRICHCandidate*>(RICHEvent->GetRingCandidate(inTimeID));
  double P = GetMomFromR(RICHCand->GetRingRadius(), MMU);
  FillHisto("hRICHRingRadiusVsMom", P, RICHCand->GetRingRadius());
  if(P<(MMU/sqrt(refInd*refInd - 1.))){
    FillHisto("hRICHMomAboveThreshold", 0);
    return;
  }else{
    FillHisto("hRICHMomAboveThreshold", 1);
  };
  TVector3 muonMomAM = GetMuonMomAM(P, RICHCand->GetRingCenter());
  TVector3 muonMom = GetMomBM(muonMomAM);
  FillHisto("hMuonMom", muonMom.Mag());

  // muon in MUV3 
  TVector3 muonPosAtMUV3 = GetPositionAtZ(muonMom, vertex, GeometricAcceptance::GetInstance()->GetZMUV3());
  if(!(GeometricAcceptance::GetInstance()->InAcceptance(muonPosAtMUV3.X(), muonPosAtMUV3.Y(), kMUV3))) return;
  TRecoMUV3Event *MUV3Event = static_cast<TRecoMUV3Event*>(GetEvent("MUV3"));
  int MUV3nc = MUV3Event->GetNCandidates();
  int countMUV3InTimeCloseInPos = 0;
  for(int i=0; i<MUV3nc; i++){
    TRecoMUV3Candidate *MUV3cand = static_cast<TRecoMUV3Candidate*>(MUV3Event->GetCandidate(i));
    double time = MUV3cand->GetTime();
    FillHisto("hTimeDiffPI0_MUV3", time - ftime);
    bool isCloseInPosMUV3 = GetIsCloseInPosMUV3(MUV3cand->GetPosition(), muonPosAtMUV3, muonMom.Mag());
    if((fabs(time - ftime)<fCutTimeDiffMUV3) && (isCloseInPosMUV3)) countMUV3InTimeCloseInPos++;
  };
  FillHisto("hNInTimeMUV3CloseInPos", countMUV3InTimeCloseInPos);
  if(countMUV3InTimeCloseInPos<1) return;

  //MUV1+MUV2 veto (hits) (k2pi suppression - pion should leave more hits in hadronic calorimeters than muon)
  int countMUV1 = 0;
  TRecoMUV1Event* MUV1Event = static_cast<TRecoMUV1Event*>(GetEvent("MUV1"));
  for (Int_t ihit = 0; ihit < MUV1Event->GetNHits(); ihit++) {
    auto MUV1hit = static_cast<TRecoMUV1Hit*>(MUV1Event->GetHit(ihit));
    Double_t time = MUV1hit->GetTime();
    FillHisto("hTimeDiffPI0_MUV1", time - ftime);
    FillHisto("hEofHit_MUV1", MUV1hit->GetEnergy());
    FillHisto("hHitEvsTimeDiff_MUV1", time - ftime, MUV1hit->GetEnergy());
    if ((fabs(time - ftime) < fCutTimeDiffMUV1Pi0) && (MUV1hit->GetEnergy() > fCutMinEnergyOfMUV1Hit)) countMUV1++;
  }
  int countMUV2 = 0;
  TRecoMUV2Event* MUV2Event = static_cast<TRecoMUV2Event*>(GetEvent("MUV2"));
  for (Int_t ihit = 0; ihit < MUV2Event->GetNHits(); ihit++) {
    auto MUV2hit = static_cast<TRecoMUV2Hit*>(MUV2Event->GetHit(ihit));
    Double_t time = MUV2hit->GetTime();
    FillHisto("hTimeDiffPI0_MUV2", time - ftime);
    FillHisto("hEofHit_MUV2", MUV2hit->GetEnergy());
    FillHisto("hHitEvsTimeDiff_MUV2", time - ftime, MUV2hit->GetEnergy());
    if ((fabs(time - ftime) < fCutTimeDiffMUV2Pi0) && (MUV2hit->GetEnergy() > fCutMinEnergyOfMUV2Hit)) countMUV2++;
  }
  FillHisto("hSumNHits_MUV12", countMUV1+countMUV2);
  if ((countMUV1 + countMUV2) >= fCutMaxNHitsMUV12) return;
  
  TLorentzVector Muon;
  Muon.SetVectM(muonMom, MMU);

  //LAV veto (with timing) (to be sure that all energy has been measured by other calorimeters, also to be sure that no other photon was present in the event and we accidentally used the wrong one in pi0 reconstruction)
  TRecoLAVEvent* LAVEvent = static_cast<TRecoLAVEvent*>(GetEvent("LAV"));
  LAVMatching* pLAVMatching = *(LAVMatching**)GetOutput("PhotonVetoHandler.LAVMatching");
  pLAVMatching->SetReferenceTime(ftime);
  FillHisto("hLAVHasTimeMatching", pLAVMatching->LAVHasTimeMatching(LAVEvent));
  if(pLAVMatching->LAVHasTimeMatching(LAVEvent)) return;

  // IRC and SAC veto (with timing)
  TRecoIRCEvent* IRCEvent = static_cast<TRecoIRCEvent*>(GetEvent("IRC"));
  TRecoSACEvent* SACEvent = static_cast<TRecoSACEvent*>(GetEvent("SAC"));
  SAVMatching* pSAVMatching = *(SAVMatching**)GetOutput("PhotonVetoHandler.SAVMatching");
  pSAVMatching->SetReferenceTime(ftime);
  pSAVMatching->SetIRCTimeCuts(fTimeWindowIRC, fTimeWindowIRC); // half time window; default = 5ns
  pSAVMatching->SetSACTimeCuts(fTimeWindowSAC, fTimeWindowSAC); // half time window; default = 5ns
  Bool_t SAVmatched = pSAVMatching->SAVHasTimeMatching(IRCEvent, SACEvent);
  FillHisto("hSAVHasTimeMatching", SAVmatched);
  if(SAVmatched) return;

  //kaon
  TVector3 kaonThreeMomentum;
  TRecoGigaTrackerEvent *GTKEvent = static_cast<TRecoGigaTrackerEvent*>(GetEvent("GigaTracker"));
  int gtkID = pi0output[0].fGTKID; 
  if(gtkID>=0){
    if(gtkID<GTKEvent->GetNCandidates()){
      FillHisto("hNGTKCandidates", GTKEvent->GetNCandidates());
      TRecoGigaTrackerCandidate *GTKCand = static_cast<TRecoGigaTrackerCandidate*>(GTKEvent->GetCandidate(gtkID));
      kaonThreeMomentum = GTKCand->GetMomentum();
    }else{
      cout << user_normal() << "WARNING: Requested GTK ID out of range (too large)!" << endl;
      FillHisto("hNGTKCandidates", -1);
    };
  } else {                      // take average kaon momentum
    kaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  };
  FillHisto("hKaonMom", kaonThreeMomentum.Mag());

  TLorentzVector Kaon;
  Kaon.SetVectM(kaonThreeMomentum, MKCH);

  double missM2= (Kaon-Muon-Pion).M2();
  FillHisto("hMissM2", missM2);
  if(fabs(missM2)>fCutMissM2) return;
  FillHisto("hMissM2_AS", missM2);
  FilterAccept();

  fTrackFourMomentum = Muon;
  fTrackPosition = vertex; 
  fEventSelected = true;
}

void Kmu3SelectionNoSpectrometer::PostProcess(){}

void Kmu3SelectionNoSpectrometer::EndOfBurstUser(){}

void Kmu3SelectionNoSpectrometer::EndOfRunUser(){}

void Kmu3SelectionNoSpectrometer::EndOfJobUser(){
  SaveAllPlots();
}

void Kmu3SelectionNoSpectrometer::DrawPlot(){}

Kmu3SelectionNoSpectrometer::~Kmu3SelectionNoSpectrometer(){}

TVector3 Kmu3SelectionNoSpectrometer::GetMuonMomAM(double P, TVector2 RC){
  double Rx = RC.X();
  double Ry = RC.Y();
  double factor = P/sqrt(1 + (Rx*Rx)/(Ry*Ry) + (Lf*Lf)/(Ry*Ry));
  double Px = (Rx/fabs(Ry))*factor;
  double Py = (Ry/fabs(Ry))*factor;
  double Pz = (Lf/fabs(Ry))*factor;
  TVector3 momAM(Px, Py, Pz);

  return momAM;
}

TVector3 Kmu3SelectionNoSpectrometer::GetMomBM(TVector3 momAM){
  double kick = 270.;
  double Px = momAM.X();
  double Pz = momAM.Z();
  double PxBM = Px + kick;
  TVector2 momT(Px, Pz);
  double pT = momT.Mod();
  double beta = acos(PxBM/pT);
  double theta = acos(Px/pT);
  double alpha = theta - beta;
  TVector3 mom = momAM;
  mom.RotateY(alpha);
  TVector3 momBM = mom;

  return momBM;
}

double Kmu3SelectionNoSpectrometer::GetMomFromR(double R, double M){
  double theta = R/Lf;
  double beta = 1./(cos(theta)*refInd);
  double P = fabs((beta*M)/sqrt(1.-beta*beta));

  return P;
}

TVector3 Kmu3SelectionNoSpectrometer::GetPositionAtZ(TVector3 mom, TVector3 oldPos, double newZ){
  if(((newZ<=197600.)&&(oldPos.Z()<=197600.)) || ((newZ>=197600.)&&(oldPos.Z()>=197600.))){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - newZ);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - newZ); 
    TVector3 newPos(xAtZ, yAtZ, newZ);
    return newPos;
  }else if((newZ>197600.)&&(oldPos.Z()<197600.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 197600.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 197600.); 
    TVector3 posAtMagnet(xAtZ, yAtZ, 197600.);
    TVector3 newMom = MomAfterKick(mom, fMNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(197600. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(197600. - newZ); 
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else if((newZ<197600.)&&(oldPos.Z()>197600.)){
    double slopeX = (mom.X())/(mom.Z());
    double slopeY = (mom.Y())/(mom.Z());
    double xAtZ = oldPos.X() - slopeX*(oldPos.Z() - 197600.);
    double yAtZ = oldPos.Y() - slopeY*(oldPos.Z() - 197600.); 
    TVector3 posAtMagnet(xAtZ, yAtZ, 197600.);
    TVector3 newMom = MomAfterKick(mom, -fMNP33kick);
    double slopeXa = (newMom.X())/(newMom.Z());
    double slopeYa = (newMom.Y())/(newMom.Z());
    double xAtZa = posAtMagnet.X() - slopeXa*(197600. - newZ);
    double yAtZa = posAtMagnet.Y() - slopeYa*(197600. - newZ); 
    TVector3 newPos(xAtZa, yAtZa, newZ);
    return newPos;
  }else{
    cout<<user_normal()<<"cannot calculate the position"<<endl;
    return oldPos;
  };
}

TVector3 Kmu3SelectionNoSpectrometer::MomAfterKick(TVector3 oldMom, double kick){
  TVector3 mom = oldMom;
  TVector2 pTvec(mom.X(), mom.Z());
  double pT = pTvec.Mod();
  double pxNew = mom.X() - kick;
  double beta = acos(mom.X()/pT);
  double theta = acos(pxNew/pT);
  double alpha = theta - beta;
  mom.RotateY(-alpha);
  TVector3 newMom = mom;

  return newMom;
}

bool Kmu3SelectionNoSpectrometer::GetIsCloseInPosMUV3(TVector3 posCand, TVector3 posTrack, double P){
  double r = 4*530000./P;
  double posDiff = (posCand-posTrack).Mag();

  bool isClose = false;
  if(posDiff<r) isClose = true;

  return isClose;
}
