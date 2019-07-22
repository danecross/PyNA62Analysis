// ---------------------------------------------------------------------------------
//
// History:
//
// Created by Joel C. Swallow (joel.christopher.swallow@cern.ch) 2017-03-03
// Modified by Joel Swallow 06/07/17
// Joel's code completely removed and replaced with more relaxed selection
//  achieving negligible (O(1e-5) to O(1e-4)) contamination by Lubos Bician (lubos.bician@cern.ch), 2019-05-15
//
// ---------------------------------------------------------------------------------

/// \class K3piStrictSelection
/// \Brief
/// Selection of a pure sample of K3pi decays. 
/// \EndBrief
/// \Detailed
/// This K3pi strict selection loops over all reconstructed 3-track vertices
/// in an event and tests each one with a set of selection cuts
/// listed as self-explanatory parameters in the class constructor.
/// If only one vertex satisfying these criteria is found, the event is accepted.
/// The tool returns a set of useful K3pi variables registered as outputs in InitOutput member function.
/// The selection if described in full detail in an internal note NA62-19-05. It achieves an acceptance of ~11%.
/// \author Lubos Bician (lubos.bician@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include <TMath.h>
#include <TClonesArray.h>
#include "K3piStrictSelection.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include "BeamParameters.hh"
#include "VertexLSF.hh"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
using namespace TMath;

K3piStrictSelection::K3piStrictSelection(Core::BaseAnalysis *ba) : Analyzer(ba, "K3piStrictSelection") {

  RequestL0Data();
  RequestL1Data();
  RequestBeamSpecialTrigger();
  RequestBeamData();
  RequestL0SpecialTrigger();

  RequestTree(new TRecoSpectrometerEvent, "Reco");
  RequestTree(new TRecoCedarEvent, "Reco");
  RequestTree(new TRecoLKrEvent, "Reco");
  RequestTree(new TRecoCHODEvent, "Reco");
  RequestTree(new TRecoNewCHODEvent, "Reco");

  // all cuts as parameters
  AddParam("CutKaonMass", "double", &fCutKaonMass, 5.);
  AddParam("CutTrackLowMomt", "double", &fCutTrackLowMomt, 5000.);
  AddParam("CutTrackHighMomt", "double", &fCutTrackHighMomt, 60000.);
  AddParam("CutTrackChi2", "double", &fCutTrackChi2, 100.);
  AddParam("CutTrackAfterBeforeFitMomDiff", "double", &fCutTrackAfterBeforeFitMomDiff, 20000.);
  AddParam("CutVertexChi2", "double", &fCutVertexChi2, 20.);
  AddParam("CutZVertexMin", "double", &fCutZVertexMin, 110000.);
  AddParam("CutZVertexMax", "double", &fCutZVertexMax, 180000.);
  AddParam("CutTotalVertexMomtWrtBeam", "double", &fCutTotalVertexMomtWrtBeam, 2500.);
  AddParam("CutVertexPt2", "double", &fCutVertexPt2, 900./1e6);
  AddParam("CutNTracksTimeCorr", "int", &fCutNTracksTimeCorr, 3);
  AddParam("CutVertexTriggerTime", "double", &fCutVertexTriggerTime, 5.);
  AddParam("CutVertexKTAGTime", "double", &fCutVertexKTAGTime, 5.);
  AddParam("CutTriggerKTAGTime", "double", &fCutTriggerKTAGTime, 5.);
  AddParam("CutTrackVertexTime", "double", &fCutTrackVertexTime, 5.);
  AddParam("CutEoPMaximal", "double", &fCutEoPMaximal, 0.9);
}

K3piStrictSelection::~K3piStrictSelection(){
}

void K3piStrictSelection::InitOutput(){
  // registering outputs
  RegisterOutput("EventSelected", &fEventSelected);
  RegisterOutput("VertexIndex",&fChosenVtxIndex);
  RegisterOutput("KaonMass", &fKaonMass);
  RegisterOutput("KaonMomt", &fKaonMomt);
  RegisterOutput("VertexPos", &fVertexPos);
  RegisterOutput("VertexTime", &fChosenVertexTime);

  RegisterOutput("PosPionLowMomt_3Momt", &fPosPionLowMomt_3Momt);
  RegisterOutput("PosPionLowMomt_Time", &fPosPionLowMomt_Time);
  RegisterOutput("PosPionLowMomt_ID", &fPosPionLowMomt_ID);

  RegisterOutput("PosPionHighMomt_3Momt", &fPosPionHighMomt_3Momt);
  RegisterOutput("PosPionHighMomt_Time", &fPosPionHighMomt_Time);
  RegisterOutput("PosPionHighMomt_ID", &fPosPionHighMomt_ID);

  RegisterOutput("NegPion_3Momt", &fNegPion_3Momt);
  RegisterOutput("NegPion_Time", &fNegPion_Time);
  RegisterOutput("NegPion_ID", &fNegPion_ID);

}

void K3piStrictSelection::InitHist(){
  
  BookHisto(new TH1D("hNRuns", "Number of runs", 1, 0, 1));
  BookHisto(new TH1D("hNBursts", "Number of bursts", 1, 0, 1));
  BookHisto(new TH1D("hNTracks", "Number of reconstructed tracks per event", 20, -0.5, 19.5));
  BookHisto(new TH1D("hNvtxTracks", "Number of reconstructed tracks per vertex", 5, -0.5, 4.5));
  BookHisto(new TH1D("hVtxCharge", "Vertex charge", 11, -5.5, 5.5));
  BookHisto(new TH1D("hVtxChi2", "Vertex chi2", 100, 0., 100.));
  BookHisto(new TH1D("hVtxMomt", "Vertex momentum wrt beam momentum", 100, -5000., 5000.));
  BookHisto(new TH1D("hVtxPt2", "Vertex pt2", 200, 0., 3600./1e6));
  BookHisto(new TH1D("hVtxZ", "Vertex Z", 200, 100000., 200000.));
  BookHisto(new TH1D("hEoP", "Track E/p", 130, 0., 1.3));
  BookHisto(new TH1D("hTrackChi2", "Track chi2", 50, 0., 500.));
  BookHisto(new TH1D("hTrackMomt", "Track p", 80, 0., 80000.));
  BookHisto(new TH1D("hNchod", "Number of CHOD/NewCHOD - time corrected tracks", 4, -0.5, 3.5));
  BookHisto(new TH1D("hNtracksInTimeWithVtx", "Number of tracks in-time with vertex", 4, -0.5, 3.5));
  BookHisto(new TH1D("hTrackVertexTime", "Track - vertex time difference", 200, -20., 20.));
  BookHisto(new TH1D("hVertexTriggerTime", "Vertex - trigger time difference", 200, -20., 20.));
  BookHisto(new TH1D("hVertexKTAGTime", "Vertex - KTAG time difference", 200, -20., 20.));
  BookHisto(new TH1D("hTriggerKTAGTime", "Trigger - KTAG time difference", 200, -20., 20.));
  BookHisto(new TH1D("hNVertices", "Number of good 3-track vertices", 10, 0, 10));
  BookHisto(new TH1D("hInvMass", "Invariant mass of 3 pions", 200, MKCH-100., MKCH+100.));

}

void K3piStrictSelection::DefineMCSimple(){
}

void K3piStrictSelection::StartOfRunUser(){
  FillHisto("hNRuns", 0.5);
}

void K3piStrictSelection::StartOfBurstUser() {
  FillHisto("hNBursts", 0.5);
}

void K3piStrictSelection::Process(int) {
 
  fEventSelected = false;
  SetOutputState("EventSelected", kOValid);
  SetOutputState("VertexIndex", kOValid);
  SetOutputState("KaonMass", kOValid);
  SetOutputState("KaonMomt", kOValid);
  SetOutputState("VertexPos", kOValid);
  SetOutputState("VertexTime", kOValid);
  
  SetOutputState("PosPionLowMomt_3Momt", kOValid);
  SetOutputState("PosPionLowMomt_Time", kOValid);
  SetOutputState("PosPionLowMomt_ID", kOValid);
  
  SetOutputState("PosPionHighMomt_3Momt", kOValid);
  SetOutputState("PosPionHighMomt_Time", kOValid);
  SetOutputState("PosPionHighMomt_ID", kOValid);
  
  SetOutputState("NegPion_3Momt", kOValid);
  SetOutputState("NegPion_Time", kOValid);
  SetOutputState("NegPion_ID", kOValid);

  fTriggerTime	  = GetL0Data()->GetReferenceFineTime()*TdcCalib;
  fBeamThreeMomt  = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  fNSTRAWTracks = GetEvent<TRecoSpectrometerEvent>()->GetNCandidates();
  FillHisto("hNTracks", fNSTRAWTracks);
  
  // external tools outputs
  fDownTrack = *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  fVertices = *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");

  // get the number of good 3-track vertices in the event
  fNVertices = 0;
  fVtxIndex  = -1;
  fChosenVtxIndex = -1;
  for (UInt_t i=0; i<fVertices.size(); i++) {
    SpectrometerTrackVertex vtx = fVertices[i];
    Int_t NTracks = vtx.GetNTracks();
    FillHisto("hNvtxTracks", NTracks);
    if (NTracks!=3) continue;
    
    double chrg = vtx.GetCharge();
    FillHisto("hVtxCharge", chrg);
    if (chrg!=1) continue;

    double chi2 = vtx.GetChi2();
    FillHisto("hVtxChi2", chi2);
    if (chi2>fCutVertexChi2) continue; 
    
    double momt = vtx.GetTotalMomentum();
    double beam = fBeamThreeMomt.Mag();
    FillHisto("hVtxMomt", momt-beam);
    if (fabs(momt-beam)>fCutTotalVertexMomtWrtBeam) continue; 
    
    double vpt2 = pow(vtx.GetTotalThreeMomentum().Pt(fBeamThreeMomt), 2.)/1e6;
    FillHisto("hVtxPt2", vpt2);
    if (vpt2>fCutVertexPt2) continue;
    
    double vtxZ = vtx.GetPosition().z();
    FillHisto("hVtxZ", vtxZ);
    if (vtxZ<fCutZVertexMin || vtxZ>fCutZVertexMax) continue;

    fVtxIndex = i;
    for (UInt_t j=0; j<3; j++) {
      fTrackID.at(j) = fVertices[fVtxIndex].GetTrackIndex(j);
      fTrackCharge.at(j) = fDownTrack[fTrackID.at(j)].GetCharge();
      fTrackTime.at(j) = fDownTrack[fTrackID.at(j)].GetTrackTime();
    }
    bool tracksOK = tracksPassCuts();
    bool ktagOK = atLeastOneGoodKTAG();
    if (!tracksOK || !ktagOK) continue;

    // this is a good vertex, remember important variables
    fNVertices++;
    fChosenVtxIndex = fVtxIndex;
    fChosenVertexTime = fVertexTime;
    for (UInt_t j=0; j<3; j++) {
      fChosenTrackID.at(j) = fTrackID.at(j);
      fChosenTrackTime.at(j) = fTrackTime.at(j);
    }
  }

  // require exactly one good 3-track vertex
  FillHisto("hNVertices", fNVertices);
  if(fNVertices!=1) return;
 
  SpectrometerTrackVertex vtx = fVertices[fChosenVtxIndex];
  TLorentzVector KaonP(0., 0., 0., 0.);

  int idNeg = -1;
  int idPosLow = -1;
  int idPosHigh = -1;
  double firstPosMom = 0.;
  for (unsigned int i=0; i<3; i++) {
    fTrackThreeMomt.at(i) = vtx.GetTrackThreeMomentum(i);
    TLorentzVector PionP;
    PionP.SetVectM(fTrackThreeMomt.at(i), MPI);
    KaonP += PionP;

    double pTrack = vtx.GetTrackMomentum(i);
    int chTrack = vtx.GetTrackCharge(i);
    
    // sort positive tracks according to momentum
    if (chTrack==-1) idNeg = i;
    if (chTrack==1) {
      if (idPosLow==-1) {
	idPosLow = i;
	firstPosMom = pTrack;
      } else {
	if (firstPosMom<pTrack) idPosHigh = i;
	  else {idPosHigh = idPosLow; idPosLow = i;}
      }
    }
  }
 
  fKaonMass = KaonP.M();
  FillHisto("hInvMass", fKaonMass);
  if (fabs(fKaonMass-MKCH)>fCutKaonMass) return;
  
  fEventSelected = true;
  fKaonMomt = vtx.GetTotalThreeMomentum();
  fVertexPos = vtx.GetPosition();

  fPosPionLowMomt_3Momt = vtx.GetTrackThreeMomentum(idPosLow);
  fPosPionLowMomt_Time = fChosenTrackTime.at(idPosLow);
  fPosPionLowMomt_ID = fChosenTrackID.at(idPosLow);

  fPosPionHighMomt_3Momt = vtx.GetTrackThreeMomentum(idPosHigh);
  fPosPionHighMomt_Time = fChosenTrackTime.at(idPosHigh);
  fPosPionHighMomt_ID = fChosenTrackID.at(idPosHigh);

  fNegPion_3Momt = vtx.GetTrackThreeMomentum(idNeg);
  fNegPion_Time = fChosenTrackTime.at(idNeg);
  fNegPion_ID = fChosenTrackID.at(idNeg);

}

void K3piStrictSelection::PostProcess(){
}

void K3piStrictSelection::EndOfBurstUser(){
}

void K3piStrictSelection::EndOfRunUser(){
}

void K3piStrictSelection::EndOfJobUser(){
  SaveAllPlots();
}

bool K3piStrictSelection::tracksPassCuts() {
  bool InAcceptance = true;
  for (Int_t i=0; i<3; i++) {
    DownstreamTrack trck = fDownTrack[fTrackID.at(i)];
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kSpectrometer, 0)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kSpectrometer, 1)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kSpectrometer, 2)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kSpectrometer, 3)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kNewCHOD)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kCHOD)) InAcceptance = false;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(&trck, kLKr)) InAcceptance = false;
  }
  if (!InAcceptance) return false;

  fVertexTime = 0.;
  int Nchod = 0;

  bool VertexTriggerInTime(false);
  bool AllTracksGoodEoP(true);
  bool AllTracksGoodCh2(true);
  bool AllTracksGoodMom(true);
  
  for (UInt_t j=0; j<3; j++) {
    int TID = fTrackID.at(j);
    double EoP = fDownTrack[TID].GetLKrEoP();
    FillHisto("hEoP", EoP);
    if (EoP>fCutEoPMaximal) AllTracksGoodEoP = false;
    
    double Ch2 = fDownTrack[TID].GetChi2();
    FillHisto("hTrackChi2", Ch2);
    if (Ch2>fCutTrackChi2) AllTracksGoodCh2 = false;

    double Mom = fDownTrack[TID].GetMomentum();
    FillHisto("hTrackMomt", Mom);
    if (Mom<fCutTrackLowMomt || Mom>fCutTrackHighMomt) AllTracksGoodMom = false;

    double BFAF = fabs(fDownTrack[TID].GetMomentum()-fDownTrack[TID].GetMomentumBeforeFit());
    if (BFAF>fCutTrackAfterBeforeFitMomDiff) AllTracksGoodMom = false;

    // vertex time
    if (fDownTrack[TID].NewCHODAssociationExists() || fDownTrack[TID].CHODAssociationExists()) {
      Nchod++;
    }
    if (fDownTrack[TID].CHODAssociationExists()) {
      fTrackTime.at(j) = fDownTrack[TID].GetCHODTime();
      fVertexTime += fTrackTime.at(j);
    } else if (fDownTrack[TID].NewCHODAssociationExists()) {
      fTrackTime.at(j) = fDownTrack[TID].GetNewCHODTime();
      fVertexTime += fTrackTime.at(j);
    }

  }
  if (!AllTracksGoodEoP) return false;
  if (!AllTracksGoodCh2) return false;
  if (!AllTracksGoodMom) return false;

  FillHisto("hNchod", Nchod);
  
  // vertex timing
  if (Nchod>0) {
    int Nclose = 0;
    fVertexTime /= Nchod;
    for (int i=0; i<3; i++) {
      FillHisto("hTrackVertexTime", fTrackTime.at(i)-fVertexTime);
      if (fabs(fTrackTime.at(i)-fVertexTime)<fCutTrackVertexTime) Nclose++;
    }
    FillHisto("hNtracksInTimeWithVtx", Nclose);
    if (Nclose!=3) return false;
  }
  FillHisto("hVertexTriggerTime", fVertexTime - fTriggerTime);
  if (fabs(fVertexTime-fTriggerTime)<fCutVertexTriggerTime) VertexTriggerInTime = true;
  if (Nchod<fCutNTracksTimeCorr || !VertexTriggerInTime) return false;

  return true;

}

Bool_t K3piStrictSelection::atLeastOneGoodKTAG() {
  TRecoCedarEvent* CedarEvent = GetEvent<TRecoCedarEvent>();
  if (CedarEvent->GetNCandidates()==0) return false;
  Bool_t FoundGoodCedarCand = false;

  for (Int_t iCand=0; iCand < CedarEvent->GetNCandidates(); iCand++) {
    TRecoCedarCandidate* CedarCand = static_cast<TRecoCedarCandidate*>(CedarEvent->GetCandidate(iCand));
    if (CedarCand->GetNSectors()<5) continue;
    Double_t KTAGTime = CedarCand->GetTime();
    FillHisto("hTriggerKTAGTime", fTriggerTime - KTAGTime);
    if (fabs(fTriggerTime-KTAGTime)>fCutTriggerKTAGTime) continue;
    FillHisto("hVertexKTAGTime", fVertexTime - KTAGTime);
    if (fabs(fVertexTime-KTAGTime)>fCutVertexKTAGTime) continue;
    FoundGoodCedarCand = true;
  }
  return FoundGoodCedarCand;
}

