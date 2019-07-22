// ---------------------------------------------------------
// History:
//
// Pileup in LKr added by Michele Corvino 2019-04-30
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-04-23
//
// ---------------------------------------------------------
/// \class BuildNewCHODPileupHisto
/// \Brief
/// This analyzer produces the hit library file used by the DownstreamPileupGenerator.
/// \EndBrief
/// \Detailed
/// This analyzer produces the hit library file used by the DownstreamPileupGenerator.
/// It adds hits/candidates from the LKr, Spectrometer, NewCHOD, and MUV3.
/// It should only be run using the BuildDPGHitLibrary.sh script, currently
/// located in the ./NA62Analysis/scripts/ directory.
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "BuildDPGHitLibrary.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include <fstream>

BuildDPGHitLibrary::BuildDPGHitLibrary(Core::BaseAnalysis *ba) :
  Analyzer(ba, "BuildDPGHitLibrary"),
  fRootFile(nullptr),
  fSpecTree(nullptr)
{
  RequestTree("Spectrometer", new TRecoSpectrometerEvent, "Reco");
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestTree("LAV", new TRecoLAVEvent, "Reco");
  RequestTree("IRC", new TRecoIRCEvent, "Reco");
  RequestTree("SAC", new TRecoSACEvent, "Reco");
  RequestTree("LKr", new TRecoLKrEvent, "Reco");

  AddParam("Name", &fOutputName, "Default");
}

void BuildDPGHitLibrary::InitOutput(){
}

void BuildDPGHitLibrary::InitHist(){
}

void BuildDPGHitLibrary::DefineMCSimple(){
}

void BuildDPGHitLibrary::StartOfRunUser(){

  TString name = "DownstreamPileupGeneratorLibrary" + fOutputName + ".root";

  fRootFile = new TFile(name,"RECREATE");
  fSpecTree = new TTree(fOutputName,fOutputName);

  fSpecTree->Branch("Spectrometer_NHits"  , &fLib_spectrometer.fNHits);
  fSpecTree->Branch("Spectrometer_Channel", &fLib_spectrometer.fChannelID, "gSC[Spectrometer_NHits]/I");
  fSpecTree->Branch("Spectrometer_Time"   , &fLib_spectrometer.fTime     , "gST[Spectrometer_NHits]/F");
  fSpecTree->Branch("Spectrometer_Width"  , &fLib_spectrometer.fOther1.f , "gSW[Spectrometer_NHits]/F");

  fSpecTree->Branch("MUV3_NHits"  , &fLib_muv3.fNHits);
  fSpecTree->Branch("MUV3_Channel", &fLib_muv3.fChannelID, "gMC[MUV3_NHits]/I");
  fSpecTree->Branch("MUV3_Time"   , &fLib_muv3.fTime     , "gMT[MUV3_NHits]/F");

  fSpecTree->Branch("NewCHOD_NHits"  , &fLib_newchod.fNHits);
  fSpecTree->Branch("NewCHOD_Channel", &fLib_newchod.fChannelID, "gNC[NewCHOD_NHits]/I");
  fSpecTree->Branch("NewCHOD_Time"   , &fLib_newchod.fTime     , "gNT[NewCHOD_NHits]/F");

  fSpecTree->Branch("LAV_NHits"  , &fLib_lav.fNHits);
  fSpecTree->Branch("LAV_Channel", &fLib_lav.fChannelID, "gLAVC[LAV_NHits]/I");
  fSpecTree->Branch("LAV_Time"   , &fLib_lav.fTime     , "gLAVT[LAV_NHits]/F");
  fSpecTree->Branch("LAV_Edge"   , &fLib_lav.fOther1.i , "gLAVE[LAV_NHits]/I");

  fSpecTree->Branch("IRC_NHits"  , &fLib_irc.fNHits);
  fSpecTree->Branch("IRC_Channel", &fLib_irc.fChannelID, "gIRCC[IRC_NHits]/I");
  fSpecTree->Branch("IRC_Time"   , &fLib_irc.fTime     , "gIRCT[IRC_NHits]/F");
  fSpecTree->Branch("IRC_Edge"   , &fLib_irc.fOther1.i , "gIRCE[IRC_NHits]/I");

  fSpecTree->Branch("SAC_NHits"  , &fLib_sac.fNHits);
  fSpecTree->Branch("SAC_Channel", &fLib_sac.fChannelID, "gSACC[SAC_NHits]/I");
  fSpecTree->Branch("SAC_Time"   , &fLib_sac.fTime     , "gSACT[SAC_NHits]/F");
  fSpecTree->Branch("SAC_Edge"   , &fLib_sac.fOther1.i , "gSACE[SAC_NHits]/I");

  fSpecTree->Branch("LKr_NCandidates", &fLib_lkr.fNHits);
  fSpecTree->Branch("LKr_Energy"     , &fLib_lkr.fOther1.f, "gLCLE[LKr_NCandidates]/F");
  fSpecTree->Branch("LKr_X"          , &fLib_lkr.fOther2.f, "gLClX[LKr_NCandidates]/F");
  fSpecTree->Branch("LKr_Y"          , &fLib_lkr.fOther3.f, "gLClY[LKr_NCandidates]/F");
  fSpecTree->Branch("LKr_Time"       , &fLib_lkr.fTime    , "gLClT[LKr_NCandidates]/F");

}

void BuildDPGHitLibrary::StartOfBurstUser(){
}

void BuildDPGHitLibrary::ProcessSpecialTriggerUser(int , unsigned int ){
}

void BuildDPGHitLibrary::Process(int ){

  // Check that this is MC.
  if(!GetIsTree()) return;
  if(!GetWithMC()) return;

  // Check MC event exists and has KineParts
  Event* MC = GetMCEvent();
  if(MC==nullptr)           return;
  if(MC->GetNKineParts()<1) return;

  if(fOutputName.Contains("Halo") || fOutputName.Contains("Pim2")){
    // do nothing.
  }
  else{

    // Check that the first KinePart is the kaon.
    KinePart* k  = MC->GetKinePart(0); // kaon
    if(!k)         return ;
    if(k->GetPDGcode() != 321) std::cout << user() << "Not a kaon!" << std::endl;

    // Check that the kaon decays in the fiducial volume
    Double_t end = k->GetEndPos().Vect().Z();

    // total decay fraction is 0.25033836466
    if(fOutputName.Contains("EDR")){
      // extended decay region (decay fraction 0.13984817541)
      if(end<180000)    return;
      if(!(end<265000)) return;
    }
    else{
      // standard decay region (decay fraction 0.12845428689)
      if(end<102425)    return;
      if(!(end<180000)) return;
    }
  }

  AddDetectorHits<TRecoSpectrometerEvent>(fLib_spectrometer);
  AddDetectorCandidates<TRecoMUV3Event>(fLib_muv3);
  AddDetectorHits<TRecoNewCHODEvent>(fLib_newchod);
  AddDetectorHits<TRecoLAVEvent>(fLib_lav);
  AddDetectorHits<TRecoIRCEvent>(fLib_irc);
  AddDetectorHits<TRecoSACEvent>(fLib_sac);
  AddDetectorCandidates<TRecoLKrEvent>(fLib_lkr);

  // fill the tree with the added hits
  fSpecTree->Fill();
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoSpectrometerEvent *event){
    Int_t hits = event->GetNHits();
    for(Int_t i=0; i<hits; ++i){
        TRecoSpectrometerHit* hit = static_cast<TRecoSpectrometerHit*>(event->GetHit(i));
        if(hit->GetChannelID()<0) std::cout << " WARNING !" << std::endl;
        fLib_spectrometer.fChannelID[i] = hit->GetChannelID();
        fLib_spectrometer.fTime[i] = hit->GetTime();
        fLib_spectrometer.fOther1.f[i] = hit->GetTimeWidth();
    }
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoMUV3Event *event){
    Int_t candidates = event->GetNCandidates();
    for(Int_t i=0; i<candidates; ++i){
      TRecoMUV3Candidate* cand = static_cast<TRecoMUV3Candidate*>(event->GetCandidate(i));
      fLib_muv3.fChannelID[i] = cand->GetTileID();
      fLib_muv3.fTime[i] = cand->GetTime();
    }
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoNewCHODEvent* event) {
    Int_t hits = event->GetNHits();
    for(Int_t i=0; i<hits; ++i){
        TRecoNewCHODHit* chit = static_cast<TRecoNewCHODHit*>(event->GetHit(i));
        fLib_newchod.fChannelID[i] = chit->GetTileID();
        fLib_newchod.fTime[i] = chit->GetTime();
    }
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoLAVEvent* event) {
    Int_t hits = event->GetNHits();
    for (Int_t i = 0; i < hits; ++i) {
        TRecoLAVHit* lav_hit = static_cast<TRecoLAVHit*>(event->GetHit(i));
        fLib_lav.fChannelID[i] = lav_hit->GetChannelID();
        fLib_lav.fTime[i] = lav_hit->GetTime();
        fLib_lav.fOther1.i[i] = lav_hit->GetEdgeMask();
    }
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoIRCEvent* event) {
    Int_t hits = event->GetNHits();
    for (Int_t i = 0; i < hits; ++i) {
        TRecoIRCHit* irc_hit = static_cast<TRecoIRCHit*>(event->GetHit(i));
        fLib_irc.fChannelID[i] = irc_hit->GetChannelID();
        fLib_irc.fTime[i] = irc_hit->GetTime();
        fLib_irc.fOther1.i[i] = irc_hit->GetEdgeMask();
    }
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoSACEvent* event) {
    Int_t hits = event->GetNHits();
    for (Int_t i = 0; i < hits; ++i) {
        TRecoSACHit* sav_hit = static_cast<TRecoSACHit*>(event->GetHit(i));
        fLib_sac.fChannelID[i] = sav_hit->GetChannelID();
        fLib_sac.fTime[i] = sav_hit->GetTime();
        fLib_sac.fOther1.i[i] = sav_hit->GetEdgeMask();
    }
}

void BuildDPGHitLibrary::AddDetectorHit(TRecoLKrEvent* event) {
    Int_t ncandi = event->GetNCandidates();
    for (Int_t icand = 0; icand < ncandi; icand++) {
        TRecoLKrCandidate *lcand = static_cast<TRecoLKrCandidate*>(event->GetCandidate(icand));
        fLib_lkr.fOther1.f[icand] = lcand->GetClusterEnergy();
        fLib_lkr.fOther2.f[icand] = lcand->GetClusterX();
        fLib_lkr.fOther3.f[icand] = lcand->GetClusterY();
        fLib_lkr.fTime[icand] = lcand->GetTime();
    }
}

void BuildDPGHitLibrary::PostProcess(){
}

void BuildDPGHitLibrary::EndOfBurstUser(){
}

void BuildDPGHitLibrary::EndOfRunUser(){
}

void BuildDPGHitLibrary::EndOfJobUser(){

  fSpecTree->Print();

  fRootFile->Write();
  fRootFile->Close();
}

void BuildDPGHitLibrary::DrawPlot(){
}

BuildDPGHitLibrary::~BuildDPGHitLibrary(){
}

