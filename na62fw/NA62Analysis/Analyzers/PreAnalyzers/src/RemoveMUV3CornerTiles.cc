// ---------------------------------------------------------------
//
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2017-11-14
//
// ---------------------------------------------------------------

/// \class RemoveMUV3CornerTiles
/// \Brief
/// Analyzer to remove reconstructed hits and candidates from the 
/// four corner tiles of MUV3 which were not used in 2016 but are 
/// included in simulated events.
/// \EndBrief
/// \Detailed
/// This analyzer removes reconstructed MUV3 hits and candidates from 
/// the four corner tiles of MUV3.
///
/// The four corner tiles were not included in the readout during the 2016
/// run, however, they were included during the 2017 run. The corner tiles
/// are therefore included in simulated events, and need to be removed if
/// the simulated events are to be compared to 2016 data.
/// 
/// Digis are not removed.
///
/// This analyzer should be run as a pre-analyzer at the analysis stage.
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "RemoveMUV3CornerTiles.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

#include "TDCEvent.hh"
#include "TMUV3Hit.hh"
#include "TRecoMUV3Hit.hh"

RemoveMUV3CornerTiles::RemoveMUV3CornerTiles(Core::BaseAnalysis *ba) : Analyzer(ba, "RemoveMUV3CornerTiles")
{
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
}

void RemoveMUV3CornerTiles::InitOutput(){
}

void RemoveMUV3CornerTiles::InitHist(){
  if(!GetIsTree()) return; // protect histo mode
  
  BookHisto(new TH1F("RemovedHits", "RemovedHits;Channel Number;N removed", 352, -0.5, 351.5));
  BookHisto(new TH1F("RemovedCandidates", "RemovedCandidates;Tile Number;N removed", 152, -0.5, 151.5));
}

void RemoveMUV3CornerTiles::DefineMCSimple(){
}

void RemoveMUV3CornerTiles::StartOfRunUser(){
}

void RemoveMUV3CornerTiles::StartOfBurstUser(){
}

void RemoveMUV3CornerTiles::ProcessSpecialTriggerUser(int , unsigned int){
}

void RemoveMUV3CornerTiles::Process(int ){
  if(!GetIsTree()) return; // protect histo mode
  
  TRecoMUV3Event* Reco = GetEvent<TRecoMUV3Event>("Reco");
  
  for(int i=0; i<Reco->GetNHits(); ++i){
    TRecoMUV3Hit* hit = static_cast<TRecoMUV3Hit*>(Reco->GetHit(i));
    if(IsCorner(hit->GetChannelID())){
      //remove reco hit
      FillHisto("RemovedHits", hit->GetChannelID());
      Reco->RemoveHit(i);
      i--;
    }
  }

  for(int i=0; i<Reco->GetNCandidates(); ++i){
    TRecoMUV3Candidate* hit = static_cast<TRecoMUV3Candidate*>(Reco->GetCandidate(i));
    if(IsCorner(hit->GetTileID())){
      //remove reco candidate
      FillHisto("RemovedCandidates", hit->GetTileID());
      Reco->RemoveCandidate(i);
      i--;
    }
  }
  
}
  
Bool_t RemoveMUV3CornerTiles::IsCorner(Int_t Channel){
  Int_t tile=Channel%200;
  if(tile==0)        return true;
  else if(tile==11)  return true;
  else if(tile==132) return true;
  else if(tile==143) return true;
  else               return false;
}

void RemoveMUV3CornerTiles::PostProcess(){
}

void RemoveMUV3CornerTiles::EndOfBurstUser(){
}

void RemoveMUV3CornerTiles::EndOfRunUser(){
}

void RemoveMUV3CornerTiles::EndOfJobUser(){
  SaveAllPlots();
}

void RemoveMUV3CornerTiles::DrawPlot(){
}

RemoveMUV3CornerTiles::~RemoveMUV3CornerTiles(){
}
