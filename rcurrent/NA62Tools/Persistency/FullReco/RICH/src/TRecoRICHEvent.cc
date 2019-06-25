// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoRICHEvent.hh"

ClassImp(TRecoRICHEvent)

TRecoRICHEvent::TRecoRICHEvent() : TRecoVEvent(TRecoRICHCandidate::Class(), TRecoRICHHit::Class()) {}

TRecoRICHEvent::~TRecoRICHEvent() {}

void TRecoRICHEvent::Clear(Option_t * option){
  TRecoVEvent::Clear(option);
  fNTimeCandidates = 0;
  fNPMTimeCandidates = 0;
  fNSCTimeCandidates = 0;
  fNRingCandidates = 0;
}

TRecoRICHCandidate * TRecoRICHEvent::GetTimeCandidate(Int_t iCandidate){
  if(iCandidate < fNTimeCandidates)
    return static_cast<TRecoRICHCandidate *>(GetCandidate(iCandidate));
  else
    return nullptr;
}

TRecoRICHCandidate * TRecoRICHEvent::GetSCTimeCandidate(Int_t iCandidate){
  if(iCandidate < fNSCTimeCandidates)
    return static_cast<TRecoRICHCandidate *>(GetCandidate(iCandidate));
  else
    return nullptr;
}

TRecoRICHCandidate * TRecoRICHEvent::GetPMTimeCandidate(Int_t iCandidate){
  if(iCandidate < fNPMTimeCandidates)
    return static_cast<TRecoRICHCandidate *>(GetCandidate(fNSCTimeCandidates + iCandidate));
  else 
    return nullptr;
}

TRecoRICHCandidate * TRecoRICHEvent::GetRingCandidate(Int_t iCandidate){
  if(iCandidate < fNRingCandidates)
    return static_cast<TRecoRICHCandidate *>(GetCandidate(fNTimeCandidates + iCandidate));
  else 
    return nullptr;
}
