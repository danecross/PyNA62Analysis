// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoCHODEvent.hh"

ClassImp(TRecoCHODEvent)

TRecoCHODEvent::TRecoCHODEvent() :
TRecoVEvent(TRecoCHODCandidate::Class(), TRecoCHODHit::Class()) {
  Clear();
}

TRecoCHODCandidate * TRecoCHODEvent::GetTimeCandidate() {
  Int_t NCandidates = GetNCandidates();
  if (fNTimeCandidates==1)
    return static_cast<TRecoCHODCandidate *>(GetCandidate(NCandidates));
  else
    return nullptr;
}

TRecoCHODEvent::~TRecoCHODEvent(){}

void TRecoCHODEvent::Clear(Option_t* option) {
  TRecoVEvent::Clear(option);
  fNTimeCandidates=0;
  fNQuadrants=0;
}
