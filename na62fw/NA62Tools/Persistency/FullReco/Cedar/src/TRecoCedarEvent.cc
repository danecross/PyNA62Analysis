// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#include "TRecoCedarEvent.hh"

ClassImp(TRecoCedarEvent)

TRecoCedarEvent::TRecoCedarEvent() : TRecoVEvent(TRecoCedarCandidate::Class(), TRecoCedarHit::Class()) {
  Clear();
}

TRecoCedarEvent::~TRecoCedarEvent() {
}

void TRecoCedarEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
