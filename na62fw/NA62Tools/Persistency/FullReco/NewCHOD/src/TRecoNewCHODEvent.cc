// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
#include "TRecoNewCHODEvent.hh"

ClassImp(TRecoNewCHODEvent)

TRecoNewCHODEvent::TRecoNewCHODEvent() :
TRecoVEvent(TRecoNewCHODCandidate::Class(), TRecoNewCHODHit::Class()) {
  Clear();
}

TRecoNewCHODEvent::~TRecoNewCHODEvent() {}

void TRecoNewCHODEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
