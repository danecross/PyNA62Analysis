// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
#include "TNewCHODEvent.hh"
#include "TNewCHODHit.hh"

ClassImp(TNewCHODEvent)

TNewCHODEvent::TNewCHODEvent() : TDetectorVEvent(TNewCHODHit::Class()){
}

TNewCHODEvent::~TNewCHODEvent() {
}

void TNewCHODEvent::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
