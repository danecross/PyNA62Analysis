// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TCHODEvent.hh"
#include "TCHODHit.hh"

ClassImp(TCHODEvent)

TCHODEvent::TCHODEvent() : TDetectorVEvent(TCHODHit::Class()){
}

TCHODEvent::~TCHODEvent() {
}

void TCHODEvent::Clear(Option_t* option) {
  TDetectorVEvent::Clear(option);
}
