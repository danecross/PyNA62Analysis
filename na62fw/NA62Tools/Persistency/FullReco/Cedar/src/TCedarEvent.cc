// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TCedarEvent.hh"
#include "TCedarHit.hh"

ClassImp(TCedarEvent)

TCedarEvent::TCedarEvent() : TDetectorVEvent(TCedarHit::Class()){
}

TCedarEvent::~TCedarEvent() {
}

void TCedarEvent::Clear(Option_t * option){
  TDetectorVEvent::Clear(option);
}
