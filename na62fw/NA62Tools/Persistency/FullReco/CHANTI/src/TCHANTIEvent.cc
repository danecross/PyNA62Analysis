// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TCHANTIEvent.hh"
#include "TCHANTIHit.hh"

ClassImp(TCHANTIEvent)

TCHANTIEvent::TCHANTIEvent() : TDetectorVEvent(TCHANTIHit::Class()){
}

TCHANTIEvent::~TCHANTIEvent() {
}

void TCHANTIEvent::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
