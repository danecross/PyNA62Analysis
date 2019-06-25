// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TIRCEvent.hh"
#include "TIRCHit.hh"

ClassImp(TIRCEvent)

TIRCEvent::TIRCEvent() : TDetectorVEvent(TIRCHit::Class()){
}

TIRCEvent::~TIRCEvent() {
}

void TIRCEvent::Clear(Option_t* option) {
  TDetectorVEvent::Clear(option);
}
