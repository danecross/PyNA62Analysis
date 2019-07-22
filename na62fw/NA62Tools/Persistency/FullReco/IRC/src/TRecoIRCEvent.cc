// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoIRCEvent.hh"

ClassImp(TRecoIRCEvent)

TRecoIRCEvent::TRecoIRCEvent() : TRecoVEvent(TRecoIRCCandidate::Class(), TRecoIRCHit::Class()){
  Clear();
}

TRecoIRCEvent::~TRecoIRCEvent(){
}

void TRecoIRCEvent::Clear(Option_t* option) {
  TRecoVEvent::Clear(option);
}
