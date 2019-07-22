// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TLAVEvent.hh"
#include "TLAVHit.hh"

ClassImp(TLAVEvent)

TLAVEvent::TLAVEvent() : TDetectorVEvent(TLAVHit::Class()){
}

TLAVEvent::~TLAVEvent() {
}

void TLAVEvent::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
