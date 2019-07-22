// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoCHANTIEvent.hh"

ClassImp(TRecoCHANTIEvent)

TRecoCHANTIEvent::TRecoCHANTIEvent() : TRecoVEvent(TRecoCHANTICandidate::Class(), TRecoCHANTIHit::Class()){
  Clear();
}

TRecoCHANTIEvent::~TRecoCHANTIEvent(){
}

void TRecoCHANTIEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
