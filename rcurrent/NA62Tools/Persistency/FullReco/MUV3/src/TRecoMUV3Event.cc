// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoMUV3Event.hh"

ClassImp(TRecoMUV3Event)

TRecoMUV3Event::TRecoMUV3Event() : TRecoVEvent(TRecoMUV3Candidate::Class(), TRecoMUV3Hit::Class()) {
  Clear();
}

TRecoMUV3Event::~TRecoMUV3Event() {}

void TRecoMUV3Event::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
