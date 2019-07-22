// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#include "TMUV1Event.hh"
#include "TMUV1Hit.hh"

ClassImp(TMUV1Event)

TMUV1Event::TMUV1Event() : TDetectorVEvent (TMUV1Hit::Class()){
}

TMUV1Event::~TMUV1Event() {
}

void TMUV1Event::Clear(Option_t * option){
  TDetectorVEvent::Clear(option);
}
