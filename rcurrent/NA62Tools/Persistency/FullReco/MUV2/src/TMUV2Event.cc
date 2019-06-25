// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#include "TMUV2Event.hh"
#include "TMUV2Hit.hh"
#include "TMUV2Digi.hh"

ClassImp(TMUV2Event)

TMUV2Event::TMUV2Event() : TDetectorVEvent(TMUV2Hit::Class()){
}

TMUV2Event::~TMUV2Event() {
}

void TMUV2Event::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
