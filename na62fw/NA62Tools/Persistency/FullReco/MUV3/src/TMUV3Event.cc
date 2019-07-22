// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TMUV3Event.hh"
#include "TMUV3Hit.hh"

ClassImp(TMUV3Event)

TMUV3Event::TMUV3Event() : TDetectorVEvent(TMUV3Hit::Class()) {}

TMUV3Event::~TMUV3Event() {}

void TMUV3Event::Clear(Option_t* option) {
  TDetectorVEvent::Clear(option);
}
