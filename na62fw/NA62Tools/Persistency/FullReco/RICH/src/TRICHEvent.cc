// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TRICHEvent.hh"
#include "TRICHHit.hh"

ClassImp(TRICHEvent)

TRICHEvent::TRICHEvent() : TDetectorVEvent(TRICHHit::Class()){
  Clear();
}

TRICHEvent::~TRICHEvent() {
}

void TRICHEvent::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
