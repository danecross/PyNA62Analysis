// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TSACEvent.hh"
#include "TSACHit.hh"

ClassImp(TSACEvent)

TSACEvent::TSACEvent() : TDetectorVEvent(TSACHit::Class()){
}

TSACEvent::~TSACEvent() {
}

void TSACEvent::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}

