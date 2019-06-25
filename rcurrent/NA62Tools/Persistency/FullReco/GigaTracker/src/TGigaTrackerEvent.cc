// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-23
//
// --------------------------------------------------------------
#include "TGigaTrackerEvent.hh"
#include "TGigaTrackerHit.hh"
//#include "TGigaTrackerDigi.hh"

ClassImp(TGigaTrackerEvent)

TGigaTrackerEvent::TGigaTrackerEvent() : TDetectorVEvent(TGigaTrackerHit::Class()){
}

//TGigaTrackerEvent::TGigaTrackerEvent() : TDetectorVEvent(TGigaTrackerDigi::Class()){
//}


TGigaTrackerEvent::~TGigaTrackerEvent() {
}

void TGigaTrackerEvent::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
