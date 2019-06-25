// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#include "TSpecialTriggerEvent.hh"

#include "Riostream.h"

ClassImp(TSpecialTriggerEvent)

TSpecialTriggerEvent::TSpecialTriggerEvent() : TDetectorVEvent(){
}

TSpecialTriggerEvent::TSpecialTriggerEvent(TClass * Class) : TDetectorVEvent(Class){
}

TSpecialTrigger * TSpecialTriggerEvent::AddSpecialTrigger(){
    return reinterpret_cast<TSpecialTrigger *>(AddHit());
}

TSpecialTrigger * TSpecialTriggerEvent::LastSpecialTrigger(){
    return reinterpret_cast<TSpecialTrigger *>(GetLastHit());
}

TSpecialTriggerEvent::~TSpecialTriggerEvent(){
  Clear();
}

void TSpecialTriggerEvent::Clear(Option_t * option){
    TDetectorVEvent::Clear(option);
}
