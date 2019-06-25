// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#include "TDCEvent.hh"

#include "TDCError.hh"
#include "TTimeCluster.hh"

ClassImp(TDCEvent)

TDCEvent::TDCEvent() : TDigiVEvent(){
}

TDCEvent::TDCEvent(TClass * HitClass) : TDigiVEvent(TTimeCluster::Class(), HitClass, TDCError::Class()){
}

TDCVHit* TDCEvent::GetHit(Int_t iHit){
  return static_cast<TDCVHit*>(TDigiVEvent::GetHit(iHit));
}

void TDCEvent::Clear(Option_t * option){
  TDigiVEvent::Clear(option);
}
