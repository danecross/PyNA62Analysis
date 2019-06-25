// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoLKrEvent.hh"

ClassImp(TRecoLKrEvent)

TRecoLKrEvent::TRecoLKrEvent() : TRecoVEvent(TRecoLKrCandidate::Class(), TRecoLKrHit::Class()){
  Clear();
}

TRecoLKrEvent::~TRecoLKrEvent() {}

void TRecoLKrEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
  fEnergyTotal = 0.;
  fRecFlag = 0.;
}
