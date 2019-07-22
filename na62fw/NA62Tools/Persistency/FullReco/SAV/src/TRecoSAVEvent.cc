// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#include "TRecoSAVEvent.hh"

ClassImp(TRecoSAVEvent)

TRecoSAVEvent::TRecoSAVEvent() : TRecoVEvent(TRecoSAVCandidate::Class(), TRecoSAVHit::Class()){
  Clear();
}

TRecoSAVEvent::~TRecoSAVEvent(){}

void TRecoSAVEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
