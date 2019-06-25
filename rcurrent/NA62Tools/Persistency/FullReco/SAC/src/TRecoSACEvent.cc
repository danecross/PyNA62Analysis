// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoSACEvent.hh"

ClassImp(TRecoSACEvent)

TRecoSACEvent::TRecoSACEvent() : TRecoVEvent(TRecoSACCandidate::Class(), TRecoSACHit::Class()){
  Clear();
}

TRecoSACEvent::~TRecoSACEvent(){
}

void TRecoSACEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
