// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoLAVEvent.hh"

ClassImp(TRecoLAVEvent)

TRecoLAVEvent::TRecoLAVEvent() : TRecoVEvent(TRecoLAVCandidate::Class(), TRecoLAVHit::Class()){
  Clear();
}

TRecoLAVEvent::~TRecoLAVEvent(){
}

void TRecoLAVEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
