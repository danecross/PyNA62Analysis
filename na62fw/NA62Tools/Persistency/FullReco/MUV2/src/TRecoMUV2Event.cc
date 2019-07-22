// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#include "TRecoMUV2Event.hh"

ClassImp(TRecoMUV2Event)

TRecoMUV2Event::TRecoMUV2Event() : TRecoVEvent(TRecoMUV2Candidate::Class(), TRecoMUV2Hit::Class()){
  Clear();
}

TRecoMUV2Event::~TRecoMUV2Event(){
}

void TRecoMUV2Event::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
