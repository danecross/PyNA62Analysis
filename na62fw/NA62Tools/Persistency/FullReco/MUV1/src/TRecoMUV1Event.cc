// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#include "TRecoMUV1Event.hh"

ClassImp(TRecoMUV1Event)

TRecoMUV1Event::TRecoMUV1Event() : TRecoVEvent(TRecoMUV1Candidate::Class(), TRecoMUV1Hit::Class()){
  Clear();
}

TRecoMUV1Event::~TRecoMUV1Event(){
}

void TRecoMUV1Event::Clear(Option_t * option){
  TRecoVEvent::Clear(option);
}
