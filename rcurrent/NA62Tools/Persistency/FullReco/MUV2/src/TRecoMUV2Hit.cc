// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#include "TRecoMUV2Hit.hh"

ClassImp(TRecoMUV2Hit)

TRecoMUV2Hit::TRecoMUV2Hit() : TRecoVHit() , MUV2ChannelID(){
}

void TRecoMUV2Hit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  MUV2ChannelID::Clear(option);
}
