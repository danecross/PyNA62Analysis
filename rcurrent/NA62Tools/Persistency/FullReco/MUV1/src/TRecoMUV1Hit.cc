// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#include "TRecoMUV1Hit.hh"

ClassImp(TRecoMUV1Hit)

TRecoMUV1Hit::TRecoMUV1Hit() : TRecoVHit() , MUV1ChannelID(){

}

void TRecoMUV1Hit::Clear(Option_t * option){
  TRecoVHit::Clear(option);
  MUV1ChannelID::Clear(option);
}
