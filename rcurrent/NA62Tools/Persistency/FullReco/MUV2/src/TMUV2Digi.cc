// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
// Copied and modified from GTK by Mario Vormstein (mario.vormstein@cern.ch) 2012-03-07
// --------------------------------------------------------------
#include "TMUV2Digi.hh"

ClassImp(TMUV2Digi)
  
TMUV2Digi::TMUV2Digi() : FADCVHit() , MUV2ChannelID() {
}


Int_t TMUV2Digi::EncodeChannelID() {
  fChannelID =  MUV2ChannelID::EncodeChannelID();
  return fChannelID;
}

void TMUV2Digi::DecodeChannelID() {
  MUV2ChannelID::DecodeChannelID(fChannelID);
}

void TMUV2Digi::Clear(Option_t* option){
  FADCVHit::Clear(option);
  MUV2ChannelID::Clear(option);
}
