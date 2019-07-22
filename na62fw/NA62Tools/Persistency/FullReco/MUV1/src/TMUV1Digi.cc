// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
// Copied and modified from GTK by Mario Vormstein (mario.vormstein@cern.ch) 2012-03-07
// --------------------------------------------------------------
#include "TMUV1Digi.hh"

ClassImp(TMUV1Digi)

TMUV1Digi::TMUV1Digi() : FADCVHit() , MUV1ChannelID() {
}

Int_t TMUV1Digi::EncodeChannelID() {
  fChannelID =  MUV1ChannelID::EncodeChannelID();
  return fChannelID;
}

void TMUV1Digi::DecodeChannelID() {
  MUV1ChannelID::DecodeChannelID(fChannelID);
}

void TMUV1Digi::Clear(Option_t * option){
  FADCVHit::Clear(option);
  MUV1ChannelID::Clear(option);
}

