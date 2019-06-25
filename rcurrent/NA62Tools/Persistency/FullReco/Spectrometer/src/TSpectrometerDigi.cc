// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#include "TSpectrometerDigi.hh"
ClassImp(TSpectrometerDigi)

Int_t TSpectrometerDigi::EncodeChannelID() {
  //add here the channel encoding
  fChannelID =  SpectrometerChannelID::EncodeChannelID();
  return fChannelID;
}

void TSpectrometerDigi::DecodeChannelID() {
  SpectrometerChannelID::DecodeChannelID(fChannelID);
  //add here the channel decoding
}

void TSpectrometerDigi::Clear(Option_t* option) {
  SRBVHit::Clear(option);
  SpectrometerChannelID::Clear(option);
}
