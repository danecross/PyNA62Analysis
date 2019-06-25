// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//            Evelina Gersabeck (Evelina.Gersabeck@cern.ch)
// --------------------------------------------------------------
#include "TLKrDigi.hh"
#include "FADCVHit.hh"
#include "LKrChannelID.hh"

ClassImp(TLKrDigi)

Int_t TLKrDigi::EncodeChannelID() {
  fChannelID =  LKrChannelID::EncodeChannelID();
  return fChannelID;
}

void TLKrDigi::DecodeChannelID() {
  LKrChannelID::DecodeChannelID(fChannelID);
}

void TLKrDigi::Clear(Option_t* option){
  FADCVHit::Clear(option);
  LKrChannelID::Clear(option);
}
