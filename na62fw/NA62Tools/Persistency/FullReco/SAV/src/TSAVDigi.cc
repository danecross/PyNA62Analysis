// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
// --------------------------------------------------------------
#include "TSAVDigi.hh"

ClassImp(TSAVDigi)
  
TSAVDigi::TSAVDigi() : FADCVHit() , SAVChannelID() {
}

Int_t TSAVDigi::EncodeChannelID() {
  fChannelID =  SAVChannelID::EncodeChannelID();
  return fChannelID;
}

void TSAVDigi::DecodeChannelID() {
  SAVChannelID::DecodeChannelID(fChannelID);
}

void TSAVDigi::Clear(Option_t* option){
  FADCVHit::Clear(option);
  SAVChannelID::Clear(option);
}

