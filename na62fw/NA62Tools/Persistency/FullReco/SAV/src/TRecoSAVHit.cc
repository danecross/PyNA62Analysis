// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#include "TRecoSAVHit.hh"

ClassImp(TRecoSAVHit)

TRecoSAVHit::TRecoSAVHit() : TRecoVHit() , SAVChannelID(){
}

void TRecoSAVHit::DecodeChannelID (Int_t ChannelID){
  fChannelID = ChannelID;
  SAVChannelID::DecodeChannelID(fChannelID);
}

void TRecoSAVHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  SAVChannelID::Clear(option);
}
