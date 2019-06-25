//----------------------------------------------------------------
// History:
//
// Created by Antonio Cassese (Antonio.Cassese@cern.ch) 2012-10-19
// Updated: E Goudzovski (eg@hep.ph.bham.ac.uk)         2015-11-02
//
//----------------------------------------------------------------

#include "TMUV3Digi.hh"

ClassImp(TMUV3Digi)

TMUV3Digi::TMUV3Digi() : TDCVHit(), MUV3ChannelID() {}

void TMUV3Digi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  MUV3ChannelID::Clear(option);
}

Int_t TMUV3Digi::EncodeChannelID() {
  fChannelID = MUV3ChannelID::EncodeChannelID();
  return fChannelID;
}

void TMUV3Digi::DecodeChannelID() {
  MUV3ChannelID::DecodeChannelID(fChannelID);
}
