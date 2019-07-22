// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-22
// Updated by E Goudzovski (eg@hep.ph.bham.ac.uk) 2016-05-11
//
// ---------------------------------------------------------

#include "MUV0ChannelID.hh"

ClassImp(MUV0ChannelID)

MUV0ChannelID::MUV0ChannelID() {
  fTileID = 0;
}

Int_t MUV0ChannelID::EncodeChannelID() {
  return fTileID;
}

Int_t MUV0ChannelID::DecodeChannelID_Static(Int_t ChannelID) {
  return ChannelID;
}

void MUV0ChannelID::DecodeChannelID(Int_t ChannelID) {
  fTileID = DecodeChannelID_Static(ChannelID);
}

void MUV0ChannelID::Clear(Option_t* /*option*/){
  fTileID = 0;
}
