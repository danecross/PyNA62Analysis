// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#include "NewCHODChannelID.hh"

ClassImp(NewCHODChannelID)

NewCHODChannelID::NewCHODChannelID() {
  fTileID = fSeqTileID = fQuadrantID = -1;
}

void NewCHODChannelID::Clear(Option_t*) {
  fTileID = fSeqTileID = fQuadrantID = -1;
}

Int_t NewCHODChannelID::EncodeChannelID() {
  return fTileID;
}

NewCHODChannelID::chIDDecoded NewCHODChannelID::DecodeChannelID_Static(Int_t ChannelID) {
  chIDDecoded ret;
  ret.fTileID     = 100*(ChannelID/100) + ChannelID%50;
  ret.fQuadrantID = ChannelID/100;
  ret.fSeqTileID  = 38*(ret.fQuadrantID-1) + ret.fTileID%100 - 1;
  return ret;
}

void NewCHODChannelID::DecodeChannelID(Int_t ChannelID) {
  chIDDecoded ret = DecodeChannelID_Static(ChannelID);
  fTileID     = ret.fTileID;
  fQuadrantID = ret.fQuadrantID;
  fSeqTileID  = ret.fSeqTileID;
}
