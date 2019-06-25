// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-10-10
//
// ---------------------------------------------------------------

#include "MUV3ChannelID.hh"

ClassImp(MUV3ChannelID)

MUV3ChannelID::MUV3ChannelID() {
  fTileID = 0;
}

void MUV3ChannelID::Clear(Option_t* /*option*/){
  fTileID = 0;
}

Int_t MUV3ChannelID::EncodeChannelID() {
  return fTileID + 200*fIsHigh;
}

////////////////////////////////////////////////////////////////////////////////////////////
// PMT numbering convention, except modules 145 and 150:
// (top PMT ID) = (pad ID), (bottom PMT ID) = (pad ID) + 200.
// PMT numbering convention for modules 145 and 150 (with horizontally aligned PMTs):
// PMTs 145, 150 are on the Jura side (positive X), PMTs 345 and 350 are on the Saleve side.

void MUV3ChannelID::DecodeChannelID(Int_t ChannelID) {
  fTileID      = ChannelID%200;
  fIsHigh      = (ChannelID>=200);
  fIsInnerTile = (fTileID>=144);
  fPMTLocation = fIsHigh ? kBottom : kTop;
  if (ChannelID==145 || ChannelID==150) fPMTLocation = kJura;
  if (ChannelID==345 || ChannelID==350) fPMTLocation = kSaleve;
}
