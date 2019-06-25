/*
 * SACChannelID.cc
 *
 *  Created on: Sep 25, 2015
 *      Author: veni
 */

#include "SACChannelID.hh"
#include <iostream>
ClassImp(SACChannelID)

SACChannelID::SACChannelID() {
  fPMTID = -1;
}

SACChannelID::SACChannelID(Int_t ChID) {
  DecodeChannelID(ChID);
}

Int_t SACChannelID::EncodeChannelID() {
  return fPMTID;
}

Int_t SACChannelID::DecodeChannelID_Static(Int_t ChannelID) {
  return ChannelID;
}

void SACChannelID::DecodeChannelID(Int_t ChannelID) {
  fPMTID = DecodeChannelID_Static(ChannelID);
}

void SACChannelID::Clear(Option_t* /*option*/){
  fPMTID = -1;
}
