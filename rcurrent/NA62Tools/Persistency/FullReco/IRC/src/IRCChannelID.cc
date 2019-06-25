/*
 * IRCChannelID.cc
 *
 *  Created on: Sep 26, 2015
 *      Author: veni
 */

#include "IRCChannelID.hh"
#include <iostream>
ClassImp(IRCChannelID)

IRCChannelID::IRCChannelID() {
  fPMTID = -1;
}

IRCChannelID::IRCChannelID(Int_t ChID) {
  DecodeChannelID(ChID);
}

Int_t IRCChannelID::EncodeChannelID() {
  return fPMTID;
}

Int_t IRCChannelID::DecodeChannelID_Static(Int_t ChannelID) {
  return ChannelID;
}

void IRCChannelID::DecodeChannelID(Int_t ChannelID) {
  fPMTID = DecodeChannelID_Static(ChannelID);
}

void IRCChannelID::Clear(Option_t* /*option*/) {
  fPMTID = -1;
}
