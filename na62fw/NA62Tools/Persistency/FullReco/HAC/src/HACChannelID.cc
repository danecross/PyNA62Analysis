// ---------------------------------------------------------
// History:
// Modified by Mario Bragadireanu (mario.bragadireanu@cern.ch) 2016-08-31
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-22
//
// ---------------------------------------------------------

#include "HACChannelID.hh"

ClassImp(HACChannelID)

HACChannelID::HACChannelID() {
  fSiPMID = fModuleID = -1;
}

HACChannelID::HACChannelID(Int_t ChID) {
  DecodeChannelID(ChID);
}

Int_t HACChannelID::EncodeChannelID() {
  return fModuleID*10+fSiPMID;
}

HACChannelID::chIDDecoded HACChannelID::DecodeChannelID_Static(Int_t ChannelID) {
  chIDDecoded ret;
  if(ChannelID==-1) ret.fSiPMID = ret.fModuleID = -1;
  else {
    ret.fModuleID = (int)(ChannelID/10); //See HACReconstruction.cc for more details
    ret.fSiPMID   =(int)(ChannelID%10);  //See HACReconstruction.cc
  }
  return ret;
}

void HACChannelID::DecodeChannelID(Int_t ChannelID) {
  chIDDecoded ret = DecodeChannelID_Static(ChannelID);
  fModuleID = ret.fModuleID; //See HACReconstruction.cc for more details
  fSiPMID   = ret.fSiPMID;  //See HACReconstruction.cc
}

void HACChannelID::Clear(Option_t* /*option*/) {
  fSiPMID = fModuleID = -1;
}
