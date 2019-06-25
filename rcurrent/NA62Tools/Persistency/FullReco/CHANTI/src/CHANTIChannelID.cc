// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-01-31
//
// --------------------------------------------------------------
#include "CHANTIChannelID.hh"

ClassImp(CHANTIChannelID)

CHANTIChannelID::CHANTIChannelID() {
  fPlaneID = -1;
  fRingType = -1;
  fSideID = -1;
  fBarID = -1;
}

Int_t CHANTIChannelID::EncodeChannelID(){
  Int_t AbsfBarID = (fBarID>0)? fBarID : -fBarID;
  return (fPlaneID + 1)*100000 + fRingType*10000 + fSideID*1000 + AbsfBarID*10;
}

CHANTIChannelID::chIDDecoded CHANTIChannelID::DecodeChannelID_Static(Int_t ChannelID){
  chIDDecoded ret;
  ret.fPlaneID = ChannelID/100000 - 1;
  ret.fRingType = (ChannelID%100000)/10000 ;
  ret.fSideID = (ChannelID%10000)/1000;
  ret.fBarID  = (1-2*ret.fSideID)*(ChannelID%1000)/10;

  return ret;
}

void CHANTIChannelID::DecodeChannelID(Int_t ChannelID){
  chIDDecoded ret = DecodeChannelID_Static(ChannelID);

  fPlaneID = ret.fPlaneID;
  fRingType = ret.fRingType;
  fSideID = ret.fSideID;
  fBarID  = ret.fBarID;
}

void CHANTIChannelID::Clear(Option_t* /*option*/) {
  fPlaneID = -1;
  fRingType = -1;
  fSideID = -1;
  fBarID = -1;
}
