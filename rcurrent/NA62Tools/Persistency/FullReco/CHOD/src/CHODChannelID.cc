// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------

#include "CHODChannelID.hh"

ClassImp(CHODChannelID)

CHODChannelID::CHODChannelID() {
  fPlaneID = fQuadrantID = fCounterID = -1;
}

Int_t CHODChannelID::EncodeChannelID() {
  return 64*fPlaneID + 16*fQuadrantID + fCounterID;
}

CHODChannelID::chIDDecoded CHODChannelID::DecodeChannelID_Static(int ChannelID){
  struct chIDDecoded CCID;
  if(ChannelID==-1){ CCID.PlaneID = CCID.QuadrantID =  CCID.CounterID = -1; }
  else{
    CCID.PlaneID = (ChannelID<64) ? kVerticalPlane : kHorizontalPlane;
    CCID.QuadrantID = (ChannelID%64)/16;
    CCID.CounterID = ChannelID%16;
  }
  return CCID; // Return filled Struct
}

void CHODChannelID::DecodeChannelID(Int_t ChannelID) {
  struct chIDDecoded CCID = DecodeChannelID_Static(ChannelID);
  fPlaneID = CCID.PlaneID;
  fQuadrantID = CCID.QuadrantID;
  fCounterID = CCID.CounterID;
}

void CHODChannelID::Clear(Option_t* /*option*/) {
  fPlaneID = fQuadrantID = fCounterID = -1;
}
