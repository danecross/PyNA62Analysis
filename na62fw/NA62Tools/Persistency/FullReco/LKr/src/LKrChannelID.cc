// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#include "LKrChannelID.hh"

ClassImp(LKrChannelID)

  LKrChannelID::LKrChannelID(Int_t CPDID, Int_t CPDChannelID) :
    fCPDID(CPDID),
    fCPDChannelID(CPDChannelID),
    fXCellID(0),
    fYCellID(0)
{ //old, it should be removed eventually
}

LKrChannelID::LKrChannelID() :
  fCPDID(-1),
  fCPDChannelID(-1), //old, it should be removed eventually
  fXCellID(-1),
  fYCellID(-1)
{
}

Int_t LKrChannelID::EncodeChannelID() {
  return fXCellID*1000 + fYCellID;
}

LKrChannelID::chIDDecoded LKrChannelID::DecodeChannelID_Static(Int_t ChannelID){
  struct chIDDecoded out;
  out.fXCellID       = ChannelID/1000;
  out.fYCellID       = ChannelID%1000;
  out.fCPDID        = (out.fXCellID/8)*16+(127-out.fYCellID)/8;
  out.fCPDChannelID = (out.fXCellID%8)*8 +(127-out.fYCellID)%8;
  return out;
}

void LKrChannelID::DecodeChannelID(Int_t ChannelID) {
  struct chIDDecoded chID = DecodeChannelID_Static(ChannelID);
  fXCellID      = chID.fXCellID;
  fYCellID      = chID.fYCellID;
  fCPDID        = chID.fCPDID;
  fCPDChannelID = chID.fCPDChannelID;
}

void LKrChannelID::Clear(Option_t* /*option*/){
  fCPDID = fCPDChannelID = -1; //old, it should be removed eventually
  fXCellID = fYCellID = -1;
}
