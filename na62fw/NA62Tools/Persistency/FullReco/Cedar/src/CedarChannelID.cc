// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2014-04-10
//
// ---------------------------------------------------------------

#include "CedarChannelID.hh"

ClassImp(CedarChannelID)

CedarChannelID::CedarChannelID() {
  fSectorID = fRowID = fConeID = -1;
}

Int_t CedarChannelID::EncodeChannelID() {
  return fSectorID*100 + fRowID*10 + fConeID;
}

CedarChannelID::chIDDecoded CedarChannelID::DecodeChannelID_Static(int ChannelID){
  struct chIDDecoded CCID;
  if(ChannelID==-1){ CCID.SectorID = CCID.RowID =  CCID.ConeID = -1; }
  else{
    CCID.SectorID = ChannelID/100;
    CCID.RowID = (ChannelID-CCID.SectorID*100)/10;;
    CCID.ConeID = (ChannelID-CCID.SectorID*100-CCID.RowID*10);
  }
  return CCID; // Return filled Struct 
}

void CedarChannelID::DecodeChannelID(int ChannelID){
  struct chIDDecoded CCID = DecodeChannelID_Static(ChannelID);
  fSectorID = CCID.SectorID;
  fRowID = CCID.RowID;
  fConeID = CCID.ConeID; 
}

void CedarChannelID::Clear(Option_t* /*option*/) {
  fSectorID = fRowID = fConeID = -1;
}
