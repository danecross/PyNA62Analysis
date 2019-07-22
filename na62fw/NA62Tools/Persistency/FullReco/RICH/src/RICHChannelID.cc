// --------------------------------------------------------------
// History:
//
// Created by Francesca Bucci (francesca.bucci@cern.ch) 2014-06-19
//
// -------------------------------------------------------------- 
/// \class RICHChannelID
/// \Brief 
/// Geo index of RICH channels\n
/// \EndBrief
///
/// \Detailed
/// It allows to know the geographical position of the hit pmt.
/// \n
/// \EndDetailed
#include "RICHChannelID.hh"

ClassImp(RICHChannelID)

RICHChannelID::RICHChannelID() {
  fDiskID = -1;
  fUpDownDiskID = -1;
  fSuperCellID = -1;
  fOrSuperCellID = -1;
  fPmtID = -1;
}

void RICHChannelID::Clear(Option_t* /*option*/) {
  fDiskID = -1;
  fUpDownDiskID = -1;
  fSuperCellID = -1;
  fOrSuperCellID = -1;
  fPmtID = -1;
}

Int_t RICHChannelID::EncodeChannelID(){
  
  return fDiskID*100000 + fUpDownDiskID*10000 + fSuperCellID*100 + fOrSuperCellID*10 + fPmtID;
}

RICHChannelID::chIDDecoded RICHChannelID::DecodeChannelID_Static(int ChannelID){
  struct chIDDecoded CCID;
  if(ChannelID==-1){ CCID.DiskID = CCID.UpDownDiskID =  CCID.SuperCellID  =CCID.OrSuperCellID = CCID.PmtID =-1; }
  else{
  CCID.DiskID = ChannelID/100000;
  CCID.UpDownDiskID = (ChannelID%100000)/10000;
  CCID.SuperCellID = (ChannelID%10000)/100;
  CCID.OrSuperCellID = (ChannelID%100)/10;
  CCID.PmtID = ChannelID%10; 
  }
  return CCID; // Return filled Struct
}

void RICHChannelID::DecodeChannelID(Int_t ChannelID){
  struct chIDDecoded CCID = DecodeChannelID_Static(ChannelID);
  fDiskID =CCID.DiskID;
  fUpDownDiskID = CCID.UpDownDiskID;
  fSuperCellID = CCID.SuperCellID;
  fOrSuperCellID = CCID.OrSuperCellID;
  fPmtID = CCID.PmtID; 
}

Int_t RICHChannelID::GetChannelSeqID(){
   EncodeChannelID();	
   if(fOrSuperCellID<1){   
     return fSuperCellID*8+fPmtID+fUpDownDiskID*61*8+fDiskID*61*8*2;  
   }else{
     return (61*8*2*2+fSuperCellID+fUpDownDiskID*61+fDiskID*61*2);
   }

}

