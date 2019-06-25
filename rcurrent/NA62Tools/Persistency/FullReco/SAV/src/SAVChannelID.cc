//
//  SAVChannelID.cc
//
//
//  Created by letizia peruzzo on 02/06/2016.
//
//

#include "SAVChannelID.hh"
#include "Riostream.h"

SAVChannelID :: SAVChannelID() :
  fDetectorID(-1),
  fDetectorChannel(-1)
{
}

SAVChannelID :: SAVChannelID(Int_t ChannelID){

  DecodeChannelID(ChannelID);

}

SAVChannelID :: ~ SAVChannelID(){
}


Int_t SAVChannelID :: EncodeChannelID(){

  Int_t ChannelID = fDetectorID*10 + fDetectorChannel;

  return ChannelID;
}

SAVChannelID::chIDDecoded SAVChannelID::DecodeChannelID_Static(Int_t ChannelID){
    struct chIDDecoded ret;
    ret.fDetectorID = ChannelID/10;
    ret.fDetectorChannel = ChannelID%10;
    return ret;
}

void SAVChannelID :: DecodeChannelID(Int_t ChannelID){
  struct chIDDecoded ret = DecodeChannelID_Static(ChannelID);
  fDetectorID = ret.fDetectorID;
  fDetectorChannel = ret.fDetectorChannel;
}

TVector2 SAVChannelID :: GetChannelPosition(){
  
  Double_t coordinate[2] = {50.,70.};
  
  if (fDetectorChannel == 1) return  TVector2(-coordinate[fDetectorID],coordinate[fDetectorID]);
  if (fDetectorChannel == 2) return  TVector2(coordinate[fDetectorID],coordinate[fDetectorID]);
  if (fDetectorChannel == 3) return  TVector2(coordinate[fDetectorID],-coordinate[fDetectorID]);
  if (fDetectorChannel == 4) return  TVector2(-coordinate[fDetectorID],-coordinate[fDetectorID]);

  return TVector2(0.,0.);
}

void SAVChannelID::Clear(Option_t* /*option*/){
  fDetectorID=-1;
  fDetectorChannel=-1;
}
