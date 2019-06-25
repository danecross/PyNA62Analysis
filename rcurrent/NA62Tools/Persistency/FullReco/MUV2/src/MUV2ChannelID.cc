//
//  MUV2ChannelID.cc
//
//
//  Created by riccardo aliberti on 10/10/14.
//
//

#include "MUV2ChannelID.hh"
#include "Riostream.h"

MUV2ChannelID :: MUV2ChannelID(){
}

MUV2ChannelID :: MUV2ChannelID(Int_t ChannelID){
  DecodeChannelID(ChannelID);
}

MUV2ChannelID :: ~ MUV2ChannelID(){
}

void MUV2ChannelID::Clear(Option_t* /*option*/){
}

Int_t MUV2ChannelID :: EncodeChannelID(){

  Int_t ChannelID = fSide * 50 + 100;

  ChannelID += fScintillatorNumber;

  return ChannelID;
}

MUV2ChannelID::chIDDecoded MUV2ChannelID::DecodeChannelID_Static(Int_t ChannelID){
  chIDDecoded ret;
  if (ChannelID>100 && ChannelID<300){
    ret.fScintillatorNumber = ChannelID%50;
    ret.fSide = (ChannelID-100)/50;
  }
  else{
    std::cout <<"MUV2ChannelID::DecodeChannelID("<<ChannelID<<") no matching quadrant!"<<std::endl;
    ret.fSide = -1;
    ret.fScintillatorNumber = -1;
  }
  return ret;
}

void MUV2ChannelID :: DecodeChannelID(Int_t ChannelID){
  chIDDecoded ret = DecodeChannelID_Static(ChannelID);
  fScintillatorNumber = ret.fScintillatorNumber;
  fSide = ret.fSide;
}


Int_t MUV2ChannelID :: GetPlane (){

  if (fSide%2==0) return kHorizontalPlane;
  else return kVerticalPlane;

}

Int_t MUV2ChannelID :: GetQuadrant (){
  Int_t ChannelID = 100 + 50*fSide + fScintillatorNumber;
  Int_t fQuadrant = -1;

  if		((ChannelID > 100 && ChannelID < 112) || (ChannelID>150 && ChannelID<162)) fQuadrant = 1;
  else if ((ChannelID > 111 && ChannelID < 123) || (ChannelID>250 && ChannelID<262)) fQuadrant = 2;
  else if ((ChannelID > 211 && ChannelID < 223) || (ChannelID>261 && ChannelID<273)) fQuadrant = 3;
  else if ((ChannelID > 200 && ChannelID < 212) || (ChannelID>161 && ChannelID<173)) fQuadrant = 4;

  return fQuadrant;

}


Double_t MUV2ChannelID :: GetScintillatorPosition (){

  Double_t position = -1238.5;

  if (fScintillatorNumber<11) position += (fScintillatorNumber-1) * 119;
  else if (fScintillatorNumber>12) position += (22-fScintillatorNumber) * 119;
  else position = -54.;

  if (fScintillatorNumber>11) position *= -1;

  return position;

}
