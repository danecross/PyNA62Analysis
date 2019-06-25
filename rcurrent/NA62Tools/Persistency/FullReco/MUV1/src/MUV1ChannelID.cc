//
//  MUV1ChannelID.cc
//
//
//  Created by riccardo aliberti on 10/10/14.
//
//

#include "MUV1ChannelID.hh"
#include "Riostream.h"

MUV1ChannelID :: MUV1ChannelID(){

}

MUV1ChannelID :: MUV1ChannelID(Int_t ChannelID){

  DecodeChannelID(ChannelID);

}

MUV1ChannelID :: ~ MUV1ChannelID(){

}

void MUV1ChannelID::Clear(Option_t * /*option*/){
}

Int_t MUV1ChannelID :: EncodeChannelID(){

  Int_t ChannelID = fSide * 50 + 100;

  ChannelID += fScintillatorNumber;

  return ChannelID;
}

MUV1ChannelID::chIDDecoded MUV1ChannelID::DecodeChannelID_Static(Int_t ChannelID){

  chIDDecoded ret;
  if (ChannelID < 300 && ChannelID > 100){
    ret.fScintillatorNumber = ChannelID%50;
    ret.fSide = (ChannelID-100)/50 ;
  }
  else{
    std::cout <<"MUV1ChannelID::DecodeChannelID("<<ChannelID<<") no matching quadrant!"<<std::endl;
    ret.fSide = -1;
    ret.fScintillatorNumber = -1;
  }
  return ret;
}

void MUV1ChannelID :: DecodeChannelID(Int_t ChannelID){
  chIDDecoded ret = DecodeChannelID_Static(ChannelID);
  fScintillatorNumber = ret.fScintillatorNumber;
  fSide = ret.fSide;
}


Int_t MUV1ChannelID :: GetPlane (){

  if (fSide%2==0) return kHorizontalPlane;
  else return kVerticalPlane;

}


Int_t MUV1ChannelID :: GetQuadrant (){

  Int_t ChannelID = 100 + fSide*50 + fScintillatorNumber;
  Int_t fQuadrant = -1;


  if ((ChannelID > 100 && ChannelID < 123) || (ChannelID>150 && ChannelID<173)) fQuadrant = 1;
  else if ((ChannelID > 122 && ChannelID < 145) || (ChannelID>250 && ChannelID<273)) fQuadrant = 2;
  else if ((ChannelID > 222 && ChannelID < 245) || (ChannelID>272 && ChannelID<295)) fQuadrant = 3;
  else if ((ChannelID > 200 && ChannelID < 223) || (ChannelID>172 && ChannelID<195)) fQuadrant = 4;

  return fQuadrant;


}


Double_t MUV1ChannelID :: GetScintillatorPosition (){

  Double_t position = -1278;

  if (fScintillatorNumber<21) position += (fScintillatorNumber - 1)*60.;
  else if (fScintillatorNumber>24) position += (44 - fScintillatorNumber)*60.;
  else if (fScintillatorNumber==21 || fScintillatorNumber==24) position = -81;
  else position = -27;

  if (fScintillatorNumber>22) position *= -1;

  return position;

}


Bool_t MUV1ChannelID :: IsLongScintillator (){

  if (fSide%2==0 && (fScintillatorNumber<18 || fScintillatorNumber>27)) return true;

  if (fSide%2==1 && (fScintillatorNumber<19 || fScintillatorNumber>26)) return true;

  else return false;

}


