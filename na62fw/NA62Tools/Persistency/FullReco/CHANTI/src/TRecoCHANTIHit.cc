// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoCHANTIHit.hh"

ClassImp(TRecoCHANTIHit)

TRecoCHANTIHit::TRecoCHANTIHit() : TRecoVHit(),fX(0.),fY(0.),fZ(0.),fThresholdFlag(0),fTimeWidth(0.),fConnectorID(0),fMult(0),fDeltaTime(0.),fDeltaWidth(0.),fQualityFlag(0){}

Int_t TRecoCHANTIHit::GetConnectorID(){
  fConnectorID = -999;
  Int_t DiffoID = fChannelID - 100000*(GetPlaneID()+1);
  if((DiffoID ==     0) || 
     (DiffoID ==     1) ||
     (DiffoID == 11110) ||
     (DiffoID == 11111) ||
     (DiffoID == 11100) ||
     (DiffoID == 11101) ||
     (DiffoID == 11090) ||
     (DiffoID == 11091) ||
     (DiffoID == 11080) ||
     (DiffoID == 11081) ||
     (DiffoID == 11070) ||
     (DiffoID == 11071) ||
     (DiffoID == 11060) ||
     (DiffoID == 11061) ||
     (DiffoID == 11050) ||
     (DiffoID == 11051) ||
     (DiffoID == 1050 ) ||
     (DiffoID == 1051 ) ||
     (DiffoID == 1060 ) ||
     (DiffoID == 1061 ) ||
     (DiffoID == 1070 ) ||
     (DiffoID == 1071 ) ||
     (DiffoID == 1080 ) ||
     (DiffoID == 1081 ) ||
     (DiffoID == 1090 ) ||
     (DiffoID == 1091 ) ||
     (DiffoID == 1100 ) ||
     (DiffoID == 1101 ) ||
     (DiffoID == 1110 ) ||
     (DiffoID == 1111 ) ||
     (DiffoID == 10000) ||
     (DiffoID == 10001))    fConnectorID =  3*GetPlaneID() + 1;
  ///////////////////////CH17-32/////////////////////////
  if((DiffoID == 10010) || 
     (DiffoID == 10011) || 
     (DiffoID == 10020) || 
     (DiffoID == 10021) || 
     (DiffoID == 10030) || 
     (DiffoID == 10031) || 
     (DiffoID == 10040) || 
     (DiffoID == 10041) || 
     (DiffoID == 10050) || 
     (DiffoID == 10051) || 
     (DiffoID == 10060) || 
     (DiffoID == 10061) || 
     (DiffoID == 10070) || 
     (DiffoID == 10071) || 
     (DiffoID == 10080) || 
     (DiffoID == 10081) || 
     (DiffoID == 10090) || 
     (DiffoID == 10091) || 
     (DiffoID == 10100) || 
     (DiffoID == 10101) || 
     (DiffoID == 10110) || 
     (DiffoID == 10111) || 
     (DiffoID == 10120) || 
     (DiffoID == 10121) || 
     (DiffoID == 10130) || 
     (DiffoID == 10131) || 
     (DiffoID == 10140) || 
     (DiffoID == 10141) || 
     (DiffoID == 10150) || 
     (DiffoID == 10151) || 
     (DiffoID == 10160) || 
     (DiffoID == 10161)) fConnectorID = 3*GetPlaneID() + 2;		      
  ///////////////////////CH33-48/////////////////////////
  if((DiffoID == 160) || 
     (DiffoID == 161) || 
     (DiffoID == 150) || 
     (DiffoID == 151) || 
     (DiffoID == 140) || 
     (DiffoID == 141) || 
     (DiffoID == 130) || 
     (DiffoID == 131) || 
     (DiffoID == 120) || 
     (DiffoID == 121) || 
     (DiffoID == 110) || 
     (DiffoID == 111) || 
     (DiffoID == 100) || 
     (DiffoID == 101) || 
     (DiffoID == 90 ) ||
     (DiffoID == 91 ) ||
     (DiffoID == 80 ) ||
     (DiffoID == 81 ) ||
     (DiffoID == 70 ) ||
     (DiffoID == 71 ) ||
     (DiffoID == 60 ) ||
     (DiffoID == 61 ) ||
     (DiffoID == 50 ) ||
     (DiffoID == 51 ) ||
     (DiffoID == 40 ) ||
     (DiffoID == 41 ) ||
     (DiffoID == 30 ) ||
     (DiffoID == 31 ) ||
     (DiffoID == 20 ) ||
     (DiffoID == 21 ) ||
     (DiffoID == 10 ) ||
     (DiffoID == 11 )) fConnectorID = 3*GetPlaneID() + 3;
  return fConnectorID;

}

Double_t TRecoCHANTIHit::GetXYTimeCorrection(Double_t Position){
  const Double_t Length = 150;
  const Double_t FiberSpeed = 180;
  if(GetSideID() == kPositive)  return (Length - Position)/FiberSpeed;
  else return (Length + Position)/FiberSpeed;

}

void TRecoCHANTIHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
}


Int_t TRecoCHANTIHit::EncodeChannelID(){
  fChannelID =  CHANTIChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoCHANTIHit::DecodeChannelID(){
    CHANTIChannelID::DecodeChannelID(fChannelID);
}
