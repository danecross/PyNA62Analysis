// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
//
// --------------------------------------------------------------
#include "TCHANTIHit.hh"

ClassImp(TCHANTIHit)

TCHANTIHit::TCHANTIHit() : TDetectorVHit() {
}

Int_t TCHANTIHit::EncodeChannelID(){
  fChannelID =  CHANTIChannelID::EncodeChannelID();
  return fChannelID;
}

void TCHANTIHit::DecodeChannelID(){
    CHANTIChannelID::DecodeChannelID(fChannelID);
}


Double_t TCHANTIHit::GetDistanceFromSiPM(){
   Double_t SipmPos = 150; //mm
   Double_t HitPos;
   if (this->GetRingType()==0) HitPos = (this->GetPosition()).Y(); 
   else HitPos = (this->GetPosition()).X();

   if (this->GetSideID()==0) return(SipmPos-HitPos);
   else return(SipmPos+HitPos);
}

void TCHANTIHit::Clear(Option_t* option){
  TDetectorVHit::Clear(option);
}
