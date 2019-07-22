// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#include "TRICHDigi.hh"
ClassImp(TRICHDigi)

Int_t TRICHDigi::Compare(const TObject *obj) const {

  if(fOrSuperCellID > ((TRICHDigi*)obj)->GetOrSuperCellID()){
    return 1;
  }else{ 
    return -1;
  }
  
}

void TRICHDigi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  RICHChannelID::Clear(option);
}

Int_t TRICHDigi::EncodeChannelID() {
  fChannelID=RICHChannelID::EncodeChannelID();
  return fChannelID;
}

void TRICHDigi::DecodeChannelID() {
  RICHChannelID::DecodeChannelID(fChannelID);
}

