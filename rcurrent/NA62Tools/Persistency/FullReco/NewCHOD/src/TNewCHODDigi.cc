// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#include "TNewCHODDigi.hh"
ClassImp(TNewCHODDigi)

TNewCHODDigi::TNewCHODDigi() : TDCVHit(), NewCHODChannelID() {
  fIsHigh = false;
}

void TNewCHODDigi::Clear(Option_t* option){
  TDCVHit::Clear(option);
  NewCHODChannelID::Clear(option);
  fIsHigh = false;
}

Int_t TNewCHODDigi::EncodeChannelID() {
  fChannelID = NewCHODChannelID::EncodeChannelID();
  return fChannelID;
}

void TNewCHODDigi::DecodeChannelID() {
  NewCHODChannelID::DecodeChannelID(fChannelID);
  fIsHigh = (fChannelID%100)>50;
}
