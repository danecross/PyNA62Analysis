// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#include "TRecoCedarHit.hh"

ClassImp(TRecoCedarHit)

TRecoCedarHit::TRecoCedarHit() : TRecoVHit(), CedarChannelID() {
}

Int_t TRecoCedarHit::EncodeChannelID() {
  fChannelID = CedarChannelID::EncodeChannelID();
  return fChannelID;
}

void TRecoCedarHit::DecodeChannelID() {
  CedarChannelID::DecodeChannelID(fChannelID);
}

void TRecoCedarHit::Clear(Option_t* option) {
  TRecoVHit::Clear(option);
  CedarChannelID::Clear(option);
}
