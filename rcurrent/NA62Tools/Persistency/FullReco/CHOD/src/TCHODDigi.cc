// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#include "TCHODDigi.hh"
ClassImp(TCHODDigi)

TCHODDigi::TCHODDigi() : TDCVHit(), CHODChannelID() {}

Int_t TCHODDigi::EncodeChannelID() {
  fChannelID = CHODChannelID::EncodeChannelID();
  return fChannelID;
}

void TCHODDigi::DecodeChannelID() {
  CHODChannelID::DecodeChannelID(fChannelID);
}

void TCHODDigi::Clear(Option_t* option) {
  TDCVHit::Clear(option);
  CHODChannelID::Clear(option);
}
