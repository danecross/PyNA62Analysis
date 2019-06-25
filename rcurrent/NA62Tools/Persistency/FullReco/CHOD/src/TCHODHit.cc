// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 2008-04-24
// Updated: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-11-02
//
// --------------------------------------------------------------

#include "TCHODHit.hh"

ClassImp(TCHODHit)

TCHODHit::TCHODHit() : TDetectorVHit(), CHODChannelID() {}

Int_t TCHODHit::EncodeChannelID() {
  fChannelID = CHODChannelID::EncodeChannelID();
  return fChannelID;
}

void TCHODHit::DecodeChannelID() {
  CHODChannelID::DecodeChannelID(fChannelID);
}

void TCHODHit::Clear(Option_t* option) {
  TDetectorVHit::Clear(option);
  CHODChannelID::Clear(option);
}
