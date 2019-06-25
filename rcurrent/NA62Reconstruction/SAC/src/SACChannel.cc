// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-20
//
// ---------------------------------------------------------

#include "TFile.h"
#include <fstream>
#include "Riostream.h"
#include "TMath.h"
#include "TString.h"

#include "SACChannel.hh"

SACChannel::SACChannel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms, Double_t LowThreshold, Double_t HighThreshold) :
  NA62VChannel(GeoChannelID%1000,ROChannelID,FillHistograms,"SAC") {

  if(GeoChannelID>=1000)   fThreshold = HighThreshold;
  else if(GeoChannelID>=0) fThreshold = LowThreshold;
  else                     fThreshold = -999.;

}

SACChannel::~SACChannel () {}

