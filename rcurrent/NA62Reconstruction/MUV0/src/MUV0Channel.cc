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

#include "MUV0Channel.hh"

MUV0Channel::MUV0Channel(Int_t GeoChannelID, Int_t ROChannelID, Bool_t FillHistograms, Double_t LowThreshold, Double_t HighThreshold) :
  NA62VChannel(GeoChannelID%10,ROChannelID,FillHistograms,"MUV0") {

  if(GeoChannelID>=10)   fThreshold =   HighThreshold; 
  else if(GeoChannelID>=0) fThreshold =    LowThreshold; 
  else                     fThreshold = -999.;

}

MUV0Channel::~MUV0Channel () {}

