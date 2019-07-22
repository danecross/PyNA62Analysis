// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoIRCHit.hh"
#include<iostream>

ClassImp(TRecoIRCHit)

TRecoIRCHit::TRecoIRCHit() : TRecoVHit(){
  fTimeNoT0 = -999.;
  fEdgeMask = 0; 
  fTimeOvThrLow = -999.;
  fTimeOvThrHigh = -999.;
  fLeadingESlewingSlope = -999.;
  fTrailingESlewingSlope = -999.;

  fLeadingEdgeLow = -999.;
  fTrailingEdgeLow = -999.;
  fLeadingEdgeHigh = -999.;
  fTrailingEdgeHigh = -999.;
  fLowThresholdROChannelID = -1;
  fHighThresholdROChannelID = -1;
}

void TRecoIRCHit::Clear(Option_t* option){
  TRecoVHit::Clear(option);
  fTimeNoT0 = -999.;
  fEdgeMask = 0; 
  fTimeOvThrLow = -999.;
  fTimeOvThrHigh = -999.;
  fLeadingESlewingSlope = -999.;
  fTrailingESlewingSlope = -999.;

  fLeadingEdgeLow = -999.;
  fTrailingEdgeLow = -999.;
  fLeadingEdgeHigh = -999.;
  fTrailingEdgeHigh = -999.;
  fLowThresholdROChannelID = -1;
  fHighThresholdROChannelID = -1;
}

Int_t TRecoIRCHit::EncodeChannelID() {
  return IRCChannelID::EncodeChannelID();
}

void TRecoIRCHit::DecodeChannelID() {
  Int_t geoChannel = fChannelID%1000;
  IRCChannelID::DecodeChannelID(geoChannel);
}

Bool_t TRecoIRCHit::HasAllTimesInOrder()const{
  if(fEdgeMask>15||fEdgeMask<0)
    std::cerr
      <<"ERROR: TRecoSACHit::HasAllTimesInOrder(): "
        <<"This message should be never printed"
           <<std::endl
           <<"fEdgeMask="<<fEdgeMask<<std::endl;
  if( HasAll4EdgesDetected()              &&
      fLeadingEdgeLow  <fLeadingEdgeHigh  &&
      fLeadingEdgeHigh <fTrailingEdgeHigh &&
      fTrailingEdgeHigh<fTrailingEdgeLow  )
    return kTRUE;
  return kFALSE;
}

Double_t TRecoIRCHit::GetSlewingCorrection(Double_t LowThr, Double_t HighThr)
{

  if(HasLeadingEdgeHigh() && HasTrailingEdgeHigh()) {
    fTimeOvThrHigh = fTrailingEdgeHigh - fLeadingEdgeHigh;
  }

  if(HasLeadingEdgeLow() && HasTrailingEdgeLow()) {
    fTimeOvThrLow = fTrailingEdgeLow - fLeadingEdgeLow;
  }

  if(HasTrailingEdgeHigh() && HasTrailingEdgeLow()) {
    fTrailingESlewingSlope = (fTrailingEdgeHigh - fTrailingEdgeLow) /
                            (HighThr - LowThr);
  } else if (HasTrailingEdgeLow()) {
    fTrailingESlewingSlope = 0;
  } else if (HasTrailingEdgeHigh()) {
    fTrailingESlewingSlope = 0;
  }

  if(TRecoIRCHit::HasLeadingEdgeHigh() && TRecoIRCHit::HasLeadingEdgeLow()) {
    fLeadingESlewingSlope = (fLeadingEdgeHigh - fLeadingEdgeLow) /
                            (HighThr - LowThr);
    fTimeNoT0 = fLeadingEdgeLow - fLeadingESlewingSlope * LowThr;
  } else if (HasLeadingEdgeLow()) {
    fLeadingESlewingSlope = 0;
    fTimeNoT0 = fLeadingEdgeLow;
  } else if (HasLeadingEdgeHigh()) {
    fLeadingESlewingSlope = 0;
    fTimeNoT0 = fLeadingEdgeHigh;
  }

  return fTimeNoT0;
}

