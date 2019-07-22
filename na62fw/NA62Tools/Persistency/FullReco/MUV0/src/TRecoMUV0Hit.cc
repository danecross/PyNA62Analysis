// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-23
//
// ---------------------------------------------------------
#include "TRecoMUV0Hit.hh"
#include<iostream>

ClassImp(TRecoMUV0Hit)

TRecoMUV0Hit::TRecoMUV0Hit() : TRecoVHit(){
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

Int_t TRecoMUV0Hit::EncodeChannelID() {
  return MUV0ChannelID::EncodeChannelID();
}

void TRecoMUV0Hit::DecodeChannelID() {
  Int_t geoChannel = fChannelID%1000;
  MUV0ChannelID::DecodeChannelID(geoChannel);
}

Bool_t TRecoMUV0Hit::HasAllTimesInOrder()const{
  if(fEdgeMask>15||fEdgeMask<0)
    std::cerr
      <<"ERROR: TRecoSACHit::HasAllTimesInOrder(): "
      <<"This message should be never printed"
      <<std::endl
      <<"fEdgeMask="<<fEdgeMask<<std::endl;

  if(HasAll4EdgesDetected()              &&
     fLeadingEdgeLow  <fLeadingEdgeHigh  &&
     fLeadingEdgeHigh <fTrailingEdgeHigh &&
     fTrailingEdgeHigh<fTrailingEdgeLow  )
    return kTRUE;

  return kFALSE;
}

Double_t TRecoMUV0Hit::GetSlewingCorrection(Double_t LowThr, Double_t HighThr)
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

  if(TRecoMUV0Hit::HasLeadingEdgeHigh() && TRecoMUV0Hit::HasLeadingEdgeLow()) {
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

void TRecoMUV0Hit::Clear(Option_t* option){
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
