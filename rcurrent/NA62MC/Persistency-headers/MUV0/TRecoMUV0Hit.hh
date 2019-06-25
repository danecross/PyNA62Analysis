// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-23
//
// ---------------------------------------------------------

#ifndef TRecoMUV0Hit_H
#define TRecoMUV0Hit_H

#include "TRecoVHit.hh"
#include "MUV0ChannelID.hh"

class TRecoMUV0Hit : public TRecoVHit, public MUV0ChannelID {

public:

  TRecoMUV0Hit();
  ~TRecoMUV0Hit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

public:

  void SetLeadingEdgeLow  (Double_t edgeTime) { fLeadingEdgeLow   = edgeTime; fEdgeMask |= 1; }
  void SetLeadingEdgeHigh (Double_t edgeTime) { fLeadingEdgeHigh  = edgeTime; fEdgeMask |= 2; }
  void SetTrailingEdgeHigh(Double_t edgeTime) { fTrailingEdgeHigh = edgeTime; fEdgeMask |= 4; }
  void SetTrailingEdgeLow (Double_t edgeTime) { fTrailingEdgeLow  = edgeTime; fEdgeMask |= 8; }
  void SetTimeNoT0        (Double_t val)      { fTimeNoT0 = val; }

  Double_t GetLeadingEdgeLow()  { if (fEdgeMask & 1) return fLeadingEdgeLow;   else return 0; }
  Double_t GetLeadingEdgeHigh() { if (fEdgeMask & 2) return fLeadingEdgeHigh;  else return 0; }
  Double_t GetTrailingEdgeHigh(){ if (fEdgeMask & 4) return fTrailingEdgeHigh; else return 0; }
  Double_t GetTrailingEdgeLow() { if (fEdgeMask & 8) return fTrailingEdgeLow;  else return 0; }

  Double_t GetTimeNoT0                ()const{return fTimeNoT0;}
  //Double_t GetTimeOverThreshold       ()const{return fTimeOvThr;}
  void SetTimeOverThresholdLowThr (Double_t val){fTimeOvThrLow=val;}
  void SetTimeOverThresholdHighThr(Double_t val){fTimeOvThrHigh=val;}
  void SetLowThresholdROChannelID (Int_t val){fLowThresholdROChannelID =val;}
  void SetHighThresholdROChannelID(Int_t val){fHighThresholdROChannelID=val;}
  void SetLeadingESlewingSlope (Double_t val){fLeadingESlewingSlope =val;}
  void SetTrailingESlewingSlope(Double_t val){fTrailingESlewingSlope=val;}

  Double_t GetLeadingEdgeLow          ()const{return ((fEdgeMask & 0x1)? fLeadingEdgeLow  :0);}
  Double_t GetLeadingEdgeHigh         ()const{return ((fEdgeMask & 0x2)? fLeadingEdgeHigh :0);}
  Double_t GetTrailingEdgeHigh        ()const{return ((fEdgeMask & 0x4)? fTrailingEdgeHigh:0);}
  Double_t GetTrailingEdgeLow         ()const{return ((fEdgeMask & 0x8)? fTrailingEdgeLow :0);}

  Double_t GetTimeOverThresholdLowThr ()const{return fTimeOvThrLow;}
  Double_t GetTimeOverThresholdHighThr()const{return fTimeOvThrHigh;}
  Int_t    GetLowThresholdROChannelID ()const{return fLowThresholdROChannelID; }
  Int_t    GetHighThresholdROChannelID()const{return fHighThresholdROChannelID;}
  Double_t GetLeadingESlewingSlope    ()const{return fLeadingESlewingSlope ;}
  Double_t GetTrailingESlewingSlope   ()const{return fTrailingESlewingSlope;}
  Double_t GetSlewingCorrection(Double_t, Double_t);
  // overloaded because TVChannelID::GetChannelID() is not const
  Int_t    GetChannelID               ()const{return fChannelID;}

  Bool_t HasLeadingEdgeLow   ()const{return fEdgeMask & 0x1;}
  Bool_t HasLeadingEdgeHigh  ()const{return fEdgeMask & 0x2;}
  Bool_t HasTrailingEdgeHigh ()const{return fEdgeMask & 0x4;}
  Bool_t HasTrailingEdgeLow  ()const{return fEdgeMask & 0x8;}
  Bool_t HasAll4EdgesDetected()const{return fEdgeMask==0xF;}
  Bool_t HasAllTimesInOrder()const;

  Int_t GetEdgeMask() const {return fEdgeMask;}

private:
  Double_t fTimeNoT0;

  Int_t fEdgeMask; ///< Mask for the edges present: bit 0 --> LeadingLow; 1 --> LeadingHigh; 2-->TrailingHigh; 3-->TrailingLow
  Double_t fTimeOvThrLow;
  Double_t fTimeOvThrHigh;
  Double_t fLeadingESlewingSlope;
  Double_t fTrailingESlewingSlope;

  Double_t fLeadingEdgeLow;   ///< Time of leading low, subtracted of the trigger time only
  Double_t fTrailingEdgeLow;  ///< Time of leading high, subtracted of the trigger time only
  Double_t fLeadingEdgeHigh;  ///< Time of trailing high, subtracted of the trigger time only
  Double_t fTrailingEdgeHigh; ///< Time of trailing low, subtracted of the trigger time only
  Int_t fLowThresholdROChannelID;
  Int_t fHighThresholdROChannelID;

  ClassDef(TRecoMUV0Hit,1);
};
#endif
