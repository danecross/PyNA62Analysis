// ---------------------------------------------------------------
// History:
//
// Created by Antonio Cassese (Antonio.Cassese@cern.ch) 2012-10-19
//
// ---------------------------------------------------------------

#ifndef TCHANTIDigi_H
#define TCHANTIDigi_H

#include "TDCVHit.hh"
#include "CHANTIChannelID.hh"

class TCHANTIDigi : public TDCVHit, public CHANTIChannelID {

public:

  TCHANTIDigi() : TDCVHit(), fThresholdType(-1), fDigiSortFlag(0) {}
  explicit TCHANTIDigi(Int_t iCh) : TDCVHit(iCh), fThresholdType(-1), fDigiSortFlag(0) {}

  void Clear(Option_t* = "");

  Bool_t IsSortable() const { return kTRUE; }
  Int_t CompareChannel(const TObject *obj) const;

  Int_t Compare(const TObject *obj) const {Int_t res = CompareChannel(obj); if(res == 0){ return TDCVHit::Compare(obj);
    } else {return res;}}
  
  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return CHANTIChannelID::GetStationID();}
  Int_t GetPlaneID()   { return CHANTIChannelID::GetPlaneID();}

public:
  
  Int_t  GetThresholdType()              { return fThresholdType;  };
  void   SetThresholdType(Int_t value)   { fThresholdType = value; };  
  void   SetSortFlag(Int_t value)        { fDigiSortFlag = value; };  

private:
  
  Int_t fThresholdType;
  Int_t fDigiSortFlag; //!   
  ClassDef(TCHANTIDigi,1);
};

#endif
