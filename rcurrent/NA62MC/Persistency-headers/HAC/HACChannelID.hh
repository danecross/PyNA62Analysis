// ---------------------------------------------------------
// History:
// Modified by Mario Bragadireanu (mario.bragadireanu@cern.ch) 2016-08-31
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-22
//
// ---------------------------------------------------------

#ifndef HAC_PERSISTENCY_INCLUDE_HACCHANNELID_HH_
#define HAC_PERSISTENCY_INCLUDE_HACCHANNELID_HH_

#include "Rtypes.h"

class HACChannelID {
public:
  HACChannelID();
  explicit HACChannelID(Int_t);
  virtual ~HACChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  void  DecodeChannelID(Int_t); // converts position ID into PMTID, IsHighThreshold
  Int_t GetSiPMID()          const { return fSiPMID;             };
  Int_t GetModuleID()        const { return fModuleID;           };
private:
  Int_t fSiPMID;
  Int_t fModuleID;
  ClassDef(HACChannelID,1);
};

#endif /* HAC_PERSISTENCY_INCLUDE_HACCHANNELID_HH_ */
