/*
 * SACChannelID.hh
 *
 *  Created on: Sep 25, 2015
 *      Author: veni
 */

#ifndef SAC_PERSISTENCY_INCLUDE_SACCHANNELID_HH_
#define SAC_PERSISTENCY_INCLUDE_SACCHANNELID_HH_

#include "Rtypes.h"

class SACChannelID {
public:
  SACChannelID();
  explicit SACChannelID(Int_t);
  virtual ~SACChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  static Int_t DecodeChannelID_Static(Int_t); // converts position ID into PMTID, IsHighThreshold
  void  DecodeChannelID(Int_t); // converts position ID into PMTID, IsHighThreshold
  Int_t GetPMTID()           const { return fPMTID;              };

private:
  Int_t fPMTID;

 ClassDef(SACChannelID,1);
};

#endif /* SAC_PERSISTENCY_INCLUDE_SACCHANNELID_HH_ */
