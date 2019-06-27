/*
 * IRCChannelID.hh
 *
 *  Created on: Sep 26, 2015
 *      Author: veni
 */

#ifndef IRC_PERSISTENCY_INCLUDE_IRCCHANNELID_HH_
#define IRC_PERSISTENCY_INCLUDE_IRCCHANNELID_HH_

#include "Rtypes.h"

class IRCChannelID {
public:
  IRCChannelID();
  explicit IRCChannelID(Int_t);
  virtual ~IRCChannelID() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();      // returns position ID
  void  DecodeChannelID(Int_t); // converts position ID into PMTID, IsHighThreshold
  Int_t GetPMTID()           const { return fPMTID;              };

private:
  Int_t fPMTID;

 ClassDef(IRCChannelID,1);
};

#endif /* IRC_PERSISTENCY_INCLUDE_IRCCHANNELID_HH_ */
