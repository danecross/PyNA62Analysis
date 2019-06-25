// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-03-22
// Updated by E Goudzovski (eg@hep.ph.bham.ac.uk) 2016-05-11
//
// ---------------------------------------------------------

#ifndef MUV0CHANNELID_HH_
#define MUV0CHANNELID_HH_

#include "Rtypes.h"
#include "TVChannelID.hh"

class MUV0ChannelID {

public:

  MUV0ChannelID();
  virtual ~MUV0ChannelID() {}

  void Clear(Option_t* = "");

  Int_t  EncodeChannelID(); // returns position ID
  void   DecodeChannelID(Int_t);
  Int_t  GetTileID() { return fTileID; }
  
private:

  Int_t  fTileID;

 ClassDef(MUV0ChannelID,1);
};

#endif
