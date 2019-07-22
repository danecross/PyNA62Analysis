// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TNewCHODHit_H
#define TNewCHODHit_H

#include "TDetectorVHit.hh"
#include "NewCHODChannelID.hh"

class TNewCHODHit : public TDetectorVHit, public NewCHODChannelID {

public:

  TNewCHODHit();
  ~TNewCHODHit() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();
  Int_t GetStationID() { return 0; }

protected:

  ClassDef(TNewCHODHit,1);
};
#endif
