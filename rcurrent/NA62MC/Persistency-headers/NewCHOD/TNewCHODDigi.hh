// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TNewCHODDigi_H
#define TNewCHODDigi_H

#include "TDCVHit.hh"
#include "NewCHODChannelID.hh"

class TNewCHODDigi : public TDCVHit, public NewCHODChannelID {

public:

  TNewCHODDigi();
  ~TNewCHODDigi() {}

  void Clear(Option_t* = "");

  Int_t  EncodeChannelID();
  void   DecodeChannelID();
  Int_t  GetStationID() { return 0;       }
  Bool_t IsHigh()       { return fIsHigh; }

private:

  Bool_t fIsHigh;

  ClassDef(TNewCHODDigi,1);
};

#endif
