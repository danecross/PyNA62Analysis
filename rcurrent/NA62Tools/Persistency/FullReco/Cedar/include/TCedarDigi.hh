// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-19
//
// ---------------------------------------------------------------

#ifndef TCedarDigi_H
#define TCedarDigi_H

#include "TDCVHit.hh"
#include "CedarChannelID.hh"

class TCedarDigi : public TDCVHit, public CedarChannelID {

public:

  TCedarDigi();
  explicit TCedarDigi(TVHit*);
  ~TCedarDigi() {}

  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return 0; }

  //  TCedarDigi(Int_t iCh) : TDCVHit(iCh) {}

  Int_t Compare(const TObject *obj) const;
  Bool_t IsSortable() const { return kTRUE; }

private:

  ClassDef(TCedarDigi,1);
};

#endif
