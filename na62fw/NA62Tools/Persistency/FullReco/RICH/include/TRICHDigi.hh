// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TRICHDigi_H
#define TRICHDigi_H

#include "TDCVHit.hh"
#include "RICHChannelID.hh"


class TRICHDigi : public TDCVHit, public RICHChannelID {

public:

  TRICHDigi() : TDCVHit(), RICHChannelID() {}
  explicit TRICHDigi(Int_t iCh) : TDCVHit(iCh), RICHChannelID(){}
  ~TRICHDigi(){}

  void Clear(Option_t* = "");

  Int_t Compare(const TObject *obj) const;
  Int_t EncodeChannelID();
  void  DecodeChannelID();

  Int_t GetStationID() { return GetOrSuperCellID(); }
    


public:


private:


  ClassDef(TRICHDigi,1);
};
#endif
