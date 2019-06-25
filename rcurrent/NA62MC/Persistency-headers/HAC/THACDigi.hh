// --------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-09-14
//
// --------------------------------------------------------------
#ifndef THACDigi_H
#define THACDigi_H

#include "TDCVHit.hh"
#include "HACChannelID.hh"

class THACDigi : public TDCVHit, public HACChannelID {

  public:

    THACDigi();// : TDCVHit(){}
    explicit THACDigi(Int_t iCh) : TDCVHit(iCh), HACChannelID(iCh%100){}
    explicit THACDigi(TVHit*);
    ~THACDigi() {}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }
    Int_t GetThresholdType() const { return fChannelID/100; } //Thr types: 0,1,2,3 (0 is the lowest, 3 the highest)

    Int_t Compare(const TObject *obj) const;
    Bool_t IsSortable() const { return kTRUE; }

  private:

    ClassDef(THACDigi,1);
};
#endif
