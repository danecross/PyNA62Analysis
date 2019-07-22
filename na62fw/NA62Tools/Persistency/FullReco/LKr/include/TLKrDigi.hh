// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//            Evelina Gersabeck (Evelina.Gersabeck@cern.ch)
// --------------------------------------------------------------
#ifndef TLKrDigi_H
#define TLKrDigi_H

#include "FADCVHit.hh"
#include "LKrChannelID.hh"

class TLKrDigi : public FADCVHit, public LKrChannelID {
  public:

    TLKrDigi(Int_t iCh, Int_t CPDID, Int_t CPDChannelID) : FADCVHit(iCh), LKrChannelID(CPDID, CPDChannelID){} //old, it should be removed eventually
    TLKrDigi() : FADCVHit(), LKrChannelID(){}
    explicit TLKrDigi(Int_t iCh) : FADCVHit(iCh), LKrChannelID(){}
    ~TLKrDigi(){}

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  private:

    ClassDef(TLKrDigi,1);
};
#endif
