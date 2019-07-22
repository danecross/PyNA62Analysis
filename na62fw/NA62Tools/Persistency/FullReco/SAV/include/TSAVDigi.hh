// --------------------------------------------------------------
// History:
//
// Create by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#ifndef TSAVDigi_H
#define TSAVDigi_H

#include "FADCVHit.hh"
#include "SAVChannelID.hh"
#include "TVector3.h"

class TSAVDigi : public FADCVHit , public SAVChannelID {

  public:

    TSAVDigi();
    ~TSAVDigi(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID () { return 0; }
    Int_t GetModuleID () { return fChannelID/10; }



  private:


    ClassDef(TSAVDigi,1);

};
#endif
