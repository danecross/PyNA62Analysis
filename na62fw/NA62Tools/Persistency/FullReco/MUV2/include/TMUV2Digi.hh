// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
// Copied and modified from GTK by Mario Vormstein (mario.vormstein@cern.ch) 2012-03-07
//
// --------------------------------------------------------------
#ifndef TMUV2Digi_H
#define TMUV2Digi_H

#include "FADCVHit.hh"
#include "MUV2ChannelID.hh"
#include "TVector3.h"

class TMUV2Digi : public FADCVHit , public MUV2ChannelID {

  public:

    TMUV2Digi();

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID () { return 0; }



  private:


    ClassDef(TMUV2Digi,1);

};
#endif
