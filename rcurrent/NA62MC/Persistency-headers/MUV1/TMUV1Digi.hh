// --------------------------------------------------------------
// History:
//
// Created by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
// Copied and modified from GTK by Mario Vormstein (mario.vormstein@cern.ch) 2012-03-07
//
// --------------------------------------------------------------
#ifndef TMUV1Digi_H
#define TMUV1Digi_H

#include "FADCVHit.hh"
#include "MUV1ChannelID.hh"
#include "TVector3.h"

class TMUV1Digi : public FADCVHit , public MUV1ChannelID {

  public:

    TMUV1Digi();
    ~TMUV1Digi(){};

    void Clear(Option_t* = "");

    Int_t EncodeChannelID();
    void  DecodeChannelID();

    Int_t GetStationID() { return 0; }

  public:


  private:


    ClassDef(TMUV1Digi,1);

};
#endif
