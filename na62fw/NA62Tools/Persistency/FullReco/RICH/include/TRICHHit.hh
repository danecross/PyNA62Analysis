// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TRICHHit_H
#define TRICHHit_H

#include "TDetectorVHit.hh"
#include "RICHChannelID.hh"

class TRICHHit : public TDetectorVHit, public RICHChannelID {

    public:

  TRICHHit();
  explicit TRICHHit(Int_t iCh);
  ~TRICHHit(){};
  
  void Clear(Option_t* = "");

  Int_t EncodeChannelID();
  void DecodeChannelID();

  Int_t GetStationID() { return 0; }



    public:


    protected:

        ClassDef(TRICHHit,1);
};
#endif
