// --------------------------------------------------------------
// History:
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-03
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------
#ifndef TSACHit_H
#define TSACHit_H

#include "TDetectorVHit.hh"
#include "SACChannelID.hh"

class TSACHit : public TDetectorVHit, public SACChannelID {

    public:

      TSACHit();
      ~TSACHit(){};

      void Clear(Option_t* = "");

      Int_t EncodeChannelID();
      void  DecodeChannelID();

    protected:

        ClassDef(TSACHit,1);
};
#endif
