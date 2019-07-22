// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#ifndef TRecoMUV2Event_H
#define TRecoMUV2Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV2Candidate.hh"
#include "TRecoMUV2Hit.hh"

class TRecoMUV2Event : public TRecoVEvent {

    public:

      TRecoMUV2Event();
      ~TRecoMUV2Event();

      void Clear(Option_t* = "");

    private:
      ClassDef(TRecoMUV2Event,1);
};
#endif
