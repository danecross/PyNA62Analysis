// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#ifndef TMUV2Event_H
#define TMUV2Event_H

#include "TDetectorVEvent.hh"

class TMUV2Event : public TDetectorVEvent {

    public:

      TMUV2Event();
      ~TMUV2Event();

      void Clear(Option_t* = "");

    private:

        ClassDef(TMUV2Event,1);
};
#endif
