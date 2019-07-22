// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TMUV1Event_H
#define TMUV1Event_H

#include "TDetectorVEvent.hh"

class TMUV1Event : public TDetectorVEvent {

    public:

      TMUV1Event();
      ~TMUV1Event();

      void Clear(Option_t* = "");

    private:

      ClassDef(TMUV1Event,1);
};
#endif
