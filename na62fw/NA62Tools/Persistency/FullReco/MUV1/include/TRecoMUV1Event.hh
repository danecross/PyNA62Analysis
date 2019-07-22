// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#ifndef TRecoMUV1Event_H
#define TRecoMUV1Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV1Candidate.hh"
#include "TRecoMUV1Hit.hh"

class TRecoMUV1Event : public TRecoVEvent {

    public:

        TRecoMUV1Event();
        ~TRecoMUV1Event();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoMUV1Event,1);
};
#endif
