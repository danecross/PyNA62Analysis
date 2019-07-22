// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSpectrometerEvent_H
#define TRecoSpectrometerEvent_H

#include "TRecoSpectrometerCandidate.hh"
#include "TRecoSpectrometerHit.hh"
#include "TRecoVEvent.hh"

class TRecoSpectrometerEvent : public TRecoVEvent {

    public:

        TRecoSpectrometerEvent();
        ~TRecoSpectrometerEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoSpectrometerEvent,1);
};
#endif