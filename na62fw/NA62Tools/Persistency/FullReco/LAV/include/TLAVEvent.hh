// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TLAVEvent_H
#define TLAVEvent_H

#include "TDetectorVEvent.hh"

class TLAVEvent : public TDetectorVEvent {

    public:

        TLAVEvent();
        ~TLAVEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TLAVEvent,1);
};
#endif
