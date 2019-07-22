// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TIRCEvent_H
#define TIRCEvent_H

#include "TDetectorVEvent.hh"

class TIRCEvent : public TDetectorVEvent {

    public:

        TIRCEvent();
        ~TIRCEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TIRCEvent,1);
};
#endif
