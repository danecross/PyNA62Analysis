// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TSACEvent_H
#define TSACEvent_H

#include "TDetectorVEvent.hh"

class TSACEvent : public TDetectorVEvent {

    public:

        TSACEvent();
        ~TSACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TSACEvent,1);
};
#endif
