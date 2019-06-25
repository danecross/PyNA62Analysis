// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TCHODEvent_H
#define TCHODEvent_H

#include "TDetectorVEvent.hh"

class TCHODEvent : public TDetectorVEvent {

    public:

        TCHODEvent();
        ~TCHODEvent();

        void Clear(Option_t* = "");

    public:

    private:

        ClassDef(TCHODEvent,1);
};
#endif
