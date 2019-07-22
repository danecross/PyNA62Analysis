// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TCHANTIEvent_H
#define TCHANTIEvent_H

#include "TDetectorVEvent.hh"

class TCHANTIEvent : public TDetectorVEvent {

    public:

        TCHANTIEvent();
        ~TCHANTIEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TCHANTIEvent,1);
};
#endif
