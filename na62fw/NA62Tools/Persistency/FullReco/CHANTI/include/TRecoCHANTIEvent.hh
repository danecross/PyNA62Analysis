// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoCHANTIEvent_H
#define TRecoCHANTIEvent_H

#include "TRecoVEvent.hh"
#include "TRecoCHANTICandidate.hh"
#include "TRecoCHANTIHit.hh"

class TRecoCHANTIEvent : public TRecoVEvent {

    public:

        TRecoCHANTIEvent();
        ~TRecoCHANTIEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoCHANTIEvent,1);
};
#endif
