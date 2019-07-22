// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoIRCEvent_H
#define TRecoIRCEvent_H

#include "TRecoVEvent.hh"
#include "TRecoIRCCandidate.hh"
#include "TRecoIRCHit.hh"

class TRecoIRCEvent : public TRecoVEvent {

    public:

        TRecoIRCEvent();
        ~TRecoIRCEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoIRCEvent,1);
};
#endif
