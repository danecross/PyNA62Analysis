// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSACEvent_H
#define TRecoSACEvent_H

#include "TRecoVEvent.hh"
#include "TRecoSACCandidate.hh"
#include "TRecoSACHit.hh"

class TRecoSACEvent : public TRecoVEvent {

    public:

        TRecoSACEvent();
        ~TRecoSACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoSACEvent,1);
};
#endif
