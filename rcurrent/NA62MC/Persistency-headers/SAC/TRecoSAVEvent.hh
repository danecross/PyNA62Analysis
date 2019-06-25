// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016_06_02
//
// --------------------------------------------------------------
#ifndef TRecoSAVEvent_H
#define TRecoSAVEvent_H

#include "TRecoVEvent.hh"
#include "TRecoSAVCandidate.hh"
#include "TRecoSAVHit.hh"

class TRecoSAVEvent : public TRecoVEvent {

    public:

        TRecoSAVEvent();
        ~TRecoSAVEvent();

        void Clear(Option_t* = "");

    private:
        ClassDef(TRecoSAVEvent,1);
};
#endif
