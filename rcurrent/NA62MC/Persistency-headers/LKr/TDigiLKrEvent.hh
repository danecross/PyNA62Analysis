// --------------------------------------------------------------
// History:
//
// Created by Tommaso Spadaro (Tommaso.Spadaro@cern.ch) 2017-01-11
//
// --------------------------------------------------------------
#ifndef TDigiLKrEvent_H
#define TDigiLKrEvent_H

#include "FADCEvent.hh"

class TDigiLKrEvent : public FADCEvent {

    public:

        TDigiLKrEvent();
        explicit TDigiLKrEvent(TClass*);
        TDigiLKrEvent(TClass*,Int_t);
        ~TDigiLKrEvent();
        void Init();

        void Clear(Option_t* = "");

    public:
        void SetTimePhase(Double_t val) { fTimePhase=val; };
        Double_t GetTimePhase() { return fTimePhase; };

    private:
        Double_t fTimePhase;

        ClassDef(TDigiLKrEvent,1);
};
#endif
