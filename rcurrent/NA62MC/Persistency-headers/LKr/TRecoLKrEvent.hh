// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLKrEvent_H
#define TRecoLKrEvent_H

#include "TRecoVEvent.hh"
#include "TRecoLKrCandidate.hh"
#include "TRecoLKrHit.hh"

class TRecoLKrEvent : public TRecoVEvent {

    public:

        TRecoLKrEvent();
        ~TRecoLKrEvent();
        void Init();

        void Clear(Option_t* = "");

    public:
        void SetEnergyTotal(Double_t val) { fEnergyTotal=val; };
        Double_t GetEnergyTotal() { return fEnergyTotal; };
        void SetRecFlag(Int_t val) { fRecFlag=val; };
        Int_t GetRecFlag() { return fRecFlag; };

    private:
        Double_t fEnergyTotal;
        Int_t fRecFlag;

        ClassDef(TRecoLKrEvent,1);
};
#endif
