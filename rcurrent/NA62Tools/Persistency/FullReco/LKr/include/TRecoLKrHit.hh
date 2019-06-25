// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoLKrHit_H
#define TRecoLKrHit_H

#include "TRecoVHit.hh"
#include "LKrChannelID.hh"

class TRecoLKrHit : public TRecoVHit, public LKrChannelID {

    public:

        TRecoLKrHit();
        ~TRecoLKrHit(){};

        void Clear(Option_t* = "");

        Int_t EncodeChannelID();
        void  DecodeChannelID();

    public:
        Double_t GetPedestal() {return fPedestal;};
        void SetPedestal(Double_t val) {fPedestal=val;};
        Int_t GetGain() {return fGain;};
        void SetGain(Int_t val) {fGain=val;};

    private:
        Int_t fGain;
        Double_t fPedestal;

        ClassDef(TRecoLKrHit,1);
};
#endif
