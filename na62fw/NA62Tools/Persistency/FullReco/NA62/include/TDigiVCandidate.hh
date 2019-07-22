// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-30
//
// --------------------------------------------------------------
#ifndef TDigiVCandidate_H
#define TDigiVCandidate_H

#include "TVCandidate.hh"
#include "TVDigi.hh"

class TDigiVCandidate : public TVCandidate {

    public:

        TDigiVCandidate();
        explicit TDigiVCandidate(Int_t);
        virtual ~TDigiVCandidate();
        void Clear(Option_t* = "");

        Bool_t AddDigi(Int_t);
        Int_t GetNDigis();
        TVDigi * GetDigi(Int_t);

    public:

    private:

        ClassDef(TDigiVCandidate,1);
};
#endif
