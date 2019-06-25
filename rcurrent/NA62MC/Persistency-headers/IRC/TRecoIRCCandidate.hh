// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoIRCCandidate_H
#define TRecoIRCCandidate_H

#include "TRecoVCandidate.hh"

class TRecoIRCCandidate : public TRecoVCandidate {

    public:

        TRecoIRCCandidate();
        ~TRecoIRCCandidate(){};

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoIRCCandidate,1);
};
#endif
