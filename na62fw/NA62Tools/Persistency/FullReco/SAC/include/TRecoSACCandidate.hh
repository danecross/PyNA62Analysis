// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoSACCandidate_H
#define TRecoSACCandidate_H

#include "TRecoVCandidate.hh"

class TRecoSACCandidate : public TRecoVCandidate {

    public:

        TRecoSACCandidate();
        ~TRecoSACCandidate(){};

        void Clear(Option_t* = "");

    private:

        ClassDef(TRecoSACCandidate,1);
};
#endif
