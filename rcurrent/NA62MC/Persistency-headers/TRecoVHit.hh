// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoVHit_H
#define TRecoVHit_H

#include "TDetectorVHit.hh"
#include "TVDigi.hh"

class TRecoVHit : public TDetectorVHit {

    public:

        TRecoVHit();
	    TRecoVHit(const TRecoVHit &);
	    explicit TRecoVHit(Int_t);
	    explicit TRecoVHit(TVDigi*);
	    explicit TRecoVHit(TDetectorVHit*);
        ~TRecoVHit();

        void Clear(Option_t* = "");

    public:

        Bool_t               GetDigiOwner()                                     { return fDigiOwner;                    };
        void                 SetDigiOwner(Bool_t value)                         { fDigiOwner = value;                   };

        TVDigi *             GetDigi()                                          { return fDigi;                         };
        void                 SetDigi(TVDigi * value)                            { fDigi = value;                        };

    private:

        Bool_t fDigiOwner; //!  Transient data member
        TVDigi * fDigi; //!  Transient data member for MCTruth Association 

        ClassDef(TRecoVHit,1);
};
#endif
