#ifndef SRBVHit_H
#define SRBVHit_H
#include "TVDigi.hh"
#include "TDCVHit.hh"
#include "Riostream.h"

// Convention for leading/trailing detected edge bits
static const Int_t SRB_HIT_EDGE_LEADING  = 1;
static const Int_t SRB_HIT_EDGE_TRAILING = 2;

class SRBVHit : public TDCVHit {

    public:

        SRBVHit();
        explicit SRBVHit(Int_t);
        explicit SRBVHit(TVHit* MCHit);
        void Clear(Option_t* = "");

    public:

      Int_t GetSRBAddr() {return fSRBAddr;};
      void SetSRBAddr(Int_t val) {fSRBAddr=val;};
      Int_t GetStrawAddr() {return fStrawAddr;};
      void SetStrawAddr(Int_t val) {fStrawAddr=val;};
      Bool_t GetMultiHit() {return fMultiHit;};
      void SetMultiHit(Bool_t val) {fMultiHit=val;};
       

    private:

      Int_t   fSRBAddr       ;
      Int_t   fStrawAddr      ;
      Bool_t  fMultiHit       ;

    private:

        ClassDef(SRBVHit,1);
};
#endif
