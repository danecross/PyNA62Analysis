// --------------------------------------------------------------
// History:
//
// Modified by Domenico Di Filippo (Domenico.DiFilippo@cern.ch) 2015-04-10
//
// --------------------------------------------------------------
#ifndef TRecoCHANTICandidate_H
#define TRecoCHANTICandidate_H

#include "TRecoVCandidate.hh"
class TRecoCHANTICandidate : public TRecoVCandidate {

    public:

        TRecoCHANTICandidate();
        ~TRecoCHANTICandidate(){};

        void Clear(Option_t* = "");

        void SetXYMult(Int_t val) {fXYMult = val;}
        Int_t GetXYMult() {return fXYMult;}
        void SetXPCharge(Double_t val) {fXPCharge = val;}
        Double_t GetXPCharge() {return fXPCharge;}
        void SetYPCharge(Double_t val) {fYPCharge = val;}
        Double_t GetYPCharge() {return fYPCharge;}
        void SetXPos(Double_t val) {fXPos = val;}
        Double_t GetXPos() {return fXPos;}
        void SetYPos(Double_t val) {fYPos = val;}
        Double_t GetYPos() {return fYPos;}

    private:

        Double_t fXPCharge; // Sum of the charges collected in the X cluster
        Double_t fYPCharge; // Sum of the charges collected in the Y cluster
        Double_t fXPos; // Sum of the charges collected in the X cluster
        Double_t fYPos; // Sum of the charges collected in the Y cluster
        Int_t fXYMult; // Type of cluster flag

  ClassDef(TRecoCHANTICandidate,1);
};
#endif
