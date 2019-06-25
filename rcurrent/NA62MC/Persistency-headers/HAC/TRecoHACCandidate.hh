#ifndef TRecoHACCandidate_H
#define TRecoHACCandidate_H

#include "TRecoVCandidate.hh"

class TRecoHACCandidate : public TRecoVCandidate {

    public:

      TRecoHACCandidate();
      ~TRecoHACCandidate(){};

      void Clear(Option_t* = "");

      void UpdateTime();   
      void UpdateTime(Double_t);

      void SetIsSelected(Bool_t val) {fIsSelected = val;}
      void SetDeltaTimeClosestCandidate(Double_t val) { fDeltaTimeClosestCandidate = val;}
      void SetNHitsClosestCandidate(Int_t val) { fNHitsClosestCandidate = val;}
      void SetCharge(Double_t val) { fCharge = val;}

      Bool_t GetIsSelected() {return fIsSelected;}
      Double_t GetDeltaTimeClosestCandidate() {return fDeltaTimeClosestCandidate;}
      Int_t GetNHitsClosestCandidate() {return fNHitsClosestCandidate;}
      Double_t GetCharge() {return fCharge;}
    
    private:

	Bool_t fIsSelected;
  	Double_t fDeltaTimeClosestCandidate;
  	Int_t fNHitsClosestCandidate;
  	Double_t fCharge;

        ClassDef(TRecoHACCandidate,1);
};
#endif

