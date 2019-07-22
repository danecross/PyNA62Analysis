#include "TRecoHACCandidate.hh"

ClassImp(TRecoHACCandidate)

TRecoHACCandidate::TRecoHACCandidate() : TRecoVCandidate() {   
	TRecoHACCandidate::Clear(); 
}
 
void TRecoHACCandidate::Clear(Option_t* option) {   
	TRecoVCandidate::Clear(option);   

	SetTime(0.0);   
	fDeltaTimeClosestCandidate = 1.e28;   
	fIsSelected = kFALSE;   
	fNHitsClosestCandidate = 0;   
	fCharge = 0.; 
}
 
void TRecoHACCandidate::UpdateTime(Double_t fTime) {
  	SetTime((GetTime()*(GetNHits() - 1) + fTime) / GetNHits()); 
} 

void TRecoHACCandidate::UpdateTime() {   
	SetTime(0.0);   
	for (Int_t iHit = 0; iHit < GetNHits(); iHit++) {
	     SetTime(GetTime() + GetHit(iHit)->GetTime());   
	}   
	SetTime(GetTime() / GetNHits()); 
}

