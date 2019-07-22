// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoVHit.hh"

ClassImp(TRecoVHit)

TRecoVHit::TRecoVHit() :
  TDetectorVHit(),
  fDigiOwner(kFALSE),
  fDigi(nullptr){}

TRecoVHit::TRecoVHit(const TRecoVHit & hit) :
  TDetectorVHit((TDetectorVHit &) hit),
  fDigiOwner(hit.fDigiOwner),
  fDigi(hit.fDigi){}

TRecoVHit::TRecoVHit(Int_t iCh) :
  TDetectorVHit(iCh),
  fDigiOwner(kFALSE),
  fDigi(nullptr){}

TRecoVHit::TRecoVHit(TVDigi* Digi) : fDigiOwner(kTRUE), fDigi(Digi) {
  (*static_cast<TVHit*>(this)) = (*static_cast<TVHit*>(Digi));
}

TRecoVHit::TRecoVHit(TDetectorVHit* MCHit) : fDigiOwner(kTRUE), fDigi(nullptr) {
  (*static_cast<TDetectorVHit*>(this)) = (*static_cast<TDetectorVHit*>(MCHit));
}

TRecoVHit::~TRecoVHit() {
  Clear();
}

void TRecoVHit::Clear(Option_t* option) {
  TDetectorVHit::Clear(option);
  fDigi = 0;
  fDigiOwner = kFALSE;
}
