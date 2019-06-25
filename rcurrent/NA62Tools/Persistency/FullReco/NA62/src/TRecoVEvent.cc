// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoVEvent.hh"

#include "Riostream.h"
#include "TClass.h"

ClassImp(TRecoVEvent)

TRecoVEvent::TRecoVEvent() : TDetectorVEvent(),
  fStatus(kFALSE), fNCandidates(0), fCandidates(nullptr), fDigiEvent(nullptr), fErrorMask(0) {}

TRecoVEvent::TRecoVEvent(const TRecoVEvent & event) :
  TDetectorVEvent((TDetectorVEvent &)event),
  fStatus(event.fStatus),
  fNCandidates(event.fNCandidates),
  fCandidates(nullptr),
  fDigiEvent(nullptr),
  fErrorMask(event.fErrorMask)
{
  if (event.fCandidates) fCandidates = new TClonesArray(*event.fCandidates);
  if (event.fDigiEvent) fDigiEvent = new TDigiVEvent(*event.fDigiEvent);
}

TRecoVEvent::TRecoVEvent(TClass* CandidateClass, TClass* HitClass) :
  TDetectorVEvent(HitClass),
  fStatus(kFALSE),
  fNCandidates(0),
  fCandidates(new TClonesArray(CandidateClass,50)),
  fDigiEvent(nullptr),
  fErrorMask(0)
{
}

TRecoVEvent::~TRecoVEvent() {
  if (fCandidates) {
    fCandidates->Clear("C");
    delete fCandidates;
  }
}

void TRecoVEvent::Clear(Option_t * option){
  TDetectorVEvent::Clear(option);
  fStatus = kFALSE;
  fNCandidates = 0;
  fCandidates->Clear(option);
  fDigiEvent = 0;
  fErrorMask = 0;
}

TRecoVHit * TRecoVEvent::AddHit(TDetectorVHit * MCHit){
  TRecoVHit * hit = static_cast<TRecoVHit*>(TDetectorVEvent::AddHit(MCHit->GetChannelID()));
  (*static_cast<TDetectorVHit*>(hit)) = (*MCHit);
  //hit->SetDigi(new TVDigi(MCHit));
  hit->SetDigiOwner(kTRUE);
  return hit;
}

TRecoVHit * TRecoVEvent::AddHit(TVDigi * Digi){
  TRecoVHit * hit = static_cast<TRecoVHit*>(TDetectorVEvent::AddHit(Digi->GetChannelID()));
  TDetectorVHit * MCHit = static_cast<TDetectorVHit*>(Digi->GetMCHit());
  if(MCHit) (*static_cast<TDetectorVHit*>(hit)) = *MCHit;
  hit->SetDigi(Digi);
  return hit;
}

TRecoVCandidate * TRecoVEvent::AddCandidate(){
  TRecoVCandidate * candidate = static_cast<TRecoVCandidate*>((fCandidates->ConstructedAt(fNCandidates++,"C")));
  candidate->SetEvent(this);
  return candidate;
}

TRecoVCandidate * TRecoVEvent::GetCandidate(Int_t iCandidate){
  TRecoVCandidate * candidate = static_cast<TRecoVCandidate*>(fCandidates->At(iCandidate));
  candidate->SetEvent(this);
  return candidate;
}

void TRecoVEvent::RemoveCandidate(Int_t iCandidate){
  fCandidates->RemoveAt(iCandidate);
  fCandidates->Compress();
  fNCandidates--;
}

void TRecoVEvent::SetErrorMaskBit(Int_t bit,Bool_t value){
  if(bit<0 || bit>=64) return;
  if((fErrorMask&(1<<bit))==0 && value) fErrorMask+=(1<<bit);
  else if((fErrorMask&(1<<bit))!=0 && !value) fErrorMask-=(1<<bit);
}
