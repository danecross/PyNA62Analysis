// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2011-02-18
//
// --------------------------------------------------------------
#include "TDigiVEvent.hh"

#include "Riostream.h"

ClassImp(TDigiVEvent)

TDigiVEvent::TDigiVEvent() :
  TDetectorVEvent(),
  fNCandidates(0),
  fCandidates(nullptr),
  fErrorMask(0),
  fNErrors(0),
  fErrors(nullptr)
{
}

TDigiVEvent::TDigiVEvent(const TDigiVEvent &event) :
  TDetectorVEvent(event),
  fNCandidates(event.fNCandidates),
  fCandidates(new TClonesArray(*event.fCandidates)),
  fErrorMask(event.fErrorMask),
  fNErrors(event.fNErrors),
  fErrors(new TClonesArray(*event.fErrors))
{
}

TDigiVEvent::TDigiVEvent(TClass * CandidateClass, TClass * HitClass, TClass * ErrorClass) :
  TDetectorVEvent(HitClass),
  fNCandidates(0),
  fCandidates(new TClonesArray(CandidateClass,50)),
  fErrorMask(0),
  fNErrors(0),
  fErrors(new TClonesArray(ErrorClass,1000))
{
}

TDigiVEvent::TDigiVEvent(TClass * CandidateClass, TClass * HitClass, TClass * ErrorClass, Int_t NHitsMax) :
  TDetectorVEvent(HitClass,NHitsMax),
  fNCandidates(0),
  fCandidates(new TClonesArray(CandidateClass,50)),
  fErrorMask(0),
  fNErrors(0),
  fErrors(new TClonesArray(ErrorClass,1000))
{
}

TDigiVEvent::~TDigiVEvent(){
  if(fCandidates){
    fCandidates->Clear("C");
    delete fCandidates;
    fCandidates=0;
  }
  if(fErrors){
    fErrors->Clear("C");
    delete fErrors;
    fErrors=0;
  }
}

TVDigi * TDigiVEvent::AddDigi(){
  return static_cast<TVDigi*>(TDetectorVEvent::AddHit());
}

TVDigi * TDigiVEvent::AddDigi(Int_t ChID){
  return static_cast<TVDigi*>(TDetectorVEvent::AddHit(ChID));
}

TVDigi * TDigiVEvent::AddDigi(TDetectorVHit * MCHit){
  TVDigi * digi = static_cast<TVDigi*>(TDetectorVEvent::AddHit(MCHit->GetChannelID()));
  digi->SetMCTrackID(MCHit->GetMCTrackID());
  digi->SetMCHit(MCHit);
  return digi;
}

TDigiVError * TDigiVEvent::AddError(Int_t ErrorType){
  TDigiVError * Error = static_cast<TDigiVError*>((fErrors->ConstructedAt(fNErrors++,"C")));
  Error->SetType(ErrorType);
  TDigiVEvent::UpdateErrorMask(ErrorType);
  return Error;
}

TDigiVError* TDigiVEvent::GetError(Int_t iError) {
  if(iError<0 || iError>=fNErrors) return 0;
  return static_cast<TDigiVError*>(fErrors->At(iError));
}

Int_t TDigiVEvent::GetNDigis(){
  return TDetectorVEvent::GetNHits();
}

TClonesArray * TDigiVEvent::GetDigis(){
  return TDetectorVEvent::GetHits();
}

TDigiVCandidate * TDigiVEvent::AddCandidate(){
  TDigiVCandidate * Candidate = static_cast<TDigiVCandidate*>((fCandidates->ConstructedAt(fNCandidates++,"C")));
  Candidate->SetEvent(this);
  return Candidate;
}

TDigiVCandidate * TDigiVEvent::GetCandidate(Int_t iCandidate){
  if(iCandidate<0 || iCandidate>=fNCandidates) return 0;
  return static_cast<TDigiVCandidate*>(fCandidates->At(iCandidate));
}

void TDigiVEvent::RemoveCandidate(Int_t iCandidate){
  fCandidates->RemoveAt(iCandidate);
  fCandidates->Compress();
  fNCandidates--;
}

void TDigiVEvent::Clear(Option_t * option){
  TDetectorVEvent::Clear(option);
  fNCandidates = 0;
  if(fCandidates) fCandidates->Clear(option);
  fErrorMask = 0;
  fNErrors = 0;
  if(fErrors) fErrors->Clear(option);
}

void TDigiVEvent::UpdateErrorMask(Int_t ErrorType){
  if(0<=ErrorType && ErrorType<64 && !(fErrorMask&(1ULL<<ErrorType))) fErrorMask+=(1ULL<<ErrorType);
}
