// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-04-30
//
// --------------------------------------------------------------
#include "TDigiVCandidate.hh"

ClassImp(TDigiVCandidate)

TDigiVCandidate::TDigiVCandidate() : TVCandidate(){
}

TDigiVCandidate::TDigiVCandidate(Int_t NMaxDigis) : TVCandidate(NMaxDigis){
}

TDigiVCandidate::~TDigiVCandidate(){
}

Bool_t TDigiVCandidate::AddDigi(Int_t DigiIndex){
  return TVCandidate::AddHit(DigiIndex);
}

Int_t TDigiVCandidate::GetNDigis(){
  return TVCandidate::GetNHits();
}

TVDigi * TDigiVCandidate::GetDigi(Int_t DigiIndex){
  return reinterpret_cast<TVDigi*>(TVCandidate::GetHit(DigiIndex));
}

void TDigiVCandidate::Clear(Option_t* option) {
  TVCandidate::Clear(option);
}
