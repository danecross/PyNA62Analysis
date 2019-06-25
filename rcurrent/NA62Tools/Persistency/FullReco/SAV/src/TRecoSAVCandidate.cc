// --------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// --------------------------------------------------------------
#include "TRecoSAVCandidate.hh"

ClassImp(TRecoSAVCandidate)

TRecoSAVCandidate::TRecoSAVCandidate() : TRecoVCandidate(){
}

void TRecoSAVCandidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
}
