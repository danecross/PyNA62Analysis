// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoSACCandidate.hh"

ClassImp(TRecoSACCandidate)

TRecoSACCandidate::TRecoSACCandidate() : TRecoVCandidate(){
}

void TRecoSACCandidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
}
