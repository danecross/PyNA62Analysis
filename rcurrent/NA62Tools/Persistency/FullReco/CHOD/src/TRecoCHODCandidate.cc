// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoCHODCandidate.hh"

ClassImp(TRecoCHODCandidate)

TRecoCHODCandidate::TRecoCHODCandidate() : TRecoVCandidate(){
}

void TRecoCHODCandidate::Clear(Option_t* option) {
  TRecoVCandidate::Clear(option);
}
