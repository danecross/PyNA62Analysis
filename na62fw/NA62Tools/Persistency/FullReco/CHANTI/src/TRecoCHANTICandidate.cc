// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoCHANTICandidate.hh"

ClassImp(TRecoCHANTICandidate)

TRecoCHANTICandidate::TRecoCHANTICandidate() : TRecoVCandidate(){
}

void TRecoCHANTICandidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
}
