// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#include "TRecoIRCCandidate.hh"

ClassImp(TRecoIRCCandidate)

TRecoIRCCandidate::TRecoIRCCandidate() : TRecoVCandidate(){
}

void TRecoIRCCandidate::Clear(Option_t* option) {
  TRecoVCandidate::Clear(option);
}
