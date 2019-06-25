// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV1)
//
// --------------------------------------------------------------
#include "TRecoMUV1Candidate.hh"

ClassImp(TRecoMUV1Candidate)

TRecoMUV1Candidate::TRecoMUV1Candidate() : TRecoVCandidate(){
}

void TRecoMUV1Candidate::Clear(Option_t * option){
  TRecoVCandidate::Clear(option);
}
