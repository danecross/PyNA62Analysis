// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
// Modified by Rainer Wanke (Rainer.Wanke@uni-mainz.de) 2010-11-26
//      (Changed MUV --> MUV2)
//
// --------------------------------------------------------------
#include "TRecoMUV2Candidate.hh"

ClassImp(TRecoMUV2Candidate)

TRecoMUV2Candidate::TRecoMUV2Candidate() : TRecoVCandidate(){
}

void TRecoMUV2Candidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
}
