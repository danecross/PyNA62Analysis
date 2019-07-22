#include "TRecoMUV0Candidate.hh"

ClassImp(TRecoMUV0Candidate)

TRecoMUV0Candidate::TRecoMUV0Candidate() : TRecoVCandidate(){
}

void TRecoMUV0Candidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);
}
