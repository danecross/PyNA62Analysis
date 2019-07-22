#include "TRecoMUV0Event.hh"

ClassImp(TRecoMUV0Event)

TRecoMUV0Event::TRecoMUV0Event() : TRecoVEvent(TRecoMUV0Candidate::Class(), TRecoMUV0Hit::Class()){
  Clear();
}

TRecoMUV0Event::~TRecoMUV0Event(){
}

void TRecoMUV0Event::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
