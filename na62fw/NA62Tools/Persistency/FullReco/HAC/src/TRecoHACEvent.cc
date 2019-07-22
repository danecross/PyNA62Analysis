#include "TRecoHACEvent.hh"

ClassImp(TRecoHACEvent)

TRecoHACEvent::TRecoHACEvent() : TRecoVEvent(TRecoHACCandidate::Class(), TRecoHACHit::Class()){
  Clear();
}

TRecoHACEvent::~TRecoHACEvent(){
}

void TRecoHACEvent::Clear(Option_t* option){
  TRecoVEvent::Clear(option);
}
