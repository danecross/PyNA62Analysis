#include "THACEvent.hh"
#include "THACHit.hh"

ClassImp(THACEvent)

THACEvent::THACEvent() : TDetectorVEvent(THACHit::Class()){
}

THACEvent::~THACEvent() {
}

void THACEvent::Clear(Option_t* option) {
  TDetectorVEvent::Clear(option);
}
