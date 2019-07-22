#include "TMUV0Event.hh"
#include "TMUV0Hit.hh"

ClassImp(TMUV0Event)

TMUV0Event::TMUV0Event() : TDetectorVEvent(TMUV0Hit::Class()){
}

TMUV0Event::~TMUV0Event() {
}

void TMUV0Event::Clear(Option_t* option){
  TDetectorVEvent::Clear(option);
}
