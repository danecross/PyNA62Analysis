#include "SRBEvent.hh"

#include "SRBError.hh"
#include "TTimeCluster.hh"

ClassImp(SRBEvent)

SRBEvent::SRBEvent() : TDigiVEvent(){}

SRBEvent::SRBEvent(TClass * HitClass) : TDigiVEvent(TTimeCluster::Class(), HitClass,SRBError::Class()){}

SRBVHit* SRBEvent::GetHit(Int_t iHit){
  return static_cast<SRBVHit*>(TDigiVEvent::GetHit(iHit));
}

void SRBEvent::Clear(Option_t * option){
  TDigiVEvent::Clear(option);
}
