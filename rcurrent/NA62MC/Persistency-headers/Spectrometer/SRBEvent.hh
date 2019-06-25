#ifndef SRBEvent_H
#define SRBEvent_H
#include "TClass.h"
#include "TDigiVEvent.hh"
#include "SRBVHit.hh"

class SRBEvent : public TDigiVEvent {

  public:

    SRBEvent();
    explicit SRBEvent(TClass *);
    SRBVHit* GetHit(Int_t);
    void Clear(Option_t* = "");

  private:

    ClassDef(SRBEvent,1);
};
#endif
