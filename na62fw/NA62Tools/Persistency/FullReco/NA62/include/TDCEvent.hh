// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TDCEvent_H
#define TDCEvent_H
#include "TClass.h"
#include "TDCVHit.hh"
#include "TDigiVEvent.hh"

class TDCEvent : public TDigiVEvent {

  public:

    TDCEvent();
    explicit TDCEvent(TClass *);
    TDCVHit* GetHit(Int_t);
    void Clear(Option_t* = "");

  private:

    ClassDef(TDCEvent,1);
};
#endif
