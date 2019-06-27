// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-08
//
// --------------------------------------------------------------
#ifndef TDetectorVEvent_H
#define TDetectorVEvent_H

#include "TClass.h"
#include "TClonesArray.h"
#include "TVEvent.hh"
#include "TVHit.hh"
#include "TDetectorVHit.hh"
#include "TVDigi.hh"

class TDetectorVEvent : public TVEvent {

public:

  TDetectorVEvent();
  TDetectorVEvent(const TDetectorVEvent &);
  TDetectorVEvent(TClass * Class, Int_t NMaxHits=1000);
  ~TDetectorVEvent();
  TVHit * AddHit();
  TVHit * AddHit(Int_t iCh);
  TDetectorVHit * AddHit(TDetectorVHit *);
  TVHit * GetLastHitOnChannel(Int_t iCh);
  TVHit * GetHit(Int_t iHit);
  TVHit * GetLastHit();
  void RemoveHit(Int_t iHit);
  void Clear(Option_t* = "");

  Int_t         GetNHits() { return fNHits; }
  TClonesArray* GetHits()  { return fHits;  }

private:

  Int_t         fNHits;
  TClonesArray* fHits;
  ClassDef(TDetectorVEvent,1);
};

#endif
