// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------
#ifndef TRecoNewCHODEvent_H
#define TRecoNewCHODEvent_H

#include "TRecoVEvent.hh"
#include "TRecoNewCHODCandidate.hh"
#include "TRecoNewCHODHit.hh"

class TRecoNewCHODEvent : public TRecoVEvent {

public:
  TRecoNewCHODEvent();
  ~TRecoNewCHODEvent();

  void Clear(Option_t* = "");

private:
  ClassDef(TRecoNewCHODEvent,1);
};
#endif
