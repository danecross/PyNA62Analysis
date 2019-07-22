// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-18
//
// ---------------------------------------------------------------

#ifndef TRecoCedarEvent_H
#define TRecoCedarEvent_H

#include "TRecoVEvent.hh"
#include "TRecoCedarCandidate.hh"
#include "TRecoCedarHit.hh"

class TRecoCedarEvent : public TRecoVEvent {

public:

  TRecoCedarEvent();
  ~TRecoCedarEvent();

  void       Clear(Option_t* = "");

private:

  ClassDef(TRecoCedarEvent,1);
};

#endif
