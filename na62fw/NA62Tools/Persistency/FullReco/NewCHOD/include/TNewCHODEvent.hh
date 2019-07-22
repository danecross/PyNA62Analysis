// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// ---------------------------------------------------------------

#ifndef TNewCHODEvent_H
#define TNewCHODEvent_H

#include "TDetectorVEvent.hh"

class TNewCHODEvent : public TDetectorVEvent {

public:
  TNewCHODEvent();
  ~TNewCHODEvent();

  void Clear(Option_t* = "");

private:

  ClassDef(TNewCHODEvent,1);
};
#endif
