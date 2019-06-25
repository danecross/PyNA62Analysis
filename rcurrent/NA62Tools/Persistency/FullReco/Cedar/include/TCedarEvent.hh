// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------
#ifndef TCedarEvent_H
#define TCedarEvent_H

#include "TDetectorVEvent.hh"

class TCedarEvent : public TDetectorVEvent {

public:

  TCedarEvent();
  ~TCedarEvent();

  void Clear(Option_t* = "");

private:

  ClassDef(TCedarEvent,1);
};

#endif
