// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-01-14
//
// --------------------------------------------------------------

#ifndef TMUV3Event_H
#define TMUV3Event_H

#include "TDetectorVEvent.hh"

class TMUV3Event : public TDetectorVEvent {

public:

  TMUV3Event();
  ~TMUV3Event();

  void Clear(Option_t* = "");

private:

  ClassDef(TMUV3Event,1);
};
#endif
