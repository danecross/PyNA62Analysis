// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------
#ifndef TRecoMUV3Event_H
#define TRecoMUV3Event_H

#include "TRecoVEvent.hh"
#include "TRecoMUV3Candidate.hh"
#include "TRecoMUV3Hit.hh"

class TRecoMUV3Event : public TRecoVEvent {

public:
  TRecoMUV3Event();
  ~TRecoMUV3Event();

  void Clear(Option_t* = "");

private:
  ClassDef(TRecoMUV3Event,1);

};
#endif
