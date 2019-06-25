#ifndef TMUV0Event_H
#define TMUV0Event_H

#include "TDetectorVEvent.hh"

class TMUV0Event : public TDetectorVEvent {
public:
  TMUV0Event();
  ~TMUV0Event();

  void Clear(Option_t* = "");

private:

  ClassDef(TMUV0Event,1);
};
#endif
