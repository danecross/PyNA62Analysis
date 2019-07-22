// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2019-04-09
//
// ---------------------------------------------------------------

#ifndef TSLIMMUV3HIT_H
#define TSLIMMUV3HIT_H

#include "TSlimRecoVHit.hh"

class TRecoMUV3Hit;

class TSlimRecoMUV3Hit : public TSlimRecoVHit {

public:
  TSlimRecoMUV3Hit()              {}
  explicit TSlimRecoMUV3Hit(TRecoMUV3Hit*) {}
  virtual ~TSlimRecoMUV3Hit()     {}

  // Conversion functions
  virtual void FromReco(TRecoVHit*) {}
  virtual void ToReco  (TRecoVHit*) {}

private:

  ClassDef(TSlimRecoMUV3Hit, 1)
};

#endif
