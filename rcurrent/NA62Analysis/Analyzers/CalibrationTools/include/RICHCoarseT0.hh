// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef RICHCOARSET0_H
#define RICHCOARSET0_H 1

class RICHCoarseT0 : public CoarseT0Evaluation {

public:

  explicit RICHCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~RICHCoarseT0() {};
};

#endif
