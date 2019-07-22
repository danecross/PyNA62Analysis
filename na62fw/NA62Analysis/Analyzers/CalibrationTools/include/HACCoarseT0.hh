// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef HACCOARSET0_H
#define HACCOARSET0_H 1

class HACCoarseT0 : public CoarseT0Evaluation {

public:

  explicit HACCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~HACCoarseT0() {};
};

#endif
