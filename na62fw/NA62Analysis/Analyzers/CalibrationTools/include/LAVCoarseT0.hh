// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef LAVCOARSET0_H
#define LAVCOARSET0_H 1

class LAVCoarseT0 : public CoarseT0Evaluation {

public:

  explicit LAVCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~LAVCoarseT0() {};
};

#endif
