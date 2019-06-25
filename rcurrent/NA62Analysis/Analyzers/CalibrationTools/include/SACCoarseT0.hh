// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef SACCOARSET0_H
#define SACCOARSET0_H 1

class SACCoarseT0 : public CoarseT0Evaluation {

public:

  explicit SACCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~SACCoarseT0() {};
};

#endif
