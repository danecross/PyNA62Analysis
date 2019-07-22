// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef CHODCOARSET0_H
#define CHODCOARSET0_H 1

class CHODCoarseT0 : public CoarseT0Evaluation {

public:

  explicit CHODCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~CHODCoarseT0() {}
};

#endif
