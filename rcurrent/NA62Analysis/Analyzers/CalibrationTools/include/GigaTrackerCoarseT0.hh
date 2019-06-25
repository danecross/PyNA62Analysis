// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef GTKCOARSET0_H
#define GTKCOARSET0_H 1

class GigaTrackerCoarseT0 : public CoarseT0Evaluation {

public:

  explicit GigaTrackerCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerCoarseT0() {};
};

#endif
