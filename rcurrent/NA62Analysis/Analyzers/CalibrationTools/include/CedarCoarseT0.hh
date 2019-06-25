// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef CEDARCOARSET0_H
#define CEDARCOARSET0_H 1

class CedarCoarseT0 : public CoarseT0Evaluation {

public:

  explicit CedarCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~CedarCoarseT0() {};
};

#endif
