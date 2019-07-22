// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef CHANTICOARSET0_H
#define CHANTICOARSET0_H 1

class CHANTICoarseT0 : public CoarseT0Evaluation {

public:

  explicit CHANTICoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~CHANTICoarseT0() {};
};

#endif
