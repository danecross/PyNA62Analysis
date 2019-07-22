// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef LKRCOARSET0_H
#define LKRCOARSET0_H 1

class LKrCoarseT0 : public CoarseT0Evaluation {

public:

  explicit LKrCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrCoarseT0() {};

  void EndOfJobUser();
};

#endif
