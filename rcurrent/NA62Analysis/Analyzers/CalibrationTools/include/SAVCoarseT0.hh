// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef SAVCOARSET0_H
#define SAVCOARSET0_H 1

class SAVCoarseT0 : public CoarseT0Evaluation {

public:

  explicit SAVCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~SAVCoarseT0() {};

  void EndOfJobUser();
};

#endif
