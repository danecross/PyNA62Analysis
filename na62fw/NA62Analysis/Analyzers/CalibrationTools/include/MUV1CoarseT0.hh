// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef MUV1COARSET0_H
#define MUV1COARSET0_H 1

class MUV1CoarseT0 : public CoarseT0Evaluation {

public:

  explicit MUV1CoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV1CoarseT0() {};

  void EndOfJobUser();
};

#endif
