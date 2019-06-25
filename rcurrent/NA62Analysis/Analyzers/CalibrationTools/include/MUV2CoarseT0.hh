// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef MUV2COARSET0_H
#define MUV2COARSET0_H 1

class MUV2CoarseT0 : public CoarseT0Evaluation {

public:

  explicit MUV2CoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV2CoarseT0() {};

  void EndOfJobUser();
};

#endif
