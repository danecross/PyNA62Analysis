// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef MUV3COARSET0_H
#define MUV3COARSET0_H 1

class MUV3CoarseT0 : public CoarseT0Evaluation {

public:

  explicit MUV3CoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV3CoarseT0() {};
};

#endif
