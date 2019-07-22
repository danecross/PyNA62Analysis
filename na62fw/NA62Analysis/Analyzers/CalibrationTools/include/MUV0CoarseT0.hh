// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef MUV0COARSET0_H
#define MUV0COARSET0_H 1

class MUV0CoarseT0 : public CoarseT0Evaluation {

public:

  explicit MUV0CoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV0CoarseT0() {};
};

#endif
