// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "PedestalEvaluation.hh"

#ifndef MUV1PEDESTAL_H
#define MUV1PEDESTAL_H 1

class MUV1Pedestal : public PedestalEvaluation {

public:

  explicit MUV1Pedestal(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV1Pedestal() {};
};

#endif
