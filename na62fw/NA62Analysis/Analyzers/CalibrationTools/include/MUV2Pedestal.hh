// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "PedestalEvaluation.hh"

#ifndef MUV2PEDESTAL_H
#define MUV2PEDESTAL_H 1

class MUV2Pedestal : public PedestalEvaluation {

public:

  explicit MUV2Pedestal(NA62Analysis::Core::BaseAnalysis *ba);
  ~MUV2Pedestal() {};
};

#endif
