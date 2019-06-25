// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "PedestalEvaluation.hh"

#ifndef SAVPEDESTAL_H
#define SAVPEDESTAL_H 1

class SAVPedestal : public PedestalEvaluation {

public:

  explicit SAVPedestal(NA62Analysis::Core::BaseAnalysis *ba);
  ~SAVPedestal() {};
};

#endif
