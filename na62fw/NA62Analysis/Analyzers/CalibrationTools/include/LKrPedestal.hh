// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2017-07-25
//
// ---------------------------------------------------------

#include "PedestalEvaluation.hh"

#ifndef LKRPEDESTAL_H
#define LKRPEDESTAL_H 1

class LKrPedestal : public PedestalEvaluation {

public:

  explicit LKrPedestal(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrPedestal() {};
};

#endif
