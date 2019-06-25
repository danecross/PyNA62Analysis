// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-11
//
// ---------------------------------------------------------

#include "T0Evaluation.hh"

#ifndef LKRT0_H
#define LKRT0_H 1

class LKrT0 : public T0Evaluation {

public:

  explicit LKrT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~LKrT0() {};
};

#endif
