// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-04-10
//
// ------------------------------------------------------------------

#include "T0Evaluation.hh"

#ifndef SAVT0_H
#define SAVT0_H 1

class SAVT0 : public T0Evaluation {

public:

  explicit SAVT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~SAVT0() {};
};

#endif
