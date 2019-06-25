// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-13
//
// ------------------------------------------------------------------

#include "T0Evaluation.hh"

#ifndef GigaTrackerT0_H
#define GigaTrackerT0_H 1

class GigaTrackerT0 : public T0Evaluation {

public:

  explicit GigaTrackerT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~GigaTrackerT0() {}
};

#endif
