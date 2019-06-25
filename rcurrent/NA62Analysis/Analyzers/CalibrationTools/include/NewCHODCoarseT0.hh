// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef NewCHODCOARSET0_H
#define NewCHODCOARSET0_H 1

class NewCHODCoarseT0 : public CoarseT0Evaluation {

public:

  explicit NewCHODCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~NewCHODCoarseT0() {};
};

#endif
