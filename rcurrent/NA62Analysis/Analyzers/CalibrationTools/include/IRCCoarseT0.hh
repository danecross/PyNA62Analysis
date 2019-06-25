// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-02-15
//
// ---------------------------------------------------------

#include "CoarseT0Evaluation.hh"

#ifndef IRCCOARSET0_H
#define IRCCOARSET0_H 1

class IRCCoarseT0 : public CoarseT0Evaluation {

public:

  explicit IRCCoarseT0(NA62Analysis::Core::BaseAnalysis *ba);
  ~IRCCoarseT0() {};
};

#endif
