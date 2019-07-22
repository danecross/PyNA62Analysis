// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

#include "FilterEOPBase.hh"

#ifndef FILTER_ELECTRON_H
#define FILTER_ELECTRON_H

class FilterElectron : public FilterEOPBase {

public:

  explicit FilterElectron(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterElectron() {};
};

#endif
