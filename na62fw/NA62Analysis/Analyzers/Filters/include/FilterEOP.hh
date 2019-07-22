// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

#include "FilterEOPBase.hh"

#ifndef FILTER_EOP_H
#define FILTER_EOP_H

class FilterEOP : public FilterEOPBase {

public:

  explicit FilterEOP(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterEOP() {};
};

#endif
