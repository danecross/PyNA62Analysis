// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-18
//
// ---------------------------------------------------------------

#include "FilterEOPBase.hh"

#ifndef FILTER_THREE_TRACKS_H
#define FILTER_THREE_TRACKS_H

class FilterThreeTracks : public FilterEOPBase {

public:

  explicit FilterThreeTracks(NA62Analysis::Core::BaseAnalysis *ba);
  ~FilterThreeTracks() {};
};

#endif
