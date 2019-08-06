// --------------------------------------------------------------
// History:
//
// Modified by Bob Velghe (bob.velghe@cern.ch) Aug. 2014
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-29
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-18
//
// --------------------------------------------------------------
#ifndef GigaTrackerDigitizer_H
#define GigaTrackerDigitizer_H 1

#include "NA62VDigitizer.hh"
#include "GigaTrackerParameterTools.hh"
#include <set>
#include <map>

class TGigaTrackerHit;

// Define an order for TGigaTrackerHit
struct THitComp {
  bool operator()(const TGigaTrackerHit * h1, const TGigaTrackerHit * h2) const; 
};

typedef std::set< TGigaTrackerHit *, THitComp > THitSet;
typedef std::map<long, std::set< TGigaTrackerHit *, THitComp > > THitMap;

class GigaTrackerDigitizer : public NA62VDigitizer {

  public:
    explicit GigaTrackerDigitizer(NA62VReconstruction*);
    virtual ~GigaTrackerDigitizer();
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

  private:
    GigaTrackerParameterTools * fParTools;

};

#endif
