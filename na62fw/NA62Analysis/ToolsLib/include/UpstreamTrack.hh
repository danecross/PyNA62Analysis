#ifndef UPSTREAMTRACK_HH
#define UPSTREAMTRACK_HH

#include "TMath.h"
#include "TVector2.h"
#include "TVector3.h"
#include <iostream>
#include <vector>

#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoCedarCandidate.hh"

class UpstreamTrack {

public:

  UpstreamTrack();
  ~UpstreamTrack() {}
  void Clear();
//  void Print();

  void SetTrackID(Int_t val)  { fTrackID = val;  }
  Int_t GetTrackID()          { return fTrackID; }

  //////////////////////
  // Pointers candidates

  void SetGigaTrackerCandidate (TRecoGigaTrackerCandidate* val)     { fGigaTrackerCandidate = val;     }
  TRecoGigaTrackerCandidate*    GetGigaTrackerCandidate()           { return fGigaTrackerCandidate;    }
 
  Int_t GetNCedarCandidates()                                       { return fCedarCandidates.size();  }
  TRecoCedarCandidate*          GetCedarCandidate(UInt_t);
  void AddCedarCandidate        (TRecoCedarCandidate* val)          { fCedarCandidates.push_back(val); }


private:

  Int_t fTrackID; ///< ID of the GigaTracker track

  TRecoGigaTrackerCandidate* fGigaTrackerCandidate; ///< Pointer to the GigaTracker candidate
  std::vector<TRecoCedarCandidate*> fCedarCandidates;
};

#endif
