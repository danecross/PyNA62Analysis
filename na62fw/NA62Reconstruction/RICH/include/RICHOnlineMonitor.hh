// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef RICHOnlineMonitor_H
#define RICHOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TProfile.h"

class RICHOnlineMonitor : public NA62VOnlineMonitor {

  public:

    RICHOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~RICHOnlineMonitor();
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:

    TGraph *fHNTimeCandidatesPerBurst, *fHNRingCandidatesPerBurst;
    TGraph *fHNHitTimeCandidatePerBurst, *fHNHitRingCandidatePerBurst;

};

#endif
