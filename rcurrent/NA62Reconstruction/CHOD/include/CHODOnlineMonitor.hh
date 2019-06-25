// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef CHODOnlineMonitor_H
#define CHODOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class CHODOnlineMonitor : public NA62VOnlineMonitor {

  public:

    CHODOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~CHODOnlineMonitor();
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:

    TH1D * fHNEventsProcessedPerBurst,
         * fHNRecoHitsPerBurst,
         * fHNCandidatesPerBurst;

};

#endif
