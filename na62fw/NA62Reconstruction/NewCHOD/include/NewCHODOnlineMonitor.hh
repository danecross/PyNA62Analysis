// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#ifndef NewCHODOnlineMonitor_H
#define NewCHODOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "NewCHODDataQualityPlotter.hh"

class NewCHODOnlineMonitor : public NA62VOnlineMonitor {

  public:

    NewCHODOnlineMonitor(TRootBrowser*, NA62VReconstruction*,Int_t);
    virtual ~NewCHODOnlineMonitor() {}
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:

    NewCHODDataQualityPlotter *fDataQualityPlotter;
    TH1D *fHNEventsProcessedPerBurst;
    TH1D *fHNRecoHitsPerBurst;
};

#endif
