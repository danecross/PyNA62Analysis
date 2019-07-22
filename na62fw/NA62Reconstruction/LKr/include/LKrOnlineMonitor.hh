// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef LKrOnlineMonitor_H
#define LKrOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class LKrOnlineMonitor : public NA62VOnlineMonitor {

  public:

    LKrOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~LKrOnlineMonitor();
    virtual void Update(Int_t);
    virtual void BurstReset(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:
    void DrawCrateSlotBoundaries();
};

#endif
