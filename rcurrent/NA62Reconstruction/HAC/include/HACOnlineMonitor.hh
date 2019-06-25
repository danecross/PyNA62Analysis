// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef HACOnlineMonitor_H
#define HACOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class HACOnlineMonitor : public NA62VOnlineMonitor {

  public:

    HACOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~HACOnlineMonitor();

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:
};

#endif
