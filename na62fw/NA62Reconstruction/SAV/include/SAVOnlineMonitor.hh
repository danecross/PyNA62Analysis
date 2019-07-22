// ---------------------------------------------------------------
// History:
//
// Created by Letizia Peruzzo (letizia.peruzzo@cern.ch) 2016-06-02
//
// ---------------------------------------------------------------

#ifndef SAVOnlineMonitor_H
#define SAVOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TObjArray.h"

class SAVOnlineMonitor : public NA62VOnlineMonitor {

  public:

    SAVOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~SAVOnlineMonitor();

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();
};

#endif
