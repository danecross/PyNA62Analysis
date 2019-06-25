// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef MUV1OnlineMonitor_H
#define MUV1OnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class MUV1OnlineMonitor : public NA62VOnlineMonitor {

  public:

    MUV1OnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~MUV1OnlineMonitor();

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();
};

#endif
