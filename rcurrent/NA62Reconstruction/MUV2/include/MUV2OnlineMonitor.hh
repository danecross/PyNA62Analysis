// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef MUV2OnlineMonitor_H
#define MUV2OnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class MUV2OnlineMonitor : public NA62VOnlineMonitor {

  public:

    MUV2OnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~MUV2OnlineMonitor();

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();
};

#endif
