// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef MUV0OnlineMonitor_H
#define MUV0OnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class MUV0OnlineMonitor : public NA62VOnlineMonitor {

  public:

    MUV0OnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~MUV0OnlineMonitor() {}

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

};

#endif
