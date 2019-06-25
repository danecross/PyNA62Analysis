// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef CHANTIOnlineMonitor_H
#define CHANTIOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

class CHANTIOnlineMonitor : public NA62VOnlineMonitor {

  public:

    CHANTIOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~CHANTIOnlineMonitor();
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:
};

#endif
