// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef IRCOnlineMonitor_H
#define IRCOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TGraphErrors.h"

class IRCOnlineMonitor : public NA62VOnlineMonitor {

  public:

    IRCOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~IRCOnlineMonitor();
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

    Double_t GetMaxGraphArray(TGraph **gArrIRC,Int_t nArr);
    Double_t GetMinGraphArray(TGraph **gArrIRC,Int_t nArr);
    Double_t GetMaxGraphArray(TGraphErrors **gArrIRC,Int_t nArr);
    Double_t GetMinGraphArray(TGraphErrors **gArrIRC,Int_t nArr);

  private:
    TGraphErrors *fGHitRateChOnline[4];

};

#endif
