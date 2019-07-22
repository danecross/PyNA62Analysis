// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef LAVOnlineMonitor_H
#define LAVOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TText.h"
#include "TPaveText.h"
#include "TVirtualPad.h"
#include "TArc.h"
#include "TMarker.h"
#include "TGraph.h"
#include "TMultiGraph.h"

class LAVOnlineMonitor : public NA62VOnlineMonitor {

  public:

    LAVOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~LAVOnlineMonitor();
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:

    TH2F *fPhiOnPhysics;
    TH2F *fPhiOffPhysics;
    TGraph **fHTotEOBRate;
    TMultiGraph *fHTotEOBRateAllLAVs;

};

#endif
