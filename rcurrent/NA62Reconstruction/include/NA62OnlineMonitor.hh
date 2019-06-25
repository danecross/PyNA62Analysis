// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-06-30
//
// ---------------------------------------------------------------

#ifndef NA62OnlineMonitor_H
#define NA62OnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TGraph.h"
#include "TPaveText.h"

#define NL1Masks 16

class NA62OnlineMonitor : public NA62VOnlineMonitor {

  public:

    NA62OnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~NA62OnlineMonitor();
    virtual void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

    TGraph* GetHNProcessedEventsInFile()  { return fHNProcessedEventsInFile; };

  private:

    TPaveText * fCurrentBurstInfo;
    TPaveText * fChokeONTimeInfo;
    TGraph *fHNProcessedEventsInFile,
           *fHT10IntensityPerBurst,
           *fHNCountsArgonionPerBurst,
           *fHL1TPRejectionPerBurst,
           *fHL1TPInputControlPerBurst,
           *fHL1TPInputPeriodicsPerBurst,
           *fHL1TPOutputControlPerBurst,
           *fHL1TPOutputPeriodicsPerBurst,
           **fHL1TPRejectionPerMaskPerBurst,
           **fHL1TPInputPhysicsPerMaskPerBurst,
           **fHL1TPOutputPhysicsPerMaskPerBurst;


    TLine *** fOMROBoardLinesRaw,
          *** fOMROBoardLinesRawFine,
          *** fOMROBoardLinesErrors;
};

#endif
