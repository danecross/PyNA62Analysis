// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef CedarOnlineMonitor_H
#define CedarOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TText.h"
#include "TPaveText.h"
#include "TVirtualPad.h"
#include "TArc.h"
#include "TMarker.h"
#include "TGraph.h"

#ifdef DIM
#include <dim/dis.hxx>
#endif

class CedarOnlineMonitor : public NA62VOnlineMonitor {

  public:

    CedarOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~CedarOnlineMonitor();
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:

    Int_t * fPadMap; //mapping between Pads and KTAG Sector IDs

    NA62VOnlineMonitorCanvas * fCedarAlignmentCanvas;
    NA62VOnlineMonitorCanvas * fCedarAlignmentTrendsCanvas;
    TPaveText * fCedarAlignmentText;
    TPaveText * fCedarAsymText;

#ifdef DIM
    DimService * fAlignSugXService;
    DimService * fAlignSugYService;
    double fAlignSugX;
    double fAlignSugY;
#endif

    TGraph **fHNCoincidencesPerBurst,
           **fHNCoincidencesPerPressure,
           * fHNHitsInCandidatePerBurst,
           * fHPressurePerBurst;
};

#endif
