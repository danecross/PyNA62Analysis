#ifndef NA62VOnlineMonitor_H
#define NA62VOnlineMonitor_H

#include "NA62VNamedModule.hh"
#include "NA62VReconstruction.hh"
#include "NA62VOnlineMonitorCanvas.hh"

#include "TEveManager.h"
#include "TEveBrowser.h"
#include "TGFrame.h"
#include "TGTab.h"
#include "TLine.h"

#include <vector>

#ifdef DIM
#include <dim/dis.hxx>
#endif

class NA62VOnlineMonitor : public NA62VNamedModule{

  public:

    NA62VOnlineMonitor(TRootBrowser*,NA62VReconstruction*, TString);
    virtual ~NA62VOnlineMonitor();
    virtual void Update(Int_t);
    virtual void Print(Int_t);
    virtual void BurstReset(Int_t);

  public:

    TRootBrowser * GetMainWindow() { return fMainWindow; }
    NA62VOnlineMonitorCanvas * AddCanvasTab(TString);
    //TCanvas * AddCanvas(TString);

  protected:

    void AddDigiTimeRawFinePlots();
    void AddDecoderErrorsPlots();
    void CompleteTab(); ///< code to finalise adding the TabFrame

    NA62VReconstruction * fReco;

    TRootBrowser * fMainWindow;
    TGMainFrame *  fMainTabFrame;
    TGTab *        fMainTab;

    std::vector<NA62VOnlineMonitorCanvas*> fCanvases;

    // Histos exclusively used in the Online Monitor
    TH1D** fHDigiTimeRawFine1D;
    TLine ** fROBoardLinesRawFineVsROCh,
          ** fROBoardLinesErrors;

    // DIM variables
    char*        fDecoderErrorsString;
    int*         fDecoderErrorsValues;
    int          fDecoderErrorsMask;
#ifdef DIM
    DimService * fDecoderErrorsStringService;
    DimService * fDecoderErrorsValuesService;
    DimService * fDecoderErrorsMaskService;
#endif
};
#endif
