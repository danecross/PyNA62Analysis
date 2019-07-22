// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef MUV3OnlineMonitor_H
#define MUV3OnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "MUV3Reconstruction.hh"
#include "MUV3DataQualityPlotter.hh"

class MUV3OnlineMonitor : public NA62VOnlineMonitor {

  public:

    MUV3OnlineMonitor(TRootBrowser*, NA62VReconstruction*,Int_t);
    virtual ~MUV3OnlineMonitor() {}
    void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:

    MUV3DataQualityPlotter *fDataQualityPlotter;

    TH1D
      *fHNEventsProcessedPerBurst,
      *fHNRecoHitsPerBurst,
      *fHNCandidatesPerBurst,
      *fHTileAsymmetry,
      *fHTileAsymmetryEOB;

    TH2F *fHAsym2, *fHAsym2Inner, *fHAsym2EOB, *fHAsym2InnerEOB;
};

#endif
