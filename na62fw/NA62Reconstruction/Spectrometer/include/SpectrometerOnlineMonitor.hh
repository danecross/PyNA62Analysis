// ---------------------------------------------------------------
// History:
// 
// Updated by Zuzana Kucerova (zuzana.kucerova@cern.ch) 2016-08-17
// Updated by Michal Koval (michal.koval@cern.ch) 2015-07-10
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef SpectrometerOnlineMonitor_H
#define SpectrometerOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"

namespace ChannelActivityBorders {
  //arrays for ChannelActivity canvas
  const double ch1uLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  const double ch1uy[9] = {1., 1., 2., 2., 4., 4., 5., 5., 1.};
  const double ch1uRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  const double ch1vLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  const double ch1vy[9] = {6., 6., 7., 7., 9., 9., 10., 10., 6.};
  const double ch1vRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  const double ch1xLx[9] = {1., 63., 63., 64., 64., 63., 63., 1., 1.};
  const double ch1xy[9] = {11., 11., 13., 13., 14., 14., 15., 15., 11.};
  const double ch1xRx[9] = {120., 70., 70., 71., 71., 70., 70., 120., 120.};

  const double ch1yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  const double ch1yy[9] = {16., 16., 17., 17., 19., 19., 20., 20., 16.};
  const double ch1yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};

  const double ch2uLx[5] = {1., 62., 62., 1., 1.};
  const double ch2uy[5] = {22., 22., 26., 26., 22.};
  const double ch2uRx[5] = {120., 69., 69., 120., 120.};

  const double ch2vLx[5] = {1., 62., 62., 1., 1.};
  const double ch2vy[5] = {27., 27., 31., 31., 27.};
  const double ch2vRx[5] = {120., 69., 69., 120., 120.};

  const double ch2xLx[5] = {1., 64., 64., 1., 1.};
  const double ch2xy[5] = {32., 32., 36., 36., 32.};
  const double ch2xRx[5] = {120., 71., 71., 120., 120.};

  const double ch2yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  const double ch2yy[9] = {37., 37., 38., 38., 40., 40., 41., 41., 37.};
  const double ch2yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};

  const double ch3uLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  const double ch3uy[9] = {43., 43., 45., 45., 46., 46., 47., 47., 43.};
  const double ch3uRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  const double ch3vLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  const double ch3vy[9] = {48., 48., 49., 49., 50., 50., 52., 52., 48.};
  const double ch3vRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  const double ch3xLx[7] = {1., 62., 62., 63., 63., 1., 1.};
  const double ch3xy[7] = {53., 53., 54., 54., 57., 57., 53.};
  const double ch3xRx[7] = {120., 69., 69., 70., 70., 120., 120.};

  const double ch3yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  const double ch3yy[9] = {58., 58., 59., 59., 61., 61., 62., 62., 58.};
  const double ch3yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};

  const double ch4uLx[7] = {1., 59., 59., 60., 60., 1., 1.};
  const double ch4uy[7] = {64., 64., 65., 65., 68., 68., 64.};
  const double ch4uRx[7] = {120., 66., 66., 67., 67., 120., 120.};

  const double ch4vLx[7] = {1., 60., 60., 59., 59., 1., 1.};
  const double ch4vy[7] = {69., 69., 72., 72., 73., 73., 69.};
  const double ch4vRx[7] = {120., 67., 67., 66., 66., 120., 120.};

  const double ch4xLx[9] = {1., 60., 60., 61., 61., 60., 60., 1., 1.};
  const double ch4xy[9] = {74., 74., 75., 75., 77., 77., 78., 78., 74.};
  const double ch4xRx[9] = {120., 67., 67., 68., 68., 67., 67., 120., 120.};

  const double ch4yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  const double ch4yy[9] = {79., 79., 80., 80., 82., 82., 83., 83., 79.};
  const double ch4yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};
}

class TH1F;
class TH2F;
class TGraphErrors;

class SpectrometerOnlineMonitor : public NA62VOnlineMonitor {

  public:
    SpectrometerOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    ~SpectrometerOnlineMonitor();

    void Update(Int_t);
 
    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:
    // pointers to histograms created in SpectrometerDigiManager
    TH2F *fHChannelActivity;
    TH2F *fHSRBLeadings;
    TH2F *fHCoverLeadings;
    TH1F *fHAllLeadings;
    // pointers to histograms created in SpectrometerReconstruction
    TH1F *fHMmissFit;
    TH1F *fHMomentumFit;
    TH2F *fHCh1Illumination;
    TH2F *fHCh4Illumination;
    TH1F *fHPlaneRadius;
    TH2F *fHMissMassVStime;
    // burst stability histos (created in SpectrometerOnlineMonitor.cc)
    TGraphErrors *fHMomFitBurst;
    TGraphErrors *fHMissFitBurst;
    TGraphErrors *fHNleadingsBurst;
};

#endif
