#ifndef SpectrometerDigiManager_H
#define SpectrometerDigiManager_H 1

#include "TROOT.h"
#include "TMath.h"
#include "TH2F.h"
#include <fcntl.h>

#include <iostream>
using namespace std;

class SRBEvent;
class SpectrometerRawDecoder;
class SpectrometerReconstruction;
class TRecoVEvent;
class SpectrometerParameters;
class TRecoSpectrometerHit;
class TSpectrometerDigi;

class SpectrometerDigiManager
{
  public: 
    explicit SpectrometerDigiManager(SpectrometerReconstruction *);
   ~SpectrometerDigiManager() = default;
    void DigiToReco(TRecoVEvent *, Double_t refTime, Int_t);
    inline void ImportEvent(SRBEvent *event, SpectrometerRawDecoder *rawdeco) {fTdcEvent = event; fRawDecoder = rawdeco; };
    void InitHistograms(Int_t monitorLevel);
    void SaveHistograms();
  public:
    TH2F* GetHChannelActivity()     {return fHChannelActivity;};
    TH1F* GetHAll()                 {return fHAll;};
    TH2F* GetHAllLeading()          {return fHAllLeading;};
    TH2F* GetHAllLeading_srb()      {return fHAllLeadingSRB;};
    TH2F* GetHAllLeading_cover()    {return fHAllLeadingCover;};
    TH2F* GetHFirstLeading()        {return fHFirstLeading;};
    TH1I* GetHLeadingsInTime()      {return fHNleadingsInTime;};
    TH1F* GetHRadiusTotal()         {return fHRadiusTotal;};
    TH1F* GetHLeadingTimeTotal()    {return fHLeadingTimeTotal;};
  private:
    SRBEvent *fTdcEvent = nullptr;
    SpectrometerRawDecoder *fRawDecoder = nullptr;
    SpectrometerReconstruction *fReco;
    SpectrometerParameters &fPar;
    Int_t    GetGlobalCoverID(TSpectrometerDigi*);
    const Double_t kTWindowLeadMin = 0;     // ns
    const Double_t kTWindowLeadMax = 190.;  // ns
    const Double_t kTWindowTrailMin = 0.;   // ns
    const Double_t kTWindowTrailMax = 300.; // ns
    Double_t fMagicT0 = 0;
    Double_t driftPerChannel[8000];
    Int_t digiLeadingPerChannel[8000];
    Double_t trailingPerChannel[8000];
    Int_t digiTrailingPerChannel[8000];
    Bool_t SelectDriftTime(Double_t,Double_t);

  private:
    TH2F *fHRTDependence = nullptr;
    TH2F *fHTRDependence = nullptr;
    TH2F *fHStrawSpaceResolution = nullptr;
    TH1F *fHAll = nullptr;
    TH2F *fHAllLeading = nullptr;
    TH2F *fHAllLeadingSRB = nullptr;
    TH2F *fHAllLeadingCover = nullptr;
    TH2F *fHFirstLeading = nullptr;
    TH2F *fHAllTrailing = nullptr;
    TH2F *fHLastTrailing = nullptr;
    TH2F *fHDiff = nullptr;
    TH2F *fHRadius = nullptr;

    // Histograms used in SpectrometerOnlineMonitor
    TH2F *fHChannelActivity = nullptr;
    TH1I *fHNleadingsInTime = nullptr;
    TH2F *fHRadius2D = nullptr;
    TH1F *fHRadiusTotal = nullptr;
    TH1F *fHLeadingTimeTotal = nullptr;
    TH1F *fHTrailingTimeTotal = nullptr;
};

#endif
