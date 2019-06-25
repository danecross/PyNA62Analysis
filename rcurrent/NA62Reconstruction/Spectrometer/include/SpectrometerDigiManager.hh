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
   ~SpectrometerDigiManager();
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
    SRBEvent *fTdcEvent;
    SpectrometerRawDecoder *fRawDecoder;
    SpectrometerReconstruction *fReco;
    SpectrometerParameters *fPar;
    Int_t    GetGlobalCoverID(TSpectrometerDigi*);
    Double_t fTWindowLeadMin;
    Double_t fTWindowLeadMax;
    Double_t fTWindowTrailMin;
    Double_t fTWindowTrailMax;
    Bool_t fRTMode;
    Double_t fMagicT0;
    Double_t driftPerChannel[8000];
    Int_t digiLeadingPerChannel[8000];
    Double_t trailingPerChannel[8000];
    Int_t digiTrailingPerChannel[8000];
    Bool_t SelectDriftTime(Double_t,Double_t);

  private:
    TH2F *fHRTDependence;
    TH2F *fHTRDependence;
    TH2F *fHStrawSpaceResolution;
    TH1F *fHAll;
    TH2F *fHAllLeading;
    TH2F *fHAllLeadingSRB;
    TH2F *fHAllLeadingCover;
    TH2F *fHFirstLeading;
    TH2F *fHAllTrailing;
    TH2F *fHLastTrailing;
    TH2F *fHDiff;
    TH2F *fHRadius;

    // Histograms used in SpectrometerOnlineMonitor
    TH2F *fHChannelActivity;
    TH1I *fHNleadingsInTime;
    TH2F *fHRadius2D;
    TH1F *fHRadiusTotal;
    TH1F *fHLeadingTimeTotal;
    TH1F *fHTrailingTimeTotal;
};

#endif
