// --------------------------------------------------------------
// History:
//
// Modified by Alina KLeimenova (alina.kleimenova@cern.ch) Sep. 2017
//
// Modified by Mathieu Perrin-Terrin (mathieu.perrin-terrin@cern.ch) Fri Jul 10 2015
//
// Modified by Bob Velghe (bob.velghe@cern.ch) Aug. 2014
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2011-04-11
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-29
//
// Modified by Simone Bifani (Simone.Bifani@cern.ch) 2009-04-24
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
//
#ifndef GigaTrackerReconstruction_H
#define GigaTrackerReconstruction_H 1

#include <vector>
#include "NA62VReconstruction.hh"

#include "TGigaTrackerEvent.hh"
#include "TGigaTrackerDigi.hh"
#include "Event.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "GigaTrackerCluster.hh"
#include "GigaTrackerRawDecoder.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "GigaTrackerParameterTools.hh"
#include "TVector3.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TFile.h"
#include "TMatrixD.h"
#include "TVectorD.h"
#include "TF1.h"

#include "G4PhysicalConstants.hh"

using namespace std;

class GigaTrackerReconstruction : public NA62VReconstruction
{

public:

  GigaTrackerReconstruction(TFile*, TString);
  ~GigaTrackerReconstruction();
  void Init(NA62VReconstruction*);
  virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
  virtual TRecoVEvent * ProcessEOBEvent(TDetectorVEvent* tEvent, Event* tGenEvent);
  virtual TDetectorVEvent * Trigger(TDetectorVEvent*, Event*);
  virtual void EndProcessing();
  virtual void FillTimes(Double_t);
  virtual void StartOfBurst();
  virtual void EndOfBurst();
  void ParseConfFile(TString f);

  void InitHistograms();

  void SaveHistograms();
  void ReconstructCandidates(TRecoVEvent* RecoEvent);

  std::string GetDBFileName() { return fDBFileName;};
  int GetRunID() {return fRunID;}; 

  Bool_t GetEnableRawTimeCorrections() { return fEnableRawTimeCorrections; };

  // Online Monitor Hooks

  TH2F * GetHitMap(int station);
  TH2F * GetDtOverburst(int station);
  TH1D * GetAbsTimeProfile(int station);
  TH1D * GetAbsTimeProfileZoom(int station);
  TH1D * GetToT(int station, int chip);
  TH1D * GetTime(int station, int chip);
  TH1D * GetNHitsPerTrigger(int station, int chip);

  // First second, i.e Out of burst OOB
  TH2F * GetHitMapOOB(int station);
  TH1D * GetAbsTimeProfileOOB(int station);
  TH1D * GetAbsTimeProfileZoomOOB(int station);
  TH1D * GetToTOOB(int station, int chip);
  TH1D * GetTimeOOB(int station, int chip);
  TH1D * GetNHitsPerTriggerOOB(int station, int chip);
  Double_t GetNoiseThreshold();
  // EOB monitor
  TH2F  * GetCoarseHitMap(int station);  

private:
  Double_t GetRecoTime(TGigaTrackerDigi *);
  Double_t GetRawRecoTime(TGigaTrackerDigi *);
 
  template<typename Order>
  void Clusterize(vector<int>, vector<vector<int>>&, double minDist, Order order, int minCont);
  void BuildCandidate(TRecoGigaTrackerCandidate* cand);
  void LinearLeastSquareFit(Double_t *x, Double_t *y, Int_t Nsample, Double_t *sigma, Double_t &a, Double_t &b, Double_t &rho, Double_t &chi2);
  
  struct TimeOrder{
    TRecoVEvent* fGigaTrackerEvent;
    explicit TimeOrder(TRecoVEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return h1->GetTime() < h2->GetTime();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetTime() - h2->GetTime());
    }
  };

  struct XOrder{
    TRecoVEvent* fGigaTrackerEvent;
    explicit XOrder(TRecoVEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return h1->GetPosition().X() < h2->GetPosition().X();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetPosition().X() - h2->GetPosition().X());
    }
  };

  struct YOrder{
    TRecoVEvent* fGigaTrackerEvent;
    explicit YOrder(TRecoVEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return h1->GetPosition().Y() < h2->GetPosition().Y();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetPosition().Y() - h2->GetPosition().Y());
    }
  };

  struct StationOrder{
    TRecoVEvent* fGigaTrackerEvent;
    explicit StationOrder(TRecoVEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return h1->GetStationNo()<h2->GetStationNo();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetStationNo()-h2->GetStationNo());
    }
  };

  struct Cluster {
    double X,Y,Z,T;
    int N,S;
    vector<int> hits;
    TRecoVEvent* fGigaTrackerEvent;
    explicit Cluster(TRecoVEvent* p): X(0),Y(0),Z(0),T(0),N(0),S(0),fGigaTrackerEvent(p) { };

    void add(int i) {
      TRecoGigaTrackerHit* h= static_cast<TRecoGigaTrackerHit*>( fGigaTrackerEvent->GetHit(i));
      if (N>0 && h->GetStationNo()!=S) 
        { cout<<"Cluster error: trying to add hit from different stations"<<endl; return;}
      if (N==0) S = h->GetStationNo();
      X = (N*X+h->GetPosition().X())/(N+1);
      Y = (N*Y+h->GetPosition().Y())/(N+1);
      Z = (N*Z+h->GetPosition().Z())/(N+1);
      T = (N*T+h->GetTime()        )/(N+1);
      N = N+1;
      hits.push_back(i);
    }

    ~Cluster(){
      hits.clear();
    }
  };

  void LoadTW();
  void LoadT0();
  void LoadXY();
  
private:

  GigaTrackerParameterTools * fParameters;
  TRecoGigaTrackerCandidate * fCandidate;

  std::string fDBFileName;
  Int_t fRunID;
  Bool_t fMC;
  
  Bool_t fEnableRawTimeCorrections;
  UInt_t fNHitsMax;

  // // Geometry related variables
  double fTimeWindow;
  double fTimeWindowTrigger;
  double fRefTime=0.0;
  double fXWindow;
  double fYWindow;
  double fChi2X;
  double fChi2Y;
  double fChi2T;

  Double_t fClight;
  
  // Control histograms
  Bool_t mHistErrSet;

  TH2F * fHHitMap[3];
  TH1D * fHAbsTimeProfile[3];
  TH1D * fHAbsTimeProfileZoom[3];
  TH1D * fHToT[3][11];
  TH1D * fHTime[3][11];
  TH2F * fHTimeProfile[3][11];
  TH1D * fHRawTime[3][11];
  TH2F * fHRawTimeProfile[3][11];

  TH1D * fHNHitsPerTrigger[3][11];
  Double_t fNHitsPerTriggerAll[3][11];
  Double_t fNHitsPerTrigger[3][11];

  TH2F * fHHitMapOOB[3];
  TH1D * fHAbsTimeProfileOOB[3];
  TH1D * fHAbsTimeProfileZoomOOB[3];
  TH1D * fHToTOOB[3][11];
  TH1D * fHTimeOOB[3][11];
  TH1D * fHNHitsPerTriggerOOB[3][11];
  Double_t fNHitsPerTriggerOOB[3][11];
  Double_t fNHitsPerTriggerOOBAll[3][11];

  TH2F * fHCoarseHitMap[3];

  //MC histos
  TH1D * fHTrueMomentum;
  TH1D * fHTrueThetaX;
  TH1D * fHTrueThetaY;
  TH1D * fHTrueMomentumX;
  TH1D * fHTrueMomentumY;
  TH1D * fHTrueMomentumZ;

  // Station Offset
  TVector2 fPosOff[3];  ///< (x,y) alignment constants of the stations [mm]
  Float_t fT0[3][18000];
  TGraph* fTW[3][10];
};
#endif
