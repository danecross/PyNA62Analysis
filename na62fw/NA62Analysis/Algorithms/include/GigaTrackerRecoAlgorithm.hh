#ifndef GigaTrackerRecoAlgorithm_HH_
#define GigaTrackerRecoAlgorithm_HH_

#include "Algorithm.hh"
#include "GigaTrackerParameterTools.hh"
#include <vector>
#include "TRecoGigaTrackerEvent.hh"
#include "TRecoGigaTrackerHit.hh"

using namespace std;

class TRecoGigaTrackerCandidate;

class GigaTrackerRecoAlgorithm : public Algorithm {

public:
  GigaTrackerRecoAlgorithm(BaseAnalysis *ba, Analyzer* ana, const string &name = "GigaTrackerRecoAlgorithm");
  virtual ~GigaTrackerRecoAlgorithm() {}
  void StartOfRunUser();
  void Process(TRecoGigaTrackerEvent* event, Double_t RefTime);
  void InitCorrections(); ///< Initialize station offsets and fine corrections
  void InitTimeCorrections();

  void SetTimeWindow(Double_t val)             { fTimeWindow = val;             }
  void SetTimeWindowWrtReference(Double_t val) { fTimeWindowWrtReference = val; }
  void SetRedoTimeCorr(Bool_t val)             { fRedoTimeCorr = val;           }
  void SetRedoXYCorr(Int_t val)                { fRedoXYCorr = val;             }
  void SetFineCorr(Bool_t val)                 { fFineCorr = val;               }
  void SetNHitsMax(Int_t val)                  { fNHitsMax = val;               }
  void SetNTracksMax(Int_t val)                { fNTracksMax = val;             }
  void SetFillHistograms(Bool_t val)           { fFillHistograms = val;         }
  void SetUseKalmanFilter(Bool_t val)          { fUseKalmanFilter = val;        }
  Bool_t GetFillHistograms()                   { return fFillHistograms;        }

protected:
  template<typename Order>
  void Clusterize(vector<int>, vector<vector<int>>&, double minDist, Order order, int minCont);
  void BuildCandidate(TRecoGigaTrackerCandidate* cand);
  void LinearLeastSquareFit(Double_t *x, Double_t *y, Int_t Nsample, Double_t *sigma,
			    Double_t &a, Double_t &b, Double_t &rho, Double_t &chi2);

  struct TimeOrder{
    TRecoGigaTrackerEvent* fGigaTrackerEvent;
    explicit TimeOrder(TRecoGigaTrackerEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return h1->GetTime() < h2->GetTime();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetTime() - h2->GetTime());
    }
  };

  struct XOrder{
    TRecoGigaTrackerEvent* fGigaTrackerEvent;
    explicit XOrder(TRecoGigaTrackerEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return h1->GetPosition().X() < h2->GetPosition().X();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetPosition().X() - h2->GetPosition().X());
    }
  };

  struct YOrder{
    TRecoGigaTrackerEvent* fGigaTrackerEvent;
    explicit YOrder(TRecoGigaTrackerEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return h1->GetPosition().Y() < h2->GetPosition().Y();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetPosition().Y() - h2->GetPosition().Y());
    }
  };

  struct StationOrder{
    TRecoGigaTrackerEvent* fGigaTrackerEvent;
    explicit StationOrder(TRecoGigaTrackerEvent* p) : fGigaTrackerEvent(p) {};
    bool operator() (int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return h1->GetStationNo()<h2->GetStationNo();
    }
    double dist(int i, int j) {
      TRecoGigaTrackerHit* h1 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
      TRecoGigaTrackerHit* h2 = static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(j));
      return fabs(h1->GetStationNo()-h2->GetStationNo());
    }
  };

  struct Cluster {
    double X,Y,Z,T;
    int N,S;
    vector<int> hits;
    TRecoGigaTrackerEvent* fGigaTrackerEvent;
    explicit Cluster(TRecoGigaTrackerEvent* p): X(0),Y(0),Z(0),T(0),N(0),S(0), fGigaTrackerEvent(p) { };

    void add(int i) {
      TRecoGigaTrackerHit* h= static_cast<TRecoGigaTrackerHit*>(fGigaTrackerEvent->GetHit(i));
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

  void InitHistograms();

  Int_t    fRunID;
  Bool_t   fHistogramsBooked;
  Double_t fTimeWindow;
  Double_t fTimeWindowWrtReference; ///< Half-width of time window wrt reference time in which hits are considered
  Int_t    fNHitsMax;               ///< Max number of in-time hits allowed to run the reconstruction
  Int_t    fNTracksMax;             ///< Max number of tracks to be built
  Double_t fXWindow;
  Double_t fYWindow;
  Double_t fChi2X;
  Double_t fChi2Y;
  Double_t fChi2T;                  ///< chi2 cut value

  GigaTrackerParameterTools * fParameters;

  Double_t fClight;
  TString  fTZeroFile, fTWalkFile;

  Float_t fT0[3][18000];
  TGraph* fTW[3][10];

  TVector3 fPosOff[3];      ///< (x,y) alignment constants of the stations [mm]
  Double_t fA, fB, fC, fD;  ///< Fine alignment parameters [x10^6]
  Double_t fMomentumScale;  ///< Momentum scale correction
  Bool_t   fRedoTimeCorr;   ///< Redo time corrections? Default=false.
  Int_t    fRedoXYCorr;     ///< 0: use alignment from NA62Reco; 1: redo alignment; 2: use raw hit positions
  Bool_t   fFineCorr;       ///< Are fine corrections (momentum scale, rotation) applied?
  Bool_t   fFillHistograms; ///< Fill the monitoring histograms? Default=false.
  Bool_t   fUseKalmanFilter; ///< Use Kalman filter instead of default algorithm. Default=false.
};

#endif
