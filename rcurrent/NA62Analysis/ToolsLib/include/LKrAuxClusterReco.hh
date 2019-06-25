// ---------------------------------------------------------------
// History:
//
// Created by Riccardo Fantechi (fantechi@cern.ch) 2016-07-05
// from a previous set of methods by G. Ruggiero
// Adaptation for NA62Analysis: E. Goudzovski, October 2016
//
// ---------------------------------------------------------------

#ifndef LKrAuxClusterReco_HH
#define LKrAuxClusterReco_HH 1

#include "Persistency.hh"
#include "TRecoLKrCandidate.hh"

#define MAXCLUSTERNO 20
using namespace std;

struct hit {
  Int_t iread;
  Int_t x;
  Int_t y;
  Double_t Ecells;
  Double_t Tcells;
  Double_t Xcells;
  Double_t Ycells;
};

class LKrAuxClusterReco {

public:

  LKrAuxClusterReco();
  ~LKrAuxClusterReco();
  void Clear();
  void PrintParameters();
  void PrintClusters();
  void PrintSummary();

  Bool_t Nearby(std::vector<hit>::iterator, std::vector<hit>::iterator);
  Int_t  FindClusters(Double_t, TRecoLKrEvent*);
  void   ReadCell(Double_t, TRecoLKrEvent*);

  Int_t GetNCells()         { return Ncell;        }
  Int_t GetNClusters()      { return newCluster;   }
  Int_t GetNTotalClusters() { return totalCluster; }
  TRecoLKrCandidate *GetCandidate(Int_t i) { return fNewCandidates[i]; }

  Int_t    GetMaxNClusters ()            { return maxCluster;              }
  Double_t GetfCutClustTrackDist ()      { return fCutClustTrackDist;      }
  Double_t GetfCutClustMatchClustDist () { return fCutClustMatchClustDist; }
  Double_t GetfCutMinHitEnergy ()        { return fCutMinHitEnergy;        }
  Double_t GetfCutMaxHitEnergy ()        { return fCutMaxHitEnergy;        }
  Double_t GetfCutDeltaTime ()           { return fCutDeltaTime;           }
  Double_t GetfCutCellDistance ()        { return fCutCellDistance;        }
  Double_t GetfHitTimeCorrection ()      { return fHitTimeCorrection;      }

  void SetMaxNClusters (Int_t max)
  {if (max<MAXCLUSTERNO) maxCluster = max; else maxCluster = MAXCLUSTERNO;}
  void SetfCutClustTrackDist (Double_t cut)      { fCutClustTrackDist = cut;      }
  void SetfCutClustMatchClustDist (Double_t cut) { fCutClustMatchClustDist = cut; }
  void SetfCutMinHitEnergy (Double_t cut)        { fCutMinHitEnergy = cut;        }
  void SetfCutMaxHitEnergy (Double_t cut)        { fCutMaxHitEnergy = cut;        }
  void SetfCutDeltaTime (Double_t cut)           { fCutDeltaTime = cut;           }
  void SetfCutCellDistance (Double_t cut)        { fCutCellDistance = cut;        }
  void SetfHitTimeCorrection (Double_t cut)      { fHitTimeCorrection = cut;      }

private:

  std::vector<hit> fNewHits;
  TRecoLKrCandidate* fNewCandidates[MAXCLUSTERNO];
  Int_t Ncell;
  Int_t newCluster;
  Int_t maxCluster;
  Int_t totalCluster;
  Double_t fCutClustTrackDist;
  Double_t fCutClustMatchClustDist;
  Double_t fCutMinHitEnergy;
  Double_t fCutMaxHitEnergy;
  Double_t fCutDeltaTime;
  Double_t fCutCellDistance;
  Double_t fHitTimeCorrection;
};
#endif
