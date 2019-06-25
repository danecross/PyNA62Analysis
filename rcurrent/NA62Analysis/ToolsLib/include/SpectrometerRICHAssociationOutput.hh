#ifndef SpectrometerRICHAssociationOutput_HH
#define SpectrometerRICHAssociationOutput_HH

#define MaxHypos 5
#define kRICHHypothesisBackground 0
#define kRICHHypothesisElectron   1
#define kRICHHypothesisMuon       2
#define kRICHHypothesisPion       3
#define kRICHHypothesisKaon       4
#define kRICHHypothesisMultiple  99

#include "TMath.h"
#include "TVector2.h"
#include <vector>
#include <map>

#include "TRecoRICHHit.hh"
class SpectrometerRICHAssociationOutput {

public:

  SpectrometerRICHAssociationOutput();
  ~SpectrometerRICHAssociationOutput() {}

  Int_t    GetTrackID()                        { return fTrackID;                }
  Bool_t   isValid()                           { return fValid;                  }
  Double_t GetLikelihood(Int_t i)              { return fLikelihood[i];          }
  Int_t    GetMostLikelyHypothesis()           { return fMostLikelyHypothesis;   }
  Int_t    GetTrackMirrorID()                  { return fTrackMirrorID;          }
  TVector2 GetTrackPosOnMirror()               { return fTrackPosOnMirror;       }
  Double_t GetDistToMirrorCentre()             { return fDistToMirrorCentre;     }
  TVector2 GetPredictedCentre()                { return fPredictedCentre;        }
  TVector2 GetPredictedCentreJura()            { return fPredictedCentreJura;    }
  TVector2 GetPredictedCentreSaleve()          { return fPredictedCentreSaleve;  }
  Double_t GetPredictedRadius(Int_t i)         { return fPredictedRadius[i];     }
  Double_t GetMirrorFraction(Int_t i, Int_t j) { return fMirrorFraction[i][j];   }
  Double_t GetPMTFraction(Int_t i, Int_t j)    { return fPMTFraction[i][j];      }
  Double_t GetTrackTimeForAssociation()        { return fTrackTimeForAssociation;}
  Int_t    GetNInTimeHits()                    { return fNInTimeHits;            }
  Int_t    GetNOutOfTimeHits()                 { return fNOutOfTimeHits;         }
  Int_t    GetNObservedHits()                  { return fNObservedHits;          }
  Int_t    GetNBackgroundHits()                { return fNBackgroundHits;        }
  Double_t GetNExpectedSignalHits(Int_t i)     { return fNExpectedSignalHits[i]; }
  Int_t    GetNHitsAssigned(Int_t i)           { return fNHitsAssigned[i];       }
  Double_t GetRingTime(Int_t i)                { return fRingTime[i];            }
  TVector2 GetRingCentre()                     { return fRingCentre;             }
  TVector2 GetRingCentreError()                { return fRingCentreError;        }
  Double_t GetRingRadius()                     { return fRingRadius;             }
  Double_t GetRingRadiusError()                { return fRingRadiusError;        }
  Double_t GetRingFitChi2()                    { return fRingFitChi2;            }

  void SetTrackID(Int_t val)                { fTrackID = val;              }
  void SetValid(Bool_t val)                 { fValid = val;                }
  void SetLikelihood(Int_t i, Double_t val) { fLikelihood[i] = val;        }
  void SetMostLikelyHypothesis(Int_t val)   { fMostLikelyHypothesis = val; }
  void SetTrackMirrorID(Int_t val)          { fTrackMirrorID = val;        }
  void SetTrackPosOnMirror(TVector2 val)    { fTrackPosOnMirror = val;     }
  void SetTrackPosOnMirror(Double_t x, Double_t y)
  { fTrackPosOnMirror = TVector2(x,y); }
  void SetDistToMirrorCentre(Double_t val)  { fDistToMirrorCentre = val;   }
  void SetPredictedCentre(TVector2 val)     { fPredictedCentre = val;      }
  void SetPredictedCentre(Double_t x, Double_t y)
  { fPredictedCentre = TVector2(x,y); }
  void SetPredictedCentreJura(TVector2 val) { fPredictedCentreJura = val;  }
  void SetPredictedCentreJura(Double_t x, Double_t y)
  { fPredictedCentreJura = TVector2(x,y); }
  void SetPredictedCentreSaleve(TVector2 val) { fPredictedCentreSaleve = val; }
  void SetPredictedCentreSaleve(Double_t x, Double_t y)
  { fPredictedCentreSaleve = TVector2(x,y); }
  void SetPredictedRadius(Int_t i, Double_t val)         { fPredictedRadius[i] = val;        }
  void SetMirrorFraction(Int_t i, Int_t j, Double_t val) { fMirrorFraction[i][j] = val;      }
  void SetPMTFraction(Int_t i, Int_t j, Double_t val)    { fPMTFraction[i][j] = val;         }
  void SetTrackTimeForAssociation(Double_t val)          { fTrackTimeForAssociation = val;   }
  void SetNInTimeHits(Int_t val)                         { fNInTimeHits = val;               }
  void SetNOutOfTimeHits(Int_t val)                      { fNOutOfTimeHits = val;            }
  void SetNObservedHits(Int_t val)                       { fNObservedHits = val;             }
  void SetNBackgroundHits(Int_t val)                     { fNBackgroundHits = val;           }
  void SetNExpectedSignalHits(Int_t i, Double_t val)     { fNExpectedSignalHits[i] = val;    }
  void SetNHitsAssigned(Int_t i, Double_t val)           { fNHitsAssigned[i] = val;          }
  void SetRingTime(Int_t i, Double_t val)                { fRingTime[i] = val;               }
  void SetRingCentre(TVector2 val)                       { fRingCentre = val;                }
  void SetRingCentre(Double_t x, Double_t y)             { fRingCentre = TVector2(x,y);      }
  void SetRingCentreError(TVector2 val)                  { fRingCentreError = val;           }
  void SetRingCentreError(Double_t x, Double_t y)        { fRingCentreError = TVector2(x,y); }
  void SetRingRadius(Double_t val)                       { fRingRadius = val;                }
  void SetRingRadiusError(Double_t val)                  { fRingRadiusError = val;           }
  void SetRingFitChi2(Double_t val)                      { fRingFitChi2 = val;               }

  void AssignHit(int, TRecoRICHHit*);
  std::vector<TRecoRICHHit*> GetAssignedHits(int);
  void Clear();
  void Print();

private:

  Int_t    fTrackID;               ///< ID of the spectrometer track
  Bool_t   fValid;                 ///< There is at least one RICH hit in the event
  Double_t fLikelihood[MaxHypos];  ///< Likelihoods normalized to the largest one, 0<=L[i]<=1
  Int_t    fTrackMirrorID;         ///< Mirror number the track goes through
  TVector2 fTrackPosOnMirror;      ///< Track extrapolated position on mirror
  Double_t fDistToMirrorCentre;    ///< Track extrapolated distance to mirror centre
  TVector2 fPredictedCentre;       ///< Ring centre at the PMT plane
  TVector2 fPredictedCentreJura;   ///< Ring centre at the Jura PMT plane
  TVector2 fPredictedCentreSaleve; ///< Ring centre at the Jura PMT plane

  Double_t fPredictedRadius[MaxHypos];     ///< Predicted ring radii for different hypotheses
  Double_t fMirrorFraction[MaxHypos][2];   ///< Fraction of light on the mirror array: Jura, Saleve
  Double_t fPMTFraction[MaxHypos][2];      ///< Fraction of ring on the PMT array: Jura, Saleve
  Double_t fTrackTimeForAssociation;       ///< Track time used for association
  Int_t    fNInTimeHits;                   ///< Number of RICH hits in time with the track
  Int_t    fNOutOfTimeHits;                ///< Number of out-of-time hits (they are not used for RICH association)
  Int_t    fNObservedHits;                 ///< Number of observed hits compatible to any hypothesis
  Int_t    fNBackgroundHits;               ///< Number of background hits including noise and ring overlaps
  Double_t fNExpectedSignalHits[MaxHypos]; ///< Expected number of signal hits in each hypothesis
  Int_t    fNHitsAssigned[MaxHypos];       ///< Assigned number of hits in each hypothesis
  Double_t fRingTime[MaxHypos];            ///< Ring times computed as averaged times of assigned hits
  Int_t    fMostLikelyHypothesis;          ///< The most likely hypothesis: background, electron, muon, kaon, pion, multiple
  TVector2 fRingCentre;                    ///< Ring centre coordinates for the most likely hypothesis
  TVector2 fRingCentreError;               ///< Errors on the ring centre coordinates for the most likely hypothesis
  Double_t fRingRadius;                    ///< Ring radius for the most likely hypothesis
  Double_t fRingRadiusError;               ///< Error on the ring radius for the most likely hypothesis
  Double_t fRingFitChi2;                   ///< Chi2 of the ring fit for the most likely hypothesis

  std::vector<TRecoRICHHit*> fAssignedHits[MaxHypos]; ///< Hits assigned to a given hypothesis

  // The "geometrical" acceptance of the RICH for a hypothesis is
  // MirrorFraction[ihyp][0]*PMTFraction[ihyp][0] +
  // MirrorFraction[ihyp][1]*PMTFraction[ihyp][1]
};

#endif
