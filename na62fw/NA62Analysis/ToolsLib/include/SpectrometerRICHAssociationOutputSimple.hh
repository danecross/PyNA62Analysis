// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-27
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECRICHASSOCIATIONOUTPUTSIMPLE_HH
#define INCLUDE_SPECRICHASSOCIATIONOUTPUTSIMPLE_HH

#include "TMath.h"
#include "TVector2.h"

class SpectrometerRICHAssociationOutputSimple {

public:

  SpectrometerRICHAssociationOutputSimple();
  SpectrometerRICHAssociationOutputSimple(Int_t, Double_t, Double_t, Double_t, Double_t);
  ~SpectrometerRICHAssociationOutputSimple() {}

  Int_t    GetTrackID()             { return fTrackID;             }
  Bool_t   isAssociated()           { return (fRingID>=0);         }
  Int_t    GetRingID()              { return fRingID;              }
  Double_t GetTrackTime()           { return fTrackTime;           }
  Double_t GetTrackMomentum()       { return fTrackMomentum;       }
  Double_t GetExpectedXRingCentre() { return fExpectedXRingCentre; }
  Double_t GetExpectedYRingCentre() { return fExpectedYRingCentre; }
  Double_t GetRadiusMargin()        { return fRadiusMargin;        }

  Double_t GetRingExpectedRadiusEl() { return fRingExpectedRadiusEl; }
  Double_t GetRingExpectedRadiusMu() { return fRingExpectedRadiusMu; }
  Double_t GetRingExpectedRadiusPi() { return fRingExpectedRadiusPi; }
  Double_t GetRingAcceptanceEl()     { return fRingAcceptanceEl;     }
  Double_t GetRingAcceptanceMu()     { return fRingAcceptanceMu;     }
  Double_t GetRingAcceptancePi()     { return fRingAcceptancePi;     }

  Double_t GetMaxMeanNHits()     { return fMaxMeanNHits;     }
  Double_t GetNExpectedHitsEl()  { return fNExpectedHitsEl;  }
  Double_t GetNExpectedHitsMu()  { return fNExpectedHitsMu;  }
  Double_t GetNExpectedHitsPi()  { return fNExpectedHitsPi;  }
  Int_t    GetNObservedHitsEl()  { return fNObservedHitsEl;  }
  Int_t    GetNObservedHitsMu()  { return fNObservedHitsMu;  }
  Int_t    GetNObservedHitsPi()  { return fNObservedHitsPi;  }

  Double_t GetPvalueEl()         { return fPvalueEl;         }
  Double_t GetPvalueMu()         { return fPvalueMu;         }
  Double_t GetPvaluePi()         { return fPvaluePi;         }

  Double_t GetMinDistanceTrackRing()  { return fMinDistanceTrackRing;  }
  Double_t GetMinXDistanceTrackRing() { return fMinXDistanceTrackRing; }
  Double_t GetMinYDistanceTrackRing() { return fMinYDistanceTrackRing; }

  TVector2 GetRingPosition()     { return fRingPosition;     }
  Double_t GetRingRadius()       { return fRingRadius;       }
  Double_t GetRingChi2()         { return fRingChi2;         }
  Double_t GetRingTime()         { return fRingTime;         }
  Double_t GetRingRadiusWrtEl()  { return fRingRadiusWrtEl;  }
  Double_t GetRingRadiusWrtMu()  { return fRingRadiusWrtMu;  }
  Double_t GetRingRadiusWrtPi()  { return fRingRadiusWrtPi;  }

  Bool_t   GetIsElectron1()      { return fIsElectron1;      }
  Bool_t   GetIsMuon1()          { return fIsMuon1;          }
  Bool_t   GetIsPion1()          { return fIsPion1;          }
  Bool_t   GetIsElectron2()      { return fIsElectron2;      }
  Bool_t   GetIsMuon2()          { return fIsMuon2;          }
  Bool_t   GetIsPion2()          { return fIsPion2;          }

  void SetTrackID(Int_t val)                { fTrackID = val;             }
  void SetRingID(Int_t val)                 { fRingID = val;              }
  void SetTrackMomentum(Double_t val)       { fTrackMomentum = val;       }
  void SetTrackTime(Double_t val)           { fTrackTime = val;           }
  void SetExpectedXRingCentre(Double_t val) { fExpectedXRingCentre = val; }
  void SetExpectedYRingCentre(Double_t val) { fExpectedYRingCentre = val; }
  void SetRadiusMargin(Double_t val)        { fRadiusMargin = val;        }

  void SetRingExpectedRadiusEl(Double_t val) { fRingExpectedRadiusEl = val; }
  void SetRingExpectedRadiusMu(Double_t val) { fRingExpectedRadiusMu = val; }
  void SetRingExpectedRadiusPi(Double_t val) { fRingExpectedRadiusPi = val; }
  void SetRingAcceptanceEl(Double_t val)     { fRingAcceptanceEl = val;     }
  void SetRingAcceptanceMu(Double_t val)     { fRingAcceptanceMu = val;     }
  void SetRingAcceptancePi(Double_t val)     { fRingAcceptancePi = val;     }

  void SetMaxMeanNHits   (Double_t val)  { fMaxMeanNHits    = val;  }
  void SetNExpectedHitsEl(Double_t val)  { fNExpectedHitsEl = val;  }
  void SetNExpectedHitsMu(Double_t val)  { fNExpectedHitsMu = val;  }
  void SetNExpectedHitsPi(Double_t val)  { fNExpectedHitsPi = val;  }
  void SetNObservedHitsEl(Int_t val)     { fNObservedHitsEl = val;  }
  void SetNObservedHitsMu(Int_t val)     { fNObservedHitsMu = val;  }
  void SetNObservedHitsPi(Int_t val)     { fNObservedHitsPi = val;  }

  void SetMinDistanceTrackRing (Double_t val) { fMinDistanceTrackRing = val;  }
  void SetMinXDistanceTrackRing(Double_t val) { fMinXDistanceTrackRing = val; }
  void SetMinYDistanceTrackRing(Double_t val) { fMinYDistanceTrackRing = val; }
  void SetRingPosition   (TVector2 val)       { fRingPosition = val;          }
  void SetRingRadius     (Double_t val)       { fRingRadius = val;            }
  void SetRingChi2       (Double_t val)       { fRingChi2 = val;              }
  void SetRingTime       (Double_t val)       { fRingTime = val;              }
  void SetRingRadiusWrtEl(Double_t val)       { fRingRadiusWrtEl = val;       }
  void SetRingRadiusWrtMu(Double_t val)       { fRingRadiusWrtMu = val;       }
  void SetRingRadiusWrtPi(Double_t val)       { fRingRadiusWrtPi = val;       } 

  void SetPvalueEl(Double_t val)              { fPvalueEl = val;              }
  void SetPvalueMu(Double_t val)              { fPvalueMu = val;              }
  void SetPvaluePi(Double_t val)              { fPvaluePi = val;              }

  void SetIsElectron1(Bool_t val)             { fIsElectron1 = val;           }
  void SetIsMuon1    (Bool_t val)             { fIsMuon1 = val;               }
  void SetIsPion1    (Bool_t val)             { fIsPion1 = val;               }
  void SetIsElectron2(Bool_t val)             { fIsElectron2 = val;           }
  void SetIsMuon2    (Bool_t val)             { fIsMuon2 = val;               }
  void SetIsPion2    (Bool_t val)             { fIsPion2 = val;               }

  void Clear();
  void Print();

private:

  Int_t    fTrackID;             ///< ID of the spectrometer track
  Int_t    fRingID;              ///< ID of the associated RICH ring
  Double_t fTrackTime;           ///< Track time [ns]
  Double_t fTrackMomentum;       ///< Track momentum [MeV]
  Double_t fExpectedXRingCentre; ///< Expected X coordinate of RICH ring centre
  Double_t fExpectedYRingCentre; ///< Expected Y coordinate of RICH ring centre
  Double_t fRadiusMargin;        ///< Half-width of the ring were RICH hits are searched, around the expected ring in a certain mass hypothesis

  Double_t fRingExpectedRadiusEl; ///< Expected ring radius for this track in electron hypothesis
  Double_t fRingExpectedRadiusMu; ///< Expected ring radius for this track in muon hypothesis
  Double_t fRingExpectedRadiusPi; ///< Expected ring radius for this track in pion hypothesis
  Double_t fRingAcceptanceEl;     ///< Approximate expected part of ring in RICH acceptance in electron hypothesis
  Double_t fRingAcceptanceMu;     ///< Approximate expected part of ring in RICH acceptance in muon hypothesis
  Double_t fRingAcceptancePi;     ///< Approximate expected part of ring in RICH acceptance in pion hypothesis

  Double_t fMaxMeanNHits;    ///< Expected mean number of hits for a ring fully in acceptance for beta=1
  Double_t fNExpectedHitsEl; ///< Expected mean number of hits produced by the track in electron hypothesis
  Double_t fNExpectedHitsMu; ///< Expected mean number of hits produced by the track in muon hypothesis
  Double_t fNExpectedHitsPi; ///< Expected mean number of hits produced by the track in pion hypothesis
  Int_t    fNObservedHitsEl; ///< Observed number of hits within +-fRadiusMargin of the expected electron ring
  Int_t    fNObservedHitsMu; ///< Observed number of hits within +-fRadiusMargin of the expected muon ring
  Int_t    fNObservedHitsPi; ///< Observed number of hits within +-fRadiusMargin of the expected pion ring

  Double_t fPvalueEl; ///< Approximate P-value of the track being an electron, based on RICH hit distribution
  Double_t fPvalueMu; ///< Approximate P-value of the track being a muon, based on RICH hit distribution
  Double_t fPvaluePi; ///< Approximate P-value of the track being a pion, based on RICH hit distribution

  Double_t fMinDistanceTrackRing;  ///< Distance between expected rung centre and the closest reconstruction ring centre
  Double_t fMinXDistanceTrackRing; ///< X projection of fMinDistanceTrackRing
  Double_t fMinYDistanceTrackRing; ///< Y projection of fMinDistanceTrackRing

  TVector2 fRingPosition;    ///< Positon of the ring associated to the track [mm]
  Double_t fRingRadius;      ///< Radius of the ring associated to the track [mm]
  Double_t fRingChi2;        ///< Chi2 of the ring associated to the track
  Double_t fRingTime;        ///< Time of the ring associated to the track [ns]
  Double_t fRingRadiusWrtEl; ///< Difference of the associated ring radius and expected electron ring radius
  Double_t fRingRadiusWrtMu; ///< Difference of the associated ring radius and expected muon ring radius
  Double_t fRingRadiusWrtPi; ///< Difference of the associated ring radius and expected pion ring radius

  Bool_t   fIsElectron1; ///< Is the track compatible with being an electron based on track-ring association?
  Bool_t   fIsMuon1;     ///< Is the track compatible with being a muon based on track-ring association?
  Bool_t   fIsPion1;     ///< Is the track compatible with being a pion based on track-ring association?
  Bool_t   fIsElectron2; ///< Is the track compatible with being an electron based on track-hit association?
  Bool_t   fIsMuon2;     ///< Is the track compatible with being a muon based on track-hit association?
  Bool_t   fIsPion2;     ///< Is the track compatible with being a pion based on track-hit association?
};

#endif
