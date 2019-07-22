// ---------------------------------------------------------------
// History:
//
//  Copied from code created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECRICHASSOCIATIONOUTPUTSINGLERING_HH
#define INCLUDE_SPECRICHASSOCIATIONOUTPUTSINGLERING_HH

#include "TMath.h"
#include "TVector2.h"

class SpectrometerRICHAssociationOutputSingleRing {

public:

  SpectrometerRICHAssociationOutputSingleRing();
  SpectrometerRICHAssociationOutputSingleRing(Int_t, Double_t, Double_t);
  ~SpectrometerRICHAssociationOutputSingleRing() {}

  Int_t    GetTrackID()             { return fTrackID;             }
  Bool_t   isAssociated()           { return (fRingID>=0);         }
  Int_t    GetRingID()              { return fRingID;              }
  Double_t GetTrackTime()           { return fTrackTime;           }
  Double_t GetTrackMomentum()       { return fTrackMomentum;       }
 
  Int_t    GetNHits()  { return fNHits;  }

  Double_t GetMinDistanceTrackRing()  { return fMinDistanceTrackRing;  }
  Double_t GetMinXDistanceTrackRing() { return fMinXDistanceTrackRing; }
  Double_t GetMinYDistanceTrackRing() { return fMinYDistanceTrackRing; }

  TVector2 GetRingPosition()     { return fRingPosition;     }
  Double_t GetRingRadius()       { return fRingRadius;       }
  Double_t GetRingChi2()         { return fRingChi2;         }
  Double_t GetRingTime()         { return fRingTime;         }
  Double_t GetMass()             { return fMass;         }
  Double_t GetMass2()             { return fMass2;         }
 
  void SetTrackID(Int_t val)                { fTrackID = val;             }
  void SetRingID(Int_t val)                 { fRingID = val;              }
  void SetTrackMomentum(Double_t val)       { fTrackMomentum = val;       }
  void SetTrackTime(Double_t val)           { fTrackTime = val;           }
 
  void SetNHits(Int_t val)     { fNHits = val;  }

  void SetMinDistanceTrackRing (Double_t val) { fMinDistanceTrackRing = val;  }
  void SetMinXDistanceTrackRing(Double_t val) { fMinXDistanceTrackRing = val; }
  void SetMinYDistanceTrackRing(Double_t val) { fMinYDistanceTrackRing = val; }
  void SetRingPosition   (TVector2 val)       { fRingPosition = val;          }
  void SetRingRadius     (Double_t val)       { fRingRadius = val;            }
  void SetRingChi2       (Double_t val)       { fRingChi2 = val;              }
  void SetRingTime       (Double_t val)       { fRingTime = val;              }
  void SetMass       (Double_t val)           { fMass = val;              }
  void SetMass2       (Double_t val)           { fMass2 = val;              }

  void Clear();
  void Print();

private:
  Int_t    fTrackID;             ///< ID of the spectrometer track
  Int_t    fRingID;              ///< ID of the associated RICH ring
  Double_t fTrackTime;           ///< Track time [ns]
  Double_t fTrackMomentum;       ///< Track momentum [MeV]
  Int_t fNHits;
  Double_t fMinDistanceTrackRing;  ///< Distance between expected rung centre and the closest reconstruction ring centre
  Double_t fMinXDistanceTrackRing; ///< X projection of fMinDistanceTrackRing
  Double_t fMinYDistanceTrackRing; ///< Y projection of fMinDistanceTrackRing
  TVector2 fRingPosition;    ///< Positon of the ring associated to the track [mm]
  Double_t fRingRadius;      ///< Radius of the ring associated to the track [mm]
  Double_t fRingChi2;        ///< Chi2 of the ring associated to the track
  Double_t fRingTime;        ///< Time of the ring associated to the track [ns]
  Double_t fMass;        ///< Reconstructed particle mass [MeV]
  Double_t fMass2;       ///< Reconstructed particle squared mass [MeV^2]
 
};

#endif
