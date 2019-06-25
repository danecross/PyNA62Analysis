// ---------------------------------------------------------------
// History:
//
//  Copied from code created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECRICHASSOCIATIONOUTPUTTRACKCENTREDRING_HH
#define INCLUDE_SPECRICHASSOCIATIONOUTPUTTRACKCENTREDRING_HH

#include "TMath.h"
#include "TVector2.h"

class SpectrometerRICHAssociationOutputTrackCentredRing {

public:

  SpectrometerRICHAssociationOutputTrackCentredRing();
  SpectrometerRICHAssociationOutputTrackCentredRing(Int_t, Double_t, Double_t);
  ~SpectrometerRICHAssociationOutputTrackCentredRing() {}

  Bool_t   isAssociated()           { return (fRingID>=0);         }

  Int_t    GetRingID()              { return fRingID;              }
  Int_t    GetNHits()  { return fNHits;  }
  TVector2 GetRingPosition()     { return fRingPosition;     }
  Double_t GetRingRadius()       { return fRingRadius;       }
  Double_t GetRingChi2()         { return fRingChi2;         }
  Double_t GetRingTime()         { return fRingTime;         }
  Double_t GetMass()             { return fMass;         }
 
  void SetRingID(Int_t val)                 { fRingID = val;              }
  void SetNHits(Int_t val)     { fNHits = val;  }
  void SetRingPosition   (TVector2 val)       { fRingPosition = val;          }
  void SetRingRadius     (Double_t val)       { fRingRadius = val;            }
  void SetRingChi2       (Double_t val)       { fRingChi2 = val;              }
  void SetRingTime       (Double_t val)       { fRingTime = val;              }
  void SetMass       (Double_t val)           { fMass = val;              }
 
  void Clear();
  void Print();

private:
  Int_t    fTrackID;             ///< ID of the spectrometer track
  Double_t fTrackTime;           ///< Track time [ns]
  Double_t fTrackMomentum;       ///< Track momentum [MeV]
  Int_t fRingID;              ///< ID of the associated RICH ring
  Int_t fNHits;
  TVector2 fRingPosition;    ///< Positon of the ring associated to the track [mm]
  Double_t fRingRadius;      ///< Radius of the ring associated to the track [mm]
  Double_t fRingChi2;        ///< Chi2 of the ring associated to the track
  Double_t fRingTime;        ///< Time of the ring associated to the track [ns]
  Double_t fMass;        ///< Reconstructed particle mass []
 
};

#endif
