// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// Adopted for the CHOD by Viachelslav Duk (Viacheslav.Duk@cern.ch) 26.02.2016
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECTROMETERCHODASSOCIATIONRECORD_HH 
#define INCLUDE_SPECTROMETERCHODASSOCIATIONRECORD_HH 1

#include "NA62Global.hh"
#include "TVector2.h"
#include "TVector3.h"
#include "TRecoCHODCandidate.hh"

class SpectrometerCHODAssociationRecord {
  
public:

  SpectrometerCHODAssociationRecord();
  SpectrometerCHODAssociationRecord(Int_t, TRecoCHODCandidate*, Int_t, Int_t,
				    Double_t, Double_t, Double_t, Double_t, Double_t, Bool_t);
  SpectrometerCHODAssociationRecord(Int_t, TRecoCHODCandidate*, Int_t, Int_t,
				    Double_t, Double_t, Double_t, TVector2, Bool_t);

  virtual ~SpectrometerCHODAssociationRecord() {}

  TRecoCHODCandidate* GetCHODCandidate() { return fCHODCandidate;          }
  Int_t    GetCHODCandidateID()          { return fCHODCandidateID;        }
  Int_t    GetIDVHit()                   { return fIDVHit;                 }
  Int_t    GetIDHHit()                   { return fIDHHit;                 }
  Double_t GetCHODCandidateTime()        { return fCHODCandidateTime;      }
  Double_t GetTrackTileDistance()        { return fTrackTileDistance;      }
  Double_t GetTrackCandidateDistance(  ) { return fTrackCandidateDistance; }
  Bool_t   GetDirectMatch()              { return fDirectMatch;            }
  Double_t GetCHODCandidateX()           { return fCHODCandidateX;         }
  Double_t GetCHODCandidateY()           { return fCHODCandidateY;         }
  TVector2 GetCHODCandidateXY()
  { return TVector2(fCHODCandidateX, fCHODCandidateY); }
  TVector3 GetCHODCandidatePosition()
  { return TVector3(fCHODCandidateX, fCHODCandidateY, 239009.0); }

  void SetCHODCandidateID(Int_t val)           { fCHODCandidateID = val;        }
  void SetIDVHit(Int_t val)                    { fIDVHit = val;                 }
  void SetIDHHit(Int_t val)                    { fIDHHit = val;                 }
  void SetCHODCandidateTime(Double_t val)      { fCHODCandidateTime = val;      }
  void SetTrackTileDistance(Double_t val)      { fTrackTileDistance = val;      }
  void SetTrackCandidateDistance(Double_t val) { fTrackCandidateDistance = val; }
  void SetCHODCandidateX(Double_t val)         { fCHODCandidateX = val;         }
  void SetCHODCandidateY(Double_t val)         { fCHODCandidateY = val;         }
  void SetCHODCandidateXY(Double_t x, Double_t y)
  { fCHODCandidateX = x; fCHODCandidateY = y; }
  void SetCHODCandidateXY(TVector2 val)
  { fCHODCandidateX = val.X(); fCHODCandidateY = val.Y(); }

  void Clear();
  void Print() const;

private:

  Int_t    fCHODCandidateID;        ///< ID of the associated CHOD candidate
  TRecoCHODCandidate* fCHODCandidate; ///< Pointer to the associated CHOD candidate, 0 if no association
  Int_t    fIDVHit;                 ///< CHOD V-hit ID
  Int_t    fIDHHit;                 ///< CHOD H-hit ID
  Double_t fCHODCandidateTime;      ///< CHOD candidate time
  Double_t fTrackTileDistance;      ///< Distance between extrapolated track and tile boundary (0 if inside tile)
  Double_t fTrackCandidateDistance; ///< Distance between extrapolated track and centre of CHOD tile
  Double_t fCHODCandidateX;         ///< Centre of CHOD tile: X coordinate
  Double_t fCHODCandidateY;         ///< Centre of CHOD tile: Y coordinate
  Bool_t   fDirectMatch;            ///< Does extrapolated track point to teh slab intersection?
};

#endif
