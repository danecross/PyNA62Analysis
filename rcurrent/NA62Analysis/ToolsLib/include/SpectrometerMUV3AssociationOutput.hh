// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECMUV3ASSOCIATIONOUTPUT_HH
#define INCLUDE_SPECMUV3ASSOCIATIONOUTPUT_HH 1

#include "TMath.h"
#include "SpectrometerMUV3AssociationRecord.hh"

class SpectrometerMUV3AssociationOutput {

public:

  SpectrometerMUV3AssociationOutput();
  explicit SpectrometerMUV3AssociationOutput(Int_t);
  ~SpectrometerMUV3AssociationOutput() {}

  Int_t    GetTrackID()        { return fTrackID;                   }
  Double_t GetTrackTime()      { return fTrackTime;                 }
  Double_t GetTrackMomentum()  { return fTrackMomentum;             }
  Double_t GetSearchRadius()   { return fSearchRadius;              }
  Double_t GetTrackX()         { return fTrackX;                    }
  Double_t GetTrackY()         { return fTrackY;                    }
  TVector2 GetTrackXY()        { return TVector2(fTrackX, fTrackY); }
  Bool_t   GetInAcceptance()   { return fInAcceptance;              }
  Double_t GetDistanceToEdge() { return fDistanceToEdge;            }
  Double_t GetMinimumTrackTileDistance();
  Double_t GetMinimumTrackCandidateDistance();

  Int_t    GetNAssociationRecords()  { return fAssociationRecordContainer.size();     }
  Bool_t   isAssociated()            { return (fAssociationRecordContainer.size()>0); }
  SpectrometerMUV3AssociationRecord* GetAssociationRecord(Int_t);

  void SetTrackID(Int_t val)           { fTrackID = val;        }
  void SetTrackMomentum(Int_t val)     { fTrackMomentum = val;  }
  void SetTrackTime(Int_t val)         { fTrackTime = val;      }
  void SetSearchRadius(Double_t val)   { fSearchRadius = val;   }
  void SetTrackX(Double_t val)         { fTrackX = val;         }
  void SetTrackY(Double_t val)         { fTrackY = val;         }
  void SetTrackXY(Double_t, Double_t);
  void SetTrackXY(TVector2);
  void SetInAcceptance(Bool_t val)     { fInAcceptance = val;   }
  void SetDistanceToEdge(Double_t val) { fDistanceToEdge = val; }

  void AddAssociationRecord(SpectrometerMUV3AssociationRecord Rec)
  { fAssociationRecordContainer.push_back(Rec); }
  void Clear();
  void Print();

private:

  Int_t    fTrackID;        ///< ID of the spectrometer track
  Double_t fTrackTime;      ///< Track time [ns]
  Double_t fTrackMomentum;  ///< Track momentum [MeV]
  Double_t fSearchRadius;   ///< MUV3 candidate tile search radius around the extrapolated track
  Double_t fTrackX;         ///< X coordinate of track extrapolated to MUV3 tile front plane
  Double_t fTrackY;         ///< Y coordinate of track extrapolated to MUV3 tile front plane
  Bool_t   fInAcceptance;   ///< Inside or outside MUV3 geometrical acceptance?
  Double_t fDistanceToEdge; ///< Distance to outer MUV3 edge
  std::vector<SpectrometerMUV3AssociationRecord> fAssociationRecordContainer; ///< Records of matching MUV3 candidates
};

#endif
