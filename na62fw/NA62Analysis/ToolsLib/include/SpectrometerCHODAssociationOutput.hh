// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// Adopted for the CHOD by Viacheslav Duk (Viacheslav.Duk@cern.ch) 26.02.2016
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECTROMETERCHODASSOCIATIONCONTAINER_HH
#define INCLUDE_SPECTROMETERCHODASSOCIATIONCONTAINER_HH 1

#include "TMath.h"
#include "SpectrometerCHODAssociationRecord.hh"

class SpectrometerCHODAssociationOutput {

public:

  SpectrometerCHODAssociationOutput();
  explicit SpectrometerCHODAssociationOutput(Int_t);
  ~SpectrometerCHODAssociationOutput() {}

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
  Bool_t   GetShowerFlag();

  Int_t    GetNAssociationRecords()  { return fAssociationRecordContainer.size();     }
  Bool_t   isAssociated()            { return (fAssociationRecordContainer.size()>0); }
  SpectrometerCHODAssociationRecord* GetAssociationRecord(Int_t);
  SpectrometerCHODAssociationRecord* GetBestAssociationRecord()
  { return GetAssociationRecord(fBestAssociationRecordID); }

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
  void SetShowerFlag(Bool_t);

  void SetBestAssociationRecordID(Int_t val) { fBestAssociationRecordID = val;  }
  void AddAssociationRecord(SpectrometerCHODAssociationRecord Rec)
  { fAssociationRecordContainer.push_back(Rec); }
  void RemoveAssociationRecord(Int_t);
  void Clear();
  void Print() const;

private:

  Int_t    fTrackID;        ///< ID of the spectrometer track
  Double_t fTrackTime;      ///< Track time [ns]
  Double_t fTrackMomentum;  ///< Track momentum [MeV]
  Double_t fSearchRadius;   ///< CHOD candidate tile search radius around the extrapolated track
  Double_t fTrackX;         ///< X coordinate of track extrapolated to CHOD tile front plane
  Double_t fTrackY;         ///< Y coordinate of track extrapolated to CHOD tile front plane
  Bool_t   fInAcceptance;   ///< Inside or outside CHOD geometrical acceptance?
  Double_t fDistanceToEdge; ///< Distance to outer CHOD edge
  Bool_t   fShowerFlag;     ///< Flag for shower-like events (association for them is done with the TimeCandidate)
  Int_t    fBestAssociationRecordID; ///< ID of the closest associated track record: -1 if no association
  std::vector<SpectrometerCHODAssociationRecord> fAssociationRecordContainer; ///< Records of matching CHOD candidates
};

#endif
