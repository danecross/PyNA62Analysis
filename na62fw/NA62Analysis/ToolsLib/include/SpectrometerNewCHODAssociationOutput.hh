// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-13
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECNEWCHODASSOCIATIONOUTPUT_HH
#define INCLUDE_SPECNEWCHODASSOCIATIONOUTPUT_HH 1

#include "TMath.h"
#include "SpectrometerNewCHODAssociationRecord.hh"

class SpectrometerNewCHODAssociationOutput {

public:

  SpectrometerNewCHODAssociationOutput();
  explicit SpectrometerNewCHODAssociationOutput(Int_t);
  ~SpectrometerNewCHODAssociationOutput() {}

  Int_t    GetTrackID()        { return fTrackID;                   }
  Double_t GetTrackTime()      { return fTrackTime;                 }
  Double_t GetTrackMomentum()  { return fTrackMomentum;             }
  Double_t GetSearchRadius()   { return fSearchRadius;              }
  Double_t GetTrackX()         { return fTrackX;                    }
  Double_t GetTrackY()         { return fTrackY;                    }
  TVector2 GetTrackXY()        { return TVector2(fTrackX, fTrackY); }
  Bool_t   GetInAcceptance()   { return fInAcceptance;              }
  Double_t GetMinimumTrackTileDistance();
  Double_t GetMinimumTrackRecoHitDistance();

  Int_t    GetNAssociationRecords()  { return fAssociationRecordContainer.size();     }
  Bool_t   isAssociated()            { return (fAssociationRecordContainer.size()>0); }
  SpectrometerNewCHODAssociationRecord* GetAssociationRecord(Int_t);
  SpectrometerNewCHODAssociationRecord* GetBestAssociationRecord()
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

  void AddAssociationRecord(SpectrometerNewCHODAssociationRecord Rec)
  { fAssociationRecordContainer.push_back(Rec); }

  void  SetBestAssociationRecordID(Int_t val) { fBestAssociationRecordID = val;  }
  Int_t GetBestAssociationRecordID()          { return fBestAssociationRecordID; }

  void Clear();
  void Print();

private:

  Int_t    fTrackID;       ///< ID of the spectrometer track
  Double_t fTrackTime;     ///< Track time [ns]
  Double_t fTrackMomentum; ///< Track momentum [MeV]
  Double_t fSearchRadius;  ///< NewCHOD candidate tile search radius around the extrapolated track
  Double_t fTrackX;        ///< X coordinate of track extrapolated to NewCHOD tile front plane
  Double_t fTrackY;        ///< Y coordinate of track extrapolated to NewCHOD tile front plane
  Bool_t   fInAcceptance;  ///< Inside or outside NewCHOD geometrical acceptance?
  std::vector<SpectrometerNewCHODAssociationRecord> fAssociationRecordContainer; ///< Records of matching NewCHOD candidates
  Int_t   fBestAssociationRecordID; ///< ID of the record corresponding to the spatially closest candidate
};

#endif
