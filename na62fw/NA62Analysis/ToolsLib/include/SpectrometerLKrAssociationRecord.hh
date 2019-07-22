// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-07
//
// ---------------------------------------------------------

#ifndef INCLUDE_SPECLKRASSOCIATIONRECORD_HH 
#define INCLUDE_SPECLKRASSOCIATIONRECORD_HH 1

#include "NA62Global.hh"
#include "TRecoLKrCandidate.hh"

class SpectrometerLKrAssociationRecord {

public:

  SpectrometerLKrAssociationRecord();
  SpectrometerLKrAssociationRecord(Int_t, TRecoLKrCandidate*);

  virtual ~SpectrometerLKrAssociationRecord() {}

  Int_t    GetClusterID()                      { return fClusterID;             }
  TRecoLKrCandidate* GetLKrCandidate()         { return fLKrCandidate;          }
  Double_t GetEoP()                            { return fEoP;                   }
  Double_t GetTrackClusterDistance()           { return fTrackClusterDistance;  }

  void SetClusterID(Int_t val)                 { fClusterID = val;              }
  void SetLKrCandidate(TRecoLKrCandidate* val) { fLKrCandidate = val;           }
  void SetEoP(Double_t val)                    { fEoP = val;                    }
  void SetTrackClusterDistance(Double_t val)   { fTrackClusterDistance = val;   }

  void Clear();
  void Print() const;

private:

  Int_t    fClusterID;              ///< ID of the associated LKr cluster, -1 if no association
  TRecoLKrCandidate* fLKrCandidate; ///< Pointer to the associated LKr candidate, 0 if no association
  Double_t fEoP;                    ///< Track energy-to-momentum ratio (E/p): -1e28 if no association
  Double_t fTrackClusterDistance;   ///< Distance from track extrapolation to cluster in LKr front plane [mm]: -1 if no association
};

#endif
