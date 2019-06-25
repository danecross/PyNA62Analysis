// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-01-28
//
// ---------------------------------------------------------

#ifndef SPECCHANTIASSOCIATIONRECORD_HH
#define SPECCHANTIASSOCIATIONRECORD_HH 1

#include "NA62Global.hh"
#include "TVector2.h"
#include "TVector3.h"
#include "TRecoCHANTICandidate.hh"

class SpectrometerCHANTIAssociationRecord {

public:

  SpectrometerCHANTIAssociationRecord();
  SpectrometerCHANTIAssociationRecord(Int_t,TRecoCHANTICandidate*);

  virtual ~SpectrometerCHANTIAssociationRecord() {}

  Int_t    GetCandidateID()                          { return fCandidateID;            }
  TRecoCHANTICandidate* GetCHANTICandidate()         { return fCHANTICandidate;        }
  Double_t GetTrackCandidateDistance()               { return fTrackCandidateDistance; }

  void SetCandidateID(Int_t val)                     { fCandidateID = val;             }
  void SetCHANTICandidate(TRecoCHANTICandidate* val) { fCHANTICandidate = val;         }
  void SetTrackCandidateDistance(Double_t val)       { fTrackCandidateDistance = val;  }

  void Clear();
  void Print() const;

private:

  Int_t    fCandidateID;                  ///< ID of the associated CHANTI candidate, -1 if no association
  TRecoCHANTICandidate* fCHANTICandidate; ///< Pointer to the associated CHANTI candidate, 0 if no association
  Double_t fTrackCandidateDistance;       ///< Distance from CHANTI candidate to track extrapolation at CHANTI front plane [mm]: -1 if no association
};

#endif
