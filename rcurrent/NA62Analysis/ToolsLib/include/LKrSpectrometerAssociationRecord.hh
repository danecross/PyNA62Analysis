// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#ifndef LKRSPECASSOCIATIONRECORD_HH
#define LKRSPECASSOCIATIONRECORD_HH 1

#include "NA62Global.hh"
#include "TVector2.h"
#include "TVector3.h"
#include "TRecoSpectrometerCandidate.hh"

class LKrSpectrometerAssociationRecord {

public:

  LKrSpectrometerAssociationRecord();
  LKrSpectrometerAssociationRecord(Int_t,TRecoSpectrometerCandidate*);

  virtual ~LKrSpectrometerAssociationRecord() {}

  Int_t    GetTrackID()                                  { return fTrackID;               }
  TRecoSpectrometerCandidate* GetSpectrometerCandidate() { return fSpectrometerCandidate; }
  Double_t GetEoP()                                      { return fEoP;                   }
  Double_t GetClusterTrackDistance()                     { return fClusterTrackDistance;  }

  void SetTrackID(Int_t val)                             { fTrackID = val;                }
  void SetSpectrometerCandidate(TRecoSpectrometerCandidate* val) { fSpectrometerCandidate = val; }
  void SetEoP(Double_t val)                              { fEoP = val;                    }
  void SetClusterTrackDistance(Double_t val)             { fClusterTrackDistance = val;   }

  void Clear();
  void Print() const;

private:

  Int_t    fTrackID;                                  ///< ID of the associated spectrometer track, -1 if no association
  TRecoSpectrometerCandidate* fSpectrometerCandidate; ///< Pointer to the associated spectrometer candidate, 0 if no association
  Double_t fEoP;                                      ///< Track energy-to-momentum ratio (E/p): -1e28 if no association
  Double_t fClusterTrackDistance;                     ///< Distance from track extrapolation to cluster in LKr front plane [mm]: -1 if no association
};

#endif
