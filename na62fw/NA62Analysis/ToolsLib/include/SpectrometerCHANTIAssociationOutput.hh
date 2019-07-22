// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-01-28
//
// ---------------------------------------------------------

#ifndef SPECCHANTIASSOCIATIONOUTPUT_HH
#define SPECCHANTIASSOCIATIONOUTPUT_HH 1

#include "TMath.h"
#include "TVector3.h"
#include "SpectrometerCHANTIAssociationRecord.hh"

class SpectrometerCHANTIAssociationOutput {

public:

  SpectrometerCHANTIAssociationOutput();
  ~SpectrometerCHANTIAssociationOutput() {}

  UInt_t   GetNAssociationRecords()              { return fAssociationRecordContainer.size();     }
  Bool_t   isAssociated()                        { return (fAssociationRecordContainer.size()>0); }
  SpectrometerCHANTIAssociationRecord* GetAssociationRecord(Int_t i);
  SpectrometerCHANTIAssociationRecord* GetBestAssociationRecord() { return GetAssociationRecord(fBestAssociationRecordID); }
  Bool_t   GetInAcceptance()                     { return fInAcceptance;                          }
  Double_t GetDistanceToEdge()                   { return fDistanceToEdge;                        }

  void SetBestAssociationRecordID(Int_t val)     { fBestAssociationRecordID = val;                }
  void AddAssociationRecord(SpectrometerCHANTIAssociationRecord Rec) { fAssociationRecordContainer.push_back(Rec); }
  void SetInAcceptance(Bool_t val)     { fInAcceptance = val;   }
  void SetDistanceToEdge(Double_t val) { fDistanceToEdge = val; }
  void Clear();
  void Print() const;

private:

  Int_t    fBestAssociationRecordID; ///< ID of the closest associated CHANTI record: -1 if no association
  std::vector<SpectrometerCHANTIAssociationRecord> fAssociationRecordContainer; ///< Records of matching CHANTI candidates

  Bool_t   fInAcceptance;   ///< Inside or outside CHANTI geometrical acceptance?
  Double_t fDistanceToEdge; ///< Distance to outer CHANTI edge
};

#endif
