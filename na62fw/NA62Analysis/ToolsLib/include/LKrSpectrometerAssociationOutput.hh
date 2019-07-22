// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#ifndef LKRSPECASSOCIATIONOUTPUT_HH
#define LKRSPECASSOCIATIONOUTPUT_HH 1

#include "TMath.h"
#include "TVector3.h"
#include "LKrSpectrometerAssociationRecord.hh"

class LKrSpectrometerAssociationOutput {

public:

  LKrSpectrometerAssociationOutput();
  ~LKrSpectrometerAssociationOutput() {}

  UInt_t   GetNAssociationRecords() const        { return fAssociationRecordContainer.size();     }
  Bool_t   isAssociated() const                  { return (fAssociationRecordContainer.size()>0); }
  LKrSpectrometerAssociationRecord* GetAssociationRecord(Int_t i);
  LKrSpectrometerAssociationRecord* GetBestAssociationRecord() { return GetAssociationRecord(fBestAssociationRecordID); }

  void SetBestAssociationRecordID(Int_t val)     { fBestAssociationRecordID = val;  }
  void AddAssociationRecord(LKrSpectrometerAssociationRecord Rec) { fAssociationRecordContainer.push_back(Rec); }
  void Clear();
  void Print() const;

private:

  Int_t    fBestAssociationRecordID; ///< ID of the closest associated track record: -1 if no association
  std::vector<LKrSpectrometerAssociationRecord> fAssociationRecordContainer; ///< Records of matching Spectrometer candidates

};

#endif
