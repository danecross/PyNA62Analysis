// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-09-15
//
// ---------------------------------------------------------------

#ifndef INCLUDE_SPECLKRASSOCIATIONOUTPUT_HH
#define INCLUDE_SPECLKRASSOCIATIONOUTPUT_HH 1

#include "TMath.h"
#include "TVector3.h"
#include "SpectrometerLKrAssociationRecord.hh"

class SpectrometerLKrAssociationOutput {

public:

  SpectrometerLKrAssociationOutput();
  ~SpectrometerLKrAssociationOutput() {}

  UInt_t   GetNAssociationRecords()              { return fAssociationRecordContainer.size();     }
  Bool_t   isAssociated()                        { return (fAssociationRecordContainer.size()>0); }
  void     SetTotalEnergy(Double_t val)          { fTotalEnergy = val;              }
  Double_t GetTotalEnergy()                      { return fTotalEnergy;             }
  void     SetTotalEoP(Double_t val)             { fTotalEoP = val;                 }
  Double_t GetTotalEoP()                         { return fTotalEoP;                }
  void     SetBestAssociationRecordID(Int_t val) { fBestAssociationRecordID = val;  }
  Int_t    GetBestAssociationRecordID()          { return fBestAssociationRecordID; }

  void AddAssociationRecord(SpectrometerLKrAssociationRecord Rec)
  { fAssociationRecordContainer.push_back(Rec); }
  SpectrometerLKrAssociationRecord* GetAssociationRecord(Int_t i);
  SpectrometerLKrAssociationRecord* GetBestAssociationRecord()
  { return GetAssociationRecord(fBestAssociationRecordID); }

  void Clear();
  void Print() const;

private:

  Int_t    fBestAssociationRecordID; ///< ID of the closest associated track record: -1 if no association
  std::vector<SpectrometerLKrAssociationRecord> fAssociationRecordContainer; ///< Records of matching spectrometer candidates
  Double_t fTotalEnergy;          ///< Total energy summed over clusters within an extended search radius [MeV]
  Double_t fTotalEoP;             ///< Track energy-to-momentum ratio (E/p) based on the total energy summed over clusters within an extended search radius and an extended time window
};

#endif
