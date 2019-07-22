// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-01-28
//
// ---------------------------------------------------------

#include <iostream>
#include "SpectrometerCHANTIAssociationOutput.hh"

/// \class SpectrometerCHANTIAssociationOutput
/// \Brief
/// Container of CHANTI association records for each Spectremeter track
/// \EndBrief

SpectrometerCHANTIAssociationOutput::SpectrometerCHANTIAssociationOutput() {
  Clear();
}

void SpectrometerCHANTIAssociationOutput::Clear() {
  fBestAssociationRecordID = -1;
  fAssociationRecordContainer.clear();
}

void SpectrometerCHANTIAssociationOutput::Print() const {
  std::cout << "[SpectrometerCHANTIAssociation] NCandidates: " << fAssociationRecordContainer.size() << std::endl;
  for(UInt_t iRecord=0;iRecord<fAssociationRecordContainer.size(); iRecord++){
    fAssociationRecordContainer[iRecord].Print();
  }
}

SpectrometerCHANTIAssociationRecord* SpectrometerCHANTIAssociationOutput::GetAssociationRecord(Int_t i) {
  if(i<0) return 0;
  if(i>=(Int_t)fAssociationRecordContainer.size()) return 0;
  return &(*(fAssociationRecordContainer.begin()+i));
}
