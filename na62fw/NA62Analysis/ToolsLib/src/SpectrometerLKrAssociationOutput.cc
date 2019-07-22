// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-09-15
//
// ---------------------------------------------------------------

/// \class SpectrometerLKrAssociationOutput
/// \Brief
/// Container of LKr association records for each spectrometer track
/// \EndBrief
/// \Detailed
/// Accociation of LKe clusters to each spectrometer track is performed by
/// the SpectrometerLKrAssociation class.
/// \EndDetailed

#include <iostream>
#include "SpectrometerLKrAssociationOutput.hh"

SpectrometerLKrAssociationOutput::SpectrometerLKrAssociationOutput() {
  Clear();
}

void SpectrometerLKrAssociationOutput::Clear() {
  fBestAssociationRecordID = -1;
  fAssociationRecordContainer.clear();
}

void SpectrometerLKrAssociationOutput::Print() const {
  std::cout << "[SpectrometerLKrAssociation] NClusters: " << fAssociationRecordContainer.size() << std::endl;
  for (UInt_t iRecord=0; iRecord<fAssociationRecordContainer.size(); iRecord++) {
    fAssociationRecordContainer[iRecord].Print();
  }
}

SpectrometerLKrAssociationRecord* SpectrometerLKrAssociationOutput::GetAssociationRecord(Int_t i) {
  if (i<0) return nullptr;
  if (i>=(Int_t)fAssociationRecordContainer.size()) return nullptr;
  return &(*(fAssociationRecordContainer.begin()+i));
}
