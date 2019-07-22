// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#include <iostream>
#include "LKrSpectrometerAssociationOutput.hh"

/// \class LKrSpectrometerAssociationOutput
/// \Brief
/// Container of Spectrometer association records for each LKr cluster
/// \EndBrief

LKrSpectrometerAssociationOutput::LKrSpectrometerAssociationOutput() {
  Clear();
}

void LKrSpectrometerAssociationOutput::Clear() {
  fBestAssociationRecordID = -1;
  fAssociationRecordContainer.clear();
}

void LKrSpectrometerAssociationOutput::Print() const {
  std::cout << "[LKrSpectrometerAssociation] NTracks: " << fAssociationRecordContainer.size() << std::endl;
  for(UInt_t iRecord=0;iRecord<fAssociationRecordContainer.size(); iRecord++){
    fAssociationRecordContainer[iRecord].Print();
  }
}

LKrSpectrometerAssociationRecord* LKrSpectrometerAssociationOutput::GetAssociationRecord(Int_t i) {
  if(i<0) return 0;
  if(i>=(Int_t)fAssociationRecordContainer.size()) return 0;
  return &(*(fAssociationRecordContainer.begin()+i));
}
