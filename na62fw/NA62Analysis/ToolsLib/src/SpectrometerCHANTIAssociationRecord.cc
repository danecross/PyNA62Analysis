// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2018-01-28
//
// ---------------------------------------------------------

#include <iostream>
#include "SpectrometerCHANTIAssociationRecord.hh"

/// \class SpectrometerCHANTIAssociationRecord
/// \Brief
/// A record of a CHANTI candidate associated to a Spectrometer track
/// \EndBrief

SpectrometerCHANTIAssociationRecord::SpectrometerCHANTIAssociationRecord(){
  Clear();
}

SpectrometerCHANTIAssociationRecord::SpectrometerCHANTIAssociationRecord(Int_t CandidateID, TRecoCHANTICandidate* Candidate){
  Clear();
  fCandidateID = CandidateID;
  fCHANTICandidate = Candidate;
}

void SpectrometerCHANTIAssociationRecord::Clear() {
  fCandidateID = -1;
  fCHANTICandidate = 0;
  fTrackCandidateDistance = -1;
}

void SpectrometerCHANTIAssociationRecord::Print() const {
  std::cout << "SpectrometerCHANTIRecord: CandidateID: " << fCandidateID
       << " Position: (" << fCHANTICandidate->GetXPos() << "," << fCHANTICandidate->GetYPos() << ")"
       << " Time: "      << fCHANTICandidate->GetTime()
       << " D(Track): "  << fTrackCandidateDistance << std::endl;
}
