// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-11-24
//
// ---------------------------------------------------------

#include <iostream>
#include "LKrSpectrometerAssociationRecord.hh"

/// \class LKrSpectrometerAssociationRecord
/// \Brief
/// A record of a Spectrometer candidate associated to a LKr cluster
/// \EndBrief

LKrSpectrometerAssociationRecord::LKrSpectrometerAssociationRecord(){
  Clear();
}

LKrSpectrometerAssociationRecord::LKrSpectrometerAssociationRecord(Int_t TrackID, TRecoSpectrometerCandidate* Candidate){
  Clear();
  fTrackID = TrackID;
  fSpectrometerCandidate = Candidate;
}

void LKrSpectrometerAssociationRecord::Clear() {
  fTrackID = -1;
  fSpectrometerCandidate = 0;
  fEoP = fClusterTrackDistance = -1;
}

void LKrSpectrometerAssociationRecord::Print() const {
  std::cout << "LKrSpectrometerRecord: TrackID: " << fTrackID
       << " Momentum: " << fSpectrometerCandidate->GetMomentum()
       << " Time: " << fSpectrometerCandidate->GetTime()
       << " EoP: " << fEoP
       << " D(Cluster): "<< fClusterTrackDistance << std::endl;
}
