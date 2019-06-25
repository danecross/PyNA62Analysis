// ---------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2016-12-07
//
// ---------------------------------------------------------

#include <iostream>
#include "SpectrometerLKrAssociationRecord.hh"

/// \class SpectrometerLKrAssociationRecord
/// \Brief
/// A record of a LKr candidate associated to a Spectrometer track
/// \EndBrief

SpectrometerLKrAssociationRecord::SpectrometerLKrAssociationRecord(){
  Clear();
}

SpectrometerLKrAssociationRecord::SpectrometerLKrAssociationRecord(Int_t ClusterID, TRecoLKrCandidate* Candidate){
  Clear();
  fClusterID = ClusterID;
  fLKrCandidate = Candidate;
}

void SpectrometerLKrAssociationRecord::Clear() {
  fClusterID = -1;
  fLKrCandidate = 0;
  fEoP = fTrackClusterDistance = -1;
}

void SpectrometerLKrAssociationRecord::Print() const {
  std::cout << "SpectrometerLKrRecord: ClusterID: " << fClusterID
       << " Energy: " << fLKrCandidate->GetClusterEnergy()
       << " Time: " << fLKrCandidate->GetTime()
       << " EoP: " << fEoP
       << " D(Cluster): "<< fTrackClusterDistance << std::endl;
}
