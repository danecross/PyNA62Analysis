// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// Adopted for the CHOD by Viacheslav Duk (Viacheslav.Duk@cern.ch) 26.02.2016
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerCHODAssociationRecord.hh"

/// \class SpectrometerCHODAssociationRecord
/// \Brief
/// A record of a CHOD candidate associated to a Spectrometer track
/// \EndBrief

using namespace std;

SpectrometerCHODAssociationRecord::SpectrometerCHODAssociationRecord(){
  Clear();
}

SpectrometerCHODAssociationRecord::SpectrometerCHODAssociationRecord
(Int_t CHODCandidateID, TRecoCHODCandidate* CHODCandidate, Int_t IDVHit, Int_t IDHHit, Double_t Time,
 Double_t TrackTileDistance, Double_t TrackCandidateDistance, Double_t CHODCandidateX, Double_t CHODCandidateY, Bool_t DirectMatch) :
  fCHODCandidateID(CHODCandidateID), fCHODCandidate(CHODCandidate),
  fIDVHit(IDVHit), fIDHHit(IDHHit), fCHODCandidateTime(Time),
  fTrackTileDistance(TrackTileDistance), fTrackCandidateDistance(TrackCandidateDistance),
  fCHODCandidateX(CHODCandidateX), fCHODCandidateY(CHODCandidateY), fDirectMatch(DirectMatch) {}

SpectrometerCHODAssociationRecord::SpectrometerCHODAssociationRecord
(Int_t CHODCandidateID, TRecoCHODCandidate* CHODCandidate, Int_t IDVHit, Int_t IDHHit, Double_t Time,
 Double_t TrackTileDistance, Double_t TrackCandidateDistance, TVector2 CHODCandidateXY, Bool_t DirectMatch) :
  fCHODCandidateID(CHODCandidateID), fCHODCandidate(CHODCandidate),
  fIDVHit(IDVHit), fIDHHit(IDHHit), fCHODCandidateTime(Time),
  fTrackTileDistance(TrackTileDistance), fTrackCandidateDistance(TrackCandidateDistance),
  fCHODCandidateX(CHODCandidateXY.X()), fCHODCandidateY(CHODCandidateXY.Y()),
  fDirectMatch(DirectMatch) {}

void SpectrometerCHODAssociationRecord::Clear() {
  fCHODCandidate = nullptr;
  fCHODCandidateID = fIDVHit = fIDHHit = fTrackTileDistance = fTrackCandidateDistance = -1;
  fCHODCandidateTime = fCHODCandidateX = fCHODCandidateY = 0.0;
  fDirectMatch = kTRUE;
}

void SpectrometerCHODAssociationRecord::Print() const {
  cout << "SpectrometerCHODCandidateRecord: CHODCandidateID: " << fCHODCandidateID
       << " Slab IDs (V and H): " << fIDVHit << " " << fIDHHit
       << " CHOD candidate time: " << fCHODCandidateTime
       << " CHODCandidateXY: " << fCHODCandidateX << " " << fCHODCandidateY
       << " Distance between this track and slab edges: "<< fTrackTileDistance
       << " Distance between this track and CHOD candidate: "<< fTrackCandidateDistance
       << " DirectMatch: "<< fDirectMatch << endl;
}
