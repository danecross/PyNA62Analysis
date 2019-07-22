// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerMUV3AssociationRecord.hh"

/// \class SpectrometerMUV3AssociationRecord
/// \Brief
/// A record of a MUV3 candidate associated to a Spectrometer track
/// \EndBrief

using namespace std;

SpectrometerMUV3AssociationRecord::SpectrometerMUV3AssociationRecord() :
  fMuonID(-1), fTileID(-1), fType(kUndefinedCandidate), fMuonTime(0),
  fTrackTileDistance(-1), fTrackCandidateDistance(-1), fMuonX(0), fMuonY(0), fDirectMatch(kTRUE) {}

SpectrometerMUV3AssociationRecord::SpectrometerMUV3AssociationRecord
(Int_t MuonID, Int_t TileID, Int_t Type, Double_t Time,
 Double_t TrackTileDistance, Double_t TrackCandidateDistance, Double_t MuonX, Double_t MuonY, Bool_t DirectMatch) :
  fMuonID(MuonID), fTileID(TileID), fType(Type), fMuonTime(Time),
  fTrackTileDistance(TrackTileDistance), fTrackCandidateDistance(TrackCandidateDistance),
  fMuonX(MuonX), fMuonY(MuonY), fDirectMatch(DirectMatch) {}

SpectrometerMUV3AssociationRecord::SpectrometerMUV3AssociationRecord
(Int_t MuonID, Int_t TileID, Int_t Type, Double_t Time,
 Double_t TrackTileDistance, Double_t TrackCandidateDistance, TVector2 MuonXY, Bool_t DirectMatch) :
  fMuonID(MuonID), fTileID(TileID), fType(Type), fMuonTime(Time),
  fTrackTileDistance(TrackTileDistance), fTrackCandidateDistance(TrackCandidateDistance),
  fMuonX(MuonXY.X()), fMuonY(MuonXY.Y()), fDirectMatch(DirectMatch) {}

void SpectrometerMUV3AssociationRecord::Clear() {
  fMuonID = fTileID = fTrackTileDistance = fTrackCandidateDistance = -1;
  fMuonTime = fMuonX = fMuonY = 0.0;
  fDirectMatch = kTRUE;
}

void SpectrometerMUV3AssociationRecord::Print() {
  cout << "SpectrometerMUV3Record: MuonID: " << fMuonID
       << " Tile: " << fTileID
       << " Time: " << fMuonTime
       << " MuonXY: " << fMuonX << " " << fMuonY
       << " D(TrTile): "<< fTrackTileDistance
       << " D(TrMu): "<< fTrackCandidateDistance
       << " DirectMatch: "<< fDirectMatch << endl;
}
