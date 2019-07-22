// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-13
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerNewCHODAssociationRecord.hh"

/// \class SpectrometerNewCHODAssociationRecord
/// \Brief
/// A record of a NewCHOD candidate associated to a Spectrometer track
/// \EndBrief

using namespace std;

SpectrometerNewCHODAssociationRecord::SpectrometerNewCHODAssociationRecord() :
  fRecoHitID(-1), fTileID(-1), fType(kUndefinedCandidate), fRecoHitTime(0),
  fTrackTileDistance(-1), fTrackRecoHitDistance(-1), fRecoHitX(0), fRecoHitY(0), fDirectMatch(kTRUE) {}

SpectrometerNewCHODAssociationRecord::SpectrometerNewCHODAssociationRecord
(Int_t RecoHitID, Int_t TileID, Int_t Type, Double_t Time,
 Double_t TrackTileDistance, Double_t TrackRecoHitDistance, Double_t RecoHitX, Double_t RecoHitY, Bool_t DirectMatch) :
  fRecoHitID(RecoHitID), fTileID(TileID), fType(Type), fRecoHitTime(Time),
  fTrackTileDistance(TrackTileDistance), fTrackRecoHitDistance(TrackRecoHitDistance),
  fRecoHitX(RecoHitX), fRecoHitY(RecoHitY), fDirectMatch(DirectMatch) {}

SpectrometerNewCHODAssociationRecord::SpectrometerNewCHODAssociationRecord
(Int_t RecoHitID, Int_t TileID, Int_t Type, Double_t Time,
 Double_t TrackTileDistance, Double_t TrackRecoHitDistance, TVector2 RecoHitXY, Bool_t DirectMatch) :
  fRecoHitID(RecoHitID), fTileID(TileID), fType(Type), fRecoHitTime(Time),
  fTrackTileDistance(TrackTileDistance), fTrackRecoHitDistance(TrackRecoHitDistance),
  fRecoHitX(RecoHitXY.X()), fRecoHitY(RecoHitXY.Y()), fDirectMatch(DirectMatch) {}

void SpectrometerNewCHODAssociationRecord::Clear() {
  fRecoHitID = fTileID = fTrackTileDistance = fTrackRecoHitDistance = -1;
  fRecoHitTime = fRecoHitX = fRecoHitY = 0.0;
  fDirectMatch = kTRUE;
}

void SpectrometerNewCHODAssociationRecord::Print() {
  cout << "SpectrometerNewCHODRecord: RecoHitID: " << fRecoHitID
       << " Tile: " << fTileID
       << " Time: " << fRecoHitTime
       << " RecoHitXY: " << fRecoHitX << " " << fRecoHitY
       << " D(TrTile): "<< fTrackTileDistance
       << " D(TrHit): "<< fTrackRecoHitDistance
       << " DirectMatch: "<< fDirectMatch << endl;
}
