// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (cjp@hep.ph.bham.ac.uk) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-09
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerMUV3AssociationOutput.hh"

using namespace std;

/// \class SpectrometerMUV3AssociationOutput
/// \Brief
/// Container of records of MUV3 candidates associated to each Spectrometer track
/// \EndBrief

SpectrometerMUV3AssociationOutput::SpectrometerMUV3AssociationOutput() {
  Clear();
}

SpectrometerMUV3AssociationOutput::SpectrometerMUV3AssociationOutput (Int_t TrackID) {
  Clear();
  fTrackID = TrackID;
}

SpectrometerMUV3AssociationRecord* SpectrometerMUV3AssociationOutput::GetAssociationRecord(Int_t i) {
  if (i<0) return nullptr;
  if (i>=(Int_t)fAssociationRecordContainer.size()) return nullptr;
  return &(*(fAssociationRecordContainer.begin()+i));
}

Double_t SpectrometerMUV3AssociationOutput::GetMinimumTrackTileDistance() {
  if (!fAssociationRecordContainer.size()) return 99999;
  Double_t Rmin = 99999;
  for (UInt_t i=0; i<fAssociationRecordContainer.size(); i++) {
    if (fAssociationRecordContainer[i].GetTrackTileDistance() < Rmin) {
      Rmin = fAssociationRecordContainer[i].GetTrackTileDistance();
    }
  }
  return Rmin;
}

Double_t SpectrometerMUV3AssociationOutput::GetMinimumTrackCandidateDistance() {
  if (!fAssociationRecordContainer.size()) return 99999;
  Double_t Rmin = 99999;
  for (UInt_t i=0; i<fAssociationRecordContainer.size(); i++) {
    if (fAssociationRecordContainer[i].GetTrackCandidateDistance() < Rmin) {
      Rmin = fAssociationRecordContainer[i].GetTrackCandidateDistance();
    }
  }
  return Rmin;
}

void SpectrometerMUV3AssociationOutput::SetTrackXY(Double_t TrackX, Double_t TrackY) {
  fTrackX = TrackX;
  fTrackY = TrackY;
}

void SpectrometerMUV3AssociationOutput::SetTrackXY(TVector2 TrackXY) {
  fTrackX = TrackXY.X();
  fTrackY = TrackXY.Y();
}

void SpectrometerMUV3AssociationOutput::Clear() {
  fTrackID = -1;
  fSearchRadius = -1.0;
  fTrackMomentum = fTrackTime = fTrackX = fTrackY = 0.0;
  fInAcceptance = kFALSE;
  fDistanceToEdge = 0.0;
  fAssociationRecordContainer.clear();
}

void SpectrometerMUV3AssociationOutput::Print() {
  cout << "SpectrometerMUV3Association: TrackID: " << fTrackID
       << " Time: " << fTrackTime
       << " Mom: " << fTrackMomentum
       << " SearchR: " << fSearchRadius
       << " TrackXY: " << fTrackX << " " << fTrackY
       << " MuRecords: " << fAssociationRecordContainer.size() << endl;
}
