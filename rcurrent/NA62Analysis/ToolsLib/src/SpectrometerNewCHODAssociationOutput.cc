// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-07-13
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerNewCHODAssociationOutput.hh"

using namespace std;

/// \class SpectrometerNewCHODAssociationOutput
/// \Brief
/// Container of records of NewCHOD RecoHits associated to each Spectrometer track
/// \EndBrief

SpectrometerNewCHODAssociationOutput::SpectrometerNewCHODAssociationOutput() {
  Clear();
}

SpectrometerNewCHODAssociationOutput::SpectrometerNewCHODAssociationOutput (Int_t TrackID) {
  Clear();
  fTrackID = TrackID;
}

SpectrometerNewCHODAssociationRecord* SpectrometerNewCHODAssociationOutput::GetAssociationRecord(Int_t i) {
  if (i<0) return nullptr;
  if (i>=(Int_t)fAssociationRecordContainer.size()) return nullptr;
  return &(*(fAssociationRecordContainer.begin()+i));
}

Double_t SpectrometerNewCHODAssociationOutput::GetMinimumTrackTileDistance() {
  if (!fAssociationRecordContainer.size()) return 99999;
  Double_t Rmin = 99999;
  for (UInt_t i=0; i<fAssociationRecordContainer.size(); i++) {
    if (fAssociationRecordContainer[i].GetTrackTileDistance() < Rmin) {
      Rmin = fAssociationRecordContainer[i].GetTrackTileDistance();
    }
  }
  return Rmin;
}

Double_t SpectrometerNewCHODAssociationOutput::GetMinimumTrackRecoHitDistance() {
  if (!fAssociationRecordContainer.size()) return 99999;
  Double_t Rmin = 99999;
  for (UInt_t i=0; i<fAssociationRecordContainer.size(); i++) {
    if (fAssociationRecordContainer[i].GetTrackRecoHitDistance() < Rmin) {
      Rmin = fAssociationRecordContainer[i].GetTrackRecoHitDistance();
    }
  }
  return Rmin;
}

void SpectrometerNewCHODAssociationOutput::SetTrackXY(Double_t TrackX, Double_t TrackY) {
  fTrackX = TrackX;
  fTrackY = TrackY;
}

void SpectrometerNewCHODAssociationOutput::SetTrackXY(TVector2 TrackXY) {
  fTrackX = TrackXY.X();
  fTrackY = TrackXY.Y();
}

void SpectrometerNewCHODAssociationOutput::Clear() {
  fTrackID = -1;
  fSearchRadius = -1.0;
  fTrackMomentum = fTrackTime = fTrackX = fTrackY = 0.0;
  fInAcceptance = kFALSE;
  fAssociationRecordContainer.clear();
  fBestAssociationRecordID = -1;
}

void SpectrometerNewCHODAssociationOutput::Print() {
  cout << "SpectrometerNewCHODAssociation: TrackID: " << fTrackID
       << " Time: " << fTrackTime
       << " Mom: " << fTrackMomentum
       << " SearchR: " << fSearchRadius
       << " TrackXY: " << fTrackX << " " << fTrackY
       << " MuRecords: " << fAssociationRecordContainer.size() << endl;
}
