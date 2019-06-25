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
#include "SpectrometerCHODAssociationOutput.hh"

using namespace std;

/// \class SpectrometerCHODAssociationOutput
/// \Brief
/// Container of records of CHOD candidates associated to each Spectrometer track
/// \EndBrief

SpectrometerCHODAssociationOutput::SpectrometerCHODAssociationOutput() {
  Clear();
}

SpectrometerCHODAssociationOutput::SpectrometerCHODAssociationOutput (Int_t TrackID) {
  Clear();
  fTrackID = TrackID;
}

Double_t SpectrometerCHODAssociationOutput::GetMinimumTrackTileDistance() {
  if (!fAssociationRecordContainer.size()) return 99999;
  Double_t Rmin = 99999;
  for (UInt_t i=0; i<fAssociationRecordContainer.size(); i++) {
    if (fAssociationRecordContainer[i].GetTrackTileDistance() < Rmin) {
      Rmin = fAssociationRecordContainer[i].GetTrackTileDistance();
    }
  }
  return Rmin;
}

Double_t SpectrometerCHODAssociationOutput::GetMinimumTrackCandidateDistance() {
  if (!fAssociationRecordContainer.size()) return 99999;
  Double_t Rmin = 99999;
  for (UInt_t i=0; i<fAssociationRecordContainer.size(); i++) {
    if (fAssociationRecordContainer[i].GetTrackCandidateDistance() < Rmin) {
      Rmin = fAssociationRecordContainer[i].GetTrackCandidateDistance();
    }
  }
  return Rmin;
}

void SpectrometerCHODAssociationOutput::SetTrackXY(Double_t TrackX, Double_t TrackY) {
  fTrackX = TrackX;
  fTrackY = TrackY;
}

void SpectrometerCHODAssociationOutput::SetTrackXY(TVector2 TrackXY) {
  fTrackX = TrackXY.X();
  fTrackY = TrackXY.Y();
}

void SpectrometerCHODAssociationOutput::SetShowerFlag(Bool_t Flag) {
  fShowerFlag = Flag;
}

Bool_t SpectrometerCHODAssociationOutput::GetShowerFlag() {
  return fShowerFlag;
}

void SpectrometerCHODAssociationOutput::RemoveAssociationRecord(Int_t iRec) {
  if (iRec<0) {
    cout << "SpectrometerCHODAssociationOutput: Tried to remove an AssociationOutput using a negative index." << endl ;
    return;
  }
  fAssociationRecordContainer.erase(fAssociationRecordContainer.begin()+iRec);
}

void SpectrometerCHODAssociationOutput::Clear() {
  fTrackID = -1;
  fSearchRadius = -1.0;
  fTrackMomentum = fTrackTime = fTrackX = fTrackY = 0.0;
  fInAcceptance = kFALSE;
  fDistanceToEdge = 0.0;
  fAssociationRecordContainer.clear();
  fShowerFlag = kFALSE;
  fBestAssociationRecordID = -1;
}

void SpectrometerCHODAssociationOutput::Print() const {
  cout << "SpectrometerCHODAssociationOutput: TrackID: " << fTrackID
       << " Track Time: " << fTrackTime
       << " Momentum: " << fTrackMomentum
       << " Search Radius: " << fSearchRadius
       << " TrackXY: " << fTrackX << " " << fTrackY
       << " Number of CHOD candidates associated with this track: " << fAssociationRecordContainer.size() << endl;
}

SpectrometerCHODAssociationRecord* SpectrometerCHODAssociationOutput::GetAssociationRecord(Int_t i) {
  if (i<0) return nullptr;
  if (i>=(Int_t)fAssociationRecordContainer.size()) return nullptr;
  return &(*(fAssociationRecordContainer.begin()+i));
}
