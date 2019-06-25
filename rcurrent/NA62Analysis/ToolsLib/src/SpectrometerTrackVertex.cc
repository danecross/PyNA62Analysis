// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-08-25
//
// ---------------------------------------------------------------

/// \class SpectrometerTrackVertex
/// \Brief
/// A vertex formed by spectrometer tracks
/// \EndBrief
/// \Detailed
/// The vertices are built by SpectrometerVertexBuilder (which calls the VertexLSF algorithm)
/// using reconstructed STRAW spectrometer tracks (i.e. TRecoSpectrometerCandidate objects).
/// Vertices can contain different numbers of tracks (from 2 to 5), according to SpectrometerVertexBuilder
/// settings controlled via the compilation configuration file (recommended), command line arguments,
/// or the ReconfigureAnalyzer feature (not recommended).
/// The track indices in the vertex structure are in ascending order.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include <iostream>
#include "SpectrometerTrackVertex.hh"

using namespace std;

SpectrometerTrackVertex::SpectrometerTrackVertex() {
  Clear();
}

void SpectrometerTrackVertex::Clear() {
  fNTracks = 0;
  fCharge = 0;
  fPosition = TVector3(0.0, 0.0, 0.0);
  fChi2 = 0.0;
  fTime = 0.0;
  fTotalThreeMomentum = TVector3(0.0, 0.0, 0.0);
  fTotalThreeMomentum0 = TVector3(0.0, 0.0, 0.0);
  fTrackIndices.clear();
  fTrackCharges.clear();
  fThreeMomenta.clear();
  fThreeMomenta0.clear();
  fSpectrometerCandidates.clear();
}

void SpectrometerTrackVertex::AddTrack
(TRecoSpectrometerCandidate* Cand, Int_t index, Int_t Charge, TVector3 Mom, TVector3 Mom0) {
  fTrackIndices.push_back(index);
  fTrackCharges.push_back(Charge);
  fThreeMomenta.push_back(Mom);
  fThreeMomenta0.push_back(Mom0);
  fSpectrometerCandidates.push_back(Cand);
  fNTracks = fTrackIndices.size();
}

Int_t SpectrometerTrackVertex::GetTrackIndex(Int_t i) {
  return (i>=0 && i<fNTracks) ? fTrackIndices[i] : -999;
}

Int_t SpectrometerTrackVertex::GetTrackCharge(Int_t i) {
  return (i>=0 && i<fNTracks) ? fTrackCharges[i] : 0;
}

TVector3 SpectrometerTrackVertex::GetTrackThreeMomentum(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta[i] : TVector3(0.0,0.0,0.0);
}

TVector3 SpectrometerTrackVertex::GetTrackThreeMomentum0(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta0[i] : TVector3(0.0,0.0,0.0);
}

Double_t SpectrometerTrackVertex::GetTrackMomentum(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta[i].Mag() : 0.0;
}

Double_t SpectrometerTrackVertex::GetTrackMomentum0(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta0[i].Mag() : 0.0;
}

Double_t SpectrometerTrackVertex::GetTrackSlopeX(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta[i].X()/fThreeMomenta[i].Z() : 0.0;
}

Double_t SpectrometerTrackVertex::GetTrackSlopeY(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta[i].Y()/fThreeMomenta[i].Z() : 0.0;
}

Double_t SpectrometerTrackVertex::GetTrackSlopeX0(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta0[i].X()/fThreeMomenta0[i].Z() : 0.0;
}

Double_t SpectrometerTrackVertex::GetTrackSlopeY0(Int_t i) {
  return (i>=0 && i<fNTracks) ? fThreeMomenta0[i].Y()/fThreeMomenta0[i].Z() : 0.0;
}

TRecoSpectrometerCandidate* SpectrometerTrackVertex::GetSpectrometerCandidate(Int_t i) {
  return (i>=0 && i<fNTracks) ? fSpectrometerCandidates[i] : nullptr;
}

void SpectrometerTrackVertex::Print() const {
  cout <<
    "Vertex: Ntrk= " << fNTracks <<
    " Q= "           << fCharge  <<
    " Z= "           << fPosition.Z()<<
    " P= "           << fTotalThreeMomentum.Mag()<<
    " Chi2= "        << fChi2 <<
    " Tracks=";
  for (Int_t i=0; i<fNTracks; i++) cout << " " << fTrackIndices[i];
  cout << endl;
}
