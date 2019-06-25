// ---------------------------------------------------------------
// History:
//
// Copied from code created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerRICHAssociationOutputTrackCentredRing.hh"

using namespace std;

/// \class SpectrometerRICHAssociationOutputTrackCentredRing
/// \Brief
/// Information about RICH rings and hits geometrically associatied to a Spectrometer track
/// \EndBrief

SpectrometerRICHAssociationOutputTrackCentredRing::SpectrometerRICHAssociationOutputTrackCentredRing() {
  Clear();
}

SpectrometerRICHAssociationOutputTrackCentredRing::SpectrometerRICHAssociationOutputTrackCentredRing
(Int_t TrackID, Double_t Time, Double_t Momentum) {
  Clear();
  fTrackID = TrackID;
  fTrackTime = Time;
  fTrackMomentum = Momentum;
}

void SpectrometerRICHAssociationOutputTrackCentredRing::Clear() {
  fTrackID = -1;
  fTrackTime = -99.;
  fTrackMomentum = -99.;
  fRingID = -1;
  fRingRadius = -99.;
  fRingChi2 = -99.;
  fMass = -99.;
  fRingTime = -99;
}

void SpectrometerRICHAssociationOutputTrackCentredRing::Print() {}
