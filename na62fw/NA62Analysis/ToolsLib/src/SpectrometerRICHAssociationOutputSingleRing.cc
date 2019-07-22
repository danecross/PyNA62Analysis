// ---------------------------------------------------------------
// History:
//
// Copied from code created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerRICHAssociationOutputSingleRing.hh"

using namespace std;

/// \class SpectrometerRICHAssociationOutputSingleRing
/// \Brief
/// Information about RICH rings and hits geometrically associatied to a Spectrometer track
/// \EndBrief

SpectrometerRICHAssociationOutputSingleRing::SpectrometerRICHAssociationOutputSingleRing() {
  Clear();
}

SpectrometerRICHAssociationOutputSingleRing::SpectrometerRICHAssociationOutputSingleRing
(Int_t TrackID, Double_t Time, Double_t Momentum) {
  Clear();
  fTrackID = TrackID;
  fTrackTime = Time;
  fTrackMomentum = Momentum;
}

void SpectrometerRICHAssociationOutputSingleRing::Clear() {
  fTrackID = fRingID = -1;
  fTrackTime = fTrackMomentum = -99.;
  fMinDistanceTrackRing = fMinXDistanceTrackRing = fMinYDistanceTrackRing = -99.;
  fRingRadius = -99.;
  fRingChi2 = -99.;
  fMass = -999999.9;
  fMass2 = -999999.9;
  fRingTime = -99;
}

void SpectrometerRICHAssociationOutputSingleRing::Print() {}
