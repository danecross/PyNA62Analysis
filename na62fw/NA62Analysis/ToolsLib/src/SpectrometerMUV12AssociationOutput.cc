// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-10-28
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerMUV12AssociationOutput.hh"

using namespace std;

/// \class SpectrometerMUV12AssociationOutput
/// \Brief
/// Container of MUV1 and MUV2 association information for each Spectrometer track
/// \EndBrief

SpectrometerMUV12AssociationOutput::SpectrometerMUV12AssociationOutput() {
  Clear();
}

SpectrometerMUV12AssociationOutput::SpectrometerMUV12AssociationOutput(Int_t TrackID) {
  Clear();
  fTrackID = TrackID;
}

void SpectrometerMUV12AssociationOutput::Clear() {
  fTrackID = fMUV1ClusterID = fMUV2ClusterID = -1;
  fMUV1ClusterPosition.SetXYZ(0.0, 0.0, 0.0); // unphysical position
  fMUV2ClusterPosition.SetXYZ(0.0, 0.0, 0.0);
  fTrackMUV1ClusterDistance = fTrackMUV2ClusterDistance = 9999.;
  fMUV1ClusterEnergy = fMUV2ClusterEnergy = -999.;
  fMUV1ClusterTime = fMUV2ClusterTime = -999.;
}

void SpectrometerMUV12AssociationOutput::Print() {
}
