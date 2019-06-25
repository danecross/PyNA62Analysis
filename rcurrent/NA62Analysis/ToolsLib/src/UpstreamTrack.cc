/// \class UpstreamTrack
/// \Brief
/// A container for beam tracks
/// \EndBrief
/// \Detailed
/// Contains a GigaTracker candidate and a vector of Cedar candidates
/// \endcode
/// \author
/// \EndDetailed

#include "UpstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include <iostream>

using namespace std;

UpstreamTrack::UpstreamTrack() {
  Clear();
}

void UpstreamTrack::Clear() {
  fTrackID = -1;
  fGigaTrackerCandidate = 0;
  fCedarCandidates.clear();
}

///////////////
// Cedar methods

TRecoCedarCandidate* UpstreamTrack::GetCedarCandidate(UInt_t index) {
  if (index >= fCedarCandidates.size()) {
    cout << "[UpstreamTrack::GetCedarCandidate]: index exceeds vector size" << endl;
    return NULL;
  }
  return fCedarCandidates[index];
}
