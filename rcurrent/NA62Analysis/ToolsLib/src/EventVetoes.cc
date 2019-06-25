/// \class EventVetoes
/// \Brief
/// A container Vetoes candidates
/// \EndBrief
/// \Detailed
/// Contains a vector of LAV CHANTI IRC and SAC candidates
/// \endcode
/// \author
/// \EndDetailed

#include "EventVetoes.hh"
#include "GeometricAcceptance.hh"
#include <iostream>

using namespace std;

EventVetoes::EventVetoes() {
  Clear();
}

void EventVetoes::Clear() {
  fCHANTICandidates.clear();
  fLAVCandidates.clear();
  fIRCHits.clear();
  fSACHits.clear();
}

// CHANTI methods

TRecoCHANTICandidate* EventVetoes::GetCHANTICandidate(UInt_t index) {
  if (index >= fCHANTICandidates.size()) {
    cout << "[EventVetoes::GetCHANTICandidate]: index exceeds vector size" << endl;
    return NULL;
  }
  return fCHANTICandidates[index];
}

// LAV methods

TRecoLAVCandidate* EventVetoes::GetLAVCandidate(UInt_t index) {
  if (index >= fLAVCandidates.size()) {
    cout << "[EventVetoes::GetLAVCandidate]: index exceeds vector size" << endl;
    return NULL;
  }
  return fLAVCandidates[index];
}

// IRC methods

TIRCHit* EventVetoes::GetIRCHit(UInt_t index) {
  if (index >= fIRCHits.size()) {
    cout << "[EventVetoes::GetIRCHit]: index exceeds vector size" << endl;
    return NULL;
  }
  return fIRCHits[index];
 }

// SAC methods

TSACHit* EventVetoes::GetSACHit(UInt_t index) {
  if (index >= fSACHits.size()) {
    cout << "[EventVetoes::GetSACHit]: index exceeds vector size" << endl;
    return NULL;
  }
  return fSACHits[index];
}

