// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------

/// \class TRecoMUV3Candidate
/// \Brief
/// MUV3 candidate: a single hit or a coincidence of two hits in a tile
/// \EndBrief

#include "TRecoMUV3Candidate.hh"

ClassImp(TRecoMUV3Candidate)

TRecoMUV3Candidate::TRecoMUV3Candidate() : TRecoVCandidate() {
  Clear();
}

void TRecoMUV3Candidate::Clear(Option_t* option) {
  TRecoVCandidate::Clear(option);
  SetTime(-999);
  fX = fY = 0.0;
  fZ = 246800.0;
  fTileID = fChannel1 = fChannel2 = fROChannel1 = fROChannel2 = -1;
  fTime1 = fTime2 = fTime1NoT0 = fTime2NoT0 = -999;
  fTimeNoT0 = fTimeNoTileT0 = -999;
  fType = kUndefinedCandidate;
}

// Time differences (defined for tight candidates only): high-low PMT
Double_t TRecoMUV3Candidate::GetDeltaTime() {
  return (fType==kTightCandidate) ? fTime2-fTime1 : 0.0;
}
Double_t TRecoMUV3Candidate::GetDeltaTimeNoT0() {
  return (fType==kTightCandidate) ? fTime2NoT0-fTime1NoT0 : 0.0;
}

// Average hit time, as opposed to candidate time, which is the latest hit time
Double_t TRecoMUV3Candidate::GetAverageTime() {
  return (fType==kTightCandidate) ? 0.5*(fTime1+fTime2) : fTime1;
}
