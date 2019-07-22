// --------------------------------------------------------------
// History:
//
// Modified by Massimiliano Fiorini (Massimiliano.Fiorini@cern.ch) 2009-10-29
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2009-10-04
//
// --------------------------------------------------------------

/// \class TRecoGigaTrackerCandidate
/// \Brief
/// This class, together with GigaTrackerCluster, represents the core 
/// part of the GigaTracker reconstruction.
/// \EndBrief
/// \Detailed
/// This class collects information from the 3 clusters out of which the candidate is formed
/// and returns:
///    - momentum of the candidate track
///    - position of the cluster in 3rd station of the GigaTracker (StationNo = 2)
/// \EndDetailed

#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoGigaTrackerHit.hh"
#include "TMath.h"
#include "Riostream.h"

ClassImp(TRecoGigaTrackerCandidate)

TRecoGigaTrackerCandidate::TRecoGigaTrackerCandidate() : TRecoVCandidate() {
  Clear();
}

TRecoGigaTrackerCandidate::TRecoGigaTrackerCandidate(const TRecoGigaTrackerCandidate & candidate) :
  TRecoVCandidate((TRecoVCandidate &) candidate),
  fType(candidate.fType),
  fChi2X(candidate.fChi2X),
  fChi2Y(candidate.fChi2Y),
  fChi2Time(candidate.fChi2Time),
  fChi2(candidate.fChi2) {
  //fMomentum automatically initialised to 0
  for (Int_t i=0; i<3; i++) {
    fTimeStation[i] = candidate.fTimeStation[i];
    fPosition[i] = candidate.fPosition[i];
  }
  for (Int_t ix=0; ix<5; ix++) {
    for (Int_t iy=0; iy<5; iy++) {
      fCovariance[ix][iy] = candidate.fCovariance[ix][iy];
    }
  }
}

void TRecoGigaTrackerCandidate::Clear(Option_t* option){
  TRecoVCandidate::Clear(option);

  fMomentum = TVector3(0.0, 0.0, 0.0);
  fTimeError = 0.0;
  fType = 0;
  fChi2X = 0.0;
  fChi2Y = 0.0;
  fChi2Time = 0.0;
  fChi2 = 0.0;

  for (int i(0); i<3; i++){
    fTimeStation[i] = 0.0;
    fPosition[i] = TVector3(0.0, 0.0, 0.0);
  }
  for (Int_t ix=0; ix<5; ix++) {
    for (Int_t iy=0; iy<5; iy++) {
      fCovariance[ix][iy] = -1.0;
    }
  }
}

void TRecoGigaTrackerCandidate::SetPosition(Int_t StationNo,TVector3 value){
  if (StationNo>2 || StationNo<0) {
    //std::cout<<"file: "<<__FILE__<<" at line: "<<__LINE__<<" there is 3 GTK stations"<<endl;
    return;
  }
  fPosition[StationNo] = value;
}

void TRecoGigaTrackerCandidate::SetTimeStation(Int_t StationNo,Double_t value){
  if (StationNo>2 || StationNo<0) {
    //std::cout<<"file: "<<__FILE__<<" at line: "<<__LINE__<<" there is 3 GTK stations"<<endl;
    return;
  }
  fTimeStation[StationNo] = value;
}

Double_t TRecoGigaTrackerCandidate::GetTimeStation(Int_t StationNo){
  if (StationNo>2 || StationNo<0) {
    //std::cout<<"file: "<<__FILE__<<" at line: "<<__LINE__<<" there is 3 GTK stations"<<endl;
    return 0;
  }
  return fTimeStation[StationNo]; 
}

TVector3 TRecoGigaTrackerCandidate:: GetPosition(Int_t StationNo){
  if (StationNo>2 || StationNo<0) {
    //std::cout<<"file: "<<__FILE__<<" at line: "<<__LINE__<<" there is 3 GTK stations"<<endl;
    return TVector3(0.0,0.0,0.0);
  }
  return fPosition[StationNo]; 
}
