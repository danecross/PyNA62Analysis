// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-07-27
//
// ---------------------------------------------------------------

#include <iostream>
#include "SpectrometerRICHAssociationOutput.hh"

using namespace std;

/// \class SpectrometerRICHAssociationOutput
/// \Brief
///  RICH particle identification information for each spectrometer track
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndBrief

SpectrometerRICHAssociationOutput::SpectrometerRICHAssociationOutput() {
  Clear();
}

void SpectrometerRICHAssociationOutput::Clear() {
  fTrackID               = -1;
  fValid                 = false;
  fTrackMirrorID         = -1;
  fTrackPosOnMirror      = TVector2(0.0, 0.0);
  fDistToMirrorCentre    = 0.0;
  fPredictedCentre       = TVector2(0.0, 0.0);
  fPredictedCentreJura   = TVector2(0.0, 0.0);
  fPredictedCentreSaleve = TVector2(0.0, 0.0);
  fTrackTimeForAssociation = 0.0;
  fNInTimeHits           = 0;
  fNOutOfTimeHits        = 0;
  fNObservedHits         = 0;
  fNBackgroundHits       = 0;
  fMostLikelyHypothesis  = kRICHHypothesisBackground;
  fRingCentre            = TVector2(0.0, 0.0);
  fRingCentreError       = TVector2(0.0, 0.0);
  fRingRadius            = 0.0;
  fRingRadiusError       = 0.0;
  fRingFitChi2           = 0.0;
  for (Int_t i=0; i<MaxHypos; i++) {
    fLikelihood[i] = 1.0;
    fPredictedRadius[i] = 0.0;
    fMirrorFraction[i][0] = fMirrorFraction[i][1] = 0.0;
    fPMTFraction[i][0] = fPMTFraction[i][1] = 0.0;
    fNExpectedSignalHits[i] = 0.0;
    fNHitsAssigned[i] = 0;
    fRingTime[i] = -999.0;
    fAssignedHits[i].clear();
  }
}

void SpectrometerRICHAssociationOutput::AssignHit(int h, TRecoRICHHit* hit) {
  if (h>MaxHypos || h<0) {
    cout << "[SpectrometerRICHAssociationOutput::GetAssignedHits] Error: invalid hypothesis" << endl;
    return;
  }
  // cppcheck-suppress arrayIndexOutOfBoundsCond
  fAssignedHits[h].push_back(hit);
}

std::vector<TRecoRICHHit*> SpectrometerRICHAssociationOutput::GetAssignedHits(int h) {
 if (h>MaxHypos || h<0) {
   cout << "[SpectrometerRICHAssociationOutput::GetAssignedHits] Error: invalid hypothesis" << endl;
    std::vector<TRecoRICHHit*> tmp;
    return tmp;
 }
 // cppcheck-suppress arrayIndexOutOfBoundsCond
 return fAssignedHits[h];
}

void SpectrometerRICHAssociationOutput::Print() {
  cout << "fTrackID =               " << fTrackID << endl;
  cout << "fValid =                 " << fValid << endl;
  cout << "fLikelihood[] =          " <<
    fLikelihood[kRICHHypothesisBackground]<<" "<<
    fLikelihood[kRICHHypothesisElectron]<<" "<<
    fLikelihood[kRICHHypothesisMuon]<<" "<<
    fLikelihood[kRICHHypothesisPion]<<" "<<
    fLikelihood[kRICHHypothesisKaon]<<endl;
  cout << "fMostLikelyHypothesis =  " << fMostLikelyHypothesis << endl;
  cout << "fTrackMirrorID =         " << fTrackMirrorID << endl;
  cout << "fTrackPosOnMirror =      " << fTrackPosOnMirror.X()<<" "<<fTrackPosOnMirror.Y()<<endl;
  cout << "fDistToMirrorCentre =    " << fDistToMirrorCentre << endl;
  cout << "fPredictedCentre =       " << fPredictedCentre.X() << " " << fPredictedCentre.Y() << endl;
  cout << "fPredictedCentreJura =   " << fPredictedCentreJura.X() << " " << fPredictedCentreJura.Y() << endl;
  cout << "fPredictedCentreSaleve = " << fPredictedCentreSaleve.X() << " " << fPredictedCentreSaleve.Y() << endl;
  cout << "fPredictedRadius[] =     " <<
    fPredictedRadius[kRICHHypothesisBackground]<<" "<<
    fPredictedRadius[kRICHHypothesisElectron]<<" "<<
    fPredictedRadius[kRICHHypothesisMuon]<<" "<<
    fPredictedRadius[kRICHHypothesisPion]<<" "<<
    fPredictedRadius[kRICHHypothesisKaon]<<endl;
  cout << "fNObservedHits =         " << fNObservedHits << endl;
  cout << "fNBackgroundHits =       " << fNBackgroundHits << endl;
  cout << "fNExpectedSignalHits[] = " <<
    fNExpectedSignalHits[kRICHHypothesisBackground]<<" "<<
    fNExpectedSignalHits[kRICHHypothesisElectron]<<" "<<
    fNExpectedSignalHits[kRICHHypothesisMuon]<<" "<<
    fNExpectedSignalHits[kRICHHypothesisPion]<<" "<<
    fNExpectedSignalHits[kRICHHypothesisKaon]<<endl;
  cout << "fNHitsAssigned[] =       " <<
    fNHitsAssigned[kRICHHypothesisBackground]<<" "<<
    fNHitsAssigned[kRICHHypothesisElectron]<<" "<<
    fNHitsAssigned[kRICHHypothesisMuon]<<" "<<
    fNHitsAssigned[kRICHHypothesisPion]<<" "<<
    fNHitsAssigned[kRICHHypothesisKaon]<<endl;
  cout << "fRingCentre =            " << fRingCentre.X()<<" "<<fRingCentre.Y()<<endl;
  cout << "fRingCentreError =       " << fRingCentreError.X()<<" "<<fRingCentreError.Y()<<endl;
  cout << "fRingRadius =            " << fRingRadius << endl;
  cout << "fRingRadiusError =       " << fRingRadiusError << endl;
  cout << "fRingFitChi2 =           " << fRingFitChi2 << endl;
  cout << "-----" << endl;
}
