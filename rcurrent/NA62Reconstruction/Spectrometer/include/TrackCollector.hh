#ifndef TrackCollector_H
#define TrackCollector_H 1

#include <vector>
#include "Combination.hh"
#include "ChamberHitCollector.hh"
#include "TVector3.h"
#include <stdarg.h>
#include "TRecoSpectrometerEvent.hh"
#include "Track.hh"
#include "Event.hh"

class SpectrometerParameters;

class TrackCollector
{

public:
  explicit TrackCollector(ChamberCollector *);
  virtual ~TrackCollector();
  void Reset();
  void SaveHistograms();
  void Reconstruct(TRecoSpectrometerEvent *);
  Int_t GetNCombinations() {return (Int_t)fCombination.size();};
  Combination *GetCombination(Int_t j) {return fCombination.at(j);}
  Int_t GetNTracks() { return fRecoEvent->GetNCandidates();}
  TRecoSpectrometerCandidate *GetTrack(Int_t j) {
    return static_cast<TRecoSpectrometerCandidate *>(fRecoEvent->GetCandidates()->At(j));
  }

private:
  ChamberCollector *fChamber; ///< Pointer to the ensemble of the reconstructed chamber-hits.
  TRecoSpectrometerEvent *fRecoEvent; ///< Pointer to the spectrometer reconstructed event class. 
  Event *fGenEvent; ///< Pointer to the generated event class.
  Track *fTrack; ///< Pointer to the Track class. 
  std::vector<Combination*> fCombination; ///< vector of pointers to the Combination class.
  SpectrometerParameters *fPar;

  // General
  Bool_t fIsMuonRun;
  Double_t fThetaMS;
  Double_t fSigmaXY;
  Double_t fZcoord[4];
  Double_t fXspread[4];
  Double_t fYspread[4];
  Int_t fNCouple[4];
  Double_t fDeg90; ///< Angle of 0.49*pi-greco used to flag the y view.
  Double_t fEC; ///< e*c
  Double_t fBMag; ///< Intensity of the magnetic field
  Double_t dMag; ///< Thickness of the magnet
  Double_t fZMag; ///< Position of middle of the magnet along Z. 
  Int_t fNChamberHit[4]; ///< Total number of chamber-hits per chamber.
  Double_t fMomentumMax; ///< Maximum momentum for combination reconstruction in TrackCollector. 
  Double_t fAngleMax; ///< Maximum slope for combination reconstruction in TrackCollector. 
  Double_t fCombQualityCut; 
  Double_t fCombHoughDeltaCut; 
  Double_t fInterTtrailingCut;
  Double_t f3HitsCombRadiusAtMC;

  // Main
  Int_t FewHits();
  ChamberHitCollector *GetChamber(Int_t jChamber){return fChamber->at(jChamber);};
  void PatternRecognition();

  // Pattern Recognition
  void FindCombinations(Int_t);
  Int_t NextIteration(Int_t,Int_t,Int_t*);
  Double_t HoughTransformation(Int_t, Double_t*, Double_t*, Double_t, Double_t*);
  Double_t XZPlaneRecognition(Double_t*, Double_t*, Double_t*);
  Double_t XZPlaneRecognition(Int_t, Double_t*, Double_t*, Double_t, Double_t*);
  Bool_t RemoveCombination(const Combination&);
  void RemoveCombination();
  void PrintoutCombination();
  void ComputeTotalHitNumber(); 

  // Track reconstruction
  void TrackMeasurement(); 

};
#endif
