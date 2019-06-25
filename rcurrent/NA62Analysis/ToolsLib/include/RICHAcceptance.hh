#ifndef RICHACCEPTANCE_HH
#define RICHACCEPTANCE_HH

#include "TMath.h"
#include "KinePart.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "TRecoLKrCandidate.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "DownstreamTrack.hh"
#include <iostream>

class RICHAcceptance {

public:
  static RICHAcceptance* GetInstance();

  Double_t GetZRICHFrontPlane()       { return fZRICHFrontPlane;       }
  Double_t GetZRICHMirror()           { return fZRICHMirror;           }
  Double_t GetZRICHBackPlane()        { return fZRICHBackPlane;        }
  Double_t GetXRICHFrontPlaneCentre() { return fXRICHFrontPlaneCentre; }
  Double_t GetXRICHBackPlaneCentre()  { return fXRICHBackPlaneCentre;  }

  Bool_t GetRICHMirrorPlaneAcceptance(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionMirrorPlaneJura(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionMirrorPlaneSaleve(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionMirrorPlane(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Bool_t RICHMirrorPlaneIntersectionByTrack(TVector3 TrackPosition, TVector3 TrackMomentum);
  Bool_t GetRICHBeamPipeAcceptance(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius);
  Bool_t RICHBeamPipeIntersectionByTrack(TVector3 TrackPosition, TVector3 TrackMomentum);
  Bool_t GetRICHPMPlaneAcceptance(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionPMPlaneJura(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionPMPlaneSaleve(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionJura(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFractionSaleve(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);
  Double_t GetRICHRingFraction(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut);

private:

  RICHAcceptance();
  ~RICHAcceptance() {}

  //////////////////////
  // Geometry parameters

  Double_t fZRICHFrontPlane;        ///< RICH front plane
  Double_t fZRICHMirror;            ///< RICH mirror plane
  Double_t fZRICHBackPlane;         ///< RICH back plane
  Double_t fXRICHFrontPlaneCentre;  ///< X coordinate of the cut centre at the RICH front plane
  Double_t fXRICHBackPlaneCentre;   ///< X coordinate of the cut centre at the RICH back plane

  Bool_t   fRICHMirrorAcceptance;
  Bool_t   fRICHBeamPipeAcceptance;
  Bool_t   fRICHPMAcceptance;
  Bool_t   fRICHPMAcceptanceSaleve;
  Bool_t   fRICHPMAcceptanceJura;
  Double_t fXRICHBeamPipeHoleCentre;
  Double_t fYRICHBeamPipeHoleCentre;
  Double_t fRICHBeamPipeHoleRadius;
  Double_t fRICHMirrorFocalLength;
  Bool_t   fInSaleve;
  Bool_t   fInJura;
  Double_t fPMReference[2];
  Double_t fSaleveRotation[2];
  Double_t fJuraRotation[2];
  Double_t fRICHPMAreaRadius;
  Double_t fRICHBoundaryLine_SaleveJura[6][2];
  Double_t fRICHMirrorExternalCorner[31][2];
};

#endif
