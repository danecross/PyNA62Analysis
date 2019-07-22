// ------------------------------------------------------------------
// History:
//
// Created by Anne Chappuis (anne.chappuis9@gmail.com) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-06-18
//
// ------------------------------------------------------------------

#ifndef BLUETUBETRACKER_HH
#define BLUETUBETRACKER_HH

#include "TMath.h"
#include <iostream>
#include <TVector3.h>
#include "TMatrixD.h"

class BlueTubeTracker {

public:

  static BlueTubeTracker* GetInstance();

  void TrackParticle(Bool_t PropagateCovarianceMatrix=false);
  void SetInitialPosition(TVector3 value) { fInitialPosition = value; }
  void SetInitialPosition(Double_t x, Double_t y, Double_t z) { fInitialPosition = TVector3(x,y,z); }
  void SetInitialMomentum(TVector3 value) { fInitialMomentum = value; }
  void SetInitialMomentum(Double_t x, Double_t y, Double_t z) { fInitialMomentum = TVector3(x,y,z); }
  void SetZFinal         (Double_t value) { fZFinal = value;          }
  void SetCharge         (Int_t value)    { fCharge = value;          }

  TVector3 GetInitialPosition()                 { return fInitialPosition; }
  TVector3 GetInitialMomentum()                 { return fInitialMomentum; }
  TVector3 GetFinalPosition()                   { return fFinalPosition;   }
  TVector3 GetFinalMomentum()                   { return fFinalMomentum;   }
  TVector3 GetFinalPositionNonCorrected();
  void     SetCovMatrix(TMatrixD cov)           { fCovMatrix = cov;        }
  TMatrixD GetCovMatrix()                       { return fCovMatrix;       }
  void     EnableSilentMode()                   { fSilentMode = true;      }
  void     DisableSilentMode()                  { fSilentMode = false;     }

private:

  BlueTubeTracker();
  ~BlueTubeTracker() {}
  static BlueTubeTracker* fInstance;

  void TrackOneStep(Int_t, Int_t, Bool_t);

  TVector3 fInitialPosition, fInitialMomentum;
  TVector3 fFinalPosition, fFinalMomentum;
  Double_t fZFinal; ///< Tracking is performed into this Z plane
  Int_t    fCharge; ///< Track charge
  Int_t    fSign;   ///< +1 or -1, as required to generalize formulae for backward/forward tracking
  Bool_t   fTrackingBackward; ///< Tracking backward or forward?
  Bool_t   fSilentMode;       ///< Issue warnings if coordinates are outside the nominal range?
  TVector3 fPosIn, fPosOut, fMomIn, fMomOut;

  Int_t    fNPlanes;
  Double_t fZmin, fZmax;
  Double_t fZ[40];
  Double_t fBx_param_integral[6][40], fBy_param_integral[4][40], fBz_param0_integral[40];
  Double_t fSpeedOfLight;
  TMatrixD fCovMatrix;
};

#endif
