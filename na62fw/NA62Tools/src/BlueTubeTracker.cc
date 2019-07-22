// --------------------------------------------------------------------
// History:
//
// Created by Anne Chappuis (anne.chappuis9@gmail.com) &
//            Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)      2015-06-18
// Forward tracking functionality added: E Goudzovski        2015-10-10
// Backward tracking outside BF region enabled: E Goudzovski 2016-05-17
//
// --------------------------------------------------------------------

/// \class BlueTubeTracker
/// \Brief
/// Tracker of charged particles in the blue tube magnetic field
/// \EndBrief
/// \Detailed
/// The parameterization of the blue tube field and the principle of the algorithm
/// are described in the note NA62-15-06. Tracking both upstream and downstream is implemented.
/// Input/output units are standard: [mm] and [MeV/c].
/// The Blue Field z range is 100 m < z < 183.311 m. The initial position must be in this range,
/// while the final position must be not downstream of this range. If these conditions are not met,
/// no tracking is performed, and the returned the final position and momentum coincide with the initial ones.
/// In this case the algorithm issues a warning message (unless fSilentMode is set to true).
/// If the requested final position is upstream of the Blue Field z range, extrapolation assuming constant
/// magnetic field outside the Blue Field z range is performed.
/// Covariance matrix (in the format used by the VertexLSF class) is also propagated, optionally.
/// An example of use is given below.
/// \code
/// #include "BlueTubeTracker.hh"
/// ...
/// BlueTubeTracker::GetInstance()->SetCharge(+1); // unit: positron charge
/// BlueTubeTracker::GetInstance()->SetInitialPosition(+1.60, +0.25, 180000);  // Unit: mm
/// BlueTubeTracker::GetInstance()->SetInitialMomentum(+0.33, +0.10,  10000);  // Unit: MeV/c
/// BlueTubeTracker::GetInstance()->SetZFinal(104000);                         // Unit: mm
/// BlueTubeTracker::GetInstance()->TrackParticle();
/// TVector3 NewPosition = BlueTubeTracker::GetInstance()->GetFinalPosition(); // Unit: mm
/// TVector3 NewMomentum = BlueTubeTracker::GetInstance()->GetFinalMomentum(); // Unit: MeV/c
/// \endcode
/// \author Anne Chappuis (anne.chappuis9@gmail.com), Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "BlueTubeTracker.hh"
#include "BlueTubeMagneticFieldMap.hh"

using namespace std;

BlueTubeTracker* BlueTubeTracker::fInstance = nullptr;

BlueTubeTracker* BlueTubeTracker::GetInstance() {
  if (!fInstance) fInstance = new BlueTubeTracker();
  return fInstance;
}

BlueTubeTracker::BlueTubeTracker() :
  fSign(0), fTrackingBackward(false), fSilentMode(false),
  //////////////////////////////////////////////////////////////////////////////////////
  // The 40 Z planes in BlueTubeMagneticFieldMap are from z=104458 mm to z=181175 mm.
  // Outside this range, constant magnetic field is assumed.
  // The valid range of initial and final Z coordinates extends beyond the above limits.
  fZmin(100000.0),
  fZmax(183311.0) // start of Spectrometer responsibility region 0 in NA62MC
{
  BlueTubeMagneticFieldMap* Map = new BlueTubeMagneticFieldMap(fZmin, fZmax);
  fNPlanes = Map->GetNPlanes();
  for (Int_t i=0; i<fNPlanes; i++) {
    fZ[i] = Map->GetZ(i);
    for (Int_t j=0; j<6; j++) fBx_param_integral[j][i] = Map->GetBxParamIntegral(j,i);
    for (Int_t j=0; j<4; j++) fBy_param_integral[j][i] = Map->GetByParamIntegral(j,i);
  }
  delete Map;
  fSpeedOfLight = 299792458.0; // [m/s]

  fInitialPosition = TVector3(0, 0, -999.);
  fInitialMomentum = TVector3(0, 0, -999.);
  fFinalPosition   = TVector3(0, 0, 0);
  fFinalMomentum   = TVector3(0, 0, 0);
  fZFinal = -999.;
  fCharge = 0;
  fCovMatrix.ResizeTo(5,5);
}

/////////////////////////////////////////////////////////////
// Compute the final position assuming straight line.
// Useful for debugging and quantifying the blue tube effect.

TVector3 BlueTubeTracker::GetFinalPositionNonCorrected() {
  return fInitialPosition +
    (fZFinal-fInitialPosition.Z())/fInitialMomentum.Z() * fInitialMomentum;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Track by one step, either forward or backward, depending on fTrackingBackward setting.
// Note: ipl2 - ipl1 = 1.

void BlueTubeTracker::TrackOneStep(Int_t ipl1, Int_t ipl2, Bool_t PropagateCovarianceMatrix) {

  // Step length
  Double_t StandardStepSize = fZ[ipl2] - fZ[ipl1];
  Double_t StepLength       = StandardStepSize;

  // Step length: special cases of irregular first and last steps
  if (fTrackingBackward) { // tracking backward
    if (fZ[ipl1]<fZFinal)
      StepLength = fZ[ipl2] - fZFinal;             // short last step
    else if (ipl1==0 && fZFinal<fZ[0])
      StepLength = fZ[1] - fZFinal;                // long last step
    else if (fZ[ipl2]>fInitialPosition[2])
      StepLength = fInitialPosition[2] - fZ[ipl1]; // short first step
    else if (ipl2==fNPlanes-1 && fInitialPosition[2]>fZ[ipl2])
      StepLength = fInitialPosition[2] - fZ[ipl1]; // long first step
  }
  else { // tracking forward
    if (fZ[ipl2]>fZFinal)
      StepLength = fZFinal - fZ[ipl1];             // short last step
    else if (ipl2==fNPlanes-1 && fZFinal>fZ[ipl2])
      StepLength = fZFinal - fZ[ipl1];             // long last step
    else if (fZ[ipl1]<fInitialPosition[2])
      StepLength = fZ[ipl2] - fInitialPosition[2]; // short first step
    else if (ipl1==0 && fInitialPosition[2]<fZ[0])
      StepLength = fZ[ipl2] - fInitialPosition[2]; // long first step
  }

  // Unit conversion for field integral computation: [mm] --> [cm],
  // required for compatibility with the BlueFieldMagneticFieldMap class.
  Double_t x = 0.1 * fPosIn.X();
  Double_t y = 0.1 * fPosIn.Y();

  // Evaluate the field integrals over the step from pre-computed quantities [mkT*mm]
  Double_t FieldIntX =
    fBx_param_integral[0][ipl1] +
    fBx_param_integral[1][ipl1]*x +
    fBx_param_integral[2][ipl1]*y +
    fBx_param_integral[3][ipl1]*x*x +
    fBx_param_integral[4][ipl1]*y*y +
    fBx_param_integral[5][ipl1]*x*y;
  Double_t FieldIntY =
    fBy_param_integral[0][ipl1] +
    fBy_param_integral[1][ipl1]*x +
    fBy_param_integral[2][ipl1]*y +
    fBy_param_integral[3][ipl1]*x*y;

  // Account for the non-standard first and last steps
  FieldIntX *= (StepLength / StandardStepSize);
  FieldIntY *= (StepLength / StandardStepSize);

  // Make the tracking step: we use the approximation Px,Py<<Pz and neglect the "transverse"
  // field gradients dB/dx, dB/dy over the length of the tracking step.
  fMomOut.SetX(fMomIn.X() + fSign * fCharge * FieldIntY * 1e-15 * fSpeedOfLight);
  fMomOut.SetY(fMomIn.Y() - fSign * fCharge * FieldIntX * 1e-15 * fSpeedOfLight);
  fMomOut.SetZ(sqrt(fMomIn.Mag2() - fMomOut.Perp2()));
  fPosOut.SetX(fPosIn.X() - 0.5 * fSign * StepLength *
	       (fMomIn.X()/fMomIn.Z() + fMomOut.X()/fMomOut.Z()));
  fPosOut.SetY(fPosIn.Y() - 0.5 * fSign * StepLength *
	       (fMomIn.Y()/fMomIn.Z() + fMomOut.Y()/fMomOut.Z()));

  // Set the output Z position, including the special case of the last step
  if (fTrackingBackward) {
    fPosOut.SetZ(fZ[ipl1]);
    if (fZ[ipl1]<fZFinal) fPosOut.SetZ(fZFinal); // short step
    if (ipl1==0 && fZFinal<fZ[ipl1]) fPosOut.SetZ(fZFinal); // long step beyond boundary
  }
  else {
    fPosOut.SetZ(fZ[ipl2]);
    if (fZ[ipl2]>fZFinal) fPosOut.SetZ(fZFinal); // short step
    if (ipl2==fNPlanes-1 && fZFinal>fZ[ipl2]) fPosOut.SetZ(fZFinal); // long step beyond boundary
  }

  // Covariance matrix propagation (by Plamen): required by ToolsLib/VertexLSF
  if (PropagateCovarianceMatrix) {
    Double_t A  = fCharge * 1e-15 * fSpeedOfLight / fMomIn.Z();
    Double_t ay = fBy_param_integral[1][ipl1];
    Double_t by = fBy_param_integral[2][ipl1];
    Double_t ey = fBy_param_integral[3][ipl1];
    Double_t ax = fBx_param_integral[1][ipl1];
    Double_t bx = fBx_param_integral[2][ipl1];
    Double_t ex = fBx_param_integral[3][ipl1];
    Double_t gx = fBx_param_integral[4][ipl1];
    Double_t lx = fBx_param_integral[5][ipl1];
    TMatrixD F(5,5);
    F(0,0) =  1.;
    F(0,1) =  0.;
    F(0,2) =  A*(ay+ey*y)*0.1;
    F(0,3) =  A*(by+ey*x)*0.1;
    F(0,4) =  0.;
    F(1,0) =  0.;
    F(1,1) =  1.;
    F(1,2) =  A*(ax+ex*y+2*gx*x)*0.1;
    F(1,3) =  A*(bx+ex*x+2*lx*y)*0.1;
    F(1,4) =  0.;
    F(2,0) = -1.*(StepLength);
    F(2,1) =  0.;
    F(2,2) =  1.-A*(ay+ey*y)*0.5*StepLength*0.1;
    F(2,3) = -A*(by+ey*x)*0.5*StepLength*0.1;
    F(2,4) =  0.;
    F(3,0) =  0.;
    F(3,1) = -1.*StepLength;
    F(3,2) = -1.*A*(ax+ex*y+2*gx*x)*0.5*StepLength*0.1;
    F(3,3) =  1.-A*(bx+ex*x+2*lx*y)*0.5*StepLength*0.1;
    F(3,4) =  0.;
    F(4,0) =  0.;
    F(4,1) =  0.;
    F(4,2) =  0.;
    F(4,3) =  0.;
    F(4,4) =  1.;
    TMatrixD cov1(F, TMatrixD::kMult, fCovMatrix);
    TMatrixD cov2(cov1, TMatrixD::kMultTranspose, F);
    fCovMatrix = cov2;
  }
}

////////////////////////////////////////////////////////
// Main method tracking a particle, to be called by user

void BlueTubeTracker::TrackParticle(Bool_t PropagateCovarianceMatrix) {

  ////////////////
  // Sanity checks

  fFinalPosition = fInitialPosition;
  fFinalMomentum = fInitialMomentum;

  // Initial position must be in the blue tube region
  if (fInitialPosition.Z()<fZmin || fInitialPosition.Z()>fZmax) {
    if (!fSilentMode)
      cout << "[BlueTubeTracker] initial position (" << fInitialPosition.Z() <<
	" mm) outside ["<<fZmin<<"; "<<fZmax<<"] mm: no tracking" << endl;
    return;
  }

  // Final position must not be downstream of the blue tube region
  if (fZFinal>fZmax) {
    if (!fSilentMode)
      cout << "[BlueTubeTracker] final position (" << fZFinal <<
	" mm) is downstream of Zmax = "<<fZmax<<" mm: no tracking" << endl;
    return;
  }

  if (fInitialMomentum.Z()<=0.0) {
    cout << "[BlueTubeTracker] error: particle must have positive Pz" << endl;
    return;
  }
  if (fabs(fCharge)!=1) {
    cout << "[BlueTubeTracker] error: charge |Q|=1 is expected" << endl;
    return;
  }

  //////////////////////////////////////////
  // Backward or forward tracking requested?

  fTrackingBackward = (fInitialPosition.Z()>fZFinal);
  fSign = fTrackingBackward ? +1 : -1;

  ///////////////////////////////////////////////////////////////////////////////
  // Find the two Z planes such that all tracking steps are between these planes.
  // Note the exception: the initial/final position can be
  // beyond the boudary planes, in which case long first/last steps are made.

  Double_t Zlower = (fTrackingBackward) ? fZFinal : fInitialPosition[2];
  Double_t Zupper = (fTrackingBackward) ? fInitialPosition[2] : fZFinal;

  Int_t iPlane1 = fNPlanes-1;
  while (iPlane1>0 && fZ[iPlane1]>Zlower) iPlane1--;
  Int_t iPlane2 = 0;
  while ((iPlane2<fNPlanes-1) && fZ[iPlane2]<Zupper) iPlane2++;
  if (iPlane2-iPlane1<2) {
    // cout << "[BlueTubeTracker] Z interval too small, using straight line propagation" << endl;
    fFinalPosition = GetFinalPositionNonCorrected();
    fFinalMomentum = fInitialMomentum;
    return;
  }

  fPosIn = fInitialPosition;
  fMomIn = fInitialMomentum;

  // Tracking backward from plane 2 to plane 1
  if (fTrackingBackward) {
    for (Int_t ipl2=iPlane2; ipl2>iPlane1; ipl2--) {
      TrackOneStep(ipl2-1, ipl2, PropagateCovarianceMatrix);
      fPosIn = fPosOut;
      fMomIn = fMomOut;
    }
  }
  // Tracking forward from plane 1 to plane 2
  else {
    for (Int_t ipl1=iPlane1; ipl1<iPlane2; ipl1++) {
      TrackOneStep(ipl1, ipl1+1, PropagateCovarianceMatrix);
      fPosIn = fPosOut;
      fMomIn = fMomOut;
    }
  }

  fFinalPosition = fPosOut;
  fFinalMomentum = fMomOut;
}
