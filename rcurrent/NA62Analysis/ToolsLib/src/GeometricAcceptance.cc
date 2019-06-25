// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-01-28
//
// ------------------------------------------------------------------

/// \class GeometricAcceptance
/// \Brief
/// Reference values for detector geometry; geometric acceptance checks for Spectrometer tracks and LKr clusters
/// \EndBrief
/// \Detailed
/// Provides the reference values of detector positions and acceptances.
/// Evaluates the standard geometric acceptance for 1) spectrometer candidates in Straw chamber,
/// RICH, CHOD, NewCHOD, LAV, IRC, LKr, MUV1, MUV2, MUV3 and SAC planes; 2) LKr clusters in the LKr front plane.
/// Standard pre-defined inner (par1) and outer (par2) "radii" of the acceptance cuts are defined for each detector.
/// The outer "radius" can refer to a half-size of the of a square or an apothem of a regular octagon
/// depending on the subdetector; see further documentation for data members and the source code.
/// Non-standard settings for the inner and outer "radii" of the acceptance cut can be specified by user.
/// Parameters of InAcceptance(): 1) pointer a candidate or x and y coordinates in detector entry plane;
/// 2) detector ID; 3) station ID (meaningful for Straw and LAV only);
/// 4) and 5): par1 and par2 are the inner and outer radii re-defined by user.
/// Only parameters 1 and 2 are mandatory.
/// An example of use is below.
/// \code
/// TRecoSpectrometerCandidate* Scand = (TRecoSpectrometerCandidate*)STRAWevent->GetCandidate(0);
/// bool in_straw1 = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0); // chamber 1
/// bool in_straw2 = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1); // chamber 2
/// bool in_straw3 = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2); // chamber 3
/// bool in_straw4 = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3); // chamber 4
/// bool in_chod   = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kCHOD, 0, 200, 1000); // user-defined boundaries
/// bool in_lav12  = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kLAV, kLAV12); // TRUE means INSIDE the central vacuum volume
/// bool in_lkr    = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kLKr);
/// bool in_RICH   = GeometricAcceptance::GetInstance()->InAcceptance(Scand, kRICH);
/// ...
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "GeometricAcceptance.hh"

using namespace std;

static GeometricAcceptance* fInstance = 0;

GeometricAcceptance* GeometricAcceptance::GetInstance() {
  if (!fInstance) fInstance = new GeometricAcceptance();
  return fInstance;
}

GeometricAcceptance::GeometricAcceptance():
  fZGTK1                  ( 79600.0),
  fZGTK2                  ( 92800.0),
  fZTrim5                 (101800.0), // Trim5 magnet centre
  fZGTK3                  (102400.0),
  fZStraw                 {183508.0, 194066.0, 204459.0, 218885.0},
  fStrawViewHoleSize      (    63.8), // half of the beam hole size in each straw chamber view
  fXStrawChamberCentre    {101.2, 114.4, 92.4, 52.8},
  fZRICHFrontPlane        (219445.0),
  fZRICHMirror            (236873.0), // from Mauro Piccini
  fZRICHBackPlane         (237326.0),
  fXRICHFrontPlaneCentre  (    34.0),
  fXRICHBackPlaneCentre   (     2.0),
  fZCHODVPlane            (239009.0),
  fZCHODHPlane            (239389.0),
  fZNewCHODFront          (238100.0), // front of first scintillator layer
  fZNewCHOD               (238131.5), // centre of NewCHOD
  fZNewCHODBack           (238163.0), // back of second scintillator layer
  fZIRC                   (239700.0),
  fZLKr                   (241093.0),
  fLKrCellSize            (19.7383881),
  fZMUV0                  (237358.0), // MUV0 front plane
  fZMUV1                  (243418.0), // MUV1 front plane
  fZMUV2                  (244435.0), // MUV2 front plane
  fZMUV3                  (246800.0), // MUV3 front plane
  fZSAC                   (260956.0),
  fZArgonion              (264500.0),

  fMUV3GapWidth           (     0.8),

  fRmax_Straw             (  1000.0), // radius
  fRmin_RICH              (   100.0), // radius
  fRmax_RICH              (  1100.0), // radius
  fRmin_CHOD              (   130.0), // radius
  fRmax_CHOD              (  1100.0), // radius
  fRmin_NewCHOD           (   140.0), // radius
  fRmax_NewCHOD           (  1070.0), // radius
  fRmin_MUV1              (   130.0), // radius
  fRmax_MUV1              (  1100.0), // half-side of a square
  fRmin_MUV2              (   130.0), // radius
  fRmax_MUV2              (  1100.0), // half-side of a square
  fRmin_MUV3              (   103.0), // radius
  fRmax_MUV3              (  1320.0), // half-side of a square
  fRmin_IRC               (    60.0), // radius
  fRmax_IRC               (   145.0), // radius
  fRmin_LKr               (   150.0), // radius
  fRmax_LKr               (  1130.0), // apothem of a regular octagon
  fRmax_SAC               (   100.0)  // half-side of a square (the SAC is 205*205mm^2)
{
  fRmin_Straw = fStrawViewHoleSize;   // default minimal radius = beam hole half-size
  // LAV stations: front and back Z planes, inner and outer radii
  Double_t f[12] =
    {121363, 128973, 136583, 144193, 151803, 165313,
     172823, 180333, 192709, 203102, 217528, 238313};
  Double_t b[12] =
    {121953, 129563, 137173, 144783, 152393, 165903,
     173413, 180923, 193179, 203572, 217998, 238783};
  Double_t ri[12] =
    { 536.5, 536.5, 536.5, 536.5, 536.5,  767.5,
      767.5, 767.5, 980.0, 980.0, 980.0, 1070.0}; // inner radius
  Double_t ro[12] =
    {  906.5,  906.5,  906.5,  906.5,  906.5, 1137.5,
      1137.5, 1137.5, 1350.0, 1350.0, 1350.0, 1440.0}; // outer radius
  for (Int_t i=0; i<12; i++) {
    fZLAVFront[i]  = f[i];
    fZLAVBack [i]  = b[i];
    fRinner_LAV[i] = ri[i];
    fRouter_LAV[i] = ro[i];
  }
}

Bool_t GeometricAcceptance::InAcceptance
(DownstreamTrack *Track, Int_t DetectorID, Int_t StationID, Double_t par1, Double_t par2) {
  TRecoSpectrometerCandidate *Scand = Track->GetSpectrometerCandidate();
  return InAcceptance(Scand, DetectorID, StationID, par1, par2);
}

Bool_t GeometricAcceptance::InAcceptance
(TRecoVCandidate *Candidate, Int_t DetectorID, Int_t StationID, Double_t par1, Double_t par2) {

  ///////////////////////////////////////////////////////////////////
  // Acceptance for spectrometer tracks
  // par1 and par2 are inner and outer radii, unless stated otherwise

  if (Candidate->IsA() == TRecoSpectrometerCandidate::Class()) {
    TRecoSpectrometerCandidate *Scand = static_cast<TRecoSpectrometerCandidate*>(Candidate);
    if (DetectorID == kSpectrometer) { // Straw acceptance
      if (StationID>=0 && StationID<=3) {
	Double_t x = Scand->xAt(fZStraw[StationID]);
	Double_t y = Scand->yAt(fZStraw[StationID]);
        return InAcceptance(x, y, DetectorID, StationID, par1, par2);
      }
      else {
	cout << "[GeometricAcceptance] Invalid STRAW station ID=" << StationID << endl;
	return kFALSE;
      }
    }
    else if (DetectorID==kRICH) { // RICH acceptance
      Double_t x    = Scand->xAt(fZRICHFrontPlane) - fXRICHFrontPlaneCentre;
      Double_t y    = Scand->yAt(fZRICHFrontPlane);
      Double_t R    = sqrt(x*x+y*y);
      Double_t Rmin = (par1>0.0) ? par1 : fRmin_RICH;
      return (R>Rmin);
    }
    else if (DetectorID==kCHOD) { // CHOD acceptance
      Double_t xV = Scand->xAtAfterMagnet(fZCHODVPlane);
      Double_t yV = Scand->yAtAfterMagnet(fZCHODVPlane);
      Double_t xH = Scand->xAtAfterMagnet(fZCHODHPlane);
      Double_t yH = Scand->yAtAfterMagnet(fZCHODHPlane);
      Double_t RV = sqrt(xV*xV+yV*yV);
      Double_t RH = sqrt(xH*xH+yH*yH);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_CHOD;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_CHOD;
      return (RV>R1 && RV<R2 && RH>R1 && RH<R2);
    }
    else if (DetectorID==kNewCHOD) { // NewCHOD acceptance
      Double_t x  = Scand->xAtAfterMagnet(fZNewCHOD);
      Double_t y  = Scand->yAtAfterMagnet(fZNewCHOD);
      Double_t R  = sqrt(x*x+y*y);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_NewCHOD;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_NewCHOD;
      return (R>R1 && R<R2);
    }
    else if (DetectorID==kLAV) { // LAV acceptance: front and back planes
      if (StationID>=0 && StationID<=11) {
	Double_t xf = Scand->xAtAfterMagnet(fZLAVFront[StationID]);
	Double_t yf = Scand->yAtAfterMagnet(fZLAVFront[StationID]);
	Double_t Rf = sqrt(xf*xf+yf*yf);
	Double_t xb = Scand->xAtAfterMagnet(fZLAVBack[StationID]);
	Double_t yb = Scand->yAtAfterMagnet(fZLAVBack[StationID]);
	Double_t Rb = sqrt(xb*xb+yb*yb);
	Double_t Rmax = (par2>0.0) ? par2 : fRinner_LAV[StationID];
	return (Rf<Rmax && Rb<Rmax);
      }
      else {
	cout << "[GeometricAcceptance] Invalid LAV station ID=" << StationID << endl;
	return kFALSE;
      }
    }
    else if (DetectorID==kIRC) { // IRC acceptance
      Double_t x  = Scand->xAtAfterMagnet(fZIRC);
      Double_t y  = Scand->yAtAfterMagnet(fZIRC);
      Double_t R  = sqrt(x*x+y*y);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_IRC;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_IRC;
      return (R>R1 && R<R2);
    }
    else if (DetectorID==kLKr) { // LKr acceptance
      Double_t ax = fabs(Scand->xAtAfterMagnet(fZLKr));
      Double_t ay = fabs(Scand->yAtAfterMagnet(fZLKr));
      Double_t R  = sqrt(ax*ax+ay*ay);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_LKr;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_LKr;
      return (R>R1 && ax<R2 && ay<R2 && ax+ay<sqrt(2.0)*R2);
    }
    else if (DetectorID==kMUV1) { // MUV1 acceptance
      Double_t x  = Scand->xAtAfterMagnet(fZMUV1);
      Double_t y  = Scand->yAtAfterMagnet(fZMUV1);
      Double_t R  = sqrt(x*x+y*y);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_MUV1;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_MUV1;
      return (R>R1 && fabs(x)<R2 && fabs(y)<R2);
    }
    else if (DetectorID==kMUV2) { // MUV2 acceptance
      Double_t x  = Scand->xAtAfterMagnet(fZMUV2);
      Double_t y  = Scand->yAtAfterMagnet(fZMUV2);
      Double_t R  = sqrt(x*x+y*y);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_MUV2;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_MUV2;
      return (R>R1 && fabs(x)<R2 && fabs(y)<R2);
    }
    else if (DetectorID==kMUV3) { // MUV3 acceptance
      Double_t x  = Scand->xAtAfterMagnet(fZMUV3);
      Double_t y  = Scand->yAtAfterMagnet(fZMUV3);
      Double_t R  = sqrt(x*x+y*y);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_MUV3;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_MUV3;
      return (R>R1 && fabs(x)<R2+0.5*fMUV3GapWidth && fabs(y)<R2);
    }
    else if (DetectorID==kSAC) { // SAC acceptance
      Double_t x = Scand->xAtAfterMagnet(fZSAC);
      Double_t y = Scand->yAtAfterMagnet(fZSAC);
      Double_t D = (par1>0.0) ? par1 : fRmax_SAC;
      return (fabs(x)<D && fabs(y)<D);
    }
  }

  /////////////////////////////////////
  // Provision for GTK track acceptance
  /*
  if (Candidate->IsA() == TRecoGigaTrackerCandidate::Class()) {
    TRecoGigaTrackerCandidate *Gcand = (TRecoGigaTrackerCandidate*)Candidate;
    if (DetectorID==kGigaTracker) {
    }
    if (DetectorID==kCHANTI) {
    }
  }
  */

  //////////////////////////////
  // Acceptance for LKr clusters

  if (Candidate->IsA() == TRecoLKrCandidate::Class()) {
    TRecoLKrCandidate *Lcand = static_cast<TRecoLKrCandidate*>(Candidate);
    if (DetectorID==kLKr) {
      // par1 is the minimum radius, par2 is the maximum radius
      Double_t x  = Lcand->GetClusterX();
      Double_t y  = Lcand->GetClusterY();
      Double_t ax = fabs(x);
      Double_t ay = fabs(y);
      Double_t R  = sqrt(ax*ax+ay*ay);
      Double_t R1 = (par1>0.0) ? par1 : fRmin_LKr;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_LKr;
      return (R>R1 && ax<R2 && ay<R2 && ax+ay<sqrt(2.0)*R2);
    }
  }
  return kTRUE;
}

/////////////////////////////////////////////////////
// A simplified version: acceptance for a point (x,y)

Bool_t GeometricAcceptance::InAcceptance
(Double_t x, Double_t y, Int_t DetectorID, Int_t StationID, Double_t par1, Double_t par2) {
  // par1 is the minimum radius, par2 is the maximum radius

  if (DetectorID == kSpectrometer) { // Straw acceptance
    if (StationID>=0 && StationID<=3) {
      Double_t xshifted = x - fXStrawChamberCentre[StationID];
      Double_t ushifted = (xshifted - y) / sqrt(2.);
      Double_t vshifted = (xshifted + y) / sqrt(2.);
      // at least two Straw views (outside square regions defined by beam hole)
      Bool_t atLeastTwoViews =
        (fabs(xshifted) > fStrawViewHoleSize || fabs(y) > fStrawViewHoleSize) &&
        (fabs(ushifted) > fStrawViewHoleSize || fabs(vshifted) > fStrawViewHoleSize);
      Double_t R = sqrt(x*x + y*y); // wrt x=y=0, for the outer cut
      Double_t Rshifted = sqrt(pow(xshifted, 2) + y*y); // wrt hole centre, for the inner cut
      Double_t R1 = (par1>0.0) ? par1 : fRmin_Straw;
      Double_t R2 = (par2>0.0) ? par2 : fRmax_Straw;
      return (atLeastTwoViews && Rshifted>R1 && R<R2);
    }
    else {
      cout << "[GeometricAcceptance] Invalid STRAW station ID=" << StationID << endl;
      return kFALSE;
    }
  }
  else if (DetectorID==kRICH) { // RICH acceptance
    Double_t xc = x - fXRICHFrontPlaneCentre;
    Double_t R  = sqrt(xc*xc+y*y);
    Double_t Rmin = (par1>0.0) ? par1 : fRmin_RICH;
    return (R>Rmin);
  }
  else if (DetectorID==kCHOD) { // CHOD acceptance: vertical plane only
    Double_t R = sqrt(x*x+y*y);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_CHOD;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_CHOD;
    return (R>R1 && R<R2);
  }
  else if (DetectorID==kNewCHOD) { // NewCHOD acceptance
    Double_t R  = sqrt(x*x+y*y);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_NewCHOD;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_NewCHOD;
    return (R>R1 && R<R2);
  }
  else if (DetectorID==kLAV) {
    if (StationID>=0 && StationID<=11) {
      Double_t R  = sqrt(x*x+y*y);
      Double_t R2 = (par2>0.0) ? par2 : fRinner_LAV[StationID];
      return (R<R2);
    }
    else {
      cout << "[GeometricAcceptance] Invalid LAV station ID=" << StationID << endl;
      return kFALSE;
    }
  }
  else if (DetectorID==kIRC) { // IRC acceptance
    Double_t R  = sqrt(x*x+y*y);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_IRC;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_IRC;
    return (R>R1 && R<R2);
  }
  else if (DetectorID==kLKr) { // LKr acceptance
    Double_t ax = fabs(x);
    Double_t ay = fabs(y);
    Double_t R  = sqrt(ax*ax+ay*ay);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_LKr;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_LKr;
    return (R>R1 && ax<R2 && ay<R2 && ax+ay<sqrt(2.0)*R2);
  }
  else if (DetectorID==kMUV1) { // MUV1 acceptance
    Double_t R  = sqrt(x*x+y*y);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_MUV1;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_MUV1;
    return (R>R1 && fabs(x)<R2 && fabs(y)<R2);
  }
  else if (DetectorID==kMUV2) { // MUV2 acceptance
    Double_t R  = sqrt(x*x+y*y);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_MUV2;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_MUV2;
    return (R>R1 && fabs(x)<R2 && fabs(y)<R2);
  }
  else if (DetectorID==kMUV3) { // MUV3 acceptance
    Double_t R  = sqrt(x*x+y*y);
    Double_t R1 = (par1>0.0) ? par1 : fRmin_MUV3;
    Double_t R2 = (par2>0.0) ? par2 : fRmax_MUV3;
    return (R>R1 && fabs(x)<R2+0.5*fMUV3GapWidth && fabs(y)<R2);
  }
  else if (DetectorID==kSAC) { // SAC acceptance
    Double_t D = (par1>0.0) ? par1 : fRmax_SAC;
    return (fabs(x)<D && fabs(y)<D);
  }
  return kTRUE;
}

///////////////////////////////////////////////////////////////////////
// A simplified version for KineParts; check the acceptance for a point

Bool_t GeometricAcceptance::InAcceptance
(KinePart *part, Int_t DetectorID, Int_t StationID, Double_t par1, Double_t par2) {
  Double_t z = -999;

  if (DetectorID==kSpectrometer && (StationID<0 || StationID>3)) {
    cout << "[GeometricAcceptance] Invalid STRAW station ID=" << StationID << endl;
    return kFALSE;
  }
  if (DetectorID==kLAV && (StationID<0 || StationID>11)) {
    cout << "[GeometricAcceptance] Invalid LAV station ID=" << StationID << endl;
    return kFALSE;
  }

  if      (DetectorID==kSpectrometer) z = fZStraw[StationID];
  else if (DetectorID==kCHOD)         z = fZCHODVPlane;
  else if (DetectorID==kRICH)         z = fZRICHFrontPlane;
  else if (DetectorID==kNewCHOD)      z = fZNewCHOD;
  else if (DetectorID==kIRC)          z = fZIRC;
  else if (DetectorID==kLAV)          z = fZLAVFront[StationID];
  else if (DetectorID==kLKr)          z = fZLKr;
  else if (DetectorID==kMUV1)         z = fZMUV1;
  else if (DetectorID==kMUV2)         z = fZMUV2;
  else if (DetectorID==kMUV3)         z = fZMUV3;
  else if (DetectorID==kSAC)          z = fZSAC;
  if (z<0) return kFALSE;
  return InAcceptance(part->xAt(z), part->yAt(z), DetectorID, StationID, par1, par2);
}

///////////////////////////////////////////////////////////////////////
// A simplified version for given SpacePoint and direction; check the acceptance for a point

Bool_t GeometricAcceptance::InAcceptance
(TVector3 SpacePoint, TVector3 Direction, Int_t DetectorID, Int_t StationID, Double_t par1, Double_t par2){
  Double_t z = -999;

  if (DetectorID==kSpectrometer && (StationID<0 || StationID>3)) {
    cout << "[GeometricAcceptance] Invalid STRAW station ID=" << StationID << endl;
    return kFALSE;
  }
  if (DetectorID==kLAV && (StationID<0 || StationID>11)) {
    cout << "[GeometricAcceptance] Invalid LAV station ID=" << StationID << endl;
    return kFALSE;
  }

  if      (DetectorID==kSpectrometer) z = fZStraw[StationID];
  else if (DetectorID==kCHOD)         z = fZCHODVPlane;
  else if (DetectorID==kRICH)         z = fZRICHFrontPlane;
  else if (DetectorID==kNewCHOD)      z = fZNewCHOD;
  else if (DetectorID==kIRC)          z = fZIRC;
  else if (DetectorID==kLAV)          z = fZLAVFront[StationID];
  else if (DetectorID==kLKr)          z = fZLKr;
  else if (DetectorID==kMUV1)         z = fZMUV1;
  else if (DetectorID==kMUV2)         z = fZMUV2;
  else if (DetectorID==kMUV3)         z = fZMUV3;
  else if (DetectorID==kSAC)          z = fZSAC;
  if (z-SpacePoint.z()<0) return kFALSE;

  Double_t x = SpacePoint.x()+Direction.x()*(z-SpacePoint.z())/Direction.z();
  Double_t y = SpacePoint.y()+Direction.y()*(z-SpacePoint.z())/Direction.z();

  return InAcceptance(x, y, DetectorID, StationID, par1, par2);
}
