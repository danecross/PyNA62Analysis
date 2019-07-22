// --------------------------------------------------------------------
// History:
//
// 2016-05-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Complete redesign: the real geometry is simulated
//
// 2015-03-19 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Fixed Responsibility region to adjust for A12 final design
//
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Updated to be consistent with the NewCHOD simulation
//
// Created by Giuseppe Ruggiero 04-09-2012
//
// --------------------------------------------------------------------

#include "MUV0GeometryParameters.hh"
#include "DetectorParameter.hh"

MUV0GeometryParameters* MUV0GeometryParameters::fInstance = 0;

MUV0GeometryParameters::MUV0GeometryParameters() : NA62VGeometryParameters(G4String("MUV0")) {

  // NB: the MUV0 responsibility region is longitudinally inside the RICH one.
  // The RICH responsibility region is (219.546 m; 237.700 m).

  fRespRegionZStart      = 237.350*m;
  fRespRegionZEnd        = 237.380*m;
  fRespRegionZCentre     = 0.5*(fRespRegionZStart+fRespRegionZEnd);
  fRespRegionZLength     = fRespRegionZEnd-fRespRegionZStart;
  fRespRegionXCentre     = 2245*mm; // The RR should not touch RICHMirrorWindowOuterFlange (R<1510mm)
  fRespRegionXLength     = 1450*mm;
  fRespRegionYLength     = 1450*mm;

  fScintillatorThickness =     20*mm;
  fDetectorXPosition     =   2245*mm;
  fDetectorYPosition     =      0*mm;
  fDetectorZPosition     = 237358*mm + 0.5*fScintillatorThickness;

  fFrameInnerSize        =   1400*mm;
  fFrameThickness        =     20*mm;
  fCoverThickness        =      2*mm;

  fResponsibilityRegion.push_back
    (new ResponsibilityRegion(fRespRegionZStart,fRespRegionZEnd));

  // Dimensions and positions (wrt detector detector centre) of the counters
  fNCounters = 9;
  G4double X0[9]    = {-40*cm, -40*cm, -40*cm,  10*cm,  10*cm,  10*cm,  50*cm,  50*cm,  50*cm};
  G4double Y0[9]    = {-50*cm, -10*cm, +40*cm, -50*cm, -10*cm, +40*cm, -50*cm, -10*cm, +40*cm};
  G4double Xsize[9] = { 60*cm,  60*cm,  60*cm,  40*cm,  40*cm,  40*cm,  40*cm,  40*cm,  40*cm};
  G4double Ysize[9] = { 40*cm,  40*cm,  60*cm,  40*cm,  40*cm,  60*cm,  40*cm,  40*cm,  60*cm};
  for (G4int i=0; i<9; i++) {
    fScintillatorPosition[i] =
      G4ThreeVector(X0[i], Y0[i], fDetectorZPosition-fRespRegionZCentre);
    fScintillatorSize[i] = G4ThreeVector(Xsize[i], Ysize[i], fScintillatorThickness);
  }
}

MUV0GeometryParameters* MUV0GeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new MUV0GeometryParameters();
  return fInstance;
}

TObjArray MUV0GeometryParameters::GetHashTable() {
  return 0;
}
