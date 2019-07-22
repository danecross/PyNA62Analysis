// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-17
//
// ---------------------------------------------------------------

/// \class GigaTrackerScraperField
/// \Brief
/// The toroidal magnetic field of the scraper magnet upstream of GTK2
/// \EndBrief
/// \Detailed
/// The field is simulated following the "CERN Magnets Kit" by L.Gatignon (reference: CERN-OPEN-2004-03), Fig.2.
/// The geometry parameters are taken from NA62MC/Beam/datacard/halo_datacard_k12hika+.
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "GigaTrackerScraperField.hh"
#include "G4SystemOfUnits.hh"

///////////////////////////////////////////////
// Initialization of the field parameterization

GigaTrackerScraperField::GigaTrackerScraperField
(G4double ZReference, G4ThreeVector Position, G4double ZLength,
 G4double ApertureHalfWidth, G4double ApertureHalfHeight, G4double OverallHalfHeight,
 G4double FieldStrength) :
  fPosition          (Position + G4ThreeVector(0.0, 0.0, ZReference)), // transform from GTK frame to World frame
  fZLength           (ZLength),
  fApertureHalfWidth (ApertureHalfWidth),
  fApertureHalfHeight(ApertureHalfHeight),
  fOverallHalfHeight (OverallHalfHeight),
  fFieldStrength     (FieldStrength)
{
  /*
  G4double Point[4];
  G4double B[3];
  Point[2] = 89810.0;
  for (int x=-599; x<=599; x+=2) {
    for (int y=-499; y<=499; y+=2) {
      Point[0] = x;
      Point[1] = y;
      GetFieldValue(Point, B);
    }
  }
  */
}

/////////////////////////////////////////////////////////////
// Evaluation of the field value: used by Geant4 for tracking

void GigaTrackerScraperField::GetFieldValue(const G4double Point[4], G4double *B) const {
  G4double x = Point[0];
  G4double y = Point[1]-fPosition.y();
  G4double z = Point[2]-fPosition.z();

  B[0] = B[1] = B[2] = 0.0;
  if (fabs(z)>0.5*fZLength) return;

  G4double ax = fabs(x);
  G4double ay = fabs(y);
  if (ax<fApertureHalfWidth) {
    if (ay>fApertureHalfHeight && ay<fOverallHalfHeight) B[0] = +fFieldStrength;
  }
  else {
    G4double R = sqrt((ax-fApertureHalfWidth)*(ax-fApertureHalfWidth)+ay*ay);
    if (R>fApertureHalfHeight && R<fOverallHalfHeight) {
      G4double Phi = atan(ay/(ax-fApertureHalfWidth));
      B[0] = +fFieldStrength * sin(Phi);
      B[1] = -fFieldStrength * cos(Phi);
    }
  }
  if (x<0.0) B[1] = -B[1];
  if (y<0.0) B[0] = -B[0];
  //  std::cout << "@@@ " << x <<" "<< y << " | " << B[0]/tesla <<" " << B[1]/tesla << std::endl;
  return;
}
