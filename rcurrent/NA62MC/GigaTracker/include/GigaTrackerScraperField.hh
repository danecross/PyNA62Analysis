// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-02-17
//
// ---------------------------------------------------------------

#ifndef GigaTrackerScraperField_H
#define GigaTrackerScraperField_H 1

#include "G4MagneticField.hh"
#include "G4ThreeVector.hh"

class GigaTrackerScraperField : public G4MagneticField {

public:
  GigaTrackerScraperField(G4double, G4ThreeVector, G4double, G4double, G4double, G4double, G4double);
  ~GigaTrackerScraperField() {}

protected:
  void GetFieldValue(const G4double Point[4], G4double *Bfield) const;

private:
  G4ThreeVector fPosition;      ///< Magnet position in the World reference frame
  G4double      fZLength;
  G4double      fApertureHalfWidth;
  G4double      fApertureHalfHeight;
  G4double      fOverallHalfHeight;
  G4double      fFieldStrength; ///< Magnetic field strength
};

#endif
