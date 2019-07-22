// ------------------------------------------------------------------
// History:
//
// Created by Simone Schuchmann 2019-07-03
//
// Class to use G4beamline magnet field maps in Geant4
//
// ------------------------------------------------------------------

#ifndef G4BLMagnetFields_H
#define G4BLMagnetFields_H 1

#include "G4BLMagneticFieldMap.hh"
#include "G4MagneticField.hh"
#include "G4ThreeVector.hh"

class G4BLMagnetFields : public G4MagneticField {

public:
  G4BLMagnetFields(G4double ZRef,G4bool Rotation, G4ThreeVector Position,double FieldStrength,G4BLMagneticFieldMap *FieldMap,G4bool FlipBxBy);
  ~G4BLMagnetFields();
  void GetFieldValue(const G4double Point[4], G4double *Bfield) const;
  
private:
  G4double fZPos;
  G4bool fRot;
  G4ThreeVector fPosition;  
  double fFieldScale;
  G4BLMagneticFieldMap *fMap;
  G4bool fFlipBxBy = false;
};
#endif
