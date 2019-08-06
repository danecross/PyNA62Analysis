// Created by Vito Palladino 6.1.09

#ifndef CHANTIRing_H
#define CHANTIRing_H 1

#include "globals.hh"
#include "NA62VComponent.hh"
#include "CHANTIGeometryParameters.hh"
#include "G4Box.hh"
#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"
#include <vector>

using namespace std;

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class CHANTIRing : public NA62VComponent
{

public:
  CHANTIRing(G4Material*, 
	    G4LogicalVolume*, 
	    G4double ZPosition, 
	    G4RotationMatrix* RingRotation,
	    G4double XInnerHalfLength,
	    G4double YInnerHalfLegth,
	    G4int Copies);

  ~CHANTIRing() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

  vector<G4int> GetStripsID() { return fStripsID; }
  
private:

  G4String fCHANTISensitiveDetectorName;
  G4String fCHANTICollectionName;

  G4double fZRingPos;
  G4int fNCopies;

  G4double fRingThickness;
  G4double fRingXsideLength;
  G4double fRingYsideLength;
  G4double fHalfStripLength;
  G4RotationMatrix* fRingRotation;
  G4double fTriangleBase;
  G4double fInnerHoleXLength;
  G4double fInnerHoleYLength;
  G4double fInnerRadius;
  G4double fOuterRadius;
  G4double fHalfSquareLength;
  G4double fXInnerHalfLength;
  G4double fYInnerHalfLength;

  vector<G4int> fStripsID;
};

#endif

