

// Created by Vito Palladino 6.1.09

#ifndef CHANTIFiber_H
#define CHANTIFiber_H 1

#include "globals.hh"
#include "NA62VComponent.hh"
#include "CHANTIGeometryParameters.hh"
#include "G4Tubs.hh"
#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"

#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;


class CHANTIFiber : public NA62VComponent
{

public:
  CHANTIFiber(G4Material*, 
	     G4LogicalVolume*, 
	     G4ThreeVector, 
	     G4RotationMatrix* Rotation,
	     G4int Copies,
	     G4double HalfLength);
  ~CHANTIFiber();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  
private:

  G4String fCHANTISensitiveDetectorName;
  G4String fCHANTICollectionName;

  G4double fFiberRadius;
  G4double fHalfLength;

  G4int fNofCopy;

  G4ThreeVector fPosition; // detector position
  G4RotationMatrix* fRotationMatrix;
  
  G4Tubs* fFiberSolidVolume;
  G4LogicalVolume* fFiberLogicalVolume;
  G4PVPlacement* fFiberPhysicalVolume;



};



#endif

