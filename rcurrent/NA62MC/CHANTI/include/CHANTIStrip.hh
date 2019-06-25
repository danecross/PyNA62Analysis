

// Created by Vito Palladino 6.1.09

#ifndef CHANTIStrip_H
#define CHANTIStrip_H 1

#include "globals.hh"
#include "NA62VComponent.hh"
#include "CHANTIGeometryParameters.hh"
#include "G4Trd.hh"
#include "G4RotationMatrix.hh"
#include "G4ThreeVector.hh"

#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;


class CHANTIStrip : public NA62VComponent
{

public:
  CHANTIStrip(G4Material*, 
	     G4LogicalVolume*, 
	     G4ThreeVector, 
	     G4RotationMatrix*,
	     G4double HalfLength,
	     G4int Copies);
  ~CHANTIStrip();
  
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  
private:

  G4String fCHANTISensitiveDetectorName;
  G4String fCHANTICollectionName;
  
  G4double fHalfLength;
  G4double fTriangleAltitude;
  G4double fTriangleBase;
  

  G4int fNofCopy;

  G4ThreeVector fPosition; // detector position
  G4RotationMatrix* fRotationMatrix;

  G4Trd* fStripSolidVolume;
  G4LogicalVolume* fStripLogicalVolume;
  G4PVPlacement* fStripPhysicalVolume;



};



#endif

