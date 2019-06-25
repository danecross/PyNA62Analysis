// LAVBanana.hh
// -------------------------------------------------------------------------
// History:
//
// Created by Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of class to create the LAV banana structure
// 2019-06-11 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added parameters for displacement of central part of banana volume
//     to avoid block/banana overlaps
//
// -------------------------------------------------------------------------
// 

#ifndef LAVBanana_h
#define LAVBanana_h 1

#include "globals.hh"

class G4LogicalVolume;
class LAVVPbGlBlock;

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

class LAVBanana
{

public:
  
  LAVBanana(G4int,LAVVPbGlBlock*);
  ~LAVBanana();

  // Get pointer to main logical volume of banana
  G4LogicalVolume* GetLogicalVolume() { return fLogicalVolume; };

  // Get angular span of banana
  G4double GetBananaPhiSpan()   { return fBananaPhiSpan; }

  // Get banana thickness: distance between the external surfaces of the AL slabs
  // Use this for correct banana positioning
  G4double GetBananaThickness() { return fBananaThickness; }

  // Get real thickness of banana logical volume
  // Use this for geometrical checks only
  G4double GetBananaVolumeThickness() { return fBananaThickness+2.*fBananaThicknessTolerance; }

  // Number and type of blocks used to build this banana
  G4int GetNBlocks()   { return fNBlocks; }
  G4int GetBlockType() { return fBlockType; }

private:

  // Id of banana (internal identifier)
  G4int fBananaId;

  G4int    fNBlocks; // Number of blocks
  G4int    fBlockType; // Type of block used in banana
  G4double fBananaPhiSpan; // Phi span of banana
  G4double fBananaThickness; // Thickness of banana at Al slab
  G4double fBananaThicknessTolerance; // Thickness tolerance
  G4double fBananaInnerRadius; // Inner radius of banana
  G4double fBananaOuterRadius; // Outer radius of banana
  G4double fBananaNotchLength; // Length along R of central notch to avoid block-banana overlap
  G4double fBananaNotchAngle; // Angular displacement of central notch to avoid block-banana overlap

  G4LogicalVolume* fLogicalVolume;
  LAVVPbGlBlock* fPbGlBlock;

};

#endif
