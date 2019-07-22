// LAVVPbGlBlock.hh
// -------------------------------------------------------------------------
// History:
//
// 2010-11-23 Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - New virtual class to create PbGl blocks. Defines interface to
//     be used while positioning block. Concrete instances of this
//     virtual class will be used to implement different simulations
//     of block's physics.
//
// -------------------------------------------------------------------------
#ifndef LAVVPbGlBlock_H
#define LAVVPbGlBlock_H 1

#include "globals.hh"

class G4LogicalVolume;

class LAVVPbGlBlock
{

public:

  LAVVPbGlBlock();
  ~LAVVPbGlBlock();

  // Get pointer to main logical volume of block

  G4LogicalVolume* GetLogicalVolume() { return fLogicalVolume; };

  // Pure virtual function to get block OPAL type

  virtual G4int GetBlockOpalId() = 0;

  // Pure virtual functions to return block dimensions

  virtual G4double GetBlockL1() = 0;
  virtual G4double GetBlockL2() = 0;
  virtual G4double GetBlockL3() = 0;
  virtual G4double GetBlockL4() = 0;
  virtual G4double GetBlockW1() = 0;
  virtual G4double GetBlockW2() = 0;
  virtual G4double GetBlockZLength() = 0; // From back of crystal to front of wrapping
  virtual G4double GetBlockFullZLength() = 0; // From back of steel plate to front of wrapping

  // Abs value of the Z coordinate of the front (small) and back (large) face of the block
  // in the block coordinate system
  // These parameters are used to position the block on the banana structure so
  // back face means the steel slab back face and front face is that of the wrapping
  virtual G4double GetBlockZofFrontFace() = 0;
  virtual G4double GetBlockZofBackFace() = 0;

  virtual G4double GetMuMetalDiameter() = 0;
  virtual G4double GetMuMetalExternalZLength() = 0; // From back of steel plate to end of mu-metal

  // Pure virtual functions to return naked crystal dimensions

  virtual G4double GetCrystalL1() = 0;
  virtual G4double GetCrystalL2() = 0;
  virtual G4double GetCrystalL3() = 0;
  virtual G4double GetCrystalL4() = 0;
  virtual G4double GetCrystalW1() = 0;
  virtual G4double GetCrystalW2() = 0;
  virtual G4double GetCrystalZLength() = 0;

protected:

  G4LogicalVolume* fLogicalVolume;

};

#endif
