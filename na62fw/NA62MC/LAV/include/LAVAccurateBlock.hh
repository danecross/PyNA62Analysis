// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2011-01-24
//
// --------------------------------------------------------------------

#ifndef LAVAccurateBlock_HH
#define LAVAccurateBlock_HH

//#include "globals.hh"

#include <list>

#include "G4Material.hh"
#include "LAVPhotoMultiplier.hh"
#include "G4LogicalVolume.hh"
#include "G4VSolid.hh"
#include "G4OpticalSurface.hh"
#include "G4ThreeVector.hh"
#include "LAVVPbGlBlock.hh"

//
//   <----- z
//
//                 
//               . _____
//          ______/     |______
//    _____/            |ligth |
//   |      leadglass   |guide |
//   |                  |______|
//   |__________________|
//
//   |_________BL_______|
//
//
//            
//            __ L2 __
//           |  _L4_  |
//           | |    | |
//
//           +--------+           -+
//          / \      / \           |
// A       /   \____/   \      _   |
// |      /    /    \    \      |  W1
// |     /    /      \    \     W2 |
// |    /    /        \    \    |  |
// y   /____/__________\____\  _| _|
//
//          |____L3____|
//     |_________L1_________|
//
//  x --->
//

class LAVAccurateBlock: public LAVVPbGlBlock{
public:
  explicit LAVAccurateBlock(G4int Id = 0);
  virtual ~LAVAccurateBlock();

  G4int GetBlockOpalId()     {return  fId;}

  void UpdateGeometry();

  // Get pointer to main logical volume of block

  G4LogicalVolume* GetLogicalVolume() { return fLogicalVolume; };

  // Pure virtual functions to return block dimensions

  G4double GetBlockL1()     {return L1+(AirThick+WrapThick)/2;}
  G4double GetBlockL2()     {return L2+(AirThick+WrapThick)/2;}
  G4double GetBlockL3()     {return L3+(AirThick+WrapThick)/2;}
  G4double GetBlockL4()     {return L4+(AirThick+WrapThick)/2;}
  G4double GetBlockW1()     {return W1+(AirThick+WrapThick)/2;}
  G4double GetBlockW2()     {return W2+(AirThick+WrapThick)/2;}
  
  // From back of crystal to front of wrapping
  G4double GetBlockZLength()     {return BL+AirThick+WrapThick;}
  
  // From back of steel plate to front of wrapping
  G4double GetBlockFullZLength() {return BL+AirThick+WrapThick+GlueThick+BackCap;}

  // From back of steel plate to end of mu-metal
  G4double GetMuMetalExternalZLength()     {return pmt->GetLength()-BackCap;}

  G4double GetMuMetalDiameter()  {return MD;}
  
  // Pure virtual functions to return naked crystal dimensions

  G4double GetCrystalL1()      {return L1;}
  G4double GetCrystalL2()      {return L2;}
  G4double GetCrystalL3()      {return L3;}
  G4double GetCrystalL4()      {return L4;}
  G4double GetCrystalW1()      {return W1;}
  G4double GetCrystalW2()      {return W2;}
  G4double GetCrystalZLength() {return BL;}
  G4ThreeVector GetCrystalFrontCenter() {return G4ThreeVector(0,(W2-W1)/4,BL/2);}
  G4ThreeVector GetCrystalBackCenter()  {return G4ThreeVector(0,(W1-W2)/4,-BL/2);}

  G4double GetBlockZofFrontFace() {return AirThick+WrapThick+BL/2;}
  G4double GetBlockZofBackFace()  {return GlueThick+BackCap+BL/2;}

private:

  LAVPhotoMultiplier *pmt;
  G4VPhysicalVolume *CerPhysic;
  G4LogicalVolume* LeadLogic;

  // Basic Geometry Constructor
  G4VSolid* CreateCenter(G4double=0);
  G4VSolid* CreateBack(G4double=0,G4double=0,G4double=0);
  G4VSolid* CreateFront(G4double=0,G4double=0,G4double=0);
  G4VSolid* CreateBlock(G4double=0,G4double=0,G4double=0,G4double=0,G4double=0);

  // Dimensions of Base Block
  G4int fId;
  G4double L1, L2, L3, L4, W1, W2, BL, MZ, MD;
  G4double HoleDiameter, FrontHole, BackCap, WrapThick, GlueThick, AirThick;
 
};

#endif
