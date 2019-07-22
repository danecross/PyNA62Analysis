// LAVPbGlBlock.cc
// -------------------------------------------------------------------------
// History:
//
// 2009-03-02 Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - New class to create PbGl blocks (basic components of LAVDetector)
// 2010-11-03 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Blocks geometry is now correct
//   - Added (part of) block support structure
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Improved naming conventions
//   - Used more realistic colors for block components
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Contruct the sensitive detector
// 2015-05-01 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Fixed positioning of mu-metal
//
// -------------------------------------------------------------------------

#include "LAVPbGlBlock.hh"

#include "LAVSD.hh"

#include "LAVGeometryParameters.hh"
//#include "LAVMaterialParameters.hh"

#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4PVPlacement.hh"
#include "G4LogicalVolume.hh"
#include "G4VisAttributes.hh"
#include "G4SDManager.hh"
#include "G4Material.hh"

#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"

#include "LAVSD.hh"

LAVPbGlBlock::LAVPbGlBlock(G4int blockId):LAVVPbGlBlock()
{

  // Set to true to check for overlapping volumes
  G4bool checkOverlaps = false;

  // Get access to LAV geometrical parameters
  geoParams = LAVGeometryParameters::GetInstance();
  //matParams = LAVMaterialParameters::GetInstance();

  // Check if block id is valid
  if (blockId<0 || blockId >= geoParams->GetTotalNumberOfBlockShapes()) {
    G4cerr << "LAVPbGlBlock ERROR: created with invalid block shape id " << blockId << G4endl;
    return;
  }
  fBlockId = blockId;
  fBlockOpalId = geoParams->GetBlockOpalId(blockId);

  // Dimensions of lead glass block (see OPAL document for details)
  fBlockL1 = geoParams->GetBlockL1Length(fBlockId);  // Long base of back face
  fBlockL2 = geoParams->GetBlockL2Length(fBlockId);  // Short base of back face
  fBlockL3 = geoParams->GetBlockL3Length(fBlockId);  // Long base of front face
  fBlockL4 = geoParams->GetBlockL4Length(fBlockId);  // Short base of front face
  fBlockW1 = geoParams->GetBlockW1Length(fBlockId);  // Height of back face
  fBlockW2 = geoParams->GetBlockW2Length(fBlockId);  // Height of front face
  fBlockZLen = geoParams->GetBlockZLength(fBlockId); // Z length

  // Dimensions of lightguide
  fLightGuideZLen = geoParams->GetLightGuideZLength(fBlockId);
  fLightGuideDiam = geoParams->GetLightGuideDiameter(fBlockId);

  // Dimensions of u-metal cylinder
  fMuMetalZLen = geoParams->GetMuMetalZLength(fBlockId);    // u-metal cylinder Z length
  fMuMetalDiam = geoParams->GetMuMetalDiameter(fBlockId);   // u-metal cylinder external diameter
  fMuMetalThick = geoParams->GetMuMetalThickness(fBlockId); // u-metal cylinder thickness

  // Dimensions of steel slab
  fSteelSlabThick = geoParams->GetSteelSlabThickness(fBlockId); // Steel slab thickness

  // Create solid for PbGl block
  G4Trap* solidPbGlBlock = PbGlShape("pbglblock",fBlockL1,fBlockL2,fBlockL3,fBlockL4,
				     fBlockW1,fBlockW2,fBlockZLen);

  // Create solid for steel slab shape (no hole inside)
  G4Trap* solidSteelSlabShape = PbGlShape("steelslabshape",fBlockL1,fBlockL2,fBlockL1,fBlockL2,
					  fBlockW1,fBlockW1,fSteelSlabThick);

  // Create solid for mu-metal protruding part (i.e. out of the steel slab)
  G4Tubs* solidMuMetalProt = new G4Tubs("mumetalshape",0.*cm,0.5*fMuMetalDiam,
					 0.5*(fMuMetalZLen-fSteelSlabThick),0.*rad,2.*M_PI*rad);

  //
  // Join shapes to create PbGlBlock logical volume
  //
  // Compute coordinates of center of back face of block in block reference system
  G4ThreeVector pbglCenterOfBackFace(0.,0.25*(fBlockW1-fBlockW2),-0.5*fBlockZLen);

  // Compute displacement for steel slab
  G4ThreeVector posSteelSlab = pbglCenterOfBackFace+G4ThreeVector(0.,0.,-0.5*fSteelSlabThick);

  // Compute displacement for protruding mu-metal cylinder
  //G4ThreeVector posMuMetalProt = pbglCenterOfBackFace+G4ThreeVector(0.,0.,-0.5*(fMuMetalZLen-fSteelSlabThick));
  G4ThreeVector posMuMetalProt = pbglCenterOfBackFace+G4ThreeVector(0.,0.,-fSteelSlabThick-0.5*(fMuMetalZLen-fSteelSlabThick));

  // Join pbgl block to steel slab and protruding mu metal cylinder
  G4UnionSolid* solidPbGl1 = new G4UnionSolid("pbgl1",solidPbGlBlock,solidSteelSlabShape,
					      0,posSteelSlab);
  G4UnionSolid* solidPbGl = new G4UnionSolid("pbgl",solidPbGl1,solidMuMetalProt,
					     0,posMuMetalProt);

  // Greate main logical volume for PbGl structure
  fLogicalVolume =
    new G4LogicalVolume(solidPbGl,G4Material::GetMaterial("G4_Galactic"),
			"PbGl",0,0,0);

  // Create PbGl block logical volume
  G4LogicalVolume* logicPbGlBlock =
    new G4LogicalVolume(solidPbGlBlock,G4Material::GetMaterial("LAV_PbGl_SF57"),
  			"PbGlBlock",0,0,0);

  // Create LightGuide logical volume
  G4Tubs* solidLightGuide = new G4Tubs("lightguide",0.*cm,0.5*fLightGuideDiam,0.5*fLightGuideZLen,
				       0.*rad,2.*M_PI*rad);
  G4LogicalVolume* logicLightGuide =
    new G4LogicalVolume(solidLightGuide,G4Material::GetMaterial("LAV_PbGl_SF57"),
  			"LightGuide",0,0,0);
  ////G4LogicalVolume* logicLightGuide =
  ////  new G4LogicalVolume(solidLightGuide,G4Material::GetMaterial("G4_Pb"),
  ////			"LightGuide",0,0,0);

  // Create steel slab with hole for mu-metal
  G4Trap* solidSteelSlabFull = PbGlShape("steelslabfull",fBlockL1,fBlockL2,fBlockL1,fBlockL2,
				     fBlockW1,fBlockW1,fSteelSlabThick);
  G4Tubs* solidSteelSlabHole = new G4Tubs("steelslabhole",0.*cm,0.5*fMuMetalDiam,
					 0.5*fSteelSlabThick,0.*rad,2.*M_PI*rad);
  G4SubtractionSolid* solidSteelSlab = new G4SubtractionSolid("steelslab",solidSteelSlabFull,
							      solidSteelSlabHole);
  G4LogicalVolume* logicSteelSlab =
    new G4LogicalVolume(solidSteelSlab,G4Material::GetMaterial("G4_Fe"),"SteelSlab",0,0,0);

  // Create mu-metal cylinder
  G4Tubs* solidMuMetal = new G4Tubs("mumetal",0.5*fMuMetalDiam-fMuMetalThick,0.5*fMuMetalDiam,
				    0.5*fMuMetalZLen,0.*rad,2.*M_PI*rad);
  G4LogicalVolume* logicMuMetal =
    new G4LogicalVolume(solidMuMetal,G4Material::GetMaterial("G4_Fe"),"MuMetal",0,0,0);

  //
  // Position block, lightguide, steel slab, and mu-metal cylinder inside main logical volume
  //

  // Block has same coordinate system as main logical volume
  new G4PVPlacement(0,G4ThreeVector(0.,0.,0.),logicPbGlBlock,"LAV_PbGlBlock",fLogicalVolume,false,0,checkOverlaps);

  // Position of lightguide cylinder
  G4ThreeVector posLightGuide = pbglCenterOfBackFace+G4ThreeVector(0.,0.,-0.5*fLightGuideZLen);
  new G4PVPlacement(0,posLightGuide,logicLightGuide,"LAV_PbGlLightGuide",fLogicalVolume,false,0,checkOverlaps);

  // Position of steel slab (already computed)
  new G4PVPlacement(0,posSteelSlab,logicSteelSlab,"LAV_SteelSlab",fLogicalVolume,false,0,checkOverlaps);

  // Position of full u-metal cylinder
  G4ThreeVector posMuMetal = pbglCenterOfBackFace+G4ThreeVector(0.,0.,-0.5*fMuMetalZLen);
  new G4PVPlacement(0,posMuMetal,logicMuMetal,"LAV_MuMetal",fLogicalVolume,false,0,checkOverlaps);

  //--------- Visualization attributes -----------------------

  // Main volume is transparent
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);

  // PbGl is white
  G4VisAttributes* PbGlVisAtt = new G4VisAttributes(G4Colour(1.,1.,1.));
  logicPbGlBlock->SetVisAttributes(PbGlVisAtt);
  logicLightGuide->SetVisAttributes(PbGlVisAtt);

  // Steel slab is dark grey
  G4VisAttributes* SteelVisAtt = new G4VisAttributes(G4Colour(0.3,0.3,0.3));
  logicSteelSlab->SetVisAttributes(SteelVisAtt);

  // Mu-Metal is almost black
  G4VisAttributes* MuMetalVisAtt = new G4VisAttributes(G4Colour(0.1,0.1,0.1));
  logicMuMetal->SetVisAttributes(MuMetalVisAtt);

  //------------- Sensitive detectors --------------------------

  // Associate PbGl block and lightguide to LAV sensitive detector
  G4String LAVCollectionName = geoParams->GetLAVCollectionName();
  G4SDManager* SDMan = G4SDManager::GetSDMpointer();
  G4String LAVSensitiveDetectorName = geoParams->GetLAVFastSensitiveDetectorName();
  LAVSD * LavSD = static_cast<LAVSD*>(SDMan->FindSensitiveDetector(LAVSensitiveDetectorName));
  if(!LavSD){
     LavSD = new LAVSD(LAVSensitiveDetectorName, LAVCollectionName);
     SDMan->AddNewDetector(LavSD);
  }
  LavSD->GetOptTrack()->UseMatrix();
  LavSD->SetHierarchy(1);
  logicPbGlBlock->SetSensitiveDetector(LavSD);
  logicLightGuide->SetSensitiveDetector(LavSD);

}

LAVPbGlBlock::~LAVPbGlBlock()
{
  delete fLogicalVolume;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

G4Trap* LAVPbGlBlock::PbGlShape
(const G4String& name,
 G4double l1, G4double l2, // bases of back face
 G4double l3, G4double l4, // bases of front face
 G4double w1, G4double w2, // heights of back and front faces
 G4double z                // Z-length of trapezoid
 )
{

  //--------------------------------------
  // Create trapezoid solid for PbGl block
  //--------------------------------------

  // Get half length of sides
  G4double hl1 = 0.5*l1;
  G4double hl2 = 0.5*l2;
  G4double hl3 = 0.5*l3;
  G4double hl4 = 0.5*l4;
  G4double q12 = 0.25*(w1+w2);
  G4double hz  = 0.5*z;

  // Vector to hold vertexes of trapezoid
  G4ThreeVector vtx[8];

  // Back face
  vtx[0] = G4ThreeVector(-hl1,  -q12,-hz);
  vtx[1] = G4ThreeVector( hl1,  -q12,-hz);
  vtx[2] = G4ThreeVector(-hl2,w1-q12,-hz);
  vtx[3] = G4ThreeVector( hl2,w1-q12,-hz);

  // Front face
  vtx[4] = G4ThreeVector(-hl3,  -q12,hz);
  vtx[5] = G4ThreeVector( hl3,  -q12,hz);
  vtx[6] = G4ThreeVector(-hl4,w2-q12,hz);
  vtx[7] = G4ThreeVector( hl4,w2-q12,hz);

  //G4cout << "Vertices " << vtx[0] << " " << vtx[1] << " " << vtx[2] << " " << vtx[3]
  //	 << " " << vtx[4] << " " << vtx[5] << " " << vtx[6] << " " << vtx[7] << G4endl;

  // Trapezoid
  return new G4Trap(name,vtx);

}
