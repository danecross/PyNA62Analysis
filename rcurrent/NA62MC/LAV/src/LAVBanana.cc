// LAVBanana.cc
// -------------------------------------------------------------------------
// History:
//
// 2019-06-11 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added (optional) verification of volume overlaps
//   - Added displacement of central part of banana volume
//     to avoid block/banana overlaps (if needed)
//   - Added small displacement when positioning blocks to avoid overlaps
//     with aluminum slabs
// 2015-03-19 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added Aluminum slab to A6-A8
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - First implementation of class to create the LAV banana structure
// Created by Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
// -------------------------------------------------------------------------

#include "LAVBanana.hh"

#include "LAVVPbGlBlock.hh"

#include "LAVGeometryParameters.hh"
#include "LAVMaterialParameters.hh"

#include "G4Tubs.hh"
#include "G4ExtrudedSolid.hh"
#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4PVPlacement.hh"
#include "G4LogicalVolume.hh"
#include "G4VisAttributes.hh"
#include "G4Material.hh"

#include "G4TwoVector.hh"
#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"

//LAVBanana::LAVBanana(G4int bananaId, G4int blockType)
LAVBanana::LAVBanana(G4int bananaId, LAVVPbGlBlock* pbglBlock)
{

  // Get access to LAV geometrical parameters
  LAVGeometryParameters* geoParams = LAVGeometryParameters::GetInstance();

  // Check if banana id is valid
  if (bananaId<0 || bananaId >= geoParams->GetTotalNumberOfBananaShapes()) {
    G4cerr << "LAVBanana ERROR: created with invalid banana shape id " << bananaId << G4endl;
    return;
  }

  // Set to true to check for overlapping volumes
  G4bool checkOverlaps = false;

  // Set banana info
  fBananaId = bananaId;
  fNBlocks = geoParams->GetBananaNBlocks(bananaId);

  // Set bock info
  //fBlockType = blockType;
  fPbGlBlock = pbglBlock;
  fBlockType = fPbGlBlock->GetBlockOpalId();
  G4LogicalVolume* PbGlBlockLV = fPbGlBlock->GetLogicalVolume();

  // Get banana dimensions
  fBananaPhiSpan = geoParams->GetBananaPhiSpan(bananaId);
  G4double halfBananaPhiSpan = 0.5*fBananaPhiSpan;
  fBananaThickness = geoParams->GetBananaThickness(bananaId);
  fBananaThicknessTolerance = geoParams->GetBananaThicknessTolerance(bananaId);
  fBananaInnerRadius = geoParams->GetBananaInnerRadius(bananaId);
  fBananaOuterRadius = geoParams->GetBananaOuterRadius(bananaId);
  fBananaNotchLength = geoParams->GetBananaNotchLength(bananaId);
  fBananaNotchAngle = geoParams->GetBananaNotchAngle(bananaId);

  // Create global banana logical volume
  if ( fBananaNotchAngle == 0. ) {
    G4Tubs* solidBanana = new G4Tubs("banana",fBananaInnerRadius,fBananaOuterRadius,
				     0.5*fBananaThickness+fBananaThicknessTolerance,
				     90.*deg-halfBananaPhiSpan,fBananaPhiSpan);
    fLogicalVolume = new G4LogicalVolume(solidBanana,G4Material::GetMaterial("G4_Galactic"),"Banana",0,0,0);
  } else {
    // The inner part of the banana volume is displaced by a small angle to take into account the
    // small part of Opal block which protrudes from the banana volume
    G4Tubs* solidBanana1 = new G4Tubs("banana1",fBananaInnerRadius+fBananaNotchLength,fBananaOuterRadius,
				      0.5*fBananaThickness+fBananaThicknessTolerance,
				      90.*deg-halfBananaPhiSpan,fBananaPhiSpan);
    G4Tubs* solidBanana2 = new G4Tubs("banana2",fBananaInnerRadius,fBananaInnerRadius+fBananaNotchLength,
				      0.5*fBananaThickness+fBananaThicknessTolerance,
				      90.*deg-halfBananaPhiSpan+fBananaNotchAngle,fBananaPhiSpan);
    G4UnionSolid* solidBanana = new G4UnionSolid("banana",solidBanana1,solidBanana2,0,G4ThreeVector());
    fLogicalVolume = new G4LogicalVolume(solidBanana,G4Material::GetMaterial("G4_Galactic"),"Banana",0,0,0);
  }
  fLogicalVolume->SetVisAttributes(G4VisAttributes::Invisible);

  // Radial position of center of coordinates of block (to be computed later)
  G4double blockRAtCenter = 0;

  // Create full structure for this banana
  // Currently only for banana type 0 (stations 1-5) and 1 (stations 6-8).
  // Design is not finalized for other types
  if (fBananaId==0 || fBananaId==1) {

    // Aluminum parts are light grey
    G4VisAttributes* AlVisAtt = new G4VisAttributes(G4Colour(0.7,0.7,0.7));

    // Steel parts are dark grey
    //G4VisAttributes* SteelVisAtt = new G4VisAttributes(G4Colour(0.3,0.3,0.3));

    // Create vector with slab vertices
    G4int nVtx = geoParams->GetAlSlabNVertices(fBananaId);
    std::vector<G4TwoVector> vtx(nVtx);
    for(G4int iV=0;iV<nVtx;iV++) { vtx[iV] = geoParams->GetAlSlabVertex(fBananaId,iV); }

    // Create slab logical volume using extruded solid with two identical parallel surfaces
    G4double halfSlabThick = 0.5*geoParams->GetAlSlabThickness(fBananaId);
    std::vector<G4ExtrudedSolid::ZSection> slabSections {
        G4ExtrudedSolid::ZSection(-halfSlabThick, G4TwoVector(0.,0.), 1.),
        G4ExtrudedSolid::ZSection(halfSlabThick, G4TwoVector(0.,0.), 1.) };
    G4ExtrudedSolid* alSlabSolid = new G4ExtrudedSolid("bananaAlSlab",vtx, slabSections);
    G4LogicalVolume* alSlabLV =
      new G4LogicalVolume(alSlabSolid,G4Material::GetMaterial("G4_Al"),"BananaAlSlab",0,0,0);
    alSlabLV->SetVisAttributes(AlVisAtt);

    // Vertices 5 and 6 of the slab touch the internal surface of the steel vessel
    G4double x5 = vtx[5].x(); G4double y5 = vtx[5].y();
    G4double x6 = vtx[6].x(); G4double y6 = vtx[6].y();
    if (y5 != y6) G4cerr << "WARNING - LAVBanana - y5 != y6 while creating aluminum slab" << G4endl;
    if (fabs(x5) != fabs(x6)) G4cerr << "WARNING - LAVBanana - |x5| != |x6| while creating aluminum slab" << G4endl;

    // Coordinates of center of vessel in slab coordinates system
    G4double xC = 0.;
    G4double yC = -(sqrt(fBananaOuterRadius*fBananaOuterRadius-x5*x5)-y5);

    // Translation vectors for lower and upper slabs
    G4ThreeVector transAlSlab1 = G4ThreeVector(-xC,-yC,-0.5*fBananaThickness+halfSlabThick);
    G4ThreeVector transAlSlab2 = G4ThreeVector(-xC,-yC,+0.5*fBananaThickness-halfSlabThick);

    new G4PVPlacement(0,transAlSlab1,alSlabLV,"LAV_AlSlab",fLogicalVolume,false,0,checkOverlaps);
    new G4PVPlacement(0,transAlSlab2,alSlabLV,"LAV_AlSlab",fLogicalVolume,false,1,checkOverlaps);

    // Create LV for aluminum columns
    G4double colRadius = 0.5*geoParams->GetAlColumnDiameter(fBananaId);
    G4double colHalfLength = 0.5*geoParams->GetAlColumnLength(fBananaId);
    G4Tubs* alColumnS = new G4Tubs("alColumn",0.,colRadius,colHalfLength,0.,2.*M_PI*rad);
    G4LogicalVolume* alColumnLV =
      new G4LogicalVolume(alColumnS,G4Material::GetMaterial("G4_Al"),"AlColumn",0,0,0);
    alColumnLV->SetVisAttributes(AlVisAtt);

    // Using same translation of the slabs, we position all aluminum columns
    G4int nCol = geoParams->GetBananaNAlColumns(fBananaId);
    for(G4int iC=0;iC<nCol;iC++) {
      G4TwoVector colPos = geoParams->GetAlColumnPosition(fBananaId,iC);
      G4ThreeVector transCol = G4ThreeVector(-xC+colPos.x(),-yC+colPos.y(),0.);
      new G4PVPlacement(0,transCol,alColumnLV,"LAV_AlColumn",fLogicalVolume,false,iC,checkOverlaps);
    }

    // Create LV for aluminum C-shape
    G4int nCVtx = geoParams->GetAlCShapeNVertices(fBananaId);
    std::vector<G4TwoVector> cvtx(nCVtx);
    for(G4int iV=0;iV<nCVtx;iV++) { cvtx[iV] = geoParams->GetAlCShapeVertex(fBananaId,iV); }
    G4double cshHalfHeight = 0.5*geoParams->GetAlCShapeHeight(fBananaId);
    //G4double cshHoleRadius = 0.5*geoParams->GetAlCShapeHoleDiameter(fBananaId);
    std::vector<G4ExtrudedSolid::ZSection> cshapeSections {
          G4ExtrudedSolid::ZSection(-cshHalfHeight, G4TwoVector(0.,0.), 1.),
          G4ExtrudedSolid::ZSection(cshHalfHeight, G4TwoVector(0.,0.), 1.) };
    G4ExtrudedSolid* alCShapeFullS = new G4ExtrudedSolid("alCShapeFull",cvtx,cshapeSections);
    //G4double cshHoleHalfHeight = 0.5*fabs(cvtx[3].y()-cvtx[0].y());
    //G4Tubs* alCShapeHoleS = new G4Tubs("alCShapeHole",0.,cshHoleRadius,cshHoleHalfHeight,0.,2.*M_PI*rad);
    //G4RotationMatrix* rotHole = new G4RotationMatrix;
    //rotHole->rotateX(90.*deg);
    //G4ThreeVector transHole = G4ThreeVector(0.,cshHoleHalfHeight,0.); // Hole is at center of C-Shape's back face
    //G4SubtractionSolid* alCShapeS =
    //  new G4SubtractionSolid("alCShape",alCShapeFullS,alCShapeHoleS,rotHole,transHole);
    G4LogicalVolume* alCShapeLV =
      new G4LogicalVolume(alCShapeFullS,G4Material::GetMaterial("G4_Al"),"AlCShape",0,0,0);
    alCShapeLV->SetVisAttributes(AlVisAtt);

    // Using position of mid-point between c-shape screws, compute phi angle for c-shape
    G4TwoVector h1 = geoParams->GetBananaAlCShapeScrewsPosition(fBananaId,0,0);
    G4TwoVector h2 = geoParams->GetBananaAlCShapeScrewsPosition(fBananaId,0,1);
    G4TwoVector hh = 0.5*(h1+h2);
    G4double phiCShape1 = atan2(hh.x()-xC,hh.y()-yC);
    G4double phiCShape2 = -phiCShape1; // Assume they are symmetric
    G4RotationMatrix* rotCShape1 = new G4RotationMatrix;
    rotCShape1->rotateZ(180.*deg);
    rotCShape1->rotateY( 90.*deg);
    rotCShape1->rotateX(phiCShape1);
    G4RotationMatrix* rotCShape2 = new G4RotationMatrix;
    rotCShape2->rotateZ(180.*deg);
    rotCShape2->rotateY( 90.*deg);
    rotCShape2->rotateX(phiCShape2);
    G4double rCShape = sqrt(fBananaOuterRadius*fBananaOuterRadius-cshHalfHeight*cshHalfHeight);
    G4ThreeVector transCShape1 = G4ThreeVector(rCShape*sin(phiCShape1),rCShape*cos(phiCShape1),0.);
    G4ThreeVector transCShape2 = G4ThreeVector(rCShape*sin(phiCShape2),rCShape*cos(phiCShape2),0.);
    new G4PVPlacement(rotCShape1,transCShape1,alCShapeLV,"LAV_AlCShape",fLogicalVolume,false,0,checkOverlaps);
    new G4PVPlacement(rotCShape2,transCShape2,alCShapeLV,"LAV_AlCShape",fLogicalVolume,false,1,checkOverlaps);

    // Compute radius of center of block for this banana
    G4double x1 = vtx[1].x(); G4double y1 = vtx[1].y();
    G4double dx = 0.5*x1;    // Assumes that x0,y0 = 0,0
    G4double dy = 0.5*y1-yC; // Assumes that x0,y0 = 0,0
    G4double sideDist = sqrt(dx*dx+dy*dy); // Radius at slab block-contact side
    G4double sideHLen = 0.5*sqrt(x1*x1+y1*y1); // Half length of the slab block-contact side

    // As coordinates vector of Al slab is an engineering approximation, slab side
    // could be not-completely-orthogonal to the vessel radius
    // Here we compute a small tolerance to make sure that back face of block does not
    // overlap with the slab
    G4double crossProdSideRadius = dx*dx+0.5*y1*dy;
    G4double dTheta = fabs(90.*deg-acos(crossProdSideRadius/sideDist/sideHLen));
    //G4double rTolerance = sideHLen*tan(dTheta);
    // Some bananas need an additional small radial displacement to avoid overlaps with the Al slab
    G4double rTolerance = sideHLen*tan(dTheta)+geoParams->GetBananaExtraRadiusTolerance(bananaId);

    //blockRAtCenter = sideDist-rTolerance-pbglBlock->GetBlockZofBackFace();
    blockRAtCenter = sideDist-rTolerance-fPbGlBlock->GetBlockZofBackFace();

  } else {

    // If banana structure is not yet defined, use inner radius approximation
    blockRAtCenter = fBananaInnerRadius+fPbGlBlock->GetBlockZofFrontFace();

  }

  // Phi span of block
  G4double blockPhiSpan = fBananaPhiSpan/fNBlocks;

  for(G4int iBlock=0;iBlock<fNBlocks;iBlock++) {
    //G4cout << "Banana " << bananaId << " Blocco " << iBlock << G4endl;
    G4double phiBlock = halfBananaPhiSpan-(iBlock+0.5)*blockPhiSpan;
    G4RotationMatrix* rotationBlock = new G4RotationMatrix;
    rotationBlock->rotateZ(-90.*deg);
    rotationBlock->rotateY(90.*deg);
    rotationBlock->rotateX(phiBlock);
    G4ThreeVector translationBlock = G4ThreeVector(blockRAtCenter*sin(phiBlock),blockRAtCenter*cos(phiBlock),0.);
    new G4PVPlacement(rotationBlock,translationBlock,PbGlBlockLV,"LAV_Block",fLogicalVolume,false,iBlock,checkOverlaps);
  }

}

LAVBanana::~LAVBanana()
{
  delete fLogicalVolume;
}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
