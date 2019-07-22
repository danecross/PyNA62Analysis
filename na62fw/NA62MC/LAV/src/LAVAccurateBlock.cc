// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2011-01-24
//
// --------------------------------------------------------------------

#include "LAVAccurateBlock.hh"

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"
#include "G4VisAttributes.hh"
#include "G4PVPlacement.hh"
#include "G4SDManager.hh"

#include "G4LogicalSkinSurface.hh"

#include "LAVMaterialParameters.hh"
#include "LAVGeometryParameters.hh"
#include "LAVSD.hh"

const G4ThreeVector Z = G4ThreeVector(0, 0, 1);

LAVAccurateBlock::LAVAccurateBlock(G4int Id):
  LAVVPbGlBlock(),
  LeadLogic(nullptr),
  fId(Id)
{
  pmt = new LAVPhotoMultiplier(Id);

  LAVGeometryParameters *geo = LAVGeometryParameters::GetInstance();

  BL = geo->GetBlockZLength(fId);
  L1 = geo->GetBlockL1Length(fId);
  L2 = geo->GetBlockL2Length(fId);
  L3 = geo->GetBlockL3Length(fId);
  L4 = geo->GetBlockL4Length(fId);
  W1 = geo->GetBlockW1Length(fId);
  W2 = geo->GetBlockW2Length(fId);

  MZ = geo->GetMuMetalZLength(fId);
  MD = geo->GetMuMetalDiameter(fId);
 
  FrontHole = geo->GetWrapFrontHole(fId);
  BackCap = geo->GetSteelSlabThickness(fId);
  HoleDiameter = geo->GetMuMetalDiameter(fId);
  WrapThick = geo->GetWrapThick(fId);
  GlueThick = geo->GetGlueThick(fId);
  AirThick = geo->GetAirThick(fId);

  UpdateGeometry();

}

LAVAccurateBlock::~LAVAccurateBlock(){

  if (!pmt) delete pmt;

}

void LAVAccurateBlock::UpdateGeometry(){

   G4ThreeVector Pos;
   if (AirThick <= 0) AirThick = 0.1*mm;
   if (WrapThick <= 0) WrapThick = 0.1*mm;
   LAVMaterialParameters *LAVMatPar = LAVMaterialParameters::GetInstance();

   // Prepare photomultiplier

   pmt->UpdateGeometry();

   // Create wraping

   G4VSolid* WrapSolid = 0;
   G4LogicalVolume* WrapLogic = 0;
   G4VPhysicalVolume *WrapPhysic = 0;

   WrapSolid =
      CreateBlock(AirThick+WrapThick,
                  AirThick+WrapThick, WrapThick,
                  GlueThick+BackCap, pmt->GetLength()-BackCap);

    WrapLogic =
       new G4LogicalVolume(WrapSolid,G4Material::GetMaterial("LAV_Tyvek"),"WrapLogic",0,0,0);
    WrapLogic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,1.)));
   
    WrapPhysic =
       new G4PVPlacement(0, G4ThreeVector(0,0,0), "PbGl", WrapLogic, 0, false, 0);
    new G4LogicalSkinSurface("WrapSurface",WrapLogic,LAVMatPar->GetDiffusiveSurface());

    fLogicalVolume = WrapLogic;

    // Create air thickness

    G4VSolid* AirSolid = 0;
    G4VPhysicalVolume *AirPhysic = 0;

    AirSolid =
       CreateBlock(AirThick,
                   AirThick, 0,
                   0, 0);

    G4LogicalVolume* AirLogic =
       new G4LogicalVolume(AirSolid,G4Material::GetMaterial("LAV_Galactic"),"AirThick",0,0,0);
    AirLogic->SetVisAttributes(new G4VisAttributes(G4Colour(0.,0.,1.)));
    AirPhysic =
       new G4PVPlacement(0, G4ThreeVector(0,0,0), "PbGlAir", AirLogic, WrapPhysic, false, 0);
  
    // Create cerenkov

    G4VSolid* CerenkovSolid = CreateCenter(0);
    CerPhysic = 0;

    G4LogicalVolume* CerenkovLogic =
       new G4LogicalVolume(CerenkovSolid,G4Material::GetMaterial("LAV_PbGl_SF57"),"PbGl",0,0,0);
    CerenkovLogic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,0.)));

    CerPhysic =
       new G4PVPlacement(0, G4ThreeVector(), "PbGl", CerenkovLogic, AirPhysic, false, 0);

    // Create glue
    
    G4VSolid* GlueSolid = 0;
    G4LogicalVolume* GlueLogic = 0;
    //G4VPhysicalVolume* GluePhysic = 0;

    GlueSolid = CreateBack(AirThick,GlueThick,0);

    GlueLogic =
       new G4LogicalVolume(GlueSolid,G4Material::GetMaterial("LAV_GLUE"),"PlateGlue",0,0,0);
    GlueLogic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,0.)));

    Pos = GetCrystalBackCenter() - Z*GlueThick/2;
    //GluePhysic =
       new G4PVPlacement(0, Pos, "PlateGlue", GlueLogic, WrapPhysic, false, 0);
  
  // Create back plate
  
  G4VSolid* BackSolid = 0;
  G4LogicalVolume* BackLogic = 0;
  //G4VPhysicalVolume* BackPhysic = 0;

  BackSolid = CreateBack(AirThick,BackCap,-BackCap);
  BackLogic =
    new G4LogicalVolume(BackSolid,G4Material::GetMaterial("G4_Fe"),"BackPlate",0,0,0);
  BackLogic->SetVisAttributes(new G4VisAttributes(G4Colour(0.,1.,0.)));

  Pos = GetCrystalBackCenter() - Z*(GlueThick+BackCap/2);
  //BackPhysic =
    new G4PVPlacement(0, Pos, "BackPlate", BackLogic, WrapPhysic, false, 0);
  new G4LogicalSkinSurface("BackPlateSurface",BackLogic,LAVMatPar->GetBadReflectiveSurface());

   // Place Photomultiplier

   G4LogicalVolume * pmtlogic = pmt->GetLogicalVolume();
   Pos = GetCrystalBackCenter() - Z*(GlueThick+pmt->GetLength()/2);
   new G4PVPlacement(0, Pos, "BackPlate", pmtlogic, WrapPhysic, false, 0);
   
   // Setup sensitive detector
   
   LAVGeometryParameters* geoParams = LAVGeometryParameters::GetInstance();
   G4String LAVCollectionName = geoParams->GetLAVCollectionName();
   G4SDManager* SDMan = G4SDManager::GetSDMpointer();
   G4String SDName = geoParams->GetLAVLeadglassSensitiveDetectorName();
   LAVSD * LavSD = static_cast<LAVSD*>(SDMan->FindSensitiveDetector(SDName));
   if(!LavSD){
       LavSD = new LAVSD(SDName, LAVCollectionName);
       SDMan->AddNewDetector(LavSD);
   }
   LavSD->GetOptTrack()->UseOpticalTracking();
   LavSD->SetHierarchy(2);
   CerenkovLogic->SetSensitiveDetector(LavSD);

}

G4VSolid* LAVAccurateBlock::CreateBlock(G4double XYThick, G4double ZFThick, G4double ZFHole, G4double ZBThick, G4double ZBVol){

  G4VSolid * center = CreateCenter(XYThick);

  if (ZFThick != 0){

      center =
         new G4UnionSolid("Wrap-f",
                          center,
                          CreateFront(XYThick, ZFThick, ZFHole),
                          0, GetCrystalFrontCenter()+Z*ZFThick/2);
  }

  if (ZBThick != 0){

     center =
        new G4UnionSolid("Wrap-b",
                         center,
                         CreateBack(XYThick, ZBThick, ZBVol),
                         0, GetCrystalBackCenter()-Z*ZBThick/2);

  }

  return center;

}

G4VSolid* LAVAccurateBlock::CreateCenter(G4double XYThick){

  if (XYThick < 0) XYThick = 0;
  const G4ThreeVector trans = G4ThreeVector(0,W2-W1,0)/4;

  G4ThreeVector vtx[8];

  // Back face
  vtx[0] = G4ThreeVector(-L1/2-XYThick, -W1/2-XYThick, -BL/2)-trans;
  vtx[1] = G4ThreeVector( L1/2+XYThick, -W1/2-XYThick, -BL/2)-trans;
  vtx[2] = G4ThreeVector(-L2/2-XYThick, W1/2+XYThick, -BL/2)-trans;
  vtx[3] = G4ThreeVector( L2/2+XYThick, W1/2+XYThick, -BL/2)-trans;

  // Front face
  vtx[4] = G4ThreeVector(-L3/2-XYThick, -W1/2-XYThick, BL/2)-trans;
  vtx[5] = G4ThreeVector( L3/2+XYThick, -W1/2-XYThick, BL/2)-trans;
  vtx[6] = G4ThreeVector(-L4/2-XYThick, W2-W1/2+XYThick, BL/2)-trans;
  vtx[7] = G4ThreeVector( L4/2+XYThick, W2-W1/2+XYThick, BL/2)-trans;

  return new G4Trap("cerenkov",vtx);

}


G4VSolid* LAVAccurateBlock::CreateFront(G4double XYThick, G4double ZThick, G4double Hole){

  if (XYThick < 0) XYThick = 0;
  if (ZThick < 0) ZThick = 0;
  if (Hole < 0) Hole = 0;

  G4ThreeVector vtx[8];

  vtx[0] = G4ThreeVector(-L3/2-XYThick, -W2/2-XYThick, -ZThick/2);
  vtx[1] = G4ThreeVector( L3/2+XYThick, -W2/2-XYThick, -ZThick/2);
  vtx[2] = G4ThreeVector(-L4/2-XYThick, W2/2+XYThick, -ZThick/2);
  vtx[3] = G4ThreeVector( L4/2+XYThick, W2/2+XYThick, -ZThick/2);
  vtx[4] = G4ThreeVector(-L3/2-XYThick, -W2/2-XYThick, ZThick/2);
  vtx[5] = G4ThreeVector( L3/2+XYThick, -W2/2-XYThick, ZThick/2);
  vtx[6] = G4ThreeVector(-L4/2-XYThick, W2/2+XYThick, ZThick/2);
  vtx[7] = G4ThreeVector( L4/2+XYThick, W2/2+XYThick, ZThick/2);

  G4VSolid *Solid = new G4Trap("FrontPlate",vtx);

  if (Hole > 0)
    Solid = new G4SubtractionSolid(
       "Front-h",
        Solid,
        new G4Tubs("WrapHole",
               0.*cm,
               FrontHole,
               0.5*Hole,
               0.*rad,
               2.*M_PI*rad ),
        0, Z*(ZThick-Hole)/2);

  return Solid;

}

G4VSolid* LAVAccurateBlock::CreateBack(G4double XYThick, G4double ZThick, G4double ZCyl){

  if (XYThick < 0) XYThick = 0;
  if (ZThick < 0) ZThick = 0;

  G4ThreeVector vtx[8];

  vtx[0] = G4ThreeVector(-L1/2-XYThick, -W1/2-XYThick, -ZThick/2);
  vtx[1] = G4ThreeVector( L1/2+XYThick, -W1/2-XYThick, -ZThick/2);
  vtx[2] = G4ThreeVector(-L2/2-XYThick, W1/2+XYThick, -ZThick/2);
  vtx[3] = G4ThreeVector( L2/2+XYThick, W1/2+XYThick, -ZThick/2);
  vtx[4] = G4ThreeVector(-L1/2-XYThick, -W1/2-XYThick, ZThick/2);
  vtx[5] = G4ThreeVector( L1/2+XYThick, -W1/2-XYThick, ZThick/2);
  vtx[6] = G4ThreeVector(-L2/2-XYThick, W1/2+XYThick, ZThick/2);
  vtx[7] = G4ThreeVector( L2/2+XYThick, W1/2+XYThick, ZThick/2);

  G4VSolid* Plate = new G4Trap("BackPlate",vtx);

  if (ZCyl < 0) // Make an hole
     Plate =  new G4SubtractionSolid("Back",
                                Plate,
                                new G4Tubs("Hole",
                                   0.*cm,
                                   0.5*HoleDiameter,
                                   -0.5*ZCyl,
                                   0.*rad,
                                   2.*M_PI*rad ),
                                0, -Z*(ZThick+ZCyl)/2);

  if (ZCyl > 0) // Make space for photomultiplier
     Plate =  new G4UnionSolid("Back",
                               Plate,
                               new G4Tubs("Hole",
                                  0.*cm,
                                  0.5*HoleDiameter,
                                  0.5*ZCyl,
                                  0.*rad,
                                  2.*M_PI*rad),
                               0, -Z*(ZThick+ZCyl)/2);


  return Plate;

}

