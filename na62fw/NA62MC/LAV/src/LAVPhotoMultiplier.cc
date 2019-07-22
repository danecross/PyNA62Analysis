// --------------------------------------------------------------------
// History:
//
// Created by Domenico Di Filippo (difilippo@na.infn.it) 2011-01-24
//
// --------------------------------------------------------------------

#include "LAVSD.hh"
#include "LAVPhotoMultiplier.hh"
#include "LAVGeometryParameters.hh"
#include "LAVMaterialParameters.hh"

#include "G4LogicalSkinSurface.hh"
#include "G4VisAttributes.hh"
#include "G4PVPlacement.hh"
#include "G4SDManager.hh"
#include "G4Tubs.hh"
#include "G4Box.hh"

#include "G4Material.hh"
#include "G4MaterialPropertiesTable.hh"
#include "G4MaterialPropertyVector.hh"

#include "G4OpticalSurface.hh"
#include "G4LogicalSkinSurface.hh"
#include "G4LogicalBorderSurface.hh"

const G4ThreeVector Z = G4ThreeVector(0, 0, 1);

LAVPhotoMultiplier::LAVPhotoMultiplier(G4int Id) :
  fGuidePhysic(nullptr),
  fLogicalVolume(nullptr),
  fId(Id)
{
  LAVGeometryParameters *LAVGeoPar = LAVGeometryParameters::GetInstance();
  SetCover(LAVGeoPar->GetMuMetalDiameter(fId),
           LAVGeoPar->GetMuMetalDiameter(fId) - LAVGeoPar->GetMuMetalThickness(fId),
           LAVGeoPar->GetMuMetalZLength(fId));
  SetGuide(LAVGeoPar->GetLightGuideDiameter(fId), LAVGeoPar->GetLightGuideZLength(fId));
  SetCathode(LAVGeoPar->GetCathodeDiameter(fId));
  SetThickness(LAVGeoPar->GetMuMetalThickness(fId));

}

void LAVPhotoMultiplier::UpdateGeometry(){

  G4ThreeVector Pos;
  LAVMaterialParameters *LAVMatPar = LAVMaterialParameters::GetInstance();

  // Create cover

  G4VSolid* CoverSolid = 0;
  G4LogicalVolume* CoverLogic = 0;

  CoverSolid = new G4Tubs("PMTCover",
                            0.*cm,
                            0.5*CoverDiameter,
                            0.5*CoverLength,
                            0.*rad,
                            2.*M_PI*rad );

  CoverLogic =
    new G4LogicalVolume(CoverSolid,G4Material::GetMaterial("G4_Fe"),"PMTCover",0,0,0);
  CoverLogic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,1.)));

  Pos = Z*0.5*CoverLength;
  G4VPhysicalVolume* CoverPhysics =
    new G4PVPlacement(0, -Pos,"PMTCover", CoverLogic, 0, false,0);
  
  new G4LogicalSkinSurface("PMTCover",CoverLogic,LAVMatPar->GetPhotonKillerSurface());

  fLogicalVolume = CoverLogic;

  // Create Vaccum around the lightguide
  
  G4VSolid* VacuumSolid = 0;
  G4LogicalVolume* VacuumLogic = 0;

  G4double VacuumLength = CoverLength-CoverDiameter+CoverInner;
  VacuumSolid = new G4Tubs("PMTVacuum",
                            0.*cm,
                            0.5*CoverInner,
                            0.5*VacuumLength,
                            0.*rad,
                            2.*M_PI*rad );

  VacuumLogic =
    new G4LogicalVolume(VacuumSolid,G4Material::GetMaterial("LAV_Galactic"),"PMTVacuum",0,0,0);
  VacuumLogic->SetVisAttributes(G4VisAttributes::Invisible);

  Pos = Z*0.5*(CoverLength-VacuumLength);
  G4VPhysicalVolume* VacuumPhysics =
    new G4PVPlacement(0, Pos,"PMTVacuum", VacuumLogic, CoverPhysics, false,0);

  // Create lightguide support

  G4VSolid* Dead1Solid = 0;
  G4LogicalVolume* Dead1Logic = 0;

  Dead1Solid = new G4Tubs("Dead1",
                           0.5*LightGuideDiameter,
                           0.5*CoverInner,
                           0.5*Thick,
                           0.*rad,
                           2.*M_PI*rad );

  Dead1Logic =
    new G4LogicalVolume(Dead1Solid,G4Material::GetMaterial("LAV_Galactic"),"Dead1",0,0,0);
  Dead1Logic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,1.)));
  Pos = Z*0.5*(VacuumLength-Thick);
  new G4PVPlacement(0, Pos-Z*Thick/2, "Dead1", Dead1Logic, VacuumPhysics, false,0);
  new G4LogicalSkinSurface("DeadSurface1",Dead1Logic,LAVMatPar->GetPhotonKillerSurface());

  // Create lightguide
  
  G4VSolid* GuideSolid = 0;
  G4LogicalVolume* GuideLogic = 0;

  GuideSolid = new G4Tubs("lightguide",
                          0.*cm,
                          0.5*LightGuideDiameter,
                          0.5*LightGuideZLength,
                          0.*rad,
                          2.*M_PI*rad );
  GuideLogic =
     new G4LogicalVolume(GuideSolid,G4Material::GetMaterial("LAV_PbGl_SF57"),"LightGuide",0,0,0);
  GuideLogic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,0.)));
  Pos = Z*0.5*(VacuumLength-LightGuideZLength);
  fGuidePhysic =
     new G4PVPlacement(0, Pos, "LightGuide", GuideLogic, VacuumPhysics, false,0);
  
  // Create photocathode support

  Pos -= Z*LightGuideZLength/2;
  Pos -= Z*Thick/2;

  G4VSolid* Dead2Solid = 0;
  G4LogicalVolume* Dead2Logic = 0;

  Dead2Solid = new G4Tubs("Dead2",
                           0.5*PmtDiameter,
                           0.5*CoverInner,
                           0.5*Thick,
                           0.*rad,
                           2.*M_PI*rad );

  Dead2Logic =
    new G4LogicalVolume(Dead2Solid,G4Material::GetMaterial("LAV_Galactic"),"Dead2",0,0,0);
  Dead2Logic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,1.)));
  new G4PVPlacement(0, Pos, "Dead2", Dead2Logic, VacuumPhysics, false,0);
  new G4LogicalSkinSurface("DeadSurface2",Dead2Logic,LAVMatPar->GetPhotonKillerSurface());

  // Create glue/glass/photocathode

  G4VSolid* PhotoCathodeSolid = 0;
  G4LogicalVolume* PhotoCathodeLogic = 0;
  G4LogicalVolume* GlueLogic = 0;
  G4LogicalVolume* GlassLogic = 0;

  PhotoCathodeSolid = new G4Tubs("guidecoat",
                                 0.*cm,
                                 0.5*PmtDiameter,
                                 0.5*Thick,
                                 0.*rad,
                                 2.*M_PI*rad );
  GlueLogic =
    new G4LogicalVolume(PhotoCathodeSolid,G4Material::GetMaterial("LAV_GLUE"),"Glue",0,0,0);
  GlueLogic->SetVisAttributes(new G4VisAttributes(G4Colour(0.,1.,0.)));

  GlassLogic =
    new G4LogicalVolume(PhotoCathodeSolid,G4Material::GetMaterial("LAV_PbGl_SF57"),"Glass",0,0,0);
  GlassLogic->SetVisAttributes(new G4VisAttributes(G4Colour(0.,0.,1.)));

  PhotoCathodeLogic =
    new G4LogicalVolume(PhotoCathodeSolid,G4Material::GetMaterial("LAV_PbGl_SF57"),"PhotoCathode",0,0,0);
  PhotoCathodeLogic->SetVisAttributes(new G4VisAttributes(G4Colour(1.,0.,0.)));
     
  new G4PVPlacement(0, Pos, "PmtGlue", GlueLogic, VacuumPhysics, false, 0);
  Pos -= Z*Thick;
  new G4PVPlacement(0, Pos, "PmtGlass", GlassLogic, VacuumPhysics, false, 0);
  Pos -= Z*Thick;
  new G4PVPlacement(0, Pos, "PmtCathode", PhotoCathodeLogic, VacuumPhysics, false, 0);

  // Setup sensitive detector

  LAVGeometryParameters* geoParams = LAVGeometryParameters::GetInstance();
  G4String LAVCollectionName = geoParams->GetLAVCollectionName();
  G4SDManager* SDMan = G4SDManager::GetSDMpointer();
  G4String SDName;
  LAVSD *LavSD;

  // Photocathode sensitive detector
     
  SDName = geoParams->GetLAVCathodeSensitiveDetectorName();
  LavSD = static_cast<LAVSD*>(SDMan->FindSensitiveDetector(SDName));
  if(!LavSD){
     LavSD = new LAVSD(SDName, LAVCollectionName);
     SDMan->AddNewDetector(LavSD);
  }
  LavSD->GetOptTrack()->SetEfficiencyFactor(1);
  LavSD->GetOptTrack()->UseEfficiency();
  LavSD->SetHierarchy(3);
  PhotoCathodeLogic->SetSensitiveDetector(LavSD);

  // Lightguide sensitive detector
     
  SDName = geoParams->GetLAVGuideSensitiveDetectorName();
  LavSD = static_cast<LAVSD*>(SDMan->FindSensitiveDetector(SDName));
  if(!LavSD){
     LavSD = new LAVSD(SDName, LAVCollectionName);
       SDMan->AddNewDetector(LavSD);
  }
  LavSD->GetOptTrack()->UseOpticalTracking();
  LavSD->SetHierarchy(3);
  GuideLogic->SetSensitiveDetector(LavSD);

}

