#include "G4Cons.hh"
#include "G4Tubs.hh"
#include "G4DisplacedSolid.hh"
#include "G4UnionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "G4PVParameterised.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "G4Material.hh"

#include "RICHPMTsWindow.hh"
#include "RICHMaterialParameters.hh"
#include "RICHPMsParameterisation.hh"
#include "G4LogicalBorderSurface.hh"
#include "G4OpticalSurface.hh"
#include "RICHOpticalDetector.hh"
#include "RICHPMTSD.hh"
#include "G4SDManager.hh"

RICHPMTsWindow::RICHPMTsWindow(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
  DefineOpticalSurface();
}

RICHPMTsWindow::~RICHPMTsWindow(){}

void RICHPMTsWindow::ReadGeometryParameters()
{
    RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();

    fInnerRadius = GeoPars->GetRadiatorInnerRadius();

    fOuterFlangeZLength = GeoPars->GetConicalWindowOuterFlangeZLength();
    fOuterFlangeInnerRadius = GeoPars->GetConicalWindowOuterFlangeInnerRadius();
    fOuterFlangeRadialThickness = GeoPars->GetConicalWindowOuterFlangeRadialThickness();
    fOuterFlangeZPosition = GeoPars->GetConicalWindowOuterFlangeZPosition();

    fConicalWindowZLength = GeoPars->GetConicalWindowZLength();
    fConicalWindowThickness = GeoPars->GetConicalWindowThickness();
    fConicalWindowOuterRadius = GeoPars->GetConicalWindowOuterRadius();
    fConicalWindowInnerRadius = GeoPars->GetConicalWindowInnerRadius();
    fConicalWindowZPosition = GeoPars->GetConicalWindowZPosition();

    fInnerFlangeZLength = GeoPars->GetConicalWindowInnerFlangeZLength();
    fInnerFlangeInnerRadius = GeoPars->GetConicalWindowInnerFlangeInnerRadius();
    fInnerFlangeRadialThickness = GeoPars->GetConicalWindowInnerFlangeRadialThickness();
    fInnerFlangeZPosition = GeoPars->GetConicalWindowInnerFlangeZPosition();

    fPMTsTubeZLength = GeoPars->GetPMTsTubeZLength();
    fPMTsTubeCenter = GeoPars->GetPMTsTubeCenter();
    fPMTsTubeInnerRadius = GeoPars->GetPMTsTubeInnerRadius();
    fPMTsTubeRadialThickness = GeoPars->GetPMTsTubeRadialThickness();
    fPMTsTubeZPosition = GeoPars->GetPMTsTubeZPosition();

    fPMTsDiskInnerRadius = GeoPars->GetPMTsDiskInnerRadius();
    fPMTsDiskOuterRadius = GeoPars->GetPMTsDiskOuterRadius();
    fPMTsDiskZLength = GeoPars->GetPMTsDiskZLength();
    fPMTsDiskCenter = GeoPars->GetPMTsDiskCenter();
    fPMTsDiskZPosition = GeoPars->GetPMTsDiskZPosition();

    fNPMs = GeoPars->GetNPMs();


}

void RICHPMTsWindow::CreateGeometry()
{
    G4double startPhiAngle=0;
    G4double deltaPhiAngle=360*deg;

//////////////////// Window /////////////////////////////////////////

    G4VSolid* SolidOuterFlange = new G4Tubs("RICHPMTsWindowOuterFlange",
                                          fInnerRadius,
                                          fOuterFlangeInnerRadius+fOuterFlangeRadialThickness,
                                          0.5*fOuterFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);
   
    G4VSolid* SolidConicalWindow = new G4Cons("RICHConicalWindow",
                                            fInnerRadius,                                            
                                            fConicalWindowInnerRadius+fConicalWindowThickness,
                                            fInnerRadius,
                                            fConicalWindowOuterRadius+fConicalWindowThickness,
                                            fConicalWindowZLength*0.5,
                                            startPhiAngle,
                                            deltaPhiAngle);
  
    G4VSolid* SolidInnerFlange = new G4Tubs("RICHPMTsWindowInnerFlange",
                                          fInnerRadius,
                                          fInnerFlangeInnerRadius+fInnerFlangeRadialThickness,
                                          0.5*fInnerFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);


    G4VSolid* SolidPMTsTube = new G4Tubs("RICHPMTsTube",
                                        0,
                                        fPMTsTubeInnerRadius+fPMTsTubeRadialThickness,
                                        0.5*fPMTsTubeZLength,
                                        startPhiAngle,
                                        deltaPhiAngle); 



 
    G4VSolid* SolidConicalWindowWithOuterFlange = new G4UnionSolid("SolidConicalWindowWithOuterFlange",
                                                                   new G4DisplacedSolid("DisplacedOuterFlange",
                                                                                        SolidOuterFlange,
                                                                                        0,
                                                                                        G4ThreeVector(0,0,fOuterFlangeZPosition)
                                                                                        ),
                                                                   new G4DisplacedSolid("DisplacedConicalWindow",
                                                                                         SolidConicalWindow,
                                                                                         0,
                                                                                         G4ThreeVector(0,0,fConicalWindowZPosition)
                                                                                       )    
                                                                   );

    G4VSolid* SolidConicalWindowWithOuterAndInnerFlange = new G4UnionSolid("SolidConicalWindowWithOuterAndInnerFlange",
                                                                                 SolidConicalWindowWithOuterFlange,
                                                                                 new G4DisplacedSolid("DisplacedInnerFlange",
                                                                                                      SolidInnerFlange,
                                                                                                      0,
                                                                                                      G4ThreeVector(0,0,fInnerFlangeZPosition)
                                                                                                      )
                                                                                 );

    G4VSolid** SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube;
    SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube = new G4VSolid*[2];
    
    for(Int_t i=0;i<2;i++){

     if(i==0){
        SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube[i] = new G4UnionSolid("SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube",
                                                                                 SolidConicalWindowWithOuterAndInnerFlange,
                                                                                 new G4DisplacedSolid("DisplacedPMTsTube",
                                                                                                      SolidPMTsTube,
                                                                                                      0,
                                                                                                      G4ThreeVector(fPMTsTubeCenter->x(),
                                                                                                                    fPMTsTubeCenter->y(),
                                                                                                                    fPMTsTubeZPosition)
                                                                                                      )
                                                                                 );
    }else{
        SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube[i] = new G4UnionSolid("SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube",
                                                                                 SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube[i-1],
                                                                                 new G4DisplacedSolid("DisplacedPMTsTube",
                                                                                                      SolidPMTsTube,
                                                                                                      0,
                                                                                                      G4ThreeVector(-fPMTsTubeCenter->x(),
                                                                                                                    fPMTsTubeCenter->y(),
                                                                                                                    fPMTsTubeZPosition)
                                                                                                      )
                                                                                 );
    }
   }

   fLogicalVolume  =  new G4LogicalVolume(SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTube[1],    // solid
                                          fMaterial,// material
                                          "RICHPMTsWindow",   // name
                                          0,                  // field manager
                                          0,                  // sensitive detector
                                          0);
  
   fPhysicalVolume =  new G4PVPlacement(0,                     // no rotation
                                        G4ThreeVector(0,0,0),  // at pos
                                        fLogicalVolume,        // its logical volume
                                        "RICHPMTsWindow",      // its name
                                        fMotherVolume,         // its mother  volume
                                        false,                 // no boolean operations
                                        0);                    // copy number


    
//////////////////  Radiator /////////////////////////////

    G4VSolid* SolidOuterFlangeNeon = new G4Tubs("RICHPMTsWindowOuterFlangeNeon",
                                          fInnerRadius,
                                          fOuterFlangeInnerRadius,
                                          0.5*fOuterFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);

    G4VSolid* SolidConicalWindowNeon = new G4Cons("RICHConicalWindowNeon",
                                            fInnerRadius,
                                            fConicalWindowInnerRadius,
                                            fInnerRadius,
                                            fConicalWindowOuterRadius,
                                            fConicalWindowZLength*0.5,
                                            startPhiAngle,
                                            deltaPhiAngle);

    G4VSolid* SolidInnerFlangeNeon = new G4Tubs("RICHPMTsWindowInnerFlangeNeon",
                                          fInnerRadius,
                                          fInnerFlangeInnerRadius,
                                          0.5*fInnerFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);

    G4VSolid* SolidPMTsTubeNeon = new G4Tubs("RICHPMTsTubeNeon",
                                        0,
                                        fPMTsTubeInnerRadius,
                                        0.5*fPMTsTubeZLength,
                                        startPhiAngle,
                                        deltaPhiAngle);

    G4VSolid* SolidConicalWindowWithOuterFlangeNeon = new G4UnionSolid("SolidConicalWindowWithOuterFlangeNeon",
                                                                   new G4DisplacedSolid("DisplacedOuterFlangeNeon",
                                                                                        SolidOuterFlangeNeon,
                                                                                        0,
                                                                                        G4ThreeVector(0,0,fOuterFlangeZPosition)
                                                                                        ),
                                                                   new G4DisplacedSolid("DisplacedConicalWindowNeon",
                                                                                         SolidConicalWindowNeon,
                                                                                         0,
                                                                                         G4ThreeVector(0,0,fConicalWindowZPosition)
                                                                                       )
                                                                   );

    G4VSolid* SolidConicalWindowWithOuterAndInnerFlangeNeon = new G4UnionSolid("SolidConicalWindowWithOuterAndInnerFlangeNeon",
                                                                                 SolidConicalWindowWithOuterFlangeNeon,
                                                                                 new G4DisplacedSolid("DisplacedInnerFlangeNeon",
                                                                                                      SolidInnerFlangeNeon,
                                                                                                      0,
                                                                                                      G4ThreeVector(0,0,fInnerFlangeZPosition)
                                                                                                      )
                                                                                 );
    G4VSolid** SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon;
    SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon = new G4VSolid*[2];

    for(Int_t i=0;i<2;i++){

     if(i==0){
        SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon[i] = new G4UnionSolid("SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon",
                                                                                 SolidConicalWindowWithOuterAndInnerFlangeNeon,
                                                                                 new G4DisplacedSolid("DisplacedPMTsTubeNeon",
                                                                                                      SolidPMTsTubeNeon,
                                                                                                      0,
                                                                                                      G4ThreeVector(fPMTsTubeCenter->x(),
                                                                                                                    fPMTsTubeCenter->y(),
                                                                                                                    fPMTsTubeZPosition)
                                                                                                      )
                                                                                 );
    }else{
        SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon[i] = new G4UnionSolid("SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon",
                                                                                 SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon[i-1],
                                                                                 new G4DisplacedSolid("DisplacedPMTsTubeNeon",
                                                                                                      SolidPMTsTubeNeon,
                                                                                                      0,
                                                                                                      G4ThreeVector(-fPMTsTubeCenter->x(),
                                                                                                                    fPMTsTubeCenter->y(),
                                                                                                                    fPMTsTubeZPosition)
                                                                                                      )
                                                                                 );
    }
   }




   G4LogicalVolume * LogicalNeon  =  new G4LogicalVolume(SolidConicalWindowWithOuterAndInnerFlangeAndPMTsTubeNeon[1],    // solid
                                                        G4Material::GetMaterial("RICH_Ne"),// material
                                                        "RICHPMTsWindowNeon",  // name
                                                        0,                              // field manager
                                                        0,                              // sensitive detector
                                                        0);                             // user limits


   fPhysicalNeon  =  new G4PVPlacement(0,                         // no rotation
                                       G4ThreeVector(0,0,0),// at pos
                                       LogicalNeon,             // its logical volume
                                       "RICHPMTsWindow",                // its name
                                       fLogicalVolume,              // its mother  volume
                                       false,                     // no boolean operations
                                       0);                        // copy number



  G4VSolid * SolidPMTsDisk = new G4Tubs("PMTsDisk", 
                                        fPMTsDiskInnerRadius, 
                                        fPMTsDiskOuterRadius, 
                                        0.5*fPMTsDiskZLength, 
                                        startPhiAngle, 
                                        deltaPhiAngle);

  G4LogicalVolume * LogicalPMTsDisk = new G4LogicalVolume(SolidPMTsDisk,
                                                          G4Material::GetMaterial("RICH_StainlessSteel"),
                                                          "RICHPMTsDisk", 
                                                          0, 
                                                          0, 
                                                          0);
                                        
                                      new G4PVPlacement(0,
                                                        G4ThreeVector(fPMTsDiskCenter->x(), 
                                                                      fPMTsDiskCenter->y(), 
                                                                      fPMTsDiskZPosition), 
                                                        LogicalPMTsDisk, 
                                                        "RICHPMsDiskJura", 
                                                        fMotherVolume, 
                                                        false,
                                                        0);

                                      new G4PVPlacement(0,
                                                        G4ThreeVector(-fPMTsDiskCenter->x(),
                                                                      fPMTsDiskCenter->y(),
                                                                      fPMTsDiskZPosition),       
                                                        LogicalPMTsDisk,
                                                        "RICHPMsDiskSaleve",
                                                        fMotherVolume, 
                                                        false,
                                                        1);

  RICHPMsParameterisation * PMsParameterisation = new RICHPMsParameterisation();

  RICHOpticalDetector * OD = new RICHOpticalDetector(LogicalPMTsDisk,0);

  new G4PVParameterised("RICHPMs", 
                        OD->GetLogicHole(), 
                        LogicalPMTsDisk, 
                        kUndefined, 
                        fNPMs/2., 
                        PMsParameterisation);
  
}

void RICHPMTsWindow::SetProperties()
{
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fLogicalVolume ->SetVisAttributes(fVisAtt); 

}

void RICHPMTsWindow::DefineOpticalSurface()
{
  // RICHPMTsWindow reflective surface
   fOpticalSurface = new G4OpticalSurface("RICHPMTsWindow");
   fOpticalSurface->SetType(dielectric_metal);
   fOpticalSurface->SetFinish(ground);
   fOpticalSurface->SetModel(unified);
   fOpticalSurface->SetSigmaAlpha(RICHMaterialParameters::GetInstance()
                                 ->GetPMTsWindowOpticalSurfaceSigmaAlpha());

   fOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
                                                ->GetPMTsWindowOpticalSurfacePT());

   new G4LogicalBorderSurface("Radiator/PMTsWindow Surface",
                             fPhysicalNeon,fPhysicalVolume,fOpticalSurface);

}
