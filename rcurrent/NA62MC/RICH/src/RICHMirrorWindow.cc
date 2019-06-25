#include "G4Tubs.hh"
#include "G4Sphere.hh"
#include "G4DisplacedSolid.hh"
#include "G4SubtractionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4LogicalBorderSurface.hh"
#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"


#include "RICHMaterialParameters.hh"
#include "RICHMirrorWindow.hh"

RICHMirrorWindow::RICHMirrorWindow(G4Material * Material, G4LogicalVolume * MotherVolume) : 
    NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();
    CreateGeometry();
    SetProperties();
}

RICHMirrorWindow::~RICHMirrorWindow(){}

void RICHMirrorWindow::ReadGeometryParameters()
{
    RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
    fInnerRadius = GeoPars->GetMirrorWindowInnerRadius();
    fOuterRadius = GeoPars->GetMirrorWindowOuterRadius();	
    fZLength = GeoPars->GetMirrorWindowZLength();
    fThickness = GeoPars->GetMirrorWindowThickness();
    fZPosition = GeoPars->GetMirrorWindowZPosition();		
    //G4cout<<" *********** ZPosition: "<<fZPosition<<" ***********"<<G4endl; 

    fInnerFlangeZLength = GeoPars->GetMirrorWindowInnerFlangeZLength();
    fInnerFlangeInnerRadius = GeoPars->GetMirrorWindowInnerFlangeInnerRadius();
    fInnerFlangeRadialThickness = GeoPars->GetMirrorWindowInnerFlangeRadialThickness();
    fInnerFlangeZPosition = GeoPars->GetMirrorWindowInnerFlangeZPosition();
    //G4cout<<" *********** Inner Flange ZPosition: "<<fInnerFlangeZPosition<<" ***********"<<G4endl; 

    fOuterFlangeZLength = GeoPars->GetMirrorWindowOuterFlangeZLength();
    fOuterFlangeInnerRadius = GeoPars->GetMirrorWindowOuterFlangeInnerRadius();
    fOuterFlangeRadialThickness = GeoPars->GetMirrorWindowOuterFlangeRadialThickness();
    fOuterFlangeZPosition = GeoPars->GetMirrorWindowOuterFlangeZPosition();
    //G4cout<<" *********** Outer Flange ZPosition: "<<fOuterFlangeZPosition<<" ***********"<<G4endl;

    fInterfaceRingZLength = GeoPars->GetInterfaceRingZLength();
    fInterfaceRingInnerRadius = GeoPars->GetInterfaceRingInnerRadius();
    fInterfaceRingRadialThickness = GeoPars->GetInterfaceRingRadialThickness();
    fInterfaceRingZPosition = GeoPars->GetInterfaceRingZPosition();	
}

void RICHMirrorWindow::CreateGeometry()
{
    G4double startPhiAngle=0;
    G4double deltaPhiAngle=360*deg;
    G4double CurvatureRadius = (pow(fZLength,2) + pow(fOuterRadius,2))/(2*fZLength);
    //G4cout<<" *********** CurvatureRadius: "<<CurvatureRadius<<" ***********"<<G4endl; 

    G4double MaxTheta = pi-asin(fInnerRadius/CurvatureRadius);
    G4double MinTheta = pi-asin(fOuterRadius/CurvatureRadius);


    fSolidVolume= new G4Sphere("MirrorWindow",
            	               CurvatureRadius - fThickness,
                  	       CurvatureRadius,
                               startPhiAngle,
                               deltaPhiAngle,
                               pi-MaxTheta,   //Theta
                               pi-MinTheta-pi+MaxTheta);  //DeltaTheta

   //G4cout<<" *********** Theta min: "<<MinTheta<<" ***********"<<G4endl;
   //G4cout<<" *********** Theta max: "<<MaxTheta<<" ***********"<<G4endl;	


    G4RotationMatrix rmXminus180;
    rmXminus180.rotateX(-180*deg);


    G4DisplacedSolid * RotatedMirrorWindow = new G4DisplacedSolid("RotMirrorWindow",
                                                                fSolidVolume,
                                                                G4Transform3D(rmXminus180,
                                                                              G4ThreeVector(0.,0.,0.)));



    fLogicalVolume = new G4LogicalVolume(RotatedMirrorWindow,        // solid
                                        fMaterial,             // material
                                        "RICHMirrorWindow",           // name
                                        0,                    // field manager
                                        0,                    // sensitive detector
                                        0);                   // user limits


   fPhysicalVolume = new G4PVPlacement(0,                         // no rotation
                                       G4ThreeVector(0,0,fZPosition+CurvatureRadius-0.5*fThickness),// at pos
                                       fLogicalVolume,             // its logical volume
                                       "RICHMirrorWindow",                // its name
                                       fMotherVolume,              // its mother  volume
                                       false,                     // no boolean operations
                                       0);                        // copy number

    G4Tubs* SolidInnerFlange = new G4Tubs("RICHMirrorWindowInnerFlange",
                                          fInnerFlangeInnerRadius,
                                          fInnerFlangeInnerRadius + fInnerFlangeRadialThickness,
                                          0.5*fInnerFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);                                                 


    G4LogicalVolume * LogicInnerFlange  =  new G4LogicalVolume(SolidInnerFlange,                   // solid
                                                               RICHMaterialParameters::GetInstance()->GetMirrorWindowInnerFlangeMaterial(),// material
                                                               "RICHMirrorWindowInnerFlange",  // name
                                                               0,                              // field manager
                                                               0,                              // sensitive detector
                                                               0);                             // user limits
  
 
    fPhysicalInnerFlange =  new G4PVPlacement(0,                         // no rotation
                                              G4ThreeVector(0,0,fInnerFlangeZPosition),// at pos
                                              LogicInnerFlange,             // its logical volume
                                              "RICHMirrorWindowInnerFlange",                // its name
                                              fMotherVolume,              // its mother  volume
                                              false,                     // no boolean operations
                                              0);                        // copy number
 
    G4Tubs* SolidDummyInnerFlange = new G4Tubs("RICHMirrorWindowDummyInnerFlange",
                                          fInnerFlangeInnerRadius-0.5*mm,
                                          fInnerFlangeInnerRadius + fInnerFlangeRadialThickness+0.5*mm,
                                          (fZLength-fInnerFlangeZLength*0.5)*0.5+1*mm,
                                          startPhiAngle,
                                          deltaPhiAngle); 

                                       
    G4Tubs* SolidOuterFlange = new G4Tubs("RICHMirrorWindowOuterFlange",
                                          fOuterFlangeInnerRadius,
                                          fOuterFlangeInnerRadius + fOuterFlangeRadialThickness,
                                          0.5*fOuterFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);

   G4LogicalVolume * LogicOuterFlange  =  new G4LogicalVolume(SolidOuterFlange,                   // solid
                                                               RICHMaterialParameters::GetInstance()->GetMirrorWindowOuterFlangeMaterial(),// material
                                                               "RICHMirrorWindowOuterFlange",  // name
                                                               0,                              // field manager
                                                               0,                              // sensitive detector
                                                               0);                             // user limits

    fPhysicalOuterFlange = new G4PVPlacement(0,                         // no rotation
                                             G4ThreeVector(0,0,fOuterFlangeZPosition),// at pos
                                             LogicOuterFlange,             // its logical volume
                                             "RICHMirrorWindowOuterFlange",                // its name
                                             fMotherVolume,              // its mother  volume
                                             false,                     // no boolean operations
                                             0);                        // copy number



   G4Tubs* SolidInterfaceRing = new G4Tubs("RICHMirrorWindowInterfaceRing",
                                          fInterfaceRingInnerRadius,
                                          fInterfaceRingInnerRadius + fInterfaceRingRadialThickness,
                                          0.5*fInterfaceRingZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);


   G4LogicalVolume * LogicInterfaceRing  =  new G4LogicalVolume(SolidInterfaceRing,                   // solid
                                                               RICHMaterialParameters::GetInstance()->GetInterfaceRingMaterial(),// material
                                                               "RICHInterfaceRing",  // name
                                                               0,                              // field manager
                                                               0,                              // sensitive detector
                                                               0);                             // user limits

    fPhysicalInterfaceRing = new G4PVPlacement(0,                         // no rotation
                                               G4ThreeVector(0,0,fInterfaceRingZPosition),// at pos
                                               LogicInterfaceRing,             // its logical volume
                                               "RICHInterfaceRing",                // its name
                                               fMotherVolume,              // its mother  volume
                                               false,                     // no boolean operations
                                               0);                        // copy number


   G4Sphere* SolidWindow = new G4Sphere("Window",
                                        CurvatureRadius - fZLength,
                                        CurvatureRadius,
                                        startPhiAngle,
                                        deltaPhiAngle,
                                        pi-MaxTheta,
                                        pi-MinTheta-pi+MaxTheta);

   G4DisplacedSolid * RotatedWindow = new G4DisplacedSolid("RotWindow",
                                                           SolidWindow,
                                                           G4Transform3D(rmXminus180,
                                                                         G4ThreeVector(0.,0.,0.)));



   G4VSolid * WindowInnFl = new G4UnionSolid("WindowInnFl",
                                             new G4DisplacedSolid("DisplacedWindow",
                                                                  RotatedWindow,
                                                                  0,
                                                                  G4ThreeVector(0,0,fZPosition+CurvatureRadius-0.5*fThickness)
                                                                 ),
                                             new G4DisplacedSolid("DisplacedInnerFlange",
                                                                  SolidInnerFlange,
                                                                  0,
                                                                  G4ThreeVector(0,0,fInnerFlangeZPosition)
                                                                 )
                                            );

   G4VSolid * WindowInnFlOutFl = new G4UnionSolid("WindowInnFlOutFl",
                                                  WindowInnFl,
                                                  new G4DisplacedSolid("DisplacedOuterFlange",
                                                                       SolidOuterFlange,
                                                                       0,
                                                                       G4ThreeVector(0,0,fOuterFlangeZPosition)
                                                                      )
                                                 );

   G4VSolid * WindowInnFlOutFlIntRing = new G4UnionSolid("WindowInnFlOutFlIntRing",
                                                         WindowInnFlOutFl,
                                                         new G4DisplacedSolid("DisplacedInterfaceRing",
                                                                              SolidInterfaceRing,
                                                                              0,
                                                                              G4ThreeVector(0,0,fInterfaceRingZPosition)
                                                                             )
                                                        );                                                                    

   G4VSolid * WindowInnFlOutFlIntRingDumInnFl = new G4UnionSolid("WindowInnFlOutFlIntRingDumInnFl",
                                                         WindowInnFlOutFlIntRing,
                                                         new G4DisplacedSolid("DisplacedDummyInnerFlange",
                                                                              SolidDummyInnerFlange,
                                                                              0,
                                                                              G4ThreeVector(0,0,
                                                                              fInnerFlangeZPosition+fInnerFlangeZLength*0.5+(fZLength-fInnerFlangeZLength*0.5)*0.5)
                                                                             )
                                                        );                                                               
 
  //G4LogicalVolume * LogicWindowInnFlOutFlIntRing  =  new G4LogicalVolume(WindowInnFlOutFlIntRing,   // solid
  //                                                                       fMaterial,                 // material
  //                                                                       "MirrorWindowMold",        // name
  //                                                                       0,                         // field manager
  //                                                                       0,                         // sensitive detector
  //                                                                       0);                        // user limits

  //                   	                               new G4PVPlacement(0,                           // no rotation
  //                                                                     G4ThreeVector(),             // at pos
  //                                                                     LogicWindowInnFlOutFlIntRing,// its logical volume
  //                                                                     "RICHMirrorWindowMold",      // its name
  //                                                                     fMotherVolume,               // its mother  volume
  //                                                                     false,                       // no boolean operations
  //                                                                     0);                          // copy number            

  G4Tubs* SolidNeonTube = new G4Tubs("RICHMirrorWindowNeonTube",
                                     fInnerFlangeInnerRadius+0.5*mm,
	                             fOuterFlangeInnerRadius+0.5*mm,
                                     0.5*(fOuterFlangeZLength+fInterfaceRingZLength)-0.5*mm,
                                     startPhiAngle,
                                     deltaPhiAngle);

  G4VSolid * SolidNeon = new G4SubtractionSolid("RICHMirrorNeon",
                                                new G4DisplacedSolid("DisplacedNeonTube",
                                                                     SolidNeonTube,
                                                                     0,
                                                                     G4ThreeVector(0,0,fOuterFlangeZPosition+0.5*fOuterFlangeZLength
                                                                     -0.5*(fOuterFlangeZLength+fInterfaceRingZLength))),
                                                WindowInnFlOutFlIntRingDumInnFl);


  G4LogicalVolume * LogicNeon  =  new G4LogicalVolume(SolidNeon,                                // solid
                                                      G4Material::GetMaterial("RICH_Ne"),       // material
                                                      "LogicNeon",                              // name
                                                      0,                                        // field manager
                                                      0,                                        // sensitive detector
                                                      0);                                       // user limits

  fPhysicalNeon = new G4PVPlacement(0,                       // no rotation
                                    G4ThreeVector(),         // at pos
  	                            LogicNeon,               // its logical volume
                                    "RICHMirrorWindowNeon",  // its name
                                    fMotherVolume,           // its mother  volume
                                    false,                   // no boolean operations
                                    0);                      // copy number           

}




void RICHMirrorWindow::SetProperties()
{
}


void RICHMirrorWindow::DefineOpticalSurface()
{

  // RICHMirrorWindow reflective surface
  fOpticalSurface = new G4OpticalSurface("RICHMirrorWindow");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(RICHMaterialParameters::GetInstance()
                                 ->GetVesselOpticalSurfaceSigmaAlpha());

  fOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
                                                ->GetVesselOpticalSurfacePT());

  new G4LogicalBorderSurface("Radiator/SphericalWindow Surface",
                             fPhysicalNeon,fPhysicalVolume,fOpticalSurface);

  new G4LogicalBorderSurface("Radiator/InnerFlange Surface",
                             fPhysicalNeon,fPhysicalInnerFlange,fOpticalSurface);

  new G4LogicalBorderSurface("Radiator/OuterFlange Surface",
                             fPhysicalNeon,fPhysicalOuterFlange,fOpticalSurface);

  new G4LogicalBorderSurface("Radiator/InterfaceRing Surface",
                             fPhysicalNeon,fPhysicalInterfaceRing,fOpticalSurface);

}
