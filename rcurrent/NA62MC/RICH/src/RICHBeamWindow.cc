#include "G4Tubs.hh"
#include "G4Sphere.hh"
#include "G4DisplacedSolid.hh"
#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4LogicalBorderSurface.hh"
#include "RICHBeamWindow.hh"
#include "RICHMaterialParameters.hh"


RICHBeamWindow::RICHBeamWindow(G4Material * Material, G4LogicalVolume * MotherVolume) : 
    NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();
    CreateGeometry();
    SetProperties();
}

RICHBeamWindow::~RICHBeamWindow(){}

void RICHBeamWindow::ReadGeometryParameters()
{

    RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();

    fOuterFlangeZLength = GeoPars->GetBeamWindowOuterFlangeZLength();
    fOuterFlangeInnerRadius = GeoPars->GetBeamWindowOuterFlangeInnerRadius();
    fOuterFlangeRadialThickness = GeoPars->GetBeamWindowOuterFlangeRadialThickness();
    fOuterFlangeZPosition = GeoPars->GetBeamWindowOuterFlangeZPosition();
    fOuterFlangeZShift = GeoPars->GetBeamWindowOuterFlangeZShift();

    fSphericalWindowZLength = GeoPars->GetBeamWindowZLength();
    fSphericalWindowOuterRadius = GeoPars->GetBeamWindowOuterRadius();
    fSphericalWindowInnerRadius = GeoPars->GetBeamWindowInnerRadius();
    fSphericalWindowThickness = GeoPars->GetBeamWindowThickness();
    fSphericalWindowZPosition = GeoPars->GetBeamWindowZPosition();

    fInnerFlangeZLength = GeoPars->GetBeamWindowInnerFlangeZLength();
    fInnerFlangeInnerRadius = GeoPars->GetBeamWindowInnerFlangeInnerRadius();
    fInnerFlangeRadialThickness = GeoPars->GetBeamWindowInnerFlangeRadialThickness();
    fInnerFlangeZShift = GeoPars-> GetBeamWindowInnerFlangeZShift();
    fInnerFlangeZPosition = GeoPars->GetBeamWindowInnerFlangeZPosition();

}

void RICHBeamWindow::CreateGeometry()
{

    G4double startPhiAngle=0;
    G4double deltaPhiAngle=360*deg;

    G4double CurvatureRadius = (pow(fSphericalWindowZLength,2) + pow(fSphericalWindowOuterRadius,2))/(2*fSphericalWindowZLength);
    //G4cout<<" *********** CurvatureRadius: "<<CurvatureRadius<<" ***********"<<G4endl; 

    G4double MaxTheta = pi-asin(fSphericalWindowInnerRadius/CurvatureRadius);
    G4double MinTheta = pi-asin(fSphericalWindowOuterRadius/CurvatureRadius);


    //G4cout<<" *********** MaxTheta: "<<MaxTheta<<" ***********"<<G4endl;
    //G4cout<<" *********** MinTheta: "<<MinTheta<<" ***********"<<G4endl;


//////////////////// Beam Window /////////////////////////////////////////

    G4VSolid* SolidOuterFlange = new G4Tubs("RICHBeamWindowOuterFlange",
                                          fOuterFlangeInnerRadius,
                                          fOuterFlangeInnerRadius+fOuterFlangeRadialThickness,
                                          0.5*fOuterFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);

    //G4cout<<" *********** OuterFlange InnerRadius: "<<fOuterFlangeInnerRadius<<" ***********"<<G4endl;
    //G4cout<<" *********** OuterFlange OuterRadius: "<<fOuterFlangeInnerRadius+fOuterFlangeRadialThickness<<" ***********"<<G4endl;
    //G4cout<<" *********** OuterFlange ZLength: "<<fOuterFlangeZLength<<" ***********"<<G4endl;


    G4VSolid* SolidSphericalWindow= new G4Sphere("SphericalWindow",
                                                 CurvatureRadius,
                                                 CurvatureRadius+fSphericalWindowThickness,
                                                 startPhiAngle,
                                                 deltaPhiAngle,
                                                 pi-MaxTheta,   //Theta
                                                 pi-MinTheta-pi+MaxTheta);  //DeltaTheta
    G4RotationMatrix rmXminus180;
    rmXminus180.rotateX(-180*deg);


    G4DisplacedSolid * RotatedSphericalWindow = new G4DisplacedSolid("RotSphericalWindow",
                                                                     SolidSphericalWindow,
                                                                     G4Transform3D(rmXminus180,
                                                                     G4ThreeVector(0.,0.,0.)));


    G4VSolid* SolidInnerFlange = new G4Tubs("RICHBeamWindowInnerFlange",
                                          fInnerFlangeInnerRadius,
                                          fInnerFlangeInnerRadius+fInnerFlangeRadialThickness,
                                          0.5*fInnerFlangeZLength,
                                          startPhiAngle,
                                          deltaPhiAngle);

    //G4cout<<" *********** InnerFlangeInnerRadius: "<<fInnerFlangeInnerRadius<<" ***********"<<G4endl;
    //G4cout<<" *********** InnerFlangeOuterRadius: "<<fInnerFlangeInnerRadius+fInnerFlangeRadialThickness<<" ***********"<<G4endl;
    //G4cout<<" *********** InnerFlange ZLength: "<<fInnerFlangeZLength<<" ***********"<<G4endl;



    G4VSolid* SolidSphericalWindowWithOuterFlange = new G4UnionSolid("SolidSphericalWindowWithOuterFlange",
                                                                     new G4DisplacedSolid("DisplacedOuterFlange",
                                                                                         SolidOuterFlange,
                                                                                         0,
                                                                                         G4ThreeVector(0,0,fOuterFlangeZPosition)
                                                                                         ),
                                                                     new G4DisplacedSolid("DisplacedSphericalWindow",
                                                                                         RotatedSphericalWindow,
                                                                                         0,
                                                                                         G4ThreeVector(0,0,fSphericalWindowZPosition+CurvatureRadius)
                                                                                          )    
                                                                     );


   //G4cout<<" *********** OuterFlange ZPosition: "<<fOuterFlangeZPosition<<" ***********"<<G4endl;
   //G4cout<<" *********** SphericalWindow ZPosition: "<<fSphericalWindowZPosition+CurvatureRadius<<" ***********"<<G4endl;

    G4VSolid* SolidSphericalWindowWithOuterAndInnerFlange = new G4UnionSolid("SolidSphericalWindowWithOuterAndInnerFlange",
                                                                             SolidSphericalWindowWithOuterFlange,	   
                                                                             new G4DisplacedSolid("DisplacedInnerFlange",
                                                                                                  SolidInnerFlange,
                                                                                                  0,
                                                                                                  G4ThreeVector(0,0,fInnerFlangeZPosition)
                                                                                                 )
                                                                             );

   //G4cout<<" *********** InnerFlange ZPosition: "<<fInnerFlangeZPosition<<" ***********"<<G4endl;


   fLogicalVolume  =  new G4LogicalVolume(SolidSphericalWindowWithOuterAndInnerFlange,    // solid
                                          fMaterial,// material
                                          "RICHBeamWindow",   // name
                                          0,                  // field manager
                                          0,                  // sensitive detector
                                          0);

   fPhysicalVolume =  new G4PVPlacement(0,                     // no rotation
                                        G4ThreeVector(0,0,0),  // at pos
                                        fLogicalVolume,        // its logical volume
                                        "RICHBeamWindow",      // its name
                                        fMotherVolume,         // its mother  volume
                                        false,                 // no boolean operations
                                        0);                    // copy number


////////////////////////  Dummy Beam Window ////////////////////////////////////////////////

    G4VSolid* SolidDummySphericalWindow= new G4Sphere("SphericalWindow",
                                                 CurvatureRadius,
                                                 CurvatureRadius+20*cm,
                                                 startPhiAngle,
                                                 deltaPhiAngle,
                                                 pi-MaxTheta,   //Theta
                                                 pi-MinTheta-pi+MaxTheta);  //DeltaTheta

    G4DisplacedSolid * RotatedDummySphericalWindow = new G4DisplacedSolid("RotDummySphericalWindow",
                                                                          SolidDummySphericalWindow,
                                                                          G4Transform3D(rmXminus180,
                                                                                        G4ThreeVector(0.,0.,0.)));



   G4VSolid* SolidDummySphericalWindowWithOuterFlange = new G4UnionSolid("SolidDummySphericalWindowWithOuterFlange",
                                                                     new G4DisplacedSolid("DisplacedOuterFlange",
                                                                                         SolidOuterFlange,
                                                                                         0,
                                                                                         G4ThreeVector(0,0,fOuterFlangeZPosition)
                                                                                         ),
                                                                     new G4DisplacedSolid("DisplacedDummySphericalWindow",
                                                                                         RotatedDummySphericalWindow,
                                                                                         0,
                                                                                         G4ThreeVector(0,0,fSphericalWindowZPosition+CurvatureRadius)
                                                                                          )
                                                                     );

    G4VSolid* SolidDummySphericalWindowWithOuterAndInnerFlange = new G4UnionSolid("SolidDummySphericalWindowWithOuterAndInnerFlange",
                                                                             SolidDummySphericalWindowWithOuterFlange,
                                                                             new G4DisplacedSolid("DisplacedInnerFlange",
                                                                                                  SolidInnerFlange,
                                                                                                  0,
                                                                                                  G4ThreeVector(0,0,fInnerFlangeZPosition)
                                                                                                 )
                                                                             );

////////////////////////////  Radiator    ////////////////////////////////////////

    G4Tubs* SolidNeonTube = new G4Tubs("RICHBeamWindowNeonTube",
                                     fInnerFlangeInnerRadius,
//                                     fInnerFlangeInnerRadius+fInnerFlangeRadialThickness,
                                     fOuterFlangeInnerRadius+0.5*mm,
                                     (fOuterFlangeZShift+fSphericalWindowZLength)*0.5,
                                     startPhiAngle,
                                     deltaPhiAngle);

    //G4cout<<"*********** fOuterFlangeZShift: "<<fOuterFlangeZShift<<" ***********"<<G4endl;
    //G4cout<<"*********** NeonTube ZLength: "<<fOuterFlangeZShift+fSphericalWindowZLength<<" ***********"<<G4endl;

    G4VSolid * SolidNeon = new G4SubtractionSolid("RICHMirrorNeon",
                                                  new G4DisplacedSolid("DisplacedNeonTube",
                                                                       SolidNeonTube,
                                                                       0,
                                                                       G4ThreeVector(0,0,-(fOuterFlangeZShift+fSphericalWindowZLength)*0.5+fOuterFlangeZPosition+fOuterFlangeZLength*0.5)),
                                                  SolidDummySphericalWindowWithOuterAndInnerFlange);


   //G4cout<<"*********** NeonTube ZPosition: "<<-(fOuterFlangeZShift+fSphericalWindowZLength)*0.5+fOuterFlangeZPosition+fOuterFlangeZLength*0.5<<" ***********"<<G4endl;


  fLogicNeon  =  new G4LogicalVolume(SolidNeon,                                // solid
                                     G4Material::GetMaterial("RICH_Ne"),       // material
                                     "LogicNeon",                              // name
                                      0,                                        // field manager
                                      0,                                        // sensitive detector
                                      0);                                       // user limits

  fPhysicalNeon = new G4PVPlacement(0,                       // no rotation
                                    G4ThreeVector(),         // at pos
                                    fLogicNeon,               // its logical volume
                                    "RICHBeamWindowNeon",  // its name
                                    fMotherVolume,           // its mother  volume
                                    false,                   // no boolean operations
                                    0);                      // copy number           
 }                                                     
void RICHBeamWindow::SetProperties()
{
  G4VisAttributes VisSteelAtt= new G4VisAttributes(G4Colour(1.,0.,1.));
  fLogicalVolume ->SetVisAttributes(VisSteelAtt);

  G4VisAttributes VisNeonAtt = new G4VisAttributes(G4Colour(0.5,0.5,0.5));
  fLogicNeon -> SetVisAttributes(VisNeonAtt);

}

void RICHBeamWindow::DefineOpticalSurface()
{

  // RICHBeamWindow reflective surface
  fOpticalSurface = new G4OpticalSurface("RICHBeamWindow");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(RICHMaterialParameters::GetInstance()
                                 ->GetVesselOpticalSurfaceSigmaAlpha());

  fOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
                                                ->GetVesselOpticalSurfacePT());

  new G4LogicalBorderSurface("Radiator/BeamWindow Surface",
                             fPhysicalNeon,fPhysicalVolume,fOpticalSurface);


}

