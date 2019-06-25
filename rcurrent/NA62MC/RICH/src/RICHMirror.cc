#include "G4Sphere.hh"
#include "G4Polyhedra.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4VPhysicalVolume.hh"
#include "G4ThreeVector.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"

#include "G4Material.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4Region.hh"

#include "RICHMirror.hh"
#include "RICHMaterialParameters.hh"

#include <TMath.h>

RICHMirror::~RICHMirror(){}

RICHMirror::RICHMirror(G4int SegmentIndex,G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  SetSegmentIndex(SegmentIndex);
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
  DefineOpticalSurface();
}

void RICHMirror::ReadGeometryParameters()
{

  //G4cout<<"**** sto simulando lo specchio ****" <<G4endl;


  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();
  fInnerRadius = GeoPars->GetMirrorSphereInnerRadius();
  fOuterRadius = GeoPars->GetMirrorSphereOuterRadius();
  fExternalRadius = GeoPars->GetSubMirrorExternalRadius();
  fMirrorGap = GeoPars->GetSubMirrorGap();
  fCenterOfCurvature_Jura = GeoPars->GetMirrorCenterOfCurvature_Jura();
  fCenterOfCurvature_Saleve = GeoPars->GetMirrorCenterOfCurvature_Saleve();
  fZPosition = GeoPars->GetMirrorZPositionRadRef();

  //G4cout<<" *** posizione in Z dello specchio nel rif del rad  "<<fZPosition<<" ***"<<G4endl;
  //G4cout<<" x centro di curvatura Jura RICH ref: "<<fCenterOfCurvature_Jura[0]<<G4endl;
  //G4cout<<" y centro di curvatura Jura RICH ref: "<<fCenterOfCurvature_Jura[1]<<G4endl; 
  //G4cout<<" z centro di curvatura Jura RICH ref: "<<fCenterOfCurvature_Jura[2]<<G4endl;


  //G4cout<<" x centro di curvatura Saleve RICH ref: "<<fCenterOfCurvature_Saleve[0]<<G4endl;
  //G4cout<<" y centro di curvatura Saleve RICH ref: "<<fCenterOfCurvature_Saleve[1]<<G4endl; 
  //G4cout<<" z centro di curvatura Saleve RICH ref: "<<fCenterOfCurvature_Saleve[2]<<G4endl;

  for(Int_t irow=0; irow<10;irow++){
     for(Int_t icolumn=0; icolumn<2;icolumn++){
        fMirrorShift_Jura[irow][icolumn]=GeoPars->GetMirrorCenterOfCurvatureShift_Jura(irow,icolumn);
        fMirrorShift_Saleve[irow][icolumn]=GeoPars->GetMirrorCenterOfCurvatureShift_Saleve(irow,icolumn);
     }
  }     


  //G4cout<<"MirrorInnerRadius: "<<fInnerRadius<<G4endl;
  //G4cout<<"MirrorOuterRadius: "<<fOuterRadius<<G4endl;
  //G4cout<<"MirrorZPosition nella scatola del RICH: "<<-(fInnerRadius+fOuterRadius)/2<<G4endl;

  fActuatorPinRadius = GeoPars->GetMirrorActuatorPinRadius();
  fActuatorPinHeight = GeoPars->GetMirrorActuatorPinHeight();
  fActuatorPinR1_Jura = GeoPars->GetMirrorActuatorPinR1_Jura();
  fActuatorPinR2_Jura = GeoPars->GetMirrorActuatorPinR2_Jura();
  fActuatorPinR1_Saleve = GeoPars->GetMirrorActuatorPinR1_Saleve();
  fActuatorPinR2_Saleve = GeoPars->GetMirrorActuatorPinR2_Saleve();

  fStabilizerPinRadius = GeoPars->GetMirrorStabilizerPinRadius();
  fStabilizerPinHeight_Jura = GeoPars->GetMirrorStabilizerPinHeight_Jura();
  fStabilizerPinD_Jura = GeoPars->GetMirrorStabilizerPinD_Jura();
  fStabilizerPinHeight_Saleve = GeoPars->GetMirrorStabilizerPinHeight_Saleve();
  fStabilizerPinD_Saleve = GeoPars->GetMirrorStabilizerPinD_Saleve();

//  for(Int_t i=0;i<=8;i++){
//   G4cout<<"fStabilizerPinD_Saleve["<<i<<"]: "<<fStabilizerPinD_Saleve[i]<<G4endl;
//  } 



}

void RICHMirror::CreateGeometry()
{

   G4double SphereThetaSize=2*2.6*deg; 
   G4double SphereThetaStart=0.;
   G4double SpherePhiStart=0*deg;
   G4double SpherePhiSize=360*deg;

   G4double ActPinInnerRadiusAuxiliarySphere=fOuterRadius+1*mm;
   G4double ActPinOuterRadiusAuxiliarySphere=ActPinInnerRadiusAuxiliarySphere+fActuatorPinHeight;

   G4double StabPinInnerRadiusAuxiliarySphere=fOuterRadius+1*mm;

   G4Sphere** solidSphere_Jura;
   solidSphere_Jura = new G4Sphere*[10];

   G4VSolid** displacedSphere_Jura;
   displacedSphere_Jura = new G4VSolid*[10]; 

   G4Sphere** solidActPinAuxiliarySphere_Jura;
   solidActPinAuxiliarySphere_Jura = new G4Sphere*[10];

   G4VSolid** displacedActPinAuxiliarySphere_Jura;
   displacedActPinAuxiliarySphere_Jura = new G4VSolid*[10];

   G4Sphere** solidStabPinAuxiliarySphere_Jura;
   solidStabPinAuxiliarySphere_Jura = new G4Sphere*[10];

   G4VSolid** displacedStabPinAuxiliarySphere_Jura;
   displacedStabPinAuxiliarySphere_Jura = new G4VSolid*[10];

   G4Sphere** solidSphere_Saleve;
   solidSphere_Saleve = new G4Sphere*[10];

   G4VSolid** displacedSphere_Saleve;
   displacedSphere_Saleve = new G4VSolid*[10]; 

   G4Sphere** solidActPinAuxiliarySphere_Saleve;
   solidActPinAuxiliarySphere_Saleve = new G4Sphere*[10];

   G4VSolid** displacedActPinAuxiliarySphere_Saleve;
   displacedActPinAuxiliarySphere_Saleve = new G4VSolid*[10];

   G4Sphere** solidStabPinAuxiliarySphere_Saleve;
   solidStabPinAuxiliarySphere_Saleve = new G4Sphere*[10];

   G4VSolid** displacedStabPinAuxiliarySphere_Saleve;
   displacedStabPinAuxiliarySphere_Saleve = new G4VSolid*[10];


   G4double apothem = fExternalRadius*sqrt(3)/2;
   G4double z[2]={-25.0*cm,25.0*cm};
   G4double rMin[2]={0,0};
   G4double rMax[2]={apothem,apothem};
   G4Polyhedra * HexagonPrism= new G4Polyhedra("HexagonPrism",0*deg,360*deg,6,2,z,rMin,rMax);

   G4double HexMirrorsDiscrPos_Jura[10][2]={{2*apothem+fMirrorGap[0],3*fExternalRadius+2*fMirrorGap[1]},// mirror 4
                                      {0.5*fMirrorGap[0]+apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 12
                                      {1.5*fMirrorGap[0]+3*apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 21 
                                      {2*apothem+fMirrorGap[0],0},// mirror 3
                                      {4*apothem+2*fMirrorGap[0],0},// mirror 1
                                      {0.5*fMirrorGap[0]+apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 13
                                      {1.5*fMirrorGap[0]+3*apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 16
                                      {0,-3*fExternalRadius-2*fMirrorGap[1]},// mirror 22
                                      {2*apothem+fMirrorGap[0],-3*fExternalRadius-2*fMirrorGap[1]},// mirror 15
                                      { 0.,0.}};// mirror A                                                                      

  G4double HexMirrorsDiscrPos_Saleve[10][2]={{-2*apothem-fMirrorGap[0],3*fExternalRadius+2*fMirrorGap[1]},// mirror 9
                                   {-0.5*fMirrorGap[0]-apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 20
                                   {-1.5*fMirrorGap[0]-3*apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 10 
                                   {-2*apothem-fMirrorGap[0],0},// mirror 5
                                   {-4*apothem-2*fMirrorGap[0],0},// mirror 6
                                   {-0.5*fMirrorGap[0]-apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 14
                                   {-1.5*fMirrorGap[0]-3*apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 11
                                   {0,+3*fExternalRadius+2*fMirrorGap[1]},// mirror 17
                                   {-2*apothem-fMirrorGap[0],-3*fExternalRadius-2*fMirrorGap[1]},// mirror 8
                                   { 0.,0.}};// mirror B                                                          


   G4RotationMatrix rmZminus90;
   rmZminus90.rotateZ(-90*deg);

// per risolvere il problema di visualizzazione del semiesagono Saleve
   G4RotationMatrix rmZminus45;
   rmZminus45.rotateZ(-45*deg);


   G4Polyhedra * HalfHexagonPrism_Jura = new G4Polyhedra("HalfHexagonPrism_Jura",0*deg,180*deg,3,2,z,rMin,rMax);

   G4Polyhedra * HalfHexagonPrism_Saleve = new G4Polyhedra("HalfHexagonPrism_Saleve",135*deg,180*deg,3,2,z,rMin,rMax);
   
   G4Tubs * BeamPipeHole= new G4Tubs("BeamPipeHole",
                                     0.,
                                     RICHGeometryParameters::GetInstance()->GetSubHalfMirrorHoleRadius(),
                                     z[1]+0.5*cm,
                                     0*deg,
                                     360*deg);

   G4VSolid * HalfHexagonWithHole_Jura = new G4SubtractionSolid("HalfHexagonWithHole_Jura",HalfHexagonPrism_Jura,BeamPipeHole);
   G4VSolid * HalfHexagonWithHole_Saleve = new G4SubtractionSolid("HalfHexagonWithHole_Saleve",HalfHexagonPrism_Saleve,BeamPipeHole);


   G4VSolid** displacedHexagon_Jura;
   displacedHexagon_Jura = new G4VSolid*[10];

   G4VSolid** intersectionMirror_Jura;
   intersectionMirror_Jura = new G4VSolid*[10];


   G4Tubs * solidActuatorPin= new G4Tubs("ActuatorPin",
                                         0.,
                                         fActuatorPinRadius,
                                         10*cm,
                                         0*deg,
                                         360*deg);
   

   G4VSolid** displacedActuatorPinR1_Jura = new G4VSolid*[10];
   G4VSolid** displacedActuatorPinR2_Jura = new G4VSolid*[10];


   G4VSolid** intersectionActuatorPinR1_Jura = new G4VSolid*[10];
   G4VSolid** intersectionActuatorPinR2_Jura = new G4VSolid*[10];


   G4VSolid** displacedActuatorPinR1_Saleve = new G4VSolid*[10];
   G4VSolid** displacedActuatorPinR2_Saleve = new G4VSolid*[10];


   G4VSolid** intersectionActuatorPinR1_Saleve = new G4VSolid*[10];
   G4VSolid** intersectionActuatorPinR2_Saleve = new G4VSolid*[10];



   G4Tubs * solidStabilizerPin= new G4Tubs("StabilizerPin",
                                         0.,
                                         fStabilizerPinRadius,
                                         10*cm,
                                         0*deg,
                                         360*deg);


   G4VSolid** displacedStabilizerPin_Jura = new G4VSolid*[10];
   
   G4VSolid** intersectionStabilizerPin_Jura = new G4VSolid*[10];

   G4VSolid** displacedStabilizerPin_Saleve = new G4VSolid*[10];

   G4VSolid** intersectionStabilizerPin_Saleve = new G4VSolid*[10];
 


   fSolidMirror_Jura = new G4VSolid*[10];
   fLogicalMirror_Jura = new G4LogicalVolume*[10];
   fPhysicalMirror_Jura = new G4VPhysicalVolume*[10];

   fSolidActuatorPin1_Jura = new G4VSolid*[10];
   fLogicalActuatorPin1_Jura = new G4LogicalVolume*[10];
   fPhysicalActuatorPin1_Jura = new G4VPhysicalVolume*[10];

   fSolidActuatorPin2_Jura = new G4VSolid*[10];
   fLogicalActuatorPin2_Jura = new G4LogicalVolume*[10];
   fPhysicalActuatorPin2_Jura = new G4VPhysicalVolume*[10];

   fSolidStabilizerPin_Jura = new G4VSolid*[10];
   fLogicalStabilizerPin_Jura = new G4LogicalVolume*[10];
   fPhysicalStabilizerPin_Jura = new G4VPhysicalVolume*[10];


   fSolidActuatorPin1_Saleve = new G4VSolid*[10];
   fLogicalActuatorPin1_Saleve = new G4LogicalVolume*[10];
   fPhysicalActuatorPin1_Saleve = new G4VPhysicalVolume*[10];

   fSolidActuatorPin2_Saleve = new G4VSolid*[10];
   fLogicalActuatorPin2_Saleve = new G4LogicalVolume*[10];
   fPhysicalActuatorPin2_Saleve = new G4VPhysicalVolume*[10];

   fSolidStabilizerPin_Saleve = new G4VSolid*[10];
   fLogicalStabilizerPin_Saleve = new G4LogicalVolume*[10];
   fPhysicalStabilizerPin_Saleve = new G4VPhysicalVolume*[10];


   G4VSolid** displacedHexagon_Saleve;
   displacedHexagon_Saleve = new G4VSolid*[10];

   G4VSolid** intersectionMirror_Saleve;
   intersectionMirror_Saleve = new G4VSolid*[10];
                                                   
   fSolidMirror_Saleve = new G4VSolid*[10];
                                                   
   fLogicalMirror_Saleve = new G4LogicalVolume*[10];
                                                   
   fPhysicalMirror_Saleve = new G4VPhysicalVolume*[10];   


for(Int_t i=0;i<10;i++){	

       solidSphere_Jura[i] = new G4Sphere("Sphere",
                                     fInnerRadius,
                                     fOuterRadius,
                                     SpherePhiStart,
                                     SpherePhiSize,
                                     SphereThetaStart,
                                     SphereThetaSize);

 
       solidSphere_Saleve[i] = new G4Sphere("Sphere",
                                     fInnerRadius,
                                     fOuterRadius,
                                     SpherePhiStart,
                                     SpherePhiSize,
                                     SphereThetaStart,
                                     SphereThetaSize);

       displacedSphere_Jura[i] = new G4DisplacedSolid("displacedSphere",
                                                 solidSphere_Jura[i],
                                                 0,
                                                 G4ThreeVector(fCenterOfCurvature_Jura[0]+fMirrorShift_Jura[i][0],
                                                               fCenterOfCurvature_Jura[1]+fMirrorShift_Jura[i][1],
                                                               -(fInnerRadius+fOuterRadius)/2));                     


       displacedSphere_Saleve[i] = new G4DisplacedSolid("displacedSphere",
                                                 solidSphere_Saleve[i],
                                                 0,
                                                 G4ThreeVector(fCenterOfCurvature_Saleve[0]+fMirrorShift_Saleve[i][0],
                                                               fCenterOfCurvature_Saleve[1]+fMirrorShift_Saleve[i][1],
                                                               -(fInnerRadius+fOuterRadius)/2));                      

       solidActPinAuxiliarySphere_Jura[i] = new G4Sphere("ActPinAuxiliarySphere",
                                     ActPinInnerRadiusAuxiliarySphere,
                                     ActPinOuterRadiusAuxiliarySphere,
                                     SpherePhiStart,
                                     SpherePhiSize,
                                     SphereThetaStart,
                                     SphereThetaSize);                                   
                                      
                                      
      displacedActPinAuxiliarySphere_Jura[i] = new G4DisplacedSolid("displacedActPinAuxiliarySphere",
                                                        solidActPinAuxiliarySphere_Jura[i],
                                                        0,
                                                        G4ThreeVector(fCenterOfCurvature_Jura[0]+fMirrorShift_Jura[i][0],
                                                                      fCenterOfCurvature_Jura[1]+fMirrorShift_Jura[i][1],
                                                                      -(fInnerRadius+fOuterRadius)/2));                           

     solidActPinAuxiliarySphere_Saleve[i] = new G4Sphere("ActPinAuxiliarySphere",
                                     ActPinInnerRadiusAuxiliarySphere,
                                     ActPinOuterRadiusAuxiliarySphere,
                                     SpherePhiStart,
                                     SpherePhiSize,
                                     SphereThetaStart,
                                     SphereThetaSize);

     displacedActPinAuxiliarySphere_Saleve[i] = new G4DisplacedSolid("displacedActPinAuxiliarySphere",
                                                                     solidActPinAuxiliarySphere_Saleve[i],
                                                                     0,
                                                                     G4ThreeVector(fCenterOfCurvature_Saleve[0]+fMirrorShift_Saleve[i][0],
                                                                      fCenterOfCurvature_Saleve[1]+fMirrorShift_Saleve[i][1],
                                                                      -(fInnerRadius+fOuterRadius)/2));


    if(i==9){    
        displacedHexagon_Jura[i] = new G4DisplacedSolid("displacedHexagon_Jura",
                                                         HalfHexagonWithHole_Jura,
                                                         G4Transform3D(rmZminus90,
                                                                       G4ThreeVector(HexMirrorsDiscrPos_Jura[i][0],
                                                                                     HexMirrorsDiscrPos_Jura[i][1],
                                                                                     0.)));

        displacedActuatorPinR2_Jura[i] = new G4DisplacedSolid("displacedActuatorPin",
                                                       solidActuatorPin,
                                                       0,
                                                       G4ThreeVector(HexMirrorsDiscrPos_Jura[i][0]+fActuatorPinR2_Jura[i]*sin(0.23),
                                                                     HexMirrorsDiscrPos_Jura[i][1]+fActuatorPinR2_Jura[i]*cos(0.23),
                                                                     0.));


       intersectionActuatorPinR2_Jura[i] = new G4IntersectionSolid("ActuatorPin2_Jura",displacedActPinAuxiliarySphere_Jura[i],
                                                                  displacedActuatorPinR2_Jura[i]);

       fSolidActuatorPin2_Jura[i] = new G4DisplacedSolid("solidActuatorPin2_Jura",
                                                  intersectionActuatorPinR2_Jura[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));

       fLogicalActuatorPin2_Jura[i] =  new G4LogicalVolume(fSolidActuatorPin2_Jura[i],                // solid
                                                   G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                   "RICHActuatorPin2",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits     

       fPhysicalActuatorPin2_Jura[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalActuatorPin2_Jura[i],             // its logical volume
                                                 "RICHActuatorPin2",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                          
      


         displacedHexagon_Saleve[i] = new G4DisplacedSolid("displacedHexagon_Saleve",
                                                           HalfHexagonWithHole_Saleve,
                                                           G4Transform3D(rmZminus45,
                                                                         G4ThreeVector(HexMirrorsDiscrPos_Saleve[i][0],
                                                                                       HexMirrorsDiscrPos_Saleve[i][1],
                                                                                       0.)));                              
        displacedActuatorPinR1_Saleve[i] = new G4DisplacedSolid("displacedActuatorPin",
                                                                solidActuatorPin,
                                                                0,
                                                                G4ThreeVector(HexMirrorsDiscrPos_Saleve[i][0]-fActuatorPinR1_Saleve[i]*sin(0.23),
                                                                     HexMirrorsDiscrPos_Saleve[i][1]+fActuatorPinR1_Saleve[i]*cos(0.23),
                                                                     0.));


       intersectionActuatorPinR1_Saleve[i] = new G4IntersectionSolid("ActuatorPin1_Saleve",displacedActPinAuxiliarySphere_Saleve[i],
                                                                  displacedActuatorPinR1_Saleve[i]);

       fSolidActuatorPin1_Saleve[i] = new G4DisplacedSolid("solidActuatorPin1_Saleve",
                                                  intersectionActuatorPinR1_Saleve[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));

       fLogicalActuatorPin1_Saleve[i] =  new G4LogicalVolume(fSolidActuatorPin1_Saleve[i],                // solid
                                                   G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                   "RICHActuatorPin1",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits     

       fPhysicalActuatorPin1_Saleve[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalActuatorPin1_Saleve[i],             // its logical volume
                                                 "RICHActuatorPin1",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number      


      }else{

         displacedHexagon_Jura[i] = new G4DisplacedSolid("displacedHexagon_Jura",
                                                         HexagonPrism,
                                                         G4Transform3D(rmZminus90,
                                                                       G4ThreeVector(HexMirrorsDiscrPos_Jura[i][0],
                                                                                     HexMirrorsDiscrPos_Jura[i][1],
                                                                                     0.)));                                    

         displacedHexagon_Saleve[i] = new G4DisplacedSolid("displacedHexagon_Saleve",
                                                         HexagonPrism,
                                                         G4Transform3D(rmZminus90,
                                                                       G4ThreeVector(HexMirrorsDiscrPos_Saleve[i][0],
                                                                                     HexMirrorsDiscrPos_Saleve[i][1],
                                                                                     0.)));                              



     displacedActuatorPinR1_Jura[i] = new G4DisplacedSolid("displacedActuatorPin",
                                                       solidActuatorPin,
                                                       0,
                                                       G4ThreeVector(HexMirrorsDiscrPos_Jura[i][0]-fActuatorPinR1_Jura[i]*sin(TMath::Pi()/4),
                                                                     HexMirrorsDiscrPos_Jura[i][1]+fActuatorPinR1_Jura[i]*cos(TMath::Pi()/4),
                                                                     0.));                                                    

     displacedActuatorPinR2_Jura[i] = new G4DisplacedSolid("displacedActuatorPin",
                                                       solidActuatorPin,
                                                       0,
                                                       G4ThreeVector(HexMirrorsDiscrPos_Jura[i][0]+fActuatorPinR2_Jura[i]*sin(TMath::Pi()/4),
                                                                     HexMirrorsDiscrPos_Jura[i][1]+fActuatorPinR2_Jura[i]*cos(TMath::Pi()/4),
                                                                     0.));

     intersectionActuatorPinR1_Jura[i] = new G4IntersectionSolid("ActuatorPin1_Jura",displacedActPinAuxiliarySphere_Jura[i],
								 displacedActuatorPinR1_Jura[i]);

     intersectionActuatorPinR2_Jura[i] = new G4IntersectionSolid("ActuatorPin2_Jura",displacedActPinAuxiliarySphere_Jura[i],
                                                                  displacedActuatorPinR2_Jura[i]);                                  


     fSolidActuatorPin1_Jura[i] = new G4DisplacedSolid("solidActuatorPin1_Jura",
                                                  intersectionActuatorPinR1_Jura[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));

     fLogicalActuatorPin1_Jura[i] =  new G4LogicalVolume(fSolidActuatorPin1_Jura[i],                // solid
                                                   G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                   "RICHActuatorPin1",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits       

     fPhysicalActuatorPin1_Jura[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalActuatorPin1_Jura[i],             // its logical volume
                                                 "RICHActuatorPin1",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                          

     fSolidActuatorPin2_Jura[i] = new G4DisplacedSolid("solidActuatorPin2_Jura",
                                                  intersectionActuatorPinR2_Jura[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));

     fLogicalActuatorPin2_Jura[i] =  new G4LogicalVolume(fSolidActuatorPin2_Jura[i],                // solid
                                                   G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                   "RICHActuatorPin2",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits     

     fPhysicalActuatorPin2_Jura[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalActuatorPin2_Jura[i],             // its logical volume
                                                 "RICHActuatorPin2",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                          
     
     solidStabPinAuxiliarySphere_Jura[i] = new G4Sphere("StabPinAuxiliarySphere",
                                                   StabPinInnerRadiusAuxiliarySphere,
                                                   StabPinInnerRadiusAuxiliarySphere+fStabilizerPinHeight_Jura[i],
                                                   SpherePhiStart,
                                                   SpherePhiSize,
                                                   SphereThetaStart,
                                                   SphereThetaSize);                                                               

     displacedStabPinAuxiliarySphere_Jura[i] = new G4DisplacedSolid("displacedStabPinAuxiliarySphere",
                                                        solidStabPinAuxiliarySphere_Jura[i],
                                                        0,
                                                        G4ThreeVector(fCenterOfCurvature_Jura[0]+fMirrorShift_Jura[i][0],
                                                                      fCenterOfCurvature_Jura[1]+fMirrorShift_Jura[i][1],
                                                                      -(fInnerRadius+fOuterRadius)/2));

    displacedStabilizerPin_Jura[i] = new G4DisplacedSolid("displacedStabilizerPin",
                                                          solidStabilizerPin,
                                                          0,
                                                          G4ThreeVector(HexMirrorsDiscrPos_Jura[i][0]+fStabilizerPinD_Jura[i],
                                                                        HexMirrorsDiscrPos_Jura[i][1],
                                                                        0.));

   intersectionStabilizerPin_Jura[i] = new G4IntersectionSolid("StabilizerPin_Jura",displacedStabPinAuxiliarySphere_Jura[i],
                                                                  displacedStabilizerPin_Jura[i]);


   fSolidStabilizerPin_Jura[i] = new G4DisplacedSolid("solidStabilizerPin_Jura",
                                                      intersectionStabilizerPin_Jura[i],
                                                      0,
                                                      G4ThreeVector(0.,
                                                                    0.,
                                                                    0.));

   fLogicalStabilizerPin_Jura[i] =  new G4LogicalVolume(fSolidStabilizerPin_Jura[i],                // solid
                                                        G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                        "RICHStabilizerPin",                   // name
                                                        0,                              // field manager
                                                        0,                              // sensitive detector
                                                        0);                             // user limits     

   fPhysicalStabilizerPin_Jura[i] = new G4PVPlacement(0,                         // no rotation
                                                      G4ThreeVector(0,0,fZPosition),// at pos
                                                      fLogicalStabilizerPin_Jura[i],             // its logical volume
                                                      "RICHStabilizerPin",                // its name
                                                      fMotherVolume,              // its mother  volume
                                                      false,                     // no boolean operations
                                                      0);                        // copy number                

     displacedActuatorPinR1_Saleve[i] = new G4DisplacedSolid("displacedActuatorPin",
                                                       solidActuatorPin,
                                                       0,
                                                       G4ThreeVector(HexMirrorsDiscrPos_Saleve[i][0]-fActuatorPinR1_Saleve[i]*sin(TMath::Pi()/4),
                                                                     HexMirrorsDiscrPos_Saleve[i][1]+fActuatorPinR1_Saleve[i]*cos(TMath::Pi()/4),
                                                                     0.));

     displacedActuatorPinR2_Saleve[i] = new G4DisplacedSolid("displacedActuatorPin",
                                                       solidActuatorPin,
                                                       0,
                                                       G4ThreeVector(HexMirrorsDiscrPos_Saleve[i][0]+fActuatorPinR2_Saleve[i]*sin(TMath::Pi()/4),
                                                                     HexMirrorsDiscrPos_Saleve[i][1]+fActuatorPinR2_Saleve[i]*cos(TMath::Pi()/4),
                                                                     0.));

     intersectionActuatorPinR1_Saleve[i] = new G4IntersectionSolid("ActuatorPin1_Saleve",displacedActPinAuxiliarySphere_Saleve[i],
                                                                 displacedActuatorPinR1_Saleve[i]);

     intersectionActuatorPinR2_Saleve[i] = new G4IntersectionSolid("ActuatorPin2_Saleve",displacedActPinAuxiliarySphere_Saleve[i],
                                                                  displacedActuatorPinR2_Saleve[i]);


     fSolidActuatorPin1_Saleve[i] = new G4DisplacedSolid("solidActuatorPin1_Saleve",
                                                  intersectionActuatorPinR1_Saleve[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));

     fLogicalActuatorPin1_Saleve[i] =  new G4LogicalVolume(fSolidActuatorPin1_Saleve[i],                // solid
                                                   G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                   "RICHActuatorPin1",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits       

     fPhysicalActuatorPin1_Saleve[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalActuatorPin1_Saleve[i],             // its logical volume
                                                 "RICHActuatorPin1",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                          

     fSolidActuatorPin2_Saleve[i] = new G4DisplacedSolid("solidActuatorPin2_Saleve",
                                                  intersectionActuatorPinR2_Saleve[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));

     fLogicalActuatorPin2_Saleve[i] =  new G4LogicalVolume(fSolidActuatorPin2_Saleve[i],                // solid
                                                   G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                   "RICHActuatorPin2",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits 
    
     fPhysicalActuatorPin2_Saleve[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalActuatorPin2_Saleve[i],             // its logical volume
                                                 "RICHActuatorPin2",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                          

     solidStabPinAuxiliarySphere_Saleve[i] = new G4Sphere("StabPinAuxiliarySphere",
                                                   StabPinInnerRadiusAuxiliarySphere,
                                                   StabPinInnerRadiusAuxiliarySphere+fStabilizerPinHeight_Saleve[i],
                                                   SpherePhiStart,
                                                   SpherePhiSize,
                                                   SphereThetaStart,
                                                   SphereThetaSize);

     displacedStabPinAuxiliarySphere_Saleve[i] = new G4DisplacedSolid("displacedStabPinAuxiliarySphere",
                                                        solidStabPinAuxiliarySphere_Saleve[i],
                                                        0,
                                                        G4ThreeVector(fCenterOfCurvature_Saleve[0]+fMirrorShift_Saleve[i][0],
                                                                      fCenterOfCurvature_Saleve[1]+fMirrorShift_Saleve[i][1],
                                                                      -(fInnerRadius+fOuterRadius)/2));

    displacedStabilizerPin_Saleve[i] = new G4DisplacedSolid("displacedStabilizerPin",
                                                          solidStabilizerPin,
                                                          0,
                                                          G4ThreeVector(HexMirrorsDiscrPos_Saleve[i][0]+fStabilizerPinD_Saleve[i],
                                                                        HexMirrorsDiscrPos_Saleve[i][1],
                                                                        0.));

   intersectionStabilizerPin_Saleve[i] = new G4IntersectionSolid("StabilizerPin_Saleve",displacedStabPinAuxiliarySphere_Saleve[i],
                                                                  displacedStabilizerPin_Saleve[i]);


   fSolidStabilizerPin_Saleve[i] = new G4DisplacedSolid("solidStabilizerPin_Saleve",
                                                      intersectionStabilizerPin_Saleve[i],
                                                      0,
                                                      G4ThreeVector(0.,
                                                                    0.,
                                                                    0.));

   fLogicalStabilizerPin_Saleve[i] =  new G4LogicalVolume(fSolidStabilizerPin_Saleve[i],                // solid
                                                        G4Material::GetMaterial("RICH_Stesalite"),      // material
                                                        "RICHStabilizerPin",                   // name
                                                        0,                              // field manager
                                                        0,                              // sensitive detector
                                                        0);                             // user limits     

   fPhysicalStabilizerPin_Saleve[i] = new G4PVPlacement(0,                         // no rotation
                                                      G4ThreeVector(0,0,fZPosition),// at pos
                                                      fLogicalStabilizerPin_Saleve[i],             // its logical volume
                                                      "RICHStabilizerPin",                // its name
                                                      fMotherVolume,              // its mother  volume
                                                      false,                     // no boolean operations
                                                      0);                        // copy number   

 }

     intersectionMirror_Jura[i] = new G4IntersectionSolid("segmentedMirror_Jura",displacedSphere_Jura[i], displacedHexagon_Jura[i]); 


     fSolidMirror_Jura[i] = new G4DisplacedSolid("solidMirror_Jura",
                                                  intersectionMirror_Jura[i],
                                                  0,
                                                  G4ThreeVector(0.,
                                                                0.,
                                                                0.));


     fLogicalMirror_Jura[i] =  new G4LogicalVolume(fSolidMirror_Jura[i],                // solid
                                                   fMaterial,                      // material
                                                   "RICHMirror",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits                

     fPhysicalMirror_Jura[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalMirror_Jura[i],             // its logical volume
                                                 "RICHMirror",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                                                      

     intersectionMirror_Saleve[i] = new G4IntersectionSolid("segmentedMirror_Saleve",displacedSphere_Saleve[i], displacedHexagon_Saleve[i]); 

     fSolidMirror_Saleve[i] = new G4DisplacedSolid("solidMirror_Saleve",
                                                    intersectionMirror_Saleve[i],
                                                    0,
                                                    G4ThreeVector(0.,
                                                                  0.,
                                                                  0.));


     fLogicalMirror_Saleve[i] =  new G4LogicalVolume(fSolidMirror_Saleve[i],                // solid
                                                   fMaterial,                      // material
                                                   "RICHMirror",                   // name
                                                   0,                              // field manager
                                                   0,                              // sensitive detector
                                                   0);                             // user limits                

     fPhysicalMirror_Saleve[i] = new G4PVPlacement(0,                         // no rotation
                                                 G4ThreeVector(0,0,fZPosition),// at pos
                                                 fLogicalMirror_Saleve[i],             // its logical volume
                                                 "RICHMirror",                // its name
                                                 fMotherVolume,              // its mother  volume
                                                 false,                     // no boolean operations
                                                 0);                        // copy number                                                    

   }                                                                                                                                              


}

void RICHMirror::SetProperties()
{

   fVisAtt= new G4VisAttributes(G4Colour(1.,0.,0.));
   G4VisAttributes VisAttAuxiliary= new G4VisAttributes(G4Colour(1.,0.,1.));

 
  for(Int_t i=0;i<10;i++){
      fLogicalMirror_Jura[i] ->SetVisAttributes(fVisAtt);
      fLogicalActuatorPin2_Jura[i]->SetVisAttributes(VisAttAuxiliary);
      fLogicalMirror_Saleve[i] ->SetVisAttributes(fVisAtt);
      fLogicalActuatorPin1_Saleve[i]->SetVisAttributes(VisAttAuxiliary);
      }                                                             
 
  for(Int_t i=0;i<9;i++){
      fLogicalActuatorPin1_Jura[i]->SetVisAttributes(VisAttAuxiliary);
      fLogicalStabilizerPin_Jura[i]->SetVisAttributes(VisAttAuxiliary);	      
      fLogicalActuatorPin2_Saleve[i]->SetVisAttributes(VisAttAuxiliary);
      fLogicalStabilizerPin_Saleve[i]->SetVisAttributes(VisAttAuxiliary);
     }

}

void RICHMirror::DefineOpticalSurface()
{
  // Mirror reflective surface
  fOpticalSurface = new G4OpticalSurface("RICHMirror_GLASS");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(RICHMaterialParameters::GetInstance()->GetMirrorOpticalSurfaceSigmaAlpha());
  fOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()->GetMirrorOpticalSurfacePT());
}
