#include "G4Tubs.hh"
#include "G4Box.hh"
#include "G4Trd.hh"
#include "G4Cons.hh"
#include "G4UnionSolid.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4SubtractionSolid.hh"

#include "globals.hh"

#include "RICHMaterialParameters.hh"
#include "RICHMirrorSupports.hh"

#include <TMath.h>

RICHMirrorSupports::RICHMirrorSupports(G4Material * Material, G4LogicalVolume * MotherVolume) : 
NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();
  CreateGeometry();
  SetProperties();
  DefineOpticalSurface();
}

RICHMirrorSupports::~RICHMirrorSupports(){}

void RICHMirrorSupports::ReadGeometryParameters()
{
  RICHGeometryParameters* GeoPars = RICHGeometryParameters::GetInstance();

//Mirror support parameters
  
  fZLength=GeoPars->GetMirrorSupportPanelEquivalentThickness();
  fInnerRadius=GeoPars->GetMirrorSupportPanelInnerRadius();
  fOuterRadius=GeoPars->GetMirrorSupportPanelOuterRadius();
  fZPosition=GeoPars->GetMirrorSupportPanelZPositionRadRef();

  fToothXLenght=GeoPars->GetMirrorSupportPanelToothXLenght();
  fToothYLenght=GeoPars->GetMirrorSupportPanelToothYLenght();
  fToothZLenght=GeoPars->GetMirrorSupportPanelToothZLenght();
  fToothZPosition=GeoPars->GetMirrorSupportPanelToothZPositionRadRef();



  fRingZLength=GeoPars->GetMirrorSupportPanelRingThickness();
  fRingInnerRadius=GeoPars->GetMirrorSupportPanelRingInnerRadius();
  fRingOuterRadius=GeoPars->GetMirrorSupportPanelRingOuterRadius();
  fRingZPosition=GeoPars->GetMirrorSupportPanelRingZPositionRadRef(); 

  fRotation_Jura=GeoPars->GetMirrorSupportPanelRotation_Jura();
  fRotation_Saleve=GeoPars->GetMirrorSupportPanelRotation_Saleve();

  fConeUpOuterRadius=GeoPars->GetMirrorSupportPanelConeUpOuterRadius();
  fConeBottomOuterRadius=GeoPars->GetMirrorSupportPanelConeBottomOuterRadius();
  fConeThickness=GeoPars->GetMirrorSupportPanelConeThickness();
  fConeHeight=GeoPars->GetMirrorSupportPanelConeHeight();
  fConeZPosition=GeoPars->GetMirrorSupportPanelConeZPositionRadRef();

  fConeBaseOuterRadius=GeoPars->GetMirrorSupportPanelConeBaseOuterRadius();
  fConeBaseInnerRadius=GeoPars->GetMirrorSupportPanelConeBaseInnerRadius();
  fConeBaseThickness=GeoPars->GetMirrorSupportPanelConeBaseThickness();
  fConeBaseZPosition=GeoPars->GetMirrorSupportPanelConeBaseZPositionRadRef();

  fConeHatOuterRadius=GeoPars->GetMirrorSupportPanelConeHatOuterRadius();
  fConeHatInnerRadius=GeoPars->GetMirrorSupportPanelConeHatInnerRadius();
  fConeHatThickness=GeoPars->GetMirrorSupportPanelConeHatThickness();
  fConeHatZPosition=GeoPars->GetMirrorSupportPanelConeHatZPositionRadRef();

  fConeScrewRadius=GeoPars->GetMirrorSupportPanelConeScrewRadius();
  fConeScrewHeight=GeoPars->GetMirrorSupportPanelConeScrewHeight();
  fConeScrewZPosition=GeoPars->GetMirrorSupportPanelConeScrewZPositionRadRef();

  fTurnerBaseXLenght=GeoPars->GetMirrorSupportPanelTurnerBaseXLenght();
  fTurnerBaseYLenght=GeoPars->GetMirrorSupportPanelTurnerBaseYLenght();
  fTurnerBaseZLenght=GeoPars->GetMirrorSupportPanelTurnerBaseZLenght();
  fTurnerBaseZPosition=GeoPars->GetMirrorSupportPanelTurnerBaseZPositionRadRef();

  fTurnerXMaxLenght=GeoPars->GetMirrorSupportPanelTurnerXMaxLenght();
  fTurnerXMinLenght=GeoPars->GetMirrorSupportPanelTurnerXMinLenght();
  fTurnerYMaxLenght=GeoPars->GetMirrorSupportPanelTurnerYMaxLenght();
  fTurnerYMinLenght=GeoPars->GetMirrorSupportPanelTurnerYMinLenght();
  fTurnerZLenght=GeoPars->GetMirrorSupportPanelTurnerZLenght();
  fTurnerZPosition=GeoPars->GetMirrorSupportPanelTurnerZPositionRadRef();

  fTurnerHoleXMaxLenght=GeoPars->GetMirrorSupportPanelTurnerHoleXMaxLenght();
  fTurnerHoleXMinLenght=GeoPars->GetMirrorSupportPanelTurnerHoleXMinLenght();
  fTurnerHoleYMaxLenght=GeoPars->GetMirrorSupportPanelTurnerHoleYMaxLenght();
  fTurnerHoleYMinLenght=GeoPars->GetMirrorSupportPanelTurnerHoleYMinLenght();
  fTurnerHoleZLenght=GeoPars->GetMirrorSupportPanelTurnerHoleZLenght();
  fTurnerHoleZPosition=GeoPars->GetMirrorSupportPanelTurnerHoleZPositionRadRef();

  fPrismBaseXLenght=GeoPars->GetMirrorSupportPanelPrismBaseXLenght();
  fPrismBaseYLenght=GeoPars->GetMirrorSupportPanelPrismBaseYLenght();
  fPrismBaseZLenght=GeoPars->GetMirrorSupportPanelPrismBaseZLenght();
  fPrismBaseZPosition=GeoPars->GetMirrorSupportPanelPrismBaseZPositionRadRef();

  fPrismHatXLenght=GeoPars->GetMirrorSupportPanelPrismHatXLenght();
  fPrismHatYLenght=GeoPars->GetMirrorSupportPanelPrismHatYLenght();
  fPrismHatZLenght=GeoPars->GetMirrorSupportPanelPrismHatZLenght();
  fPrismHatZPosition=GeoPars->GetMirrorSupportPanelPrismHatZPositionRadRef();

  fPrismX1Lenght=GeoPars->GetMirrorSupportPanelPrismX1Lenght();
  fPrismX2Lenght=GeoPars->GetMirrorSupportPanelPrismX2Lenght();
  fPrismY1Lenght=GeoPars->GetMirrorSupportPanelPrismY1Lenght();
  fPrismY2Lenght=GeoPars->GetMirrorSupportPanelPrismY2Lenght();
  fPrismZLenght=GeoPars->GetMirrorSupportPanelPrismZLenght();
  fPrismZPosition=GeoPars->GetMirrorSupportPanelPrismZPositionRadRef();


  fPrismHoleX1Lenght=GeoPars->GetMirrorSupportPanelPrismHoleX1Lenght();
  fPrismHoleX2Lenght=GeoPars->GetMirrorSupportPanelPrismHoleX2Lenght();
  fPrismHoleY1Lenght=GeoPars->GetMirrorSupportPanelPrismHoleY1Lenght();
  fPrismHoleY2Lenght=GeoPars->GetMirrorSupportPanelPrismHoleY2Lenght();
  fPrismHoleZLenght=GeoPars->GetMirrorSupportPanelPrismHoleZLenght();
  fPrismHoleZPosition=GeoPars->GetMirrorSupportPanelPrismHoleZPositionRadRef();

  fPrismXPosition_Jura=GeoPars->GetMirrorSupportPanelPrismXPosition_Jura();
  fPrismXPosition_Saleve=GeoPars->GetMirrorSupportPanelPrismXPosition_Saleve();

  fPrismScrewRadius=GeoPars->GetMirrorSupportPanelPrismScrewRadius();
  fPrismScrewHeight=GeoPars->GetMirrorSupportPanelPrismScrewHeight();
  fPrismScrewZPosition=GeoPars->GetMirrorSupportPanelPrismScrewZPositionRadRef();

  fDowelRadius=GeoPars->GetMirrorSupportPanelDowelRadius();
  fDowelHeight=GeoPars->GetMirrorSupportPanelDowelHeight();

  fExternalRadius = GeoPars->GetSubMirrorExternalRadius();
  fMirrorGap = GeoPars->GetSubMirrorGap();


}

void RICHMirrorSupports::CreateGeometry()
{

   G4double apothem = fExternalRadius*sqrt(3)/2;
   G4double SupportConePos_Jura[9][2]={{2*apothem+fMirrorGap[0],3*fExternalRadius+2*fMirrorGap[1]},// mirror 4
                                      {0.5*fMirrorGap[0]+apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 12
                                      {1.5*fMirrorGap[0]+3*apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 21 
                                      {2*apothem+fMirrorGap[0],0},// mirror 3
                                      {4*apothem+2*fMirrorGap[0],0},// mirror 1
                                      {0.5*fMirrorGap[0]+apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 13
                                      {1.5*fMirrorGap[0]+3*apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 16
                                      {0,-3*fExternalRadius-2*fMirrorGap[1]},// mirror 22
                                      {2*apothem+fMirrorGap[0],-3*fExternalRadius-2*fMirrorGap[1]}};// mirror 15
                                                                                                         

   G4double SupportConePos_Saleve[9][2]={{-2*apothem-fMirrorGap[0],3*fExternalRadius+2*fMirrorGap[1]},// mirror 9
                                      {-0.5*fMirrorGap[0]-apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 20
                                      {-1.5*fMirrorGap[0]-3*apothem,1.5*fExternalRadius+fMirrorGap[1]},// mirror 10 
                                      {-2*apothem-fMirrorGap[0],0},// mirror 5
                                      {-4*apothem-2*fMirrorGap[0],0},// mirror 6
                                      {-0.5*fMirrorGap[0]-apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 14
                                      {-1.5*fMirrorGap[0]-3*apothem,-1.5*fExternalRadius-fMirrorGap[1]},// mirror 11
                                      {0,+3*fExternalRadius+2*fMirrorGap[1]},// mirror 17
                                      {-2*apothem-fMirrorGap[0],-3*fExternalRadius-2*fMirrorGap[1]}};// mirror 8                

   G4double ConeScrewRadialPos=(fConeBaseOuterRadius+fConeBaseInnerRadius)*0.5;

   G4double ConeScrewPos[6][2]={{ConeScrewRadialPos*cos(45*deg),ConeScrewRadialPos*sin(45*deg)},
                                {0.,ConeScrewRadialPos},
                                {-ConeScrewRadialPos*cos(45*deg),ConeScrewRadialPos*sin(45*deg)},
                                {-ConeScrewRadialPos*cos(45*deg),-ConeScrewRadialPos*sin(45*deg)},
                                {0.,-ConeScrewRadialPos},
                                {ConeScrewRadialPos*cos(45*deg),-ConeScrewRadialPos*sin(45*deg)}};

   G4double PrismScrewPos[6][2]={{-35*mm,fPrismY2Lenght*0.5+(fPrismBaseYLenght*0.5-fPrismY2Lenght*0.5)*0.5},
                                {0.,fPrismY2Lenght*0.5+(fPrismBaseYLenght*0.5-fPrismY2Lenght*0.5)*0.5},
                                {+35*mm,fPrismY2Lenght*0.5+(fPrismBaseYLenght*0.5-fPrismY2Lenght*0.5)*0.5},
                                {-35*mm,-fPrismY2Lenght*0.5-(fPrismBaseYLenght*0.5-fPrismY2Lenght*0.5)*0.5},
                                {0.,-fPrismY2Lenght*0.5-(fPrismBaseYLenght*0.5-fPrismY2Lenght*0.5)*0.5},
                                {+35*mm,-fPrismY2Lenght*0.5-(fPrismBaseYLenght*0.5-fPrismY2Lenght*0.5)*0.5}};



  G4double startPhiAngle=0;
  G4double deltaPhiAngle=180*deg;

  G4RotationMatrix  rmZminus90;
  rmZminus90.rotateZ(-90*deg);
  G4RotationMatrix  rmZplus90;
  rmZplus90.rotateZ(+90*deg);

//////////////////////////////   Dowel  ////////////////////////////////

 G4VSolid* SolidDowel = new G4Tubs("solidDowel",
                                   0.,
                                   fDowelRadius,
                                   fDowelHeight*0.5,
                                   0,
                                   360*deg);

//////////////////////////////   Support Cone  ///////////////////////////////

 G4VSolid** SolidCone;
 SolidCone = new G4VSolid*[9];

 G4VSolid** SolidConeWithBase;
 SolidConeWithBase = new G4VSolid*[9];

 G4VSolid** SolidConeWithBaseAndHat;
 SolidConeWithBaseAndHat = new G4VSolid*[9];

 G4VSolid** SolidConeWithBaseAndHatAndDowel;
 SolidConeWithBaseAndHatAndDowel = new G4VSolid*[9];

 G4VSolid** SolidFullSupportCone;
 SolidFullSupportCone = new G4VSolid*[9];

 G4VSolid* SolidConeBase = new G4Tubs("solidConeBase",
                                     fConeBaseInnerRadius,
                                     fConeBaseOuterRadius,
                                     fConeBaseThickness*0.5,
                                     0,
                                     360*deg);

 G4VSolid* SolidConeHat = new G4Tubs("solidConeHat",
                                     fConeHatInnerRadius,
                                     fConeHatOuterRadius,
                                     fConeHatThickness*0.5,
                                     0,
                                     360*deg);

 G4VSolid* SolidConeScrew = new G4Tubs("solidConeScrew",
                                       0.,
                                       fConeScrewRadius,
                                       fConeScrewHeight*0.5,
                                       0,
                                       360*deg);  

for(Int_t l=0;l<9;l++){

 SolidCone[l] = new G4Cons("solidCone",
                           fConeUpOuterRadius-fConeThickness, 
                           fConeUpOuterRadius,
                           fConeBottomOuterRadius-fConeThickness,
                           fConeBottomOuterRadius,
                           fConeHeight[l]*0.5,
                           0,
                           360*deg);
                                                                                                                              
 
 SolidConeWithBase[l] = new G4UnionSolid("solidConeWithBase",
                                       new G4DisplacedSolid("DisplacedCone",
                                                            SolidCone[l],
                                                            0,
                                                            G4ThreeVector(0,0,fConeZPosition[l])
                                                           ),
                                       new G4DisplacedSolid("DisplacedConeBase",
                                                            SolidConeBase,
                                                            0,
                                                            G4ThreeVector(0,0,fConeBaseZPosition)
                                                           )
                                      );
                                                                                                                              
 SolidConeWithBaseAndHat[l] = new G4UnionSolid("solidConeWithBaseAndHat",
                                               SolidConeWithBase[l],
                                               new G4DisplacedSolid("DisplacedConeHat",
                                                                    SolidConeHat,
                                                                    0,
                                                                    G4ThreeVector(0,0,fConeHatZPosition[l])
                                                                    )
                                              );                                                                              

 SolidConeWithBaseAndHatAndDowel[l] = new G4UnionSolid("solidConeWithBaseAndHatAndDowel",
                                               SolidConeWithBaseAndHat[l],
                                               new G4DisplacedSolid("DisplacedDowel",
                                                                    SolidDowel,
                                                                    0,
                                                                    G4ThreeVector(0,0,fConeHatZPosition[l]-fConeHatThickness*0.5)
                                                                    )
                                              );                                                   

 G4VSolid** SolidSupportCone;
 SolidSupportCone =new G4VSolid*[6];

 for(Int_t i=0;i<6;i++){

    if(i==0){
      SolidSupportCone[i] = new G4UnionSolid("solidSupportCone",
                                              SolidConeWithBaseAndHatAndDowel[l],
                                              new G4DisplacedSolid("DisplacedConeScrew",
                                                                    SolidConeScrew,
                                                                    0,
                                                                    G4ThreeVector(ConeScrewPos[i][0],ConeScrewPos[i][1],fConeScrewZPosition)
                                                                    )
                                              );
    }else{
      SolidSupportCone[i] = new G4UnionSolid("solidSupportCone",
                                              SolidSupportCone[i-1],
                                              new G4DisplacedSolid("DisplacedConeScrew",
                                                                    SolidConeScrew,
                                                                    0,
                                                                    G4ThreeVector(ConeScrewPos[i][0],ConeScrewPos[i][1],fConeScrewZPosition)
                                                                    )
                                              );
    }

 }

 SolidFullSupportCone[l]=SolidSupportCone[5];
}
////////////////////////////////////////////////////////////////////

///////////////////// Support Prism ///////////////////////////////

 G4VSolid* SolidPrismBase = new G4Box("solidPrismBase",
                                      fPrismBaseXLenght*0.5,
                                      fPrismBaseYLenght*0.5,
                                      fPrismBaseZLenght*0.5);

 G4VSolid* SolidPrismHat = new G4Box("solidPrismHat",
                                      fPrismHatXLenght*0.5,
                                      fPrismHatYLenght*0.5,
                                      fPrismHatZLenght*0.5);

 G4VSolid* SolidFullPrism = new G4Trd("solidFullPrism",
                                      fPrismX1Lenght*0.5,
                                      fPrismX2Lenght*0.5,
                                      fPrismY1Lenght*0.5,
                                      fPrismY2Lenght*0.5,
                                      fPrismZLenght*0.5);

 G4VSolid* SolidPrismHole = new G4Trd("solidPrismHole",
                                      fPrismHoleX1Lenght*0.5,
                                      fPrismHoleX2Lenght*0.5,
                                      fPrismHoleY1Lenght*0.5,
                                      fPrismHoleY2Lenght*0.5,
                                      fPrismHoleZLenght*0.5);

 G4VSolid* SolidPrismScrew = new G4Tubs("solidPrismScrew",
                                       0.,
                                       fPrismScrewRadius,
                                       fPrismScrewHeight*0.5,
                                       0,
                                       360*deg);  

 G4VSolid* SolidPrismWithBase = new G4UnionSolid("PrismWithBase",
                                                 new G4DisplacedSolid("DisplacedPrism",
                                                                      SolidFullPrism,
                                                                      0,
                                                                      G4ThreeVector(0,0,fPrismZPosition)
                                                                      ),
                                                 new G4DisplacedSolid("DisplacedPrismBase",
                                                                      SolidPrismBase,
                                                                      0,
                                                                      G4ThreeVector(0,0,fPrismBaseZPosition)
                                                                     )
                                                );
                                                 
 G4VSolid* SolidPrismWithBaseAndHat = new G4UnionSolid("PrismWithBaseAndHat",
                                                       SolidPrismWithBase,
                                                       new G4DisplacedSolid("DisplacedPrismHat",
                                                                            SolidPrismHat,
                                                                            0,
                                                                            G4ThreeVector(0,0,fPrismHatZPosition)
                                                                            ) 
                                                      );
 G4VSolid** SolidPrismWithDowel;
 SolidPrismWithDowel =new G4VSolid*[2];

 for(Int_t i=0;i<2;i++){

     if(i==0){
        SolidPrismWithDowel[i] = new G4UnionSolid("solidPrismWithDowel",
                                                  SolidPrismWithBaseAndHat,
                                                  new G4DisplacedSolid("DisplacedDowel",
                                                                       SolidDowel,
                                                                       0,
                                                                       G4ThreeVector(-35*mm,0.,fPrismHatZPosition-fPrismHatZLenght*0.5)
                                                                      )
                                                 );
     }else{

        SolidPrismWithDowel[i] = new G4UnionSolid("solidPrismWithDowel",
                                                  SolidPrismWithDowel[i-1],
                                                  new G4DisplacedSolid("DisplacedDowel",
                                                                       SolidDowel,
                                                                       0,
                                                                       G4ThreeVector(+35*mm,0.,fPrismHatZPosition-fPrismHatZLenght*0.5)
                                                                      )
                                                 );
     }

 }


 G4VSolid** SolidPrismWithScrew;
 SolidPrismWithScrew =new G4VSolid*[6];

 for(Int_t i=0;i<6;i++){

     if(i==0){
        SolidPrismWithScrew[i] = new G4UnionSolid("solidPrismWithScrew",
                                                  SolidPrismWithDowel[1],
                                                  new G4DisplacedSolid("DisplacedPrismScrew",
                                                                       SolidPrismScrew,
                                                                       0,
                                                                       G4ThreeVector(PrismScrewPos[i][0],PrismScrewPos[i][1],fPrismScrewZPosition)
                                                                      )
                                                 );
     }else{
        SolidPrismWithScrew[i] = new G4UnionSolid("solidPrismWithScrew",
                                                  SolidPrismWithScrew[i-1],
                                                  new G4DisplacedSolid("DisplacedPrismScrew",
                                                                       SolidPrismScrew,
                                                                       0,
                                                                       G4ThreeVector(PrismScrewPos[i][0],PrismScrewPos[i][1],fPrismScrewZPosition)
                                                                      )
                                                 );
    }
 }                                                                                                                                                        

 G4VSolid* SolidPrismSupport = new G4SubtractionSolid("PrismSupport",
//                                                      SolidPrismWithBaseAndHat,
                                                      SolidPrismWithScrew[5],
                                                      new G4DisplacedSolid("DisplacedPrismHole",
                                                                      SolidPrismHole,
                                                                      0,
                                                                      G4ThreeVector(0,0,fPrismHoleZPosition)
                                                                     )
                                                     ); 

//////////////////////////////////////////////////////////////////



////////////////////    Turner   //////////////////////////////////

 G4VSolid* SolidTurnerBase = new G4Box("solidTurnerBase",
                                       fTurnerBaseXLenght*0.5,   
                                       fTurnerBaseYLenght*0.5,
                                       fTurnerBaseZLenght*0.5);


 G4VSolid* SolidFullTurner = new G4Trd("solidFullTurner",
                                   fTurnerXMaxLenght*0.5,
                                   fTurnerXMinLenght*0.5,
                                   fTurnerYMaxLenght*0.5,
                                   fTurnerYMinLenght*0.5,
                                   fTurnerZLenght*0.5);

 G4VSolid* SolidTurnerHole = new G4Trd("solidTurnerHole",
                                   fTurnerHoleXMaxLenght*0.5,
                                   fTurnerHoleXMinLenght*0.5,
                                   fTurnerHoleYMaxLenght*0.5,
                                   fTurnerHoleYMinLenght*0.5,
                                   fTurnerHoleZLenght*0.5);


 G4VSolid* SolidTurnerWithHole = new G4SubtractionSolid("TurnerWithHole",
                                                        SolidFullTurner,
                                                        new G4DisplacedSolid("DisplacedHole",
                                                                             SolidTurnerHole,
                                                                             0,
                                                                             G4ThreeVector(0,0,-fTurnerZLenght*0.5+fTurnerHoleZLenght*0.5))
                                                       );

 G4VSolid* SolidTurner = new G4UnionSolid("Turner",
                                          new G4DisplacedSolid("DisplacedTurnerBase",
                                                               SolidTurnerBase,
                                                               0,
                                                               G4ThreeVector(0,0,fTurnerBaseZPosition)),
                                          new G4DisplacedSolid("DisplacedTurnerWithHole",
                                                               SolidTurnerWithHole,
                                                               0,
                                                               G4ThreeVector(0,0,fTurnerZPosition))
                                         );


////////////////////////////////////////////////////////////////////



////////////////////    Honeycomb and Reinforcement Ring ///////////

  G4VSolid* SolidHoneyComb =  new G4Tubs("solidHoneyComb",
                            fInnerRadius,
                            fOuterRadius,
                            fZLength*0.5,
                            startPhiAngle,
                            deltaPhiAngle);

  G4VSolid* SolidRing =  new G4Tubs("solidRing",
                            fRingInnerRadius,
                            fRingOuterRadius,
                            fRingZLength*0.5,
                            startPhiAngle,
                            deltaPhiAngle);
 
  G4VSolid* SolidTooth = new G4Box("solidTooth",
                            fToothXLenght*0.5,                                             
                            fToothYLenght*0.5,
                            fToothZLenght*0.5+0.1*mm);

  G4VSolid* SolidSupportWithoutTooth = new G4SubtractionSolid("solidSupportWithoutTooth",
                                                              new G4DisplacedSolid("DisplacedHoneyComb",
                                                                                   SolidHoneyComb,
                                                                                   0,
                                                                                   G4ThreeVector(0,0,0)
                                                                                   ),
                                                              new G4DisplacedSolid("DisplacedTooth_Up",
                                                                                   SolidTooth,
                                                                                   0,
                                                                                   G4ThreeVector(-3*fExternalRadius+2*fMirrorGap[1],0,fToothZPosition-fZPosition)
                                                                                   )
                                                              );

  G4VSolid* SolidSupportWithTooth = new G4UnionSolid("solidSupportWithTooth",
                                                     SolidSupportWithoutTooth,  
                                                     new G4DisplacedSolid("DisplacedTooth_Up",
                                                                          SolidTooth,
                                                                          0,
                                                                          G4ThreeVector(+3*fExternalRadius-2*fMirrorGap[1],0,fToothZPosition-fZPosition)
                                                                          )
                                                     );


  G4VSolid* SolidSupport = new G4UnionSolid("solidSupport",
                                            SolidSupportWithTooth,
                                            new G4DisplacedSolid("DisplacedRing",
                                                                 SolidRing,
                                                                 0,
                                                                 G4ThreeVector(0,0,fRingZPosition-fZPosition)                                                                                                              )
                                           );

//////////////////////////////////////////////////////////////////////////////////////////////////

   G4VSolid** displacedCone_Jura;
   displacedCone_Jura = new G4VSolid*[10];

   G4VSolid** displacedCone_Saleve;
   displacedCone_Saleve = new G4VSolid*[10];


   for(Int_t i=0;i<9;i++){   

      if(i==0){	
        displacedCone_Jura[i]= new G4UnionSolid("solidSupportWithCone",
                                                new G4DisplacedSolid("RotatedSupport",
                                                                     SolidSupport,
                                                                     G4Transform3D(rmZminus90,         
                                                                                   G4ThreeVector(0,0,0))),
                                                new G4DisplacedSolid("DisplacedCone",
                                                                     SolidFullSupportCone[i],
                                                                     0,                                                                     
                                                                     G4ThreeVector(SupportConePos_Jura[i][0],SupportConePos_Jura[i][1],-fZPosition)
                                                                    )	                                               
                                               );

        displacedCone_Saleve[i]= new G4UnionSolid("solidSupportWithCone",
                                                new G4DisplacedSolid("RotatedSupport",
                                                                     SolidSupport,
                                                                     G4Transform3D(rmZplus90,
                                                                                   G4ThreeVector(0,0,0))),
                                                new G4DisplacedSolid("DisplacedCone",
                                                                     SolidFullSupportCone[i],
                                                                     0,
                                                                     G4ThreeVector(SupportConePos_Saleve[i][0],SupportConePos_Saleve[i][1],-fZPosition)
                                                                    )
                                               );

     }else{

        displacedCone_Jura[i]= new G4UnionSolid("solidSupportWithCone",
                                                displacedCone_Jura[i-1],
                                                new G4DisplacedSolid("DisplacedCone",
                                                                     SolidFullSupportCone[i],
                                                                     0,
                                                                     G4ThreeVector(SupportConePos_Jura[i][0],SupportConePos_Jura[i][1],-fZPosition)
                                                                     )
                                               );

       displacedCone_Saleve[i]= new G4UnionSolid("solidSupportWithCone",
                                                displacedCone_Saleve[i-1],
                                                new G4DisplacedSolid("DisplacedCone",
                                                                     SolidFullSupportCone[i],
                                                                     0,
                                                                     G4ThreeVector(SupportConePos_Saleve[i][0],SupportConePos_Saleve[i][1],-fZPosition)
                                                                     )
                                               );

    }     
   } //end of loop on cones

//////////////  loop on turners ///////////////////

   G4double TurnerPos[18][2]={{178.7*mm,1191.4*mm}, //posizioni per il Jura
                              {433.*mm,1190.5*mm},
                              {127.8*mm,661.3*mm},
                              {481.3*mm,661.3*mm},
                              {735.6*mm,661.8*mm},
                              {1089.1*mm,661.8*mm},
                              {62.6*mm,209.5*mm},
                              {519.8*mm,45.3*mm},
                              {696.5*mm,45.2*mm},
                              {1039.5*mm,133.7*mm},
                              {1393.2*mm,133.5*mm},
                              {127.1*mm,-311.5*mm},
                              {483.4*mm,-311.7*mm},
                              {734.5*mm,-311.6*mm},
                              {1089.1*mm,-310.6*mm},
                              {187.7*mm,-835.*mm},
                              {432.2*mm,-839.3*mm},
                              {785.8*mm,-839.6*mm}  
                             };

  G4VSolid** displacedTurner_Jura;
  displacedTurner_Jura = new G4VSolid*[18];
  G4VSolid** displacedTurner_Saleve;
  displacedTurner_Saleve = new G4VSolid*[18];



  for(Int_t i=0;i<18;i++){
   if(i==0){
     displacedTurner_Jura[i]=new G4UnionSolid("solidSupportWithTurner",
                                              displacedCone_Jura[8],
                                              new G4DisplacedSolid("DisplacedTurner",
                                                                    SolidTurner,
                                                                    0,
                                                                    G4ThreeVector(TurnerPos[i][0],TurnerPos[i][1],-fZPosition)
                                                                   )
                                              );

     displacedTurner_Saleve[i]=new G4UnionSolid("solidSupportWithTurner",
                                                displacedCone_Saleve[8],
                                                new G4DisplacedSolid("DisplacedTurner",
                                                                     SolidTurner,
                                                                     0,
                                                                     G4ThreeVector(-TurnerPos[i][0],TurnerPos[i][1],-fZPosition)
                                                                   )
                                              );
   }else{

     displacedTurner_Jura[i]=new G4UnionSolid("solidSupportWithTurner",
                                              displacedTurner_Jura[i-1],
                                              new G4DisplacedSolid("DisplacedTurner",
                                                                    SolidTurner,
                                                                    0,
                                                                    G4ThreeVector(TurnerPos[i][0],TurnerPos[i][1],-fZPosition)
                                                                   )
                                              );

    displacedTurner_Saleve[i]=new G4UnionSolid("solidSupportWithTurner",
                                               displacedTurner_Saleve[i-1],
                                               new G4DisplacedSolid("DisplacedTurner",
                                                                    SolidTurner,
                                                                    0,
                                                                    G4ThreeVector(-TurnerPos[i][0],TurnerPos[i][1],-fZPosition)
                                                                   )
                                              );
   
   }
  } //end of loop on turners


//////////////////////////////////////////////////

////////////// Panel with prism support ////////

  G4VSolid* displacedPrism_Jura = new G4UnionSolid("solidSupportWithPrismSupport",
                                                   displacedTurner_Jura[17],
                                                   new G4DisplacedSolid("DisplacedPrism",
                                                                        SolidPrismSupport,     
                                                                        0,     
                                                                        G4ThreeVector(fPrismXPosition_Jura,0,-fZPosition)
                                                                       )
                                                   );

  G4VSolid* displacedPrism_Saleve = new G4UnionSolid("solidSupportWithPrismSupport",
                                                   displacedTurner_Saleve[17],
                                                   new G4DisplacedSolid("DisplacedPrism",
                                                                        SolidPrismSupport,
                                                                        0,
                                                                        G4ThreeVector(fPrismXPosition_Saleve,0,-fZPosition)
                                                                       )
                                                   );

///////////////////////////////////////////////

  G4RotationMatrix  rmY_Jura;
  rmY_Jura.rotateY(fRotation_Jura);

  G4RotationMatrix  rmY_Saleve;
  rmY_Saleve.rotateY(fRotation_Saleve);


  fSolidSupport_Jura = new G4DisplacedSolid("RotatedPanel_Jura",
                                            displacedPrism_Jura,
                                            G4Transform3D(rmY_Jura,
                                                          G4ThreeVector(0,0,fZPosition)));

  fLogicalSupport_Jura =  new G4LogicalVolume(fSolidSupport_Jura,         // solid
                                              fMaterial,                     // material
                                              "logicHoneyComb",              // name
                                              0,                             // field manager
                                              0,                             // sensitive detector
                                              0);    


  fPhysicalSupport_Jura = new G4PVPlacement(0,                             //rot
                                            G4ThreeVector(0,0,0),          // at pos
                                            fLogicalSupport_Jura,                              // its logical volume
                                            "PhysicalSupport_Jura",                        // its name
                                            fMotherVolume,                                 // its mother  volume
                                            false,                                         // no boolean operations
                                            0);                                            // copy number              



  fSolidSupport_Saleve = new G4DisplacedSolid("RotatedPanel_Saleve",
                                            displacedPrism_Saleve,
                                            G4Transform3D(rmY_Saleve,
                                                          G4ThreeVector(0,0,fZPosition)));

  fLogicalSupport_Saleve =  new G4LogicalVolume(fSolidSupport_Saleve,         // solid
                                              fMaterial,                     // material
                                              "logicHoneyComb",              // name
                                              0,                             // field manager
                                              0,                             // sensitive detector
                                              0);


  fPhysicalSupport_Saleve = new G4PVPlacement(0,                             //rot
                                              G4ThreeVector(0,0,0),          // at pos
                                              fLogicalSupport_Saleve,                              // its logical volume
                                              "PhysicalSupport_Saleve",                        // its name
                                              fMotherVolume,                                 // its mother  volume
                                              false,                                         // no boolean operations
                                            0);                                            // copy number              



    
//  fLogicalVolume = new G4LogicalVolume(SolidPrismSupport,           // solid
//                                       fMaterial,            // material
//                                       "logicPrism",          // name
//                                       0,                    // field manager
//                                       0,                    // sensitive detector
//                                       0);
//
//  fPhysicalVolume = new G4PVPlacement(0,
// 				      G4ThreeVector(fPrismXPosition_Jura,0,0),  // at pos
//                                      fLogicalVolume,                              // its logical volume
//                                      "PhysicalPrism",                        // its name
//                                      fMotherVolume,                                 // its mother  volume
//                                      false,                                         // no boolean operations
//                                      0);             


}

void RICHMirrorSupports::SetProperties()
{
   fVisAtt= new G4VisAttributes(G4Colour(1.,1.,1.));
   fLogicalSupport_Jura ->SetVisAttributes(fVisAtt);
   fLogicalSupport_Saleve ->SetVisAttributes(fVisAtt);
//   fLogicalVolume->SetVisAttributes(fVisAtt); 
}

void RICHMirrorSupports::DefineOpticalSurface()
{
  // RICHMirrorSupport reflective surface
  fOpticalSurface = new G4OpticalSurface("RICHMirrorSupport");
  fOpticalSurface->SetType(dielectric_metal);
  fOpticalSurface->SetFinish(ground);
  fOpticalSurface->SetModel(unified);
  fOpticalSurface->SetSigmaAlpha(RICHMaterialParameters::GetInstance()
                                 ->GetVesselOpticalSurfaceSigmaAlpha());

  fOpticalSurface -> SetMaterialPropertiesTable(RICHMaterialParameters::GetInstance()
                                                ->GetVesselOpticalSurfacePT());
}

