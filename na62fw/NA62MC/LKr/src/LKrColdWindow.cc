//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------------
// History:
//
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//            Evelina Marinova(Evelina.Marinova@cern.ch) 
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"
#include "G4Sphere.hh"

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "LKrGeometryParameters.hh"
#include "LKrMaterialParameters.hh"

#include "LKrFlange.hh"
#include "LKrFlangesBolts.hh"
#include "LKrElectrodes.hh"
#include "LKrColdWindow.hh"

#include "G4SDManager.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"

#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"


/// \class LKrColdWindow 
/// \Brief
/// LKrColdWindow class.
/// \EndBrief   
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the front and back cold window.
/// \EndDetailed



LKrColdWindow::LKrColdWindow (G4Material * Material, G4LogicalVolume * MotherVolume):
  NA62VComponent(Material,MotherVolume)
{
  ReadGeometryParameters();

  // Mandatory here to Find or Build the needed materials
  LKrMaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

LKrColdWindow::~LKrColdWindow(){}

void LKrColdWindow::ReadGeometryParameters()
{
  // Read all the geometrical parameters and copy them to private members
  LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

  fThicknessSliceColdWindow = GeoPars->GetThicknessSliceColdWindow();

  fInnerCryostatMinRadius = GeoPars->GetInnerCryostatMinRadius();
  fThicknessOfInnerCryo = GeoPars->GetThicknessOfInnerCryo();
  fLongitudinalLengthInnerCryo = GeoPars->GetLongitudinalLengthInnerCryo();

  fOuterCryostatMinRadius = GeoPars->GetOuterCryostatMinRadius();
  fThicknessOfOuterCryo = GeoPars->GetThicknessOfOuterCryo(); 
  fLongitudinalLengthOuterCryo = GeoPars->GetLongitudinalLengthOuterCryo();

  fInnerDistanceFrontBackWarmWindow = GeoPars->GetInnerDistanceFrontBackWarmWindow();
  fDistanceFrontWarmWindowLKr = GeoPars->GetDistanceFrontWarmWindowLKr();

  fLongitudinalLengthLKrVolume  = GeoPars->GetLongitudinalLengthLKrVolume();
  fLengthOfColdWindowPlate = GeoPars->GetLengthOfColdWindowPlate();

  fRadiusFrontColdWindow = GeoPars->GetRadiusFrontColdWindow();
  fRadiusBackColdWindow  = GeoPars->GetRadiusBackColdWindow();
  fThicknesConvexColdWindow  = GeoPars->GetThicknesConvexColdWindow();
  fThicknesConvexColdBackWindow  = GeoPars->GetThicknesConvexColdBackWindow();

  fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();

  fOuterBeamPipeRadius =  GeoPars->GetOuterBeamPipeRadius();
  fThicknessSteelFoilColdWindow =  GeoPars->GetThicknessSteelFoilColdWindow();

  fRadiusSteelFoilColdWindow =  GeoPars->GetRadiusSteelFoilColdWindow();


  fDistanceColdWindowToMonotube = GeoPars->GetDistanceColdWindowToMonotube();

  fDistanceMonotubeToothFlange = GeoPars->GetDistanceMonotubeToothFlange() ;
  fHightToothFlange = GeoPars->GetHightToothFlange();
  fLengthToothFlange = GeoPars->GetLengthToothFlange();

  fDistanceMonotubeBellowToothFlange = GeoPars->GetDistanceMonotubeBellowToothFlange();
  fHightBellowToothFlange = GeoPars->GetHightBellowToothFlange();
  fLengthBellowToothFlange = GeoPars->GetLengthBellowToothFlange();

  fDistanceBellowToothFlangeAtBackToRareWindow  = GeoPars->GetDistanceBellowToothFlangeAtBackToRareWindow();

  fHightFlange1AtColdWindowBeam = GeoPars->GetHightFlange1AtColdWindowBeam();
  fLengthFlange1AtColdWindowBeam = GeoPars->GetLengthFlange1AtColdWindowBeam();
  fMinRadiusFlange1AtColdWindowBeam = GeoPars->GetMinRadiusFlange1AtColdWindowBeam();

  fHightFlange2AtColdWindowBeam = GeoPars->GetHightFlange2AtColdWindowBeam();
  fLengthFlange2AtColdWindowBeam = GeoPars->GetLengthFlange2AtColdWindowBeam();

  fHightFlange3AtColdWindowBeam = GeoPars->GetHightFlange3AtColdWindowBeam();
  fLengthFlange3AtColdWindowBeam = GeoPars->GetLengthFlange3AtColdWindowBeam();

  fHightFlange4AtColdWindowBeam = GeoPars->GetHightFlange4AtColdWindowBeam();
  fLengthFlange4AtColdWindowBeam  = GeoPars->GetLengthFlange4AtColdWindowBeam();
  fMinRadiusFlange4AtColdWindowBeam = GeoPars->GetMinRadiusFlange4AtColdWindowBeam();

  fHightFlange1AtColdWindowOut = GeoPars->GetHightFlange1AtColdWindowOut();
  fLengthFlange1AtColdWindowOut = GeoPars->GetLengthFlange1AtColdWindowOut();
  fMinRadiusFlange1AtColdWindowOut = GeoPars->GetMinRadiusFlange1AtColdWindowOut();

  fHightFlange2AtColdWindowOut = GeoPars->GetHightFlange2AtColdWindowOut();
  fLengthFlange2AtColdWindowOut = GeoPars->GetLengthFlange2AtColdWindowOut();
  fMinRadiusFlange2AtColdWindowOut = GeoPars->GetMinRadiusFlange2AtColdWindowOut();
  fDistanceFlange2AtColdWindowOutFaceToSteelFoil = GeoPars->GetDistanceFlange2AtColdWindowOutFaceToSteelFoil();


  fHightBackFlange1AtColdWindowBeam = GeoPars->GetHightBackFlange1AtColdWindowBeam() ;

  fLengthBackFlange1AtColdWindowBeam = GeoPars->GetLengthBackFlange1AtColdWindowBeam() ;
  fMinRadiusBackFlange1AtColdWindowBeam = GeoPars->GetMinRadiusBackFlange1AtColdWindowBeam() ;

  fHightBackFlange2AtColdWindowBeam = GeoPars->GetHightBackFlange2AtColdWindowBeam() ;
  fLengthBackFlange2AtColdWindowBeam = GeoPars->GetLengthBackFlange2AtColdWindowBeam() ;
  fMinRadiusBackFlange2AtColdWindowBeam = GeoPars->GetMinRadiusBackFlange2AtColdWindowBeam() ;

  fHightBackFlange3AtColdWindowBeam = GeoPars->GetHightBackFlange3AtColdWindowBeam() ;
  fLengthBackFlange3AtColdWindowBeam = GeoPars->GetLengthBackFlange3AtColdWindowBeam() ;
  fMinRadiusBackFlange3AtColdWindowBeam = GeoPars->GetMinRadiusBackFlange3AtColdWindowBeam();

  fHightBackFlange4AtColdWindowBeam = GeoPars->GetHightBackFlange4AtColdWindowBeam();
  fLengthBackFlange4AtColdWindowBeam = GeoPars->GetLengthBackFlange4AtColdWindowBeam() ;
  fMinRadiusBackFlange4AtColdWindowBeam = GeoPars->GetMinRadiusBackFlange4AtColdWindowBeam() ;

  fZDistanceBetweeBackEdges3and4 = GeoPars->GetZDistanceBetweeBackEdges3and4() ;

  fHightBackFlange5AtColdWindowBeam = GeoPars->GetHightBackFlange5AtColdWindowBeam() ;
  fLengthBackFlange5AtColdWindowBeam = GeoPars->GetLengthBackFlange5AtColdWindowBeam();
  fMinRadiusBackFlange5AtColdWindowBeam = GeoPars->GetMinRadiusBackFlange5AtColdWindowBeam() ;

  fHightBackFlangeAtColdWindowOut = GeoPars->GetHightBackFlangeAtColdWindowOut();
  fLengthBackFlangeAtColdWindowOut = GeoPars->GetLengthBackFlangeAtColdWindowOut();
  fMinRadiusBackFlangeAtColdWindowOut = GeoPars->GetMinRadiusBackFlangeAtColdWindowOut();


  fDistanceFaceBackFlangeOutToColdWindow = GeoPars->GetDistanceFaceBackFlangeOutToColdWindow();
  fThicknessSliceBackColdWindow = GeoPars->GetThicknessSliceBackColdWindow();


  fRadiusHolesBoltsFrontCold = GeoPars->GetRadiusHolesBoltsFrontCold() ;
  fLengthHolesBoltsFrontCold = GeoPars->GetLengthHolesBoltsFrontCold() ;
  fRadiusHeadsBoltsFrontCold = GeoPars->GetRadiusHeadsBoltsFrontCold() ;
  fLengthHeadsBoltsFrontCold = GeoPars->GetLengthHeadsBoltsFrontCold() ;
  fDistanceBoltToBeamPipe = GeoPars->GetDistanceBoltToBeamPipe(); 

  fRadiusFrontColdWindowBeamPipe = GeoPars->GetRadiusFrontColdWindowBeamPipe();

  fLengthHolesBoltsBackColdInFlange5 = GeoPars->GetLengthHolesBoltsBackColdInFlange5() ;
  fLengthHolesBoltsBackColdInFlange4 = GeoPars->GetLengthHolesBoltsBackColdInFlange4() ;
  fAngleCentralAxisBolt = GeoPars->GetAngleCentralAxisBolt();

  fBEATCHPositionFrontWarmWindowEdge = GeoPars->GetBEATCHPositionFrontWarmWindowEdge();
  fBEATCHPositionFrontEdgeOuterVessel = GeoPars->GetBEATCHPositionFrontEdgeOuterVessel();
  fBEATCHPositionBackEdgeOuterVessel = GeoPars->GetBEATCHPositionBackEdgeOuterVessel();
  fBEATCHPositionBackWarmWindowEdge = GeoPars->GetBEATCHPositionBackWarmWindowEdge();

}

void LKrColdWindow::CreateGeometry()
{

  // create solid beam pipe so this volume can later be subtracted of the windows and the filler

  G4double  pSPhi = 0*deg;
  G4double  pDPhi = 360*deg;

  //front cold window 

  G4double startPhiAngle=0;
  G4double deltaPhiAngle=360*deg;

  G4double CurvatureRadius = fRadiusFrontColdWindow;

  G4double MinTheta =  pi - asin(( fRadiusSteelFoilColdWindow )/CurvatureRadius);

  G4double MaxTheta =  pi - asin((fRadiusFrontColdWindowBeamPipe )/CurvatureRadius);

  G4RotationMatrix rmXminus180;
  rmXminus180.rotateX(-180*deg);

  G4Sphere* solidColdFrontWindow= new G4Sphere("LKrColdFrontWindow",
      CurvatureRadius - fThicknesConvexColdWindow,
      CurvatureRadius,
      startPhiAngle,
      deltaPhiAngle,
      pi-MaxTheta,        //Theta
      MaxTheta-MinTheta   //DeltaTheta
      );


  G4double OffsetLKrVolume =  ( fLongitudinalLengthOuterCryo + (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge) 
      + ( fBEATCHPositionBackWarmWindowEdge -  fBEATCHPositionBackEdgeOuterVessel) 
      - fInnerDistanceFrontBackWarmWindow )/2 +  fDistanceFrontWarmWindowLKr;

  G4double InnerCryoZPosition = - fLongitudinalLengthOuterCryo/2  - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge) 
    + fLongitudinalLengthInnerCryo/2 + OffsetLKrVolume;

  //G4RotationMatrix Ra =  G4RotationMatrix(G4ThreeVector(0., 1.,0.),-180*deg);

  //calculate the thickness of the missing segment to complete sphere at the front
  G4double MissingPieceSphereFront = (CurvatureRadius - ( fRadiusHoleSpacerPlate )/ tan(MinTheta)); 

  //Longitudinal thickness of the slice of the sphere, used as Cold Window

  G4ThreeVector Ta;
  // virtual tube for the intersection of the windows and the fillers

  VirtualTube1 = new G4Tubs("virtual tube",                        
      fRadiusFrontColdWindowBeamPipe ,
      fRadiusSteelFoilColdWindow,
      fThicknessSliceColdWindow/2,
      pSPhi,
      pDPhi);

  G4IntersectionSolid* Window = new G4IntersectionSolid("Window",
      new G4DisplacedSolid("FrontColdWindow",solidColdFrontWindow,G4Transform3D(rmXminus180,G4ThreeVector(0.,0., CurvatureRadius - fThicknessSliceColdWindow/2))),
      VirtualTube1
      );					   

  LogicalWindow = new G4LogicalVolume(Window, //WindowMinusBeamPipe,        // solid
      fMaterial,             // material
      "ColdFrontWindow",           // name
      0,                    // field manager
      0,                    // sensitive detector
      0);                   // user limits


  PhysicalWindow = new G4PVPlacement(0,
      G4ThreeVector(0.,0.,(-fLongitudinalLengthInnerCryo/2 + InnerCryoZPosition - fThicknessSliceColdWindow/2)),
      LogicalWindow,      // its logical volume
      "ColdFrontWindow",         // its name
      fMotherVolume,               // its mother  volume
      false,           // no boolean operations
      0);              // copy number



  //*********************** Create filler for front cold window *********************************//


  G4double ThicknessFillerZ = fThicknessSliceColdWindow +  0* MissingPieceSphereFront  - fThicknesConvexColdWindow; 
  G4double RadiusFiller = CurvatureRadius - fThicknesConvexColdWindow ;

  solidSphereFiller = new G4Sphere("solidSphereFiller",
      0*cm,
      RadiusFiller,
      startPhiAngle,
      deltaPhiAngle,
      pi-MaxTheta,        //Theta
      MaxTheta-MinTheta   //DeltaTheta
      );


  VirtualTube = new G4Tubs("virtual tube 1",
      fOuterBeamPipeRadius,
      fRadiusSteelFoilColdWindow,
      ThicknessFillerZ/2,
      pSPhi,
      pDPhi);

  G4IntersectionSolid* Filler = new G4IntersectionSolid("Filler",
      new G4DisplacedSolid("SphereFiller",solidSphereFiller,G4Transform3D(rmXminus180,G4ThreeVector(0.,0., RadiusFiller - ThicknessFillerZ/2))),
      VirtualTube
      );					   

  LogicalFiller = new G4LogicalVolume(Filler, 
      G4Material::GetMaterial("LKr_AluminiumHoneycomb"),             // material
      "Filler",           // name
      0,                    // field manager
      0,                    // sensitive detector
      0);                   // user limits



  PhysicalFiller = new G4PVPlacement(0,
      G4ThreeVector(0.,0.,(-fLongitudinalLengthInnerCryo/2 + InnerCryoZPosition - ThicknessFillerZ/2 - fThicknessSteelFoilColdWindow)),
      LogicalFiller,      // its logical volume
      "Filler",         // its name
      fMotherVolume,               // its mother  volume
      false,           // no boolean operations
      0);              // copy number

  fVisAtt= new G4VisAttributes(G4Colour(0.0,0.0,1.0));
  fVisAtt -> SetVisibility(true);
  LogicalFiller ->SetVisAttributes(fVisAtt);


  //************************ create a closing foil behing the filler *******************************//


  solidClosingFoilFiller = new G4Tubs("closing plate filler",                        
      fOuterBeamPipeRadius,
      fRadiusSteelFoilColdWindow, 
      fThicknessSteelFoilColdWindow/2,
      pSPhi,
      pDPhi);


  LogicalClosingFoilFiller = new G4LogicalVolume(solidClosingFoilFiller,         // solid
      fMaterial,             // material
      "ClosingFoilFiller",           // name
      0,                    // field manager
      0,                    // sensitive detector
      0);                   // user limits



  G4double ClosingFoilFillerPozitionZ = -fLongitudinalLengthInnerCryo/2 + InnerCryoZPosition - fThicknessSteelFoilColdWindow/2;

  PhysicalClosingFoilFiller = new G4PVPlacement(0,
      G4ThreeVector(0.,0.,ClosingFoilFillerPozitionZ),
      LogicalClosingFoilFiller,      // its logical volume
      "Closing Foil Filler",         // its name
      fMotherVolume,               // its mother  volume
      false,           // no boolean operations
      0);              // copy number

  fVisAtt= new G4VisAttributes(G4Colour(1.0,0.0,0.0));
  fVisAtt -> SetVisibility(true);
  LogicalClosingFoilFiller ->SetVisAttributes(fVisAtt);

  //*******************************************************
  //**************** create cold back window


  CurvatureRadius = fRadiusBackColdWindow;
  MinTheta =  asin((fOuterBeamPipeRadius +fHightBackFlange3AtColdWindowBeam + fHightBackFlange4AtColdWindowBeam)/ CurvatureRadius);
  MaxTheta =  asin( (fInnerCryostatMinRadius + fThicknessOfInnerCryo )/ CurvatureRadius);
  solidColdBackWindow= new G4Sphere("LKrColdBackWindow",
      CurvatureRadius - fThicknesConvexColdBackWindow,
      CurvatureRadius,
      startPhiAngle,
      deltaPhiAngle,
      MinTheta,
      MaxTheta - MinTheta);

  logicalColdBackWindow = new G4LogicalVolume(solidColdBackWindow, // solid
      fMaterial,            // material
      "LKrColdBackWindow",  // name
      0,                    // field manager
      0,                    // sensitive detector
      0);                   // user limits

  G4RotationMatrix Ra = G4RotationMatrix(G4ThreeVector(0., 1.,0.),0*deg);

  G4double  MissingPieceSphereBack = CurvatureRadius - ( fOuterBeamPipeRadius +fHightBackFlange3AtColdWindowBeam + fHightBackFlange4AtColdWindowBeam )/ tan(MinTheta); 

  //Longitudinal thickness of the slice of the sphere, used as Cold Window

  Ta = G4ThreeVector(0,0,- CurvatureRadius  + InnerCryoZPosition + fLongitudinalLengthInnerCryo/2 + fThicknessSliceBackColdWindow + MissingPieceSphereBack);

  physiColdBackWindow = new G4PVPlacement(G4Transform3D(Ra,Ta),
      logicalColdBackWindow,             // its logical volume
      "LKrColdBackWindow",                // its name
      fMotherVolume,              // its mother  volume
      false,                     // no boolean operations
      0);                        // copy number

  //***************************************************************************//
  //****************           FLANGES                      *******************//
  //***************************************************************************//

  // front

  G4double LengthGlobalFlangeColdFrontWindow = fLengthFlange1AtColdWindowBeam + fLengthFlange2AtColdWindowBeam 
    + fLengthFlange3AtColdWindowBeam + fLengthFlange4AtColdWindowBeam + fLengthHeadsBoltsFrontCold;
  Ta = G4ThreeVector(0.,0.,(ClosingFoilFillerPozitionZ - fThicknessSteelFoilColdWindow/2 - LengthGlobalFlangeColdFrontWindow /2 ));

  fGlobalFlangeFrontColdWindow = new LKrFlange("GlobalFlangeFrontColdWindow", G4Material::GetMaterial("G4_Galactic"), 
      fMotherVolume, fOuterBeamPipeRadius,fHightFlange2AtColdWindowBeam,LengthGlobalFlangeColdFrontWindow, Ta );


  //***************************************************************************//
  //*** add some flanges to the front cold window *****************************//
  //***************************************************************************//

  // add the little tooth - ring - make a ring of stainless steel (110 mm after the start of the monotube which is 130 mm after the filler)

  Ta = G4ThreeVector(0.,0.,( LengthGlobalFlangeColdFrontWindow /2 - fThicknessSliceColdWindow 
        + fDistanceColdWindowToMonotube + fDistanceMonotubeToothFlange));
  fFlangeTooth = new LKrFlange("FlangeTooth", fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(), 
      fOuterBeamPipeRadius, fHightToothFlange, fLengthToothFlange, Ta );


  // add the little flange to which a little bellow holds on the inner side of the front cold window

  Ta = G4ThreeVector(0.,0.,( LengthGlobalFlangeColdFrontWindow /2 - fThicknessSliceColdWindow + fDistanceColdWindowToMonotube +  fDistanceMonotubeBellowToothFlange));   
  fBellowFlangeTooth = new LKrFlange("BellowFlangeTooth",fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(), fOuterBeamPipeRadius, fHightBellowToothFlange, fLengthBellowToothFlange, Ta );

  // add one in the back, 65 mm before the rare cold window. Position is not very precise. just add the material

  Ta = G4ThreeVector(0.,0.,( InnerCryoZPosition + fLongitudinalLengthInnerCryo/2 + fThicknessSliceBackColdWindow - fDistanceBellowToothFlangeAtBackToRareWindow));
  fBellowFlangeToothAtBack = new LKrFlange("BellowFlangeToothAtBack", fMaterial, fMotherVolume, fOuterBeamPipeRadius, 
      fHightBellowToothFlange, fLengthBellowToothFlange, Ta );  


  //
  // four pieces of a flange around the beam pipe at the cold front window
  // the flanges are just made as rings around the beam pipe as close as possible to the real shape/amount of material
  // optimizations might be possible 
  // 

  //part 4

  Ta = G4ThreeVector(0.,0.,(  LengthGlobalFlangeColdFrontWindow /2 - fLengthFlange4AtColdWindowBeam / 2));
  fFlange4AtColdWindowBeam = new LKrFlange("Flange4AtColdWindowBeam", fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(),
      fMinRadiusFlange4AtColdWindowBeam , fHightFlange4AtColdWindowBeam, fLengthFlange4AtColdWindowBeam , Ta );

  //part 3

  Ta = G4ThreeVector(0.,0.,Ta.z() - fLengthFlange4AtColdWindowBeam / 2 - fLengthFlange3AtColdWindowBeam / 2);
  fFlange3AtColdWindowBeam = new LKrFlange("Flange3AtColdWindowBeam",fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(), 
      fOuterBeamPipeRadius , fHightFlange3AtColdWindowBeam, fLengthFlange3AtColdWindowBeam , Ta );

  //part 2

  Ta = G4ThreeVector(0.,0.,Ta.z() - fLengthFlange3AtColdWindowBeam / 2 - fLengthFlange2AtColdWindowBeam / 2);
  fFlange2AtColdWindowBeam = new LKrFlange("Flange2AtColdWindowBeam", fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(), 
      fOuterBeamPipeRadius , fHightFlange2AtColdWindowBeam, fLengthFlange2AtColdWindowBeam , Ta );     

  //part 1

  Ta = G4ThreeVector(0.,0.,Ta.z() - fLengthFlange2AtColdWindowBeam / 2 - fLengthFlange1AtColdWindowBeam / 2);
  fFlange1AtColdWindowBeam = new LKrFlange("Flange1AtColdWindowBeam", fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(), 
      fMinRadiusFlange1AtColdWindowBeam , fHightFlange1AtColdWindowBeam, fLengthFlange1AtColdWindowBeam , Ta );     

  //part 6 - honeycomb aluminium

  Ta = G4ThreeVector(0.,0.,(  LengthGlobalFlangeColdFrontWindow /2 - fLengthFlange4AtColdWindowBeam / 2 - fLengthFlange3AtColdWindowBeam / 2));
  fFlange6AtColdWindowBeam = new LKrFlange("Flange6AtColdWindowBeam", G4Material::GetMaterial("LKr_AluminiumHoneycomb"), 
      fGlobalFlangeFrontColdWindow->GetLogicalVolume(),fOuterBeamPipeRadius + fHightFlange3AtColdWindowBeam ,
      fHightFlange2AtColdWindowBeam - fHightFlange3AtColdWindowBeam, 
      fLengthFlange4AtColdWindowBeam + fLengthFlange3AtColdWindowBeam , Ta );

  // bolts in part one of the flange at the front window around the beam pipe

  G4double ROfHolesBolts =  fRadiusHolesBoltsFrontCold/2 + fMinRadiusFlange1AtColdWindowBeam +  fDistanceBoltToBeamPipe ;

  // 12 equidistant bolts ==> at 30 deg interval

  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){

    G4double ThetaOfHolesBolts = fAngleCentralAxisBolt + iHoleBolt * (30 * deg);

    fHolesBoltsFrontCold = new LKrFlangesBolts("HolesBoltsFrontCold",G4Material::GetMaterial("G4_Galactic"), 
        fFlange1AtColdWindowBeam->GetLogicalVolume(), fRadiusHolesBoltsFrontCold, 
        fLengthHolesBoltsFrontCold, ROfHolesBolts,  ThetaOfHolesBolts, 
        - fLengthFlange1AtColdWindowBeam / 2 + fLengthHolesBoltsFrontCold / 2);
  }

  //G4double FillerPositionZ = (-fLongitudinalLengthInnerCryo/2 + InnerCryoZPosition - ThicknessFillerZ/2 - fThicknessSteelFoilColdWindow);

  for (G4int iHeadBolt = 0; iHeadBolt < 12 ; iHeadBolt++){

    G4double ThetaOfHeadsBolts = - fAngleCentralAxisBolt + iHeadBolt * (30 * deg);

    fHeadsBoltsFrontCold = new LKrFlangesBolts("HeadsBoltsFrontCold",fMaterial, fGlobalFlangeFrontColdWindow->GetLogicalVolume(), 
        fRadiusHeadsBoltsFrontCold,fLengthHeadsBoltsFrontCold,ROfHolesBolts, ThetaOfHeadsBolts,
        - LengthGlobalFlangeColdFrontWindow / 2 + fLengthHeadsBoltsFrontCold / 2);
  }


  //
  // outer flange made in 2 parts
  //
  // part 1

  Ta = G4ThreeVector(0.,0., ClosingFoilFillerPozitionZ -  fDistanceFlange2AtColdWindowOutFaceToSteelFoil  - fLengthFlange1AtColdWindowOut / 2);

  fFlange1AtColdWindowOut = new LKrFlange("Flange1AtColdWindowOut", fMaterial, fMotherVolume, 
      fMinRadiusFlange1AtColdWindowOut, fHightFlange1AtColdWindowOut, fLengthFlange1AtColdWindowOut , Ta ); 

  // part 2

  Ta = G4ThreeVector(0.,0., ClosingFoilFillerPozitionZ -  fDistanceFlange2AtColdWindowOutFaceToSteelFoil  + fLengthFlange2AtColdWindowOut / 2);
  fFlange2AtColdWindowOut = new LKrFlange("Flange2AtColdWindowOut", fMaterial, fMotherVolume, 
      fMinRadiusFlange2AtColdWindowOut, fHightFlange2AtColdWindowOut, fLengthFlange2AtColdWindowOut , Ta ); 


  //
  // flanges at the back cold window
  // 5 pieces around the beam pipe
  //

  //part 1


  Ta = G4ThreeVector(0.,0.,fLongitudinalLengthInnerCryo/2 + InnerCryoZPosition + fThicknessSliceBackColdWindow 
      -  fLengthBackFlange2AtColdWindowBeam - fLengthBackFlange1AtColdWindowBeam / 2 - fThicknesConvexColdBackWindow);

  fBackFlange1AtColdWindowBeam = new LKrFlange("BackFlange1AtColdWindowBeam", fMaterial, fMotherVolume,
      fMinRadiusBackFlange1AtColdWindowBeam , fHightBackFlange1AtColdWindowBeam,fLengthBackFlange1AtColdWindowBeam , Ta ); 

  //
  // back  global  
  //  contains all the other pieces of the flange apart from bellow tooth flange in the back and piece 1
  //

  G4double LengthGlobalFlangeColdBackWindow = fLengthBackFlange2AtColdWindowBeam + fLengthBackFlange3AtColdWindowBeam 
    + fZDistanceBetweeBackEdges3and4 + fLengthBackFlange5AtColdWindowBeam + fLengthHeadsBoltsFrontCold;

  Ta = G4ThreeVector(0., 0., Ta.z() + fLengthBackFlange1AtColdWindowBeam / 2 + LengthGlobalFlangeColdBackWindow /2 );

  fGlobalFlangeBackColdWindow = new LKrFlange("GlobalFlangeBackColdWindow", G4Material::GetMaterial("G4_Galactic"), 
      fMotherVolume, fOuterBeamPipeRadius,fHightBackFlange3AtColdWindowBeam + fHightBackFlange4AtColdWindowBeam, 
      LengthGlobalFlangeColdBackWindow, Ta );

  //part 2

  Ta = G4ThreeVector(0.,0., -  LengthGlobalFlangeColdBackWindow/2 + fLengthBackFlange2AtColdWindowBeam/2);

  fBackFlange2AtColdWindowBeam = new LKrFlange("BackFlange2AtColdWindowBeam",fMaterial, 
      fGlobalFlangeBackColdWindow->GetLogicalVolume(), fMinRadiusBackFlange2AtColdWindowBeam , 
      fHightBackFlange2AtColdWindowBeam,fLengthBackFlange2AtColdWindowBeam , Ta ); 

  //part 3

  Ta = G4ThreeVector(0.,0.,-  LengthGlobalFlangeColdBackWindow/2 + fLengthBackFlange2AtColdWindowBeam + fLengthBackFlange3AtColdWindowBeam /2); 

  fBackFlange3AtColdWindowBeam = new LKrFlange("BackFlange3AtColdWindowBeam", fMaterial, 
      fGlobalFlangeBackColdWindow->GetLogicalVolume(), fMinRadiusBackFlange3AtColdWindowBeam , 
      fHightBackFlange3AtColdWindowBeam,fLengthBackFlange3AtColdWindowBeam , Ta ); 

  //part 4

  Ta = G4ThreeVector(0.,0.,Ta.z() + fLengthBackFlange3AtColdWindowBeam /2 - fLengthBackFlange4AtColdWindowBeam /2 + fZDistanceBetweeBackEdges3and4); 

  fBackFlange4AtColdWindowBeam = new LKrFlange("BackFlange4AtColdWindowBeam", fMaterial, 
      fGlobalFlangeBackColdWindow->GetLogicalVolume(), fMinRadiusBackFlange4AtColdWindowBeam , 
      fHightBackFlange4AtColdWindowBeam,fLengthBackFlange4AtColdWindowBeam , Ta ); 

  //part 5

  Ta = G4ThreeVector(0.,0.,Ta.z()+ fLengthBackFlange4AtColdWindowBeam/2 + fLengthBackFlange5AtColdWindowBeam/2  ); 
  fBackFlange5AtColdWindowBeam = new LKrFlange("BackFlange5AtColdWindowBeam",fMaterial, 
      fGlobalFlangeBackColdWindow->GetLogicalVolume(),fMinRadiusBackFlange5AtColdWindowBeam , 
      fHightBackFlange5AtColdWindowBeam,fLengthBackFlange5AtColdWindowBeam , Ta ); 


  // bolts in part one of the flange at the front window around the beam pipe

  ROfHolesBolts =  fRadiusHolesBoltsFrontCold/2 + fMinRadiusBackFlange5AtColdWindowBeam +  fDistanceBoltToBeamPipe;

  // 12 equidistant bolts ==> at 30 deg interval

  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    G4double ThetaOfHolesBolts = fAngleCentralAxisBolt + iHoleBolt * (30 * deg);

    fHolesBoltsBackColdInFlange5 = new LKrFlangesBolts("HolesBoltsBackColdInFlange5",G4Material::GetMaterial("G4_Galactic"), 
        fBackFlange5AtColdWindowBeam->GetLogicalVolume(), fRadiusHolesBoltsFrontCold, 
        fLengthHolesBoltsBackColdInFlange5, ROfHolesBolts,  ThetaOfHolesBolts, 
        fLengthBackFlange5AtColdWindowBeam/ 2 - fLengthHolesBoltsBackColdInFlange5/ 2);
  }

  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    G4double ThetaOfHolesBolts = fAngleCentralAxisBolt + iHoleBolt * (30 * deg);

    fHolesBoltsBackColdInFlange4 = new LKrFlangesBolts("HolesBoltsBackColdInFlange4",G4Material::GetMaterial("G4_Galactic"), 
        fBackFlange4AtColdWindowBeam->GetLogicalVolume(), fRadiusHolesBoltsFrontCold, 
        fLengthHolesBoltsBackColdInFlange4, ROfHolesBolts,  ThetaOfHolesBolts, 
        fLengthBackFlange4AtColdWindowBeam/ 2 - fLengthHolesBoltsBackColdInFlange4 / 2);
  }


  for (G4int iHeadBolt = 0; iHeadBolt < 12 ; iHeadBolt++){
    G4double ThetaOfHeadsBolts = - fAngleCentralAxisBolt + iHeadBolt * (30 * deg);

    fHeadssBoltsBackCold = new LKrFlangesBolts("HeadsBoltsBackCold",fMaterial, fGlobalFlangeBackColdWindow->GetLogicalVolume(), 
        fRadiusHeadsBoltsFrontCold,fLengthHeadsBoltsFrontCold,ROfHolesBolts, ThetaOfHeadsBolts, 
        Ta.z() + fLengthBackFlange5AtColdWindowBeam / 2 + fLengthHeadsBoltsFrontCold / 2);
  }

  // outer flange at cold back window

  Ta = G4ThreeVector(0.,0., fLongitudinalLengthInnerCryo/2 + InnerCryoZPosition - fDistanceFaceBackFlangeOutToColdWindow + fLengthBackFlangeAtColdWindowOut / 2);

  fBackFlangeAtColdWindowOut  = new LKrFlange("BackFlangeAtColdWindowOut", fMaterial, fMotherVolume, fMinRadiusBackFlangeAtColdWindowOut, 
      fHightBackFlangeAtColdWindowOut, fLengthBackFlangeAtColdWindowOut, Ta);
}

void LKrColdWindow::SetProperties()
{
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
  fVisAtt -> SetVisibility(true);
  //logicalColdFrontWindow ->SetVisAttributes(fVisAtt);
}
