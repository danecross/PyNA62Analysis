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
// Created by 
//            
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------------
#ifndef LKrColdWindow_H
#define LKrColdWindow_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class LKrElectrodeCell;
class LKrFlangesBolts;
class LKrFlange;

class G4Tubs;
class G4Sphere;

class LKrColdWindow : public NA62VComponent
{

  public:

    ~ LKrColdWindow();
    LKrColdWindow( G4Material*,G4LogicalVolume*);

    void ReadGeometryParameters();
    void CreateGeometry();
    void SetProperties();

  public:




  private:
    G4Tubs* VirtualTube1;
    G4LogicalVolume* LogicalWindow;
    G4VPhysicalVolume* PhysicalWindow;

    G4Sphere* solidSphereFiller;
    G4Tubs* VirtualTube;
    G4LogicalVolume* LogicalFiller;
    G4VPhysicalVolume* PhysicalFiller;

    G4Tubs* solidClosingFoilFiller;
    G4LogicalVolume* LogicalClosingFoilFiller;
    G4VPhysicalVolume* PhysicalClosingFoilFiller;

    G4Sphere* solidColdBackWindow;
    G4LogicalVolume* logicalColdBackWindow;
    G4VPhysicalVolume* physiColdBackWindow;

    LKrFlange* fGlobalFlangeFrontColdWindow;
    LKrFlange* fFlangeTooth;
    LKrFlange* fBellowFlangeTooth;
    LKrFlange* fBellowFlangeToothAtBack;
    LKrFlange* fFlange4AtColdWindowBeam;
    LKrFlange* fFlange3AtColdWindowBeam; 
    LKrFlange* fFlange2AtColdWindowBeam;
    LKrFlange* fFlange1AtColdWindowBeam;
    LKrFlange* fFlange6AtColdWindowBeam;
    LKrFlange* fFlange1AtColdWindowOut;
    LKrFlange* fFlange2AtColdWindowOut;
    LKrFlange* fBackFlange1AtColdWindowBeam;
    LKrFlange* fGlobalFlangeBackColdWindow;
    LKrFlange* fBackFlange2AtColdWindowBeam;
    LKrFlange* fBackFlange3AtColdWindowBeam;
    LKrFlange* fBackFlange4AtColdWindowBeam;
    LKrFlange* fBackFlange5AtColdWindowBeam; 

    LKrFlangesBolts* fHolesBoltsFrontCold;
    LKrFlangesBolts* fHeadsBoltsFrontCold;
    LKrFlangesBolts* fHolesBoltsBackColdInFlange5;
    LKrFlangesBolts* fHolesBoltsBackColdInFlange4;
    LKrFlangesBolts* fHeadssBoltsBackCold;
    LKrFlange* fBackFlangeAtColdWindowOut;

    G4double fThicknessSliceColdWindow;

    G4double fInnerCryostatMinRadius; 
    G4double fThicknessOfInnerCryo;
    G4double fRadiusFrontColdWindowBeamPipe;

    G4double fOuterCryostatMinRadius ;
    G4double fThicknessOfOuterCryo ;

    G4double  fLongitudinalLengthInnerCryo;
    G4double  fLongitudinalLengthOuterCryo;

    G4double  fInnerDistanceFrontBackWarmWindow ;
    G4double  fDistanceFrontWarmWindowLKr;

    G4double fLongitudinalLengthLKrVolume;
    G4double fLengthOfColdWindowPlate;

    G4double fRadiusFrontColdWindow;
    G4double fRadiusBackColdWindow ;
    G4double fThicknesConvexColdWindow ;
    G4double fThicknesConvexColdBackWindow ;
    G4double fRadiusHoleSpacerPlate;
    G4double fOuterBeamPipeRadius;

    G4double fThicknessSteelFoilColdWindow;
    G4double fRadiusSteelFoilColdWindow; 

    G4double fDistanceColdWindowToMonotube;

    G4double fDistanceMonotubeToothFlange ;
    G4double fHightToothFlange;
    G4double fLengthToothFlange;

    G4double fDistanceMonotubeBellowToothFlange;
    G4double fHightBellowToothFlange;
    G4double fLengthBellowToothFlange;
    G4double fDistanceBellowToothFlangeAtBackToRareWindow; 

    G4double fHightFlange1AtColdWindowBeam;
    G4double fLengthFlange1AtColdWindowBeam;
    G4double fMinRadiusFlange1AtColdWindowBeam;

    G4double fHightFlange2AtColdWindowBeam;
    G4double fLengthFlange2AtColdWindowBeam;

    G4double fHightFlange3AtColdWindowBeam;
    G4double fLengthFlange3AtColdWindowBeam;

    G4double fHightFlange4AtColdWindowBeam;
    G4double fLengthFlange4AtColdWindowBeam ;
    G4double fMinRadiusFlange4AtColdWindowBeam;


    G4double fRadiusHolesBoltsFrontCold ;
    G4double fLengthHolesBoltsFrontCold ;
    G4double fRadiusHeadsBoltsFrontCold ;
    G4double fLengthHeadsBoltsFrontCold ;
    G4double fDistanceBoltToBeamPipe;

    G4double fLengthHolesBoltsBackColdInFlange5 ;
    G4double fLengthHolesBoltsBackColdInFlange4 ;
    G4double fAngleCentralAxisBolt;

    G4double fHightFlange1AtColdWindowOut;
    G4double fLengthFlange1AtColdWindowOut;
    G4double fMinRadiusFlange1AtColdWindowOut;

    G4double fHightFlange2AtColdWindowOut;
    G4double fLengthFlange2AtColdWindowOut;
    G4double fMinRadiusFlange2AtColdWindowOut;
    G4double fDistanceFlange2AtColdWindowOutFaceToSteelFoil;

    G4double fHightBackFlange1AtColdWindowBeam ;

    G4double fLengthBackFlange1AtColdWindowBeam ;
    G4double fMinRadiusBackFlange1AtColdWindowBeam ;

    G4double fHightBackFlange2AtColdWindowBeam ;
    G4double fLengthBackFlange2AtColdWindowBeam ;
    G4double fMinRadiusBackFlange2AtColdWindowBeam ;

    G4double fHightBackFlange3AtColdWindowBeam ;
    G4double fLengthBackFlange3AtColdWindowBeam ;
    G4double fMinRadiusBackFlange3AtColdWindowBeam;

    G4double fHightBackFlange4AtColdWindowBeam;
    G4double fLengthBackFlange4AtColdWindowBeam ;
    G4double fMinRadiusBackFlange4AtColdWindowBeam ;

    G4double fZDistanceBetweeBackEdges3and4 ;

    G4double fHightBackFlange5AtColdWindowBeam ;
    G4double fLengthBackFlange5AtColdWindowBeam ;
    G4double fMinRadiusBackFlange5AtColdWindowBeam ;

    G4double fHightBackFlangeAtColdWindowOut;
    G4double fLengthBackFlangeAtColdWindowOut;
    G4double fMinRadiusBackFlangeAtColdWindowOut;
    G4double fDistanceFaceBackFlangeOutToColdWindow;

    G4double fThicknessSliceBackColdWindow;

    G4double fBEATCHPositionFrontWarmWindowEdge;
    G4double fBEATCHPositionFrontEdgeOuterVessel;
    G4double fBEATCHPositionBackEdgeOuterVessel;
    G4double fBEATCHPositionBackWarmWindowEdge ;

};

#endif
