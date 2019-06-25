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
#ifndef LKrWarmWindowFlangesBeam_H
#define LKrWarmWindowFlangesBeam_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"
#include "G4EllipticalTube.hh"
class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class LKrFlange;
class LKrFlangesBolts;
class LKrElectrodeCell;

class G4Tubs;
class G4Sphere;

class LKrWarmWindowFlangesBeam : public NA62VComponent
{

    public:

        ~ LKrWarmWindowFlangesBeam();
        LKrWarmWindowFlangesBeam( G4String Name, G4Material*,G4LogicalVolume*, G4double ZPositionWarmWindow, G4int Sign);

        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    public:

  G4double fZPositionWarmWindow;
  G4int fSign;
  G4String fName;

    private:
        LKrFlange* fGlobalWarmWindowFlange;  
        LKrFlange* fWarmWindowFrontSegment;
        LKrFlange* fFlangeAtWarmWindow4Beam;
        LKrFlange* fFlangeAtWarmWindow5Beam;
        LKrFlange* fFlangeAtWarmWindow2Beam;
        LKrFlange* fFlangeAtWarmWindow1Beam;
        LKrFlange* fFlangeAtWarmWindow3Beam;
 
        G4EllipticalTube* solidEllipticalOpeningInWarmFlange;
        G4Tubs* solidTube;

        G4IntersectionSolid* fsolidEllipticalOpeningInWarmFlange[12];
        G4LogicalVolume* logicEllipticalOpeningInWarmFlange[12];
        G4VPhysicalVolume* physyEllipticalOpeningInWarmFlange[12];

        LKrFlangesBolts* fHolesBoltsUpperWarmFlange2;
        LKrFlangesBolts* fHolesBoltsUpperWarmFlange5;
        LKrFlangesBolts* fHolesBoltsUpperHeads;
        LKrFlangesBolts* fHolesBoltsLowerWarm;
        LKrFlangesBolts* fHolesBoltsLowerHeads;

       G4double fThicknesConvexWarmWindow;
        G4double fHightFlangeAtWarmWindow1Beam ;
        G4double fLengthFlangeAtWarmWindow1Beam ;
        G4double fMinRadiusFlangeAtWarmWindow1Beam ;

        G4double fHightFlangeAtWarmWindow2Beam ;
        G4double fLengthFlangeAtWarmWindow2Beam ;
        G4double fMinRadiusFlangeAtWarmWindow2Beam ;

        G4double fHightFlangeAtWarmWindow3Beam ;
        G4double fLengthFlangeAtWarmWindow3Beam ;
        G4double fMinRadiusFlangeAtWarmWindow3Beam ;

        G4double fElipticalCutFlange3LongAxis;
        G4double fElipticalCutFlange3ShortAxis;
    
        G4double fHightFlangeAtWarmWindow4Beam ;
        G4double fLengthFlangeAtWarmWindow4Beam ;
        G4double fMinRadiusFlangeAtWarmWindow4Beam ;
    
    
        G4double fHightFlangeAtWarmWindow5Beam ;
        G4double fLengthFlangeAtWarmWindow5Beam ;
        G4double fMinRadiusFlangeAtWarmWindow5Beam ;
   
      
       G4double fOuterBeamPipeRadius;

       G4double fDistanceWarmWindowFlange4;
       G4double fDistanceWarmWindowFlange3;

       G4double fDistanceWarmWindowFlange2EndFlange3Front;


        G4double fVerticalRadiusWarmWindow;

      G4double fAngleCentralAxisBoltUpperWarm;
       G4double fRadiusHolesBoltsUpperWarm ;
       G4double fLengthHolesBoltsUpperWarmInFlange5 ;

       G4double fDistanceUpperWarmBoltToBeamPipe ;

       G4double fRadiusHeadsBoltsUpperWarm ;
       G4double fLengthHeadsBoltsUpperWarm ;

   
       G4double fAngleCentralAxisBoltLowerWarm ;
       G4double fRadiusHolesBoltsLowerWarm ;
       G4double fLengthHolesBoltsLowerWarm;

   
       G4double fDistanceLowerWarmBoltToFlange3 ;

       G4double fRadiusHeadsBoltsLowerWarm ;
       G4double fLengthHeadsBoltsLowerWarm;
        
};

#endif
