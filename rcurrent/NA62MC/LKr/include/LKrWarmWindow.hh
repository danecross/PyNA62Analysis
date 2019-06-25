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
#ifndef LKrWarmWindow_H
#define LKrWarmWindow_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"
#include "G4Cons.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class LKrFlange;
class LKrFlangesBolts;
class LKrElectrodeCell;

class LKrWarmWindowFlangesBeam;
class G4Tubs;
class G4Sphere;

class LKrWarmWindow : public NA62VComponent
{

    public:

        ~ LKrWarmWindow();
        LKrWarmWindow( G4Material*,G4LogicalVolume*);

        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    public:


    private:

        G4Sphere* solidWarmWindow;
        G4LogicalVolume* logicalWarmWindow;
        G4VPhysicalVolume* phisyWarmBackWindow;
        G4VPhysicalVolume* phisyWarmFrontWindow;

        G4Cons* solidCone;
        G4LogicalVolume* logicalCone;
        G4VPhysicalVolume* phisyCone;
 
        G4Cons* solidBackCone; 
        G4LogicalVolume* logicalBackCone;
        G4VPhysicalVolume* phisyBackCone;

        LKrWarmWindowFlangesBeam* fGlobalFlangeFrontWarmWindow;
        LKrWarmWindowFlangesBeam* fGlobalFlangeBackWarmWindow;

        LKrFlange* fFlangeAtWarmWindow1Out;
        LKrFlange* fFlangeAtWarmWindow2Out;

        LKrFlange* fFlangeAtWarmWindow1OutBack;
        LKrFlange* fFlangeAtWarmWindow2OutBack;

        G4double fOuterCryostatMinRadius ;
        G4double fThicknessOfOuterCryo ;

        G4double  fLongitudinalLengthOuterCryo;

        G4double fLongitudinalLengthLKrVolume;

        G4double fRadiusHoleSpacerPlate;

        G4double fRadiusWarmWindow;
        G4double fThicknesConvexWarmWindow;

  
        G4double fHightFlangeAtWarmWindow1Beam ;
      
  
        G4double fHightFlangeAtWarmWindow1Out ;
        G4double fLengthFlangeAtWarmWindow1Out ;
        G4double fMinRadiusFlangeAtWarmWindow1Out ;
    
       G4double fHightFlangeAtWarmWindow2Out ;
       G4double fLengthFlangeAtWarmWindow2Out ;
       G4double fMinRadiusFlangeAtWarmWindow2Out;

       G4double fOuterBeamPipeRadius;
       

        G4double fBEATCHPositionFrontWarmWindowEdge;
        G4double fBEATCHPositionFrontEdgeOuterVessel;
        G4double fBEATCHPositionOuterWarmFlange1;

        G4double fBEATCHPositionBackEdgeOuterVessel;
        G4double fBEATCHPositionBackWarmWindowEdge;

        G4double fVerticalRadiusWarmWindow;

    
        
};

#endif
