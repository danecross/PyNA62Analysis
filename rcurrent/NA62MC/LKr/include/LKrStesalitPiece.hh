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
#ifndef LKrStesalitPiece_H
#define LKrStesalitPiece_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class LKrElectrodeCell;
class G4Tubs;
class G4Sphere;
class G4String;

class LKrStesalitPiece : public NA62VComponent
{

    public:

        ~ LKrStesalitPiece();
        LKrStesalitPiece( G4Material *, G4LogicalVolume *, G4double IndexLine, G4double nSpacerPlane, G4double NCellsInLine,G4int Sign );

        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    public:

  G4double fIndexLine ;
  G4double fnSpacerPlane ;
  G4double fNCellsInLine ;
  G4int fSign;
 
   
    private:


    
    
  
  
  G4double fXfrontReferenceCell;                           
  
  G4double fYfrontReferenceCell ;                           
   
  G4double fHalfSizeCentralGap ;  
  G4double fDistanceFrontPlateBackPlate;
  G4double fLongitudinalLengthLKrVolume;
  G4double fLengthOfColdWindowPlate;
  
  G4double fRadiusHoleSpacerPlate;
  G4double fHalfCellSizeAtFrontWall;
  G4double fIncr;
  G4double fHalfSizeSpacerWallSegmentZ;
  G4double fHalfIrregularCellSizeAtFrontWallY;
  G4double fPassiveLKrInsideOctagon; 
  G4double fHalfLengthOfFrontPlateOctagon;

  G4double fSpaceToNextPlate;
  };

#endif
