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
//            Evelina Marinova(Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------------
#ifndef LKrFrontBackPlate_H
#define LKrFrontBackPlate_H 1

#include "NA62VComponent.hh"
#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class G4Tubs;
class G4Polyhedra;


class LKrFrontBackPlate : public NA62VComponent
{

    public:

        ~ LKrFrontBackPlate();
        LKrFrontBackPlate( G4Material*,G4LogicalVolume*);

        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    public:



    private:



        G4Polyhedra* solidOct;

        G4LogicalVolume*   logicOct;     //pointer to logical octagon     
        G4VPhysicalVolume* physiOct;     // pointer to the physical octagon

        G4Polyhedra* solidOctBack;
        G4LogicalVolume*   logicOctBack;     //pointer to logical octagon     
        G4VPhysicalVolume* physiOctBack;     // pointer to the physical octagon

        G4double fInnerBeamPipeRadius; 
        G4double fOuterBeamPipeRadius;

        G4double fLongitudinalLengthBeamPipe; 
        G4double fHalfLengthOfFrontPlateOctagon ;
        G4double fHalfLengthOfBackPlateOctagon ;
        G4double fRadiusHoleSpacerPlate ;
        G4double fHalfNElectrodesY ;
        G4double fDistanceToNextElectrodeBackY; 
        G4double fLongitudinalLengthInnerCryo;
        G4double fDistanceFrontPlateBackPlate;
        G4double  fLongitudinalLengthLKrVolume;
        G4double fPassiveLKrInsideOctagon;
        G4double fLengthOfColdWindowPlate;
};

#endif
