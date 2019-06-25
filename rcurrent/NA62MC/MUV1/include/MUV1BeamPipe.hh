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
//            LKr->MUV1 Mario Vormstein (mario.vormstein@cern.ch)
//
// --------------------------------------------------------------------
#ifndef MUV1BeamPipe_H
#define MUV1BeamPipe_H 1

#include "NA62VComponent.hh"
#include "MUV1GeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class G4Tubs;

class MUV1BeamPipe : public NA62VComponent
{

    public:

        ~ MUV1BeamPipe();
        MUV1BeamPipe( G4Material*,G4LogicalVolume*, G4double, G4Transform3D);

        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();

    public:

        G4double             GetInnerBeamPipeRadius()                                { return fInnerBeamPipeRadius;         };
        void                 SetInnerBeamPipeRadius(G4double value)                  { fInnerBeamPipeRadius = value;        };

        G4double             GetOuterBeamPipeRadius()                                { return fOuterBeamPipeRadius;         };
        void                 SetOuterBeamPipeRadius(G4double value)                  { fOuterBeamPipeRadius = value;        };

        G4double             GetLongitudinalLengthBeamPipe()                         { return fLongitudinalLengthBeamPipe;  };
        void                 SetLongitudinalLengthBeamPipe(G4double value)           { fLongitudinalLengthBeamPipe = value; };


    private:

        G4Transform3D fBeamPipeTransform;

        G4Tubs*            solidBeamPipe;     //pointer to beam pipe
        G4LogicalVolume*   logicBeamPipe;     //pointer to logical beam pipe     
        G4VPhysicalVolume* physiBeamPipe;     // pointer to the physical beam pipe


        G4double fInnerBeamPipeRadius; 
        G4double fOuterBeamPipeRadius;

        G4double fLongitudinalLengthBeamPipe; 


};

#endif
