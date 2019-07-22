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
//
//  03-09-2012 Sergey Podolsky (siarhei.padolski@cern.ch)      
//
// --------------------------------------------------------------------
#ifndef LKrIrregularVSpacerParameterisation_H
#define LKrIrregularVSpacerParameterisation_H 1

#include "LKrGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"
#include "G4VNestedParameterisation.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;


class LKrIrregularVSpacerParameterisation : public G4VNestedParameterisation {
  public: 
    ~ LKrIrregularVSpacerParameterisation();
    explicit LKrIrregularVSpacerParameterisation(G4VPhysicalVolume* physVol);

    void ComputeTransformation (const G4int copyNoX, 
        G4VPhysicalVolume* ) const;

    using G4VNestedParameterisation::ComputeDimensions;
    void ComputeDimensions (G4Trap& trackerLayer, const G4int copyNoX, 
        const G4VPhysicalVolume* ) const;

    using G4VNestedParameterisation::ComputeMaterial;
    virtual G4Material* ComputeMaterial(G4VPhysicalVolume *,
        const G4int iSpacer,
        const G4VTouchable *);
    virtual G4int       GetNumberOfMaterials() const {return 2;};
    virtual G4Material* GetMaterial(G4int idx) const;

    void ReadGeometryParameters();

  public:

  private:

    G4double fPositionOfWallZ0;
    G4double fSpaceToNextPlate;
    G4double fSpaceFromWallToElectrodeAtWallX; 
    G4double fSizeHoleX;
    G4double fSizeHoleY;
    G4double fSizeHoleYHalfCell;
    G4double fIncr;
    G4double fHalfSizeIrregularWallVSegmentY;
    G4double fHalfCellSizeAtFrontWall;
    G4double fHalfIrregularCellSizeAtFrontWallY;
    G4double fHalfIrregularCellSizeAtBackWallY;
    G4double fHalfSizeSpacerWallSegmentZ;                    
    G4double fIrrZigHalfSizeX;
    G4double fIrrZigHalfSizeY;
    G4double fIrrZigHalfSizeZ;
    G4double fDimensions[36][11]; 
    G4double fPosition[36][3]; 

};

#endif
