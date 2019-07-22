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
// --------------------------------------------------------------
// History:
// Updated by Simone Schuchmann with new collimator TCX installed
// in 2018 (2019-05-30)
//
// Created by Simone Bifani (Simone.Bifani@cern.ch) 2008-04-22
// --------------------------------------------------------------
//
#ifndef GigaTrackerCollimator_H
#define GigaTrackerCollimator_H 1

#include "globals.hh"
#include "NA62VComponent.hh"
#include "GigaTrackerGeometryParameters.hh"

#include "G4ThreeVector.hh"
#include <vector>

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class GigaTrackerCollimator : public NA62VComponent
{

public:
  
  ~GigaTrackerCollimator();
  GigaTrackerCollimator(G4Material*, G4LogicalVolume*, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void CreateGeometry2018();
  void CreateGeometry20162017();
  void SetProperties();

public:

  G4ThreeVector        GetPosition()                                      { return fPosition;                     };
  void                 SetPosition(G4ThreeVector value)                   { fPosition = value;                    };
 
  G4int                GetiCopy()                                         { return fiCopy;                        };
  void                 SetiCopy(G4int value)                              { fiCopy = value;                       };
 
  G4double             GetOuterXLength()                                  { return fOuterXLength;                 };
  void                 SetOuterXLength(G4double value)                    { fOuterXLength = value;                };
  G4double             GetOuterYLength()                                  { return fOuterYLength;                 };
  void                 SetOuterYLength(G4double value)                    { fOuterYLength = value;                };
  G4double             GetInnerXLength()                                  { return fInnerXLength;                 };
  void                 SetInnerXLength(G4double value)                    { fInnerXLength = value;                };
  G4double             GetInnerYLength()                                  { return fInnerYLength;                 };
  void                 SetInnerYLength(G4double value)                    { fInnerYLength = value;                };
  G4double             GetZLength()                                       { return fZLength;                      };
  void                 SetZLength(G4double value)                         { fZLength = value;                     };

  G4VSolid *           GetHoleSolidVolume()                               { return fHoleSolidVolume;              };
  void                 SetHoleSolidVolume(G4VSolid * value)               { fHoleSolidVolume = value;             };
  G4LogicalVolume *    GetHoleLogicalVolume()                             { return fHoleLogicalVolume;            };
  void                 SetHoleLogicalVolume(G4LogicalVolume * value)      { fHoleLogicalVolume = value;           };
  G4VPhysicalVolume *  GetHolePhysicalVolume()                            { return fHolePhysicalVolume;           };
  void                 SetHolePhysicalVolume(G4VPhysicalVolume * value)   { fHolePhysicalVolume = value;          };

private:

  G4ThreeVector fPosition;

  G4int fiCopy;

  G4double fOuterXLength;
  G4double fOuterYLength;
  G4double fInnerXLength;
  G4double fInnerYLength;
  G4double fZLength;

  G4String fCollimatorDesign;

  // 2018 design
  G4String fGDML;  
  G4VSolid *fInnerPart;  
  G4LogicalVolume *fLogicalVolumeCRing;
  G4VPhysicalVolume *fPhysicalVolumeCRing;

  // 2016-17 design
  G4VSolid* fHoleSolidVolume;
  G4LogicalVolume* fHoleLogicalVolume;
  G4VPhysicalVolume* fHolePhysicalVolume;

};

#endif
