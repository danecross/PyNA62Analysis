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
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#ifndef NA62VComponent_H
#define NA62VComponent_H 1

#include "globals.hh"

#include "G4LogicalVolume.hh"
#include "G4VPhysicalVolume.hh"
#include "G4Material.hh"
#include "G4VisAttributes.hh"

class NA62VComponent {

public:

  NA62VComponent(G4Material*, G4LogicalVolume*);
  // In the concrete instance you need to implement
  // the mandatory virtual methods and call them in the constructor
  virtual ~NA62VComponent();
  virtual void ReadGeometryParameters() = 0;
  virtual void CreateGeometry() = 0;
  virtual void SetProperties() = 0;

public:

  G4Material *         GetMaterial()                                      { return fMaterial;                     };
  void                 SetMaterial(G4Material * value)                    { fMaterial = value;                    };
  G4LogicalVolume *    GetMotherVolume()                                  { return fMotherVolume;                 };
  void                 SetMotherVolume(G4LogicalVolume * value)           { fMotherVolume = value;                };

  G4VSolid *           GetSolidVolume()                                   { return fSolidVolume;                  };
  void                 SetSolidVolume(G4VSolid * value)                   { fSolidVolume = value;                 };
  G4LogicalVolume *    GetLogicalVolume()                                 { return fLogicalVolume;                };
  void                 SetLogicalVolume(G4LogicalVolume * value)          { fLogicalVolume = value;               };
  G4VPhysicalVolume *  GetPhysicalVolume()                                { return fPhysicalVolume;               };
  void                 SetPhysicalVolume(G4VPhysicalVolume * value)       { fPhysicalVolume = value;              };

  G4VisAttributes *    GetVisAtt()                                        { return fVisAtt;                       };
  void                 SetVisAtt(G4VisAttributes * value)                 { fVisAtt = value;                      };

protected:

  G4Material*        fMaterial;
  G4LogicalVolume*   fMotherVolume;
  G4VSolid*          fSolidVolume;
  G4LogicalVolume*   fLogicalVolume;
  G4VPhysicalVolume* fPhysicalVolume;
  G4VisAttributes*   fVisAtt;
};

#endif
