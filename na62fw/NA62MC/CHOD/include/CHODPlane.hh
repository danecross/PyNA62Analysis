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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
#ifndef CHODPlane_H
#define CHODPlane_H 1

#include "NA62VComponent.hh"
#include "CHODGeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class CHODQuadrant;

class CHODPlane : public NA62VComponent {

public:

  CHODPlane(G4Material*, G4LogicalVolume*, G4Transform3D, G4int);
  ~CHODPlane() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

  G4double GetXLength()               { return fXLength;  }
  void     SetXLength(G4double value) { fXLength = value; }
  G4double GetYLength()               { return fYLength;  }
  void     SetYLength(G4double value) { fYLength = value; }
  G4double GetZLength()               { return fZLength;  }
  void     SetZLength(G4double value) { fZLength = value; }

private:

  G4int fPlane;
  G4Transform3D fPlaneTransform;

  G4int fNQuadrants;
  G4ThreeVector fQuadrantPosition;

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;
};

#endif
