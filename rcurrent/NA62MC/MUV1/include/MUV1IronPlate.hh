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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-11
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
//Copied from MUVIronPlate
//Changes to MUV1 by ykohl in March 2010
//
// --------------------------------------------------------------------
#ifndef MUV1IronPlate_H
#define MUV1IronPlate_H 1

#include "NA62VComponent.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV1IronPlate : public NA62VComponent
{

public:
  
  ~MUV1IronPlate();
  MUV1IronPlate(G4Material*, G4LogicalVolume*, G4ThreeVector, G4ThreeVector, G4int);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4int  GetiCopy()              { return fiCopy;  };
  void   SetiCopy(G4int value)   { fiCopy = value; };

private:

  G4int fiCopy;

  G4double fInnerHoleRadius;
  G4double fConnectionHoleRadius;
  G4double fBoltPositionX;
  G4double fBoltPositionY;

  G4ThreeVector fIronPlateSize;
  G4ThreeVector fIronPlatePosition;


};

#endif
