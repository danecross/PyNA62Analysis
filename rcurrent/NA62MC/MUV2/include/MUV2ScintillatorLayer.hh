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
//Copy from MUVIronPlate
//Changes to MUV2 by ykohl in March 2010
//
//Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
// --------------------------------------------------------------------
#ifndef MUV2ScintillatorLayer_H
#define MUV2ScintillatorLayer_H 1

#include "NA62VComponent.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV2ScintillatorLayer : public NA62VComponent
{

public:
  
  ~MUV2ScintillatorLayer();
  MUV2ScintillatorLayer(G4Material * Material, 
                                 G4LogicalVolume * MotherVolume,
                                 G4ThreeVector ScintLayerPosition,
                                 G4int iCopy, G4int Offset);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4int  GetiCopy()              { return fiCopy;  };
  void   SetiCopy(G4int value)   { fiCopy = value; };

private:

  G4int fiCopy;
  G4int fOffset;

  G4double fInnerRadius;

  G4ThreeVector fScintLayerPosition;
  G4double * fScintLengthOuter;
  G4double   fScintLengthStandard;
  G4double   fScintLengthMiddle;
  G4double  fScintLengthMiddleOuter;

  G4double fScintillatorThickness;
  G4double fScintWidthStandard ;
  G4double fScintWidthMiddle ;

  G4double  fSkinAluminumWidth;
  G4double  fSkinTapeWidth;
  G4double  fAirGapWidth;

};

#endif
