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
//Changes to MUV1 by ykohl in March 2010
//
// --------------------------------------------------------------------
#ifndef MUV1ScintillatorLayer_H
#define MUV1ScintillatorLayer_H 1

#include "NA62VComponent.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV1ScintillatorLayer : public NA62VComponent
{



public:
  
  ~MUV1ScintillatorLayer();
  MUV1ScintillatorLayer(G4Material * Material, 
                                 G4LogicalVolume * MotherVolume,
                                 G4ThreeVector ScintLayerPosition,
                                 G4int iCopy, G4int Offset, G4String fFastSimulation);
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4int  GetiCopy()              { return fiCopy;  };
  void   SetiCopy(G4int value)   { fiCopy = value; };
  void 	 SetFastSimulation(G4String value)	{ fFastSimulation=value;};
  G4String GetFastSimulation()	{ return fFastSimulation;};
  

private:

  G4int fiCopy;
  G4int fOffset;
  G4String fFastSimulation;

  G4double fHoleDimeter;
  G4double fHoleInnerRadius;
  G4double fOuterSpacerOuterRadius ;
  G4double fConnectionHoleRadius;
  G4double fBoltPositionX;
  G4double fBoltPositionY;

  G4ThreeVector fScintLayerPosition;
  G4ThreeVector fPMTSize;

  G4double fBareScintillatorThickness;
  G4double fIronThickness;
  G4double fRubberThickness;
  G4double fRubberRealThickness;

  G4double fScintMotherWidth;
  G4double fScintMotherLength;
  G4double fScintMotherThickness;
  G4double fScintLayerThickness;
  G4double fScintillatorThickness;
  G4double fScintWidthStandard ;
  G4double fScintWidthMiddle ;
  G4double * fScintLengthOuter;
  G4double   fScintLengthStandard;
  G4double   fScintLengthMiddleStd;
  G4double  fScintLengthMiddleOuter;
  G4double fGrooveDepth;
  G4double fGroovePositionInStandardScintillator;
  G4double   fFiberHightOut;
  G4double fFiberOuterRadius;
  G4double fAirGapWidth;
  G4double fSkinWidth;
  G4double fPMTOffset;
  

};

#endif
