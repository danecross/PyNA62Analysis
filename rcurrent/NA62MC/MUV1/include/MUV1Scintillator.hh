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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-03-16
//            Francesca Bucci (Francesca.Bucci@cern.ch) 
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// --------------------------------------------------------------------
//
//Copied from MUVScintillator by HArish
//Changes to MUV1 by ykohl in March 2010
//
//
// --------------------------------------------------------------------
#ifndef MUV1Scintillator_H
#define MUV1Scintillator_H 1

#include "NA62VComponent.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class MUV1Fiber;
class MUV1ScintCut;

class MUV1Scintillator : public NA62VComponent
{

public:
  
  ~MUV1Scintillator();
  MUV1Scintillator(G4RotationMatrix* fTransform,G4Material*, G4LogicalVolume*, G4ThreeVector, G4ThreeVector, G4ThreeVector, G4bool, G4int,G4int);
 
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  G4VSolid* Make4Holes(G4VSolid* box, G4bool boolOpp, G4ThreeVector position);
  G4VSolid* Make2Holes(G4VSolid* box, G4int Logical);
  G4VSolid* MakeCut(G4VSolid*, G4double, G4double, G4double, G4RotationMatrix*, G4ThreeVector);

private:

  G4LogicalVolume* fLogicalVolume;

  G4int fiCopy;

  G4double fInnerRadius;

  G4ThreeVector fScintillatorMotherSize;
  G4ThreeVector fBareScintillatorSize;
  G4ThreeVector fScintillatorSize;
  G4ThreeVector fScintillatorPosition;
  G4bool fboolOpp;
  
  G4double fScintWidthStandard ;
  G4double fScintWidthMiddle ;
  G4double   fScintLengthStandard;
  G4double   fScintLengthMiddleStd;
  G4double  fScintLengthMiddleOuter;

  MUV1Fiber * fMUV1Fiber;
  G4Material * fMaterialII;
  G4double fFirstFiberXPosition;
  G4double fBoxCutDepth;
  G4ThreeVector fFiberPosition;
  G4ThreeVector fBoxPosition;
  G4double fFiberSeperation;
  G4int fNFiber;
  G4double fFiberRadius;
  
  G4int         fLogical;
  G4Box       * fCutBox;
  G4VSolid    * fNewBox;
  G4ThreeVector fCutBoxPos;
  G4double fmOScintillatorCutHeight;
  
  G4RotationMatrix* fTransform;

  MUV1ScintCut * fMUV1ScintCut;
  G4double fCutDepthShort;
  G4double fCutDepthLong;
  G4double fCutWidth;


  G4double fSkinWidth;
  G4double fAirGapWidth;

  G4double fFiberOuterRadius;
  G4double fGrooveWidth; 
  G4double fGrooveDepth; 
  G4double fGroovePositionInStandardScintillator;
  G4double fFiberHightOut;
 

};

#endif
