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
//Changes to MUV2 by ykohl in March 2010
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
// --------------------------------------------------------------------
#ifndef MUV2Scintillator_H
#define MUV2Scintillator_H 1

#include "NA62VComponent.hh"
#include "globals.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;
class MUV2ScintCut;

class MUV2Scintillator : public NA62VComponent
{

public:
  
  ~MUV2Scintillator();
  MUV2Scintillator(G4RotationMatrix* fTransform,G4Material*, G4LogicalVolume*, G4ThreeVector, G4ThreeVector, G4bool, G4int,G4int);

  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

private:

  G4int fiCopy;

  G4double fInnerRadius;

  G4ThreeVector fScintillatorSize;
  G4ThreeVector fScintillatorPosition;
  G4bool fboolOpp;
  
  G4double fScintillatorCutWidth;
  G4double fScintillatorCutWidth2;
  G4double fmOScintillatorCutHeight;
  G4double fmOScintillatorCutHeight2;


  G4int         fLogical;
  G4Box       * fCutBox;
  G4Box       * fCutBox2;
  G4VSolid    * fNewBox;
  G4VSolid    * fNewBox2;
  G4ThreeVector fCutBoxPos;
  G4ThreeVector fCutBoxPos2;

  G4double  fSkinAluminumWidth;
  G4double  fSkinTapeWidth;
  G4double	fAirGapWidth;
  
  G4RotationMatrix* fTransform;

};

#endif
