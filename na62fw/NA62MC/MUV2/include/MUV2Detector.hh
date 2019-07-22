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
// Modified (MUV -> MUV2) Rainer Wanke              2010-11-26
//
// Modified Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-17
// --------------------------------------------------------------------
#ifndef MUV2Detector_H
#define MUV2Detector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"

class G4LogicalVolume;
class G4Material;
//class MUV2;
class MUV2IronPlate;
class MUV2Scintillator;
class MUV2ScintillatorSpacer;
class MUV2IronTube;
class MUV2BeamPipe;
class MUV2DetectorMessenger;

class MUV2Detector : public NA62VComponent, public NA62VNamed
{

public:
  
  MUV2Detector( G4Material*, G4LogicalVolume* );
  ~MUV2Detector();

  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4double  GetXLength()                  { return fXLength;    };
  void      SetXLength(G4double value)    { fXLength = value;   };
  G4double  GetYLength()                  { return fYLength;    };
  void      SetYLength(G4double value)    { fYLength = value;   };
  G4double  GetZLength()                  { return fZLength;    };
  void      SetZLength(G4double value)    { fZLength = value;   };

  G4double  GetZPosition()                { return fZPosition;  };
  void      SetZPosition(G4double value)  { fZPosition = value; };

private:

  MUV2DetectorMessenger* fMUV2Messenger;

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;

  G4double fMUV2ResponsibilityRegionZBeginning;
  G4double fMUV2ResponsibilityRegionZEnd;
  G4double fMUV2ResponsibilityRegionXLength;
  G4double fMUV2ResponsibilityRegionYLength;

  G4double fZPosition;

  G4double MUV2ResponsibilityRegionZLength;
  G4double MUV2ResponsibilityRegionZCenter;


  G4int fLogical;

  G4double fScintillatorThickness;
  G4double fScintWidthStandard ;
  G4double fScintWidthMiddle ;

  G4double fIronThickness;
  G4double fIronPlatePosZ;
  G4double fIronPlateSizeX;
  G4double fIronPlateSizeY;

  G4double fScintLengthStandard;
  G4double fScintLengthMiddle;
  G4double fScintLengthMiddleOuter;

  G4double fGapThickness;

  G4double fSkinAluminumWidth;
  G4double fSkinTapeWidth;
  G4double fAirGapWidth;


  MUV2IronPlate*    fIronPlate;
  MUV2Scintillator* fScintillator;
  MUV2IronTube*     fIronTube;
  MUV2BeamPipe*     fBeamPipe;

  G4bool fboolOpp;


};

#endif
