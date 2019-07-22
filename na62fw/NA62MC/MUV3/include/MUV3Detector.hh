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
// Created by   Antonino Sergi (Antonino.Sergi@cern.ch) 
//              Spasimir Balev (Spasimir.Balev@cern.ch)
//
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) March 2014
//
// --------------------------------------------------------------------
#ifndef MUV3Detector_H
#define MUV3Detector_H 1

#include "MUV3IronWall.hh"
#include "MUV3Plate.hh"
#include "MUV3Module.hh"

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"
#include "MUV3GeometryParameters.hh"
#include "globals.hh"
#include "G4ThreeVector.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class MUV3Detector : public NA62VComponent, public NA62VNamed {

public:

  MUV3Detector(G4Material*, G4LogicalVolume*);
  ~MUV3Detector() {}
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

  G4double  GetResponsibilityRegionXLength()             { return fResponsibilityRegionXLength; }
  void      SetResponsibilityRegionXLength(G4double val) { fResponsibilityRegionXLength = val;  }
  G4double  GetResponsibilityRegionYLength()             { return fResponsibilityRegionYLength; }
  void      SetResponsibilityRegionYLength(G4double val) { fResponsibilityRegionYLength = val;  }
  G4double  GetResponsibilityRegionZStart()              { return fResponsibilityRegionZStart;  }
  void      SetResponsibilityRegionZStart(G4double val)  { fResponsibilityRegionZStart = val;   }
  G4double  GetResponsibilityRegionZEnd()                { return fResponsibilityRegionZEnd;    }
  void      SetResponsibilityRegionZEnd(G4double val)    { fResponsibilityRegionZEnd = val;     }

private:

  G4double fResponsibilityRegionXLength;
  G4double fResponsibilityRegionYLength;
  G4double fResponsibilityRegionZStart;
  G4double fResponsibilityRegionZEnd;

  G4double fFiscZStart;      ///< FISC 5 counter front Z position
  G4double fFeWallZStart;    ///< Iron wall fron Z position
  G4double fFrontPlateZStart;
  G4double fScintillatorZStart;
  G4double fBackPlateZStart;
  G4double fFrontPlateThickness;
  G4double fBackPlateThickness;
  G4double fModuleZLength;

  G4double fBeamPipeInnerRadius;
  G4double fBeamPipeOuterRadius;
  G4double fActiveInnerRadius;
  G4double fPassiveInnerRadius;

  G4double fFeWallThickness;
  G4double fGapWidth;

  G4int    fNModulesX;
  G4int    fNModulesY;
  G4double fLargeModuleSize;
  G4double fSmallModuleSize;

  MUV3IronWall *fIronWall;
  MUV3Plate    *fFrontPlate;
  MUV3Plate    *fBackPlate;
};

#endif
