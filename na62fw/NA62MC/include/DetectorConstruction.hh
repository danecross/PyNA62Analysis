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
#ifndef MyDetectorConstruction_H
#define MyDetectorConstruction_H 1

#include "G4VUserDetectorConstruction.hh"
#include "globals.hh"

#include "NA62VNamed.hh"
#include <list>

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4VSolid;
class CHANTIDetector;
class CedarDetector;
class CHODDetector;
class GigaTrackerDetector;
class HACDetector;
class IRCDetector;
class LAVDetector;
class LKrDetector;
class MUV0Detector;
class MUV1Detector;
class MUV2Detector;
class MUV3Detector;
class NewCHODDetector;
class RICHDetector;
class SACDetector;
class SpectrometerDetector;

class GeometryParameters;
class DetectorMessenger;

class DetectorConstruction : public G4VUserDetectorConstruction {

public:

  DetectorConstruction();
  ~DetectorConstruction();
  G4VPhysicalVolume* Construct();
  void UpdateGeometry();
  void GenerateGDML();
  NA62VNamed * FindSubDetector(G4String);
  void EnableSubDetector(G4String);
  void DisableSubDetector(G4String);
  
private:

  void DefineMaterials();
  void ReadGeometryParameters();
  
public:

  G4VSolid*          GetsolidWorld()                { return solidWorld;    }
  void               SetsolidWorld(G4VSolid* value) { solidWorld = value;   }
  G4LogicalVolume*   GetlogicWorld()                { return logicWorld;    }
  void               SetlogicWorld(G4LogicalVolume* value) { logicWorld = value; }
  G4VPhysicalVolume* GetphysiWorld()              { return physiWorld;      }
  void               SetphysiWorld(G4VPhysicalVolume* value) { physiWorld = value; }

  G4double GetWorldZLength()               { return fWorldZLength;  }
  void     SetWorldZLength(G4double value) { fWorldZLength = value; }
  G4double GetWorldXLength()               { return fWorldXLength;  }
  void     SetWorldXLength(G4double value) { fWorldXLength = value; }
  G4double GetWorldYLength()               { return fWorldYLength;  }
  void     SetWorldYLength(G4double value) { fWorldYLength = value; }

  GeometryParameters* GetGeoPars() { return fGeoPars; }
  void                SetGeoPars(GeometryParameters * value) { fGeoPars = value; }

  void SetBlueTubeFieldScale(G4double);
  void SetMNP33FieldMode    (G4bool);
  void SetMNP33FieldScale   (G4double);

private:

  DetectorMessenger * fMessenger;
  G4VSolid*          solidWorld;
  G4LogicalVolume*   logicWorld;
  G4VPhysicalVolume* physiWorld;

  G4double fWorldZLength;
  G4double fWorldXLength;
  G4double fWorldYLength;

  GeometryParameters* fGeoPars;

  CHANTIDetector * fCHANTI;
  CedarDetector * fCedar;
  CHODDetector * fCHOD;
  GigaTrackerDetector * fGigaTracker;
  HACDetector * fHAC;
  IRCDetector * fIRC;
  LAVDetector * fLAV;
  LKrDetector * fLKr;
  MUV0Detector * fMUV0;
  MUV1Detector * fMUV1;
  MUV2Detector * fMUV2;
  MUV3Detector * fMUV3;
  NewCHODDetector * fNewCHOD;
  RICHDetector * fRICH;
  SACDetector * fSAC;
  SpectrometerDetector * fSpectrometer;

  typedef std::list<NA62VNamed*>  SubDetectorList;

  SubDetectorList fSubDetectorList;
};

#endif
