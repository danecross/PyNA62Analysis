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
// Modified (MUV -> MUV1) Rainer Wanke              2010-11-26
// Modified by Mario Vormstein						2011-07-14
// --------------------------------------------------------------------
#ifndef MUV1Detector_H
#define MUV1Detector_H 1

#include "NA62VComponent.hh"
#include "NA62VNamed.hh"

class G4LogicalVolume;
class G4Material;
//class MUV1;
class MUV1IronPlate;
class MUV1RubberPlate;
class MUV1Bolt;
class MUV1BeamPipe;
class MUV1TransportationTube;
class MUV1DetectorMessenger;

class MUV1Detector : public NA62VComponent, public NA62VNamed
{

public:
  
  MUV1Detector( G4Material*, G4LogicalVolume* );
  ~MUV1Detector();

  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();

public:

  G4String GetFastSimulation() {return fFastSimulation; }
  void SetFastSimulation(const G4String type) { fFastSimulation = type; }

  G4double  GetXLength()                  { return fXLength;    };
  void      SetXLength(G4double value)    { fXLength = value;   };
  G4double  GetYLength()                  { return fYLength;    };
  void      SetYLength(G4double value)    { fYLength = value;   };
  G4double  GetZLength()                  { return fZLength;    };
  void      SetZLength(G4double value)    { fZLength = value;   };

  G4double  GetZPosition()                { return fZPosition;  };
  void      SetZPosition(G4double value)  { fZPosition = value; };

private:

  MUV1DetectorMessenger* fMUV1Messenger;
  G4String fFastSimulation;

  G4double fXLength;
  G4double fYLength;
  G4double fZLength;


  G4double fMUV1ResponsibilityRegionZBeginning;
  G4double fMUV1ResponsibilityRegionZEnd;
  G4double fMUV1ResponsibilityRegionXLength;
  G4double fMUV1ResponsibilityRegionYLength;

  G4double fZPosition; //MUV1 front plate start position upstream

  G4double MUV1ResponsibilityRegionZLength;
  G4double MUV1ResponsibilityRegionZCenter;

  G4int fNIronPlate;
  G4int fNRubberPlate;

  G4double fScintLayerThickness;
  G4double fBareScintillatorThickness;
  G4double fIronThickness;
  G4double fRubberThickness;
  G4double fRubberRealThickness;
  G4double fAirGapWidth;

  G4double fIronPlateSizeX;
  G4double fIronPlateSizeY;
  G4double fIronPlateOuterSizeX;
  G4double fIronPlateOuterSizeY;

  G4double fBoltPositionX;
  G4double fBoltPositionY;


  MUV1IronPlate *fIronPlate;
  MUV1RubberPlate* fRubberPlate;
  MUV1Bolt* fBoltLeftUp;
  MUV1Bolt* fBoltRightUp;
  MUV1Bolt* fBoltRightDown;
  MUV1Bolt* fBoltLeftDown;
  MUV1BeamPipe* fBeamPipe;
  MUV1TransportationTube* fTransportationTube;
};

#endif
