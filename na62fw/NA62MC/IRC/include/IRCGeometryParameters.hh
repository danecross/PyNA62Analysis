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
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//
// 2010-11-10 Spasimir Balev
//            -- Change geometry according to TDR
//
// --------------------------------------------------------------------
#ifndef IRCGeometryParameters_H
#define IRCGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"

#include "NA62VGeometryParameters.hh"

#define NUMBER_OF_BEAM_PIPE_PIECES 8

class IRCGeometryParameters : public NA62VGeometryParameters
{

public:

  ~IRCGeometryParameters();
  static IRCGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static IRCGeometryParameters* fInstance;

protected:

  IRCGeometryParameters();

public:

  G4double             GetWorldZLength()                                  { return fWorldZLength;                 };
  void                 SetWorldZLength(G4double value)                    { fWorldZLength = value;                };
  G4double             GetWorldXLength()                                  { return fWorldXLength;                 };
  void                 SetWorldXLength(G4double value)                    { fWorldXLength = value;                };
  G4double             GetWorldYLength()                                  { return fWorldYLength;                 };
  void                 SetWorldYLength(G4double value)                    { fWorldYLength = value;                };

  G4double             GetIRCRespRegionZStart()  {return fIRCRespRegionZStart;};
  G4double             GetIRCRespRegionZEnd()    {return fIRCRespRegionZEnd;};
  G4double             GetIRCRespRegionZCenter() {return fIRCRespRegionZCenter;};
  G4double             GetIRCRespRegionXLength() {return fIRCRespRegionXLength;};
  G4double             GetIRCRespRegionYLength() {return fIRCRespRegionYLength;};
  G4double             GetIRCRespRegionZLength() {return fIRCRespRegionZLength;};

  G4double             GetIRCDetectorZFrontPosition()                          { return fIRCDetectorZFrontPosition;         };
  void                 SetIRCDetectorZFrontPosition(G4double value)            { fIRCDetectorZFrontPosition = value;        };

  G4double GetAbsorberLayerZLength()  {return fAbsorberLayerZLength;};
  G4double GetScintillatorLayerZLength()  {return fScintillatorLayerZLength;};
  G4double GetScintillatorPaintZLength()  {return fScintillatorPaintZLength;};
  G4double GetAluminumLayerZLength()  {return fAluminumLayerZLength;};
  G4double GetAluminumCloseZLength()  {return fAluminumCloseZLength;};
  G4double GetLayerSpacing()  {return fLayerSpacing;};
  
  G4int    GetIRCStation1NLayers()  {return fIRCStation1NLayers;};
  G4double GetIRCStation1InnerRadius()  {return fIRCStation1InnerRadius;};
  G4double GetIRCStation1OuterRadius()  {return fIRCStation1OuterRadius;};
  G4int    GetIRCStation2NLayers()  {return fIRCStation2NLayers;};
  G4double GetIRCStation2InnerRadius()  {return fIRCStation2InnerRadius;};
  G4double GetIRCStation2OuterRadius()  {return fIRCStation2OuterRadius;};

  G4double GetIRCInnerHoleDisplacementX() {return fIRCInnerHoleDisplacementX;};

  G4double GetIRCDistanceBetweenStations()  {return fIRCDistanceBetweenStations;};
  G4double GetIRCModuleRotation() {return fIRCModuleRotation;};
  G4int    GetNSegments()  {return fNSegments;};
  G4double GetSegmentPhiAngle()  {return fSegmentPhiAngle;};

  G4double GetIRCDetectorTotalLength() {return fIRCDetectorTotalLength;};

  G4double GetBeamPipePieceZStart(const G4int id)               { return fBeamPipePieceZStart[id];        };
  G4double GetBeamPipePieceZEnd(const G4int id)                 { return fBeamPipePieceZEnd[id];          };
  G4double GetBeamPipePieceInnerRadius(const G4int id)          { return fBeamPipePieceInnerRadius[id];   };
  G4double GetBeamPipePieceOuterRadius(const G4int id)          { return fBeamPipePieceOuterRadius[id];   };
  G4double GetBeamPipePieceInnerDisplacementX(const G4int id)   { return fBeamPipePieceInnerDisplacementX[id]; };
  G4double GetBeamPipePieceDisplacementX(const G4int id)        { return fBeamPipePieceDisplacementX[id]; };
  G4String GetBeamPipePieceMaterialName(const G4int id)         { return fBeamPipePeaceMaterialName[id];  };

  G4int GetSDnSegmentsX() {return fSDnSegmentsX;};
  G4int GetSDnSegmentsY() {return fSDnSegmentsY;};
  G4int GetSDnSegmentsZ() {return fSDnSegmentsZ;};

private:

  G4double  fWorldZLength;
  G4double  fWorldXLength;
  G4double  fWorldYLength;

  G4double  fIRCRespRegionZStart;
  G4double  fIRCRespRegionZEnd;
  G4double  fIRCRespRegionZCenter;
  G4double  fIRCRespRegionXLength;
  G4double  fIRCRespRegionYLength;
  G4double  fIRCRespRegionZLength;

  G4double  fIRCDetectorZFrontPosition;

  G4double fAbsorberLayerZLength;
  G4double fScintillatorLayerZLength;
  G4double fScintillatorPaintZLength;
  G4double fAluminumLayerZLength;
  G4double fAluminumCloseZLength;
  G4double fLayerSpacing;
  
  G4double fPMTDiskRadius;
  G4double fDistanceToPMTDisk;
  G4double fPMTDiskThickness;

  G4int    fIRCStation1NLayers;
  G4double fIRCStation1InnerRadius;
  G4double fIRCStation1OuterRadius;
  G4int    fIRCStation2NLayers;
  G4double fIRCStation2InnerRadius;
  G4double fIRCStation2OuterRadius;

  G4double fIRCInnerHoleDisplacementX;

  G4double fIRCDistanceBetweenStations;
  G4double fIRCModuleRotation;

  G4double fIRCDetectorTotalLength;

  G4int    fNSegments;
  G4double fSegmentPhiAngle;

  G4double fInnerBeamPipeThickness;
  G4double fFrontBeamPipeInnerRadius;
  G4double fFrontBeamPipeOuterRadius;

  G4double fFlangeZStart;
  G4double fFlangeZEnd;
  G4double fFlangeLength;
  G4double fFlangeDZ;

  G4double fDSFlangeRadius;
  G4double fDSFlangeThickness;
  G4double fPMTDiskDSFlangeDistance;

  G4double fIRCBeamPipeOffset;

  G4double fIRCBeamPipe1Length;
  G4double fIRCBeamPipe2Length;
  G4double fIRCBeamPipe3Length;

  G4double fIRCBeamPipe1Thickness;
  G4double fIRCBeamPipe2Thickness;
  G4double fIRCBeamPipe3Thickness;

  G4double fIRCBeamPipe1Diameter;
  G4double fIRCBeamPipe2Diameter;
  G4double fIRCBeamPipe3Diameter;

  // Beam pipe pieces
  G4double fBeamPipePieceZStart[NUMBER_OF_BEAM_PIPE_PIECES];
  G4double fBeamPipePieceZEnd[NUMBER_OF_BEAM_PIPE_PIECES];
  G4double fBeamPipePieceInnerRadius[NUMBER_OF_BEAM_PIPE_PIECES];
  G4double fBeamPipePieceOuterRadius[NUMBER_OF_BEAM_PIPE_PIECES];
  G4double fBeamPipePieceInnerDisplacementX[NUMBER_OF_BEAM_PIPE_PIECES];
  G4double fBeamPipePieceDisplacementX[NUMBER_OF_BEAM_PIPE_PIECES];
  G4String fBeamPipePeaceMaterialName[NUMBER_OF_BEAM_PIPE_PIECES];

  G4int fSDnSegmentsX;
  G4int fSDnSegmentsY;
  G4int fSDnSegmentsZ;

};
#endif
