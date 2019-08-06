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
#include "TVector.h"

#include "IRCGeometryParameters.hh"
#include "DetectorParameter.hh"

IRCGeometryParameters* IRCGeometryParameters::fInstance = 0;

IRCGeometryParameters::IRCGeometryParameters() :
  NA62VGeometryParameters(G4String("IRC")),
  // Define all the geometrical parameters and build the
  // responsibility region accordingly

  // Unused World Parameters for stand-alone configurations
  fWorldZLength(22.*m),
  fWorldXLength(10.*m),
  fWorldYLength(10.*m),

  // Responsibility region
  fIRCRespRegionZStart (239.540*m),
  fIRCRespRegionZEnd   (240.388*m),
  fIRCRespRegionZCenter((fIRCRespRegionZStart + fIRCRespRegionZEnd)/2.),
  fIRCRespRegionXLength(5.*m),
  fIRCRespRegionYLength(5.*m),
  fIRCRespRegionZLength((fIRCRespRegionZEnd   - fIRCRespRegionZStart)),

  //  fIRCDetectorZFrontPosition=239.700*m;
  fIRCDetectorZFrontPosition(239.611475*m),

  fAbsorberLayerZLength    (1.5*mm),
  fScintillatorLayerZLength(1.5*mm),
  fScintillatorPaintZLength(0.1*mm),
  fAluminumLayerZLength    (5*mm),
  fAluminumCloseZLength    (7*mm),
  fLayerSpacing            (fAbsorberLayerZLength+fScintillatorLayerZLength + 2*fScintillatorPaintZLength),

  fPMTDiskRadius    (275.*mm / 2),
  fDistanceToPMTDisk(100*mm),
  fPMTDiskThickness (5.*mm),

  fIRCStation1NLayers    (25),
  fIRCStation1InnerRadius(60*mm),
  fIRCStation1OuterRadius(145*mm),
  fIRCStation2NLayers    (45),
  fIRCStation2InnerRadius(61*mm),
  fIRCStation2OuterRadius(145*mm),

  fIRCInnerHoleDisplacementX(12.*mm),

  fIRCDistanceBetweenStations(40*mm),
  fIRCModuleRotation         (0*mrad),
  fIRCDetectorTotalLength((fIRCStation1NLayers + fIRCStation2NLayers)*fLayerSpacing + 3.*fAluminumLayerZLength + fAluminumCloseZLength + fIRCDistanceBetweenStations),

  fNSegments      (4),
  fSegmentPhiAngle(2. * pi / (G4double) fNSegments),

  // Beam pipe parameters
  fInnerBeamPipeThickness  (1*mm),
  fFrontBeamPipeInnerRadius(84*mm),
  fFrontBeamPipeOuterRadius(85*mm),

  fFlangeLength(10*mm),
  // DZ of second flange after seconf IRC module
  fFlangeDZ(10.*cm),

  fDSFlangeRadius         (225.*mm / 2),
  fDSFlangeThickness      (12.*mm),
  fPMTDiskDSFlangeDistance(171.5),

  // Beam Pipe Pieces and Flanges
  fIRCBeamPipeOffset (96.*mm),
  fIRCBeamPipe1Length(190.*mm),
  fIRCBeamPipe2Length(220.*mm),
  fIRCBeamPipe3Length(260.*mm),

  fIRCBeamPipe1Thickness(0.8 *mm),
  fIRCBeamPipe2Thickness(0.8 *mm),
  fIRCBeamPipe3Thickness(2.0 *mm),

  fIRCBeamPipe1Diameter(120.0*mm),
  fIRCBeamPipe2Diameter(121.7*mm),
  fIRCBeamPipe3Diameter(129.0*mm),

  // Signal Collection
  fSDnSegmentsX(2),
  fSDnSegmentsY(2),
  fSDnSegmentsZ(1)
{
  //The beam tube is three sections:
  // 1st: outer diameter 120.0 mm, thickness = 0.8 mm, length = 190 mm
  // 2nd: outer diameter 121.7 mm, thickness = 0.8 mm, length = 220 mm
  // 3rd: outer diameter 129.0 mm, thickness = 2.0 mm, length = 260 mm
  // Total length = 670 mm


  fBeamPipePieceZStart[0] = fIRCDetectorZFrontPosition - fIRCBeamPipeOffset;
  fBeamPipePieceZEnd[0] =  fBeamPipePieceZStart[0] + fIRCBeamPipe1Length ;

  if(fBeamPipePieceZStart[0] < fIRCRespRegionZStart ) fBeamPipePieceZStart[0] = fIRCRespRegionZStart;
  fBeamPipePieceOuterRadius[0] = fIRCBeamPipe1Diameter/2.;
  fBeamPipePieceInnerRadius[0] = fBeamPipePieceOuterRadius[0] - fIRCBeamPipe1Thickness;
  fBeamPipePieceDisplacementX[0] = fIRCInnerHoleDisplacementX;
  fBeamPipePieceInnerDisplacementX[0] = 0.;
  fBeamPipePeaceMaterialName[0] = "IRC_StainlessSteel";

  // Flange in front of IRC
  // position of first flange
  fFlangeZStart = fBeamPipePieceZStart[0];
  fFlangeZEnd   = fFlangeZStart + fFlangeLength;

  fBeamPipePieceZStart[1] = fBeamPipePieceZEnd[0];
  fBeamPipePieceZEnd[1] =  fBeamPipePieceZStart[1] + fIRCBeamPipe2Length ;
  fBeamPipePieceOuterRadius[1] = fIRCBeamPipe2Diameter/2.;
  fBeamPipePieceInnerRadius[1] = fBeamPipePieceOuterRadius[1] - fIRCBeamPipe2Thickness;
  fBeamPipePieceDisplacementX[1] = fIRCInnerHoleDisplacementX;
  fBeamPipePieceInnerDisplacementX[1] = 0.;
  fBeamPipePeaceMaterialName[1] = "IRC_StainlessSteel";

  fBeamPipePieceZStart[2] = fBeamPipePieceZEnd[1] ;
  fBeamPipePieceZEnd[2] =  fBeamPipePieceZStart[2] + fIRCBeamPipe3Length ;
  fBeamPipePieceOuterRadius[2] = fIRCBeamPipe3Diameter/2.;
  fBeamPipePieceInnerRadius[2] = fBeamPipePieceOuterRadius[2] - fIRCBeamPipe3Thickness;
  fBeamPipePieceDisplacementX[2] =fIRCInnerHoleDisplacementX;
  fBeamPipePieceInnerDisplacementX[2] = 0.;
  fBeamPipePeaceMaterialName[2] = "IRC_StainlessSteel";

  //Downstream Flange, IRC side
  fBeamPipePieceZEnd[3] = fBeamPipePieceZEnd[2];
  fBeamPipePieceZStart[3] = fBeamPipePieceZEnd[3] -fDSFlangeThickness  ;
  fBeamPipePieceOuterRadius[3] = fDSFlangeRadius;
  fBeamPipePieceInnerRadius[3] = fBeamPipePieceOuterRadius[2];
  fBeamPipePieceDisplacementX[3] = 0.;
  fBeamPipePieceInnerDisplacementX[3] = fIRCInnerHoleDisplacementX;
  fBeamPipePeaceMaterialName[3] = "IRC_StainlessSteel";

  //PMT disk
  fBeamPipePieceZStart[4] = fIRCDetectorZFrontPosition + fIRCDetectorTotalLength  + fDistanceToPMTDisk;
  fBeamPipePieceZEnd[4] = fBeamPipePieceZStart[4] + fPMTDiskThickness;
  fBeamPipePieceOuterRadius[4] = fPMTDiskRadius;
  fBeamPipePieceInnerRadius[4] = fBeamPipePieceOuterRadius[2];
  fBeamPipePieceDisplacementX[4] = 0.;
  fBeamPipePieceInnerDisplacementX[4] = fIRCInnerHoleDisplacementX;
  fBeamPipePeaceMaterialName[4] = "G4_Al";


  //Front flange (to the RICH beam tube)
  // For the moment 1 cm Al., with radius equal to the DS flange
  fBeamPipePieceZStart[5] =fFlangeZStart ;
  fBeamPipePieceZEnd[5] = fFlangeZEnd  ;
  fBeamPipePieceOuterRadius[5] = fDSFlangeRadius;
  fBeamPipePieceInnerRadius[5] = fIRCBeamPipe1Diameter/2.;
  fBeamPipePieceDisplacementX[5] = fIRCInnerHoleDisplacementX;
  fBeamPipePieceInnerDisplacementX[5] = 0.;
  fBeamPipePeaceMaterialName[5] = "G4_Al";

  // 5 - Pipe after IRC DownStream Flange
  fBeamPipePieceZStart[6] =   fBeamPipePieceZEnd[2];
  fBeamPipePieceZEnd[6] = fIRCRespRegionZEnd;
  fBeamPipePieceInnerRadius[6] = fFrontBeamPipeInnerRadius;
  fBeamPipePieceOuterRadius[6] = fFrontBeamPipeOuterRadius;
  fBeamPipePieceDisplacementX[6] = 0.;
  fBeamPipePieceInnerDisplacementX[6] = 0.;
  fBeamPipePeaceMaterialName[6] = "IRC_StainlessSteel";

  //Downstream Flange, LKr side
  fBeamPipePieceZEnd[7] = fBeamPipePieceZEnd[2] + fDSFlangeThickness;
  fBeamPipePieceZStart[7] = fBeamPipePieceZEnd[2] ;
  fBeamPipePieceOuterRadius[7] = fDSFlangeRadius;
  fBeamPipePieceInnerRadius[7] = fFrontBeamPipeOuterRadius ;
  fBeamPipePieceDisplacementX[7] = 0.;
  fBeamPipePieceInnerDisplacementX[7] = 0.;
  fBeamPipePeaceMaterialName[7] = "IRC_StainlessSteel";

  fResponsibilityRegion.push_back(new ResponsibilityRegion(fIRCRespRegionZStart,
							   fIRCRespRegionZEnd));
}

IRCGeometryParameters::~IRCGeometryParameters(){}

IRCGeometryParameters* IRCGeometryParameters::GetInstance()
{
  if ( fInstance == 0 ) { fInstance = new IRCGeometryParameters(); }
  return fInstance;
}

TObjArray IRCGeometryParameters::GetHashTable()
{
  TObjArray IRCGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  IRCGeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
					       "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  IRCGeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
					       "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  IRCGeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
					       "World Y Length", ParameterData));
  ParameterData.Clear();

  return IRCGeometryParameters;
}

void IRCGeometryParameters::Print(){
  G4cout << "fWorldZLength= "<< fWorldZLength << G4endl
	 << "fWorldXLength= "<< fWorldXLength << G4endl
	 << "fWorldYLength= "<< fWorldYLength << G4endl;
}
