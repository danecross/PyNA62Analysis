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
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 
//            Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------
#include "TVector.h"

#include "SACGeometryParameters.hh"
#include "DetectorParameter.hh"

SACGeometryParameters* SACGeometryParameters::fInstance = 0;

SACGeometryParameters::SACGeometryParameters() : NA62VGeometryParameters(G4String("SAC")) 
{
  // SAC SIMULATION MODE
  //
  // = 1 --> [DEFAULT] volume of 1 Absorber + 1 Scintillator + all the fibers replicated along Z-axis
  //
  // = 2 --> segmentation of 9.5x9.5 mm for each Absorber and Scintillator layer (+ 1 fiber)
  //         replicated along Z,X and Y axis

  fSACSimulationMode = 1;

  // Responsibility region
  fSACResponsibilityRegionXLength    =   5.000*m;
  fSACResponsibilityRegionYLength    =   5.000*m;
  fSACResponsibilityRegionZBeginning = 255.059*m;
  fSACResponsibilityRegionZEnd       = 264.325*m; // Analogue wire chamber (XWCM) plane, convenient for checkpoit definition
  fSACResponsibilityRegionZCenter    = 0.5*(fSACResponsibilityRegionZBeginning+fSACResponsibilityRegionZEnd);

  // Position and length
  fSACDetectorFrontZPosition = 260.956*m;
  fSACDetectorXLength = 205*mm;
  fSACDetectorYLength = 205*mm;
  // Rotation
  fSACDetectorYRotation = 22*mrad;

  // Absorber  
  fAbsorberLayerXLength = fSACDetectorXLength;
  fAbsorberLayerYLength = fSACDetectorYLength;
  fAbsorberLayerZLength = 1.5*mm;

  // Scintillator
  fScintillatorLayerXLength = fSACDetectorXLength;
  fScintillatorLayerYLength = fSACDetectorYLength;
  fScintillatorLayerZLength = 1.5*mm;

  fNLayers = 70;

  // Aluminium layer
  fAluminiumLayerXLength = fSACDetectorXLength;
  fAluminiumLayerYLength = fSACDetectorYLength;
  fAluminiumLayerZLength = 5*mm;


  // Fibers
  fFiberDiameter = 1.5*mm;
  fFiberSpacing = 9.5*mm; // distance between fibers
  fNFibers = 22; // number of fibers 28 x 28

  // Signal Collection
  fSDnSegmentsX = 2;
  fSDnSegmentsY = 2;
  fSDnSegmentsZ = 1;

  fResponsibilityRegion.push_back(new ResponsibilityRegion(fSACResponsibilityRegionZBeginning,
							   fSACResponsibilityRegionZEnd));

  // Parametrize beampipes and flange volumes
  G4double volumeTol = 10*um;

  // Beam pipe
  fBeamPipeZLength[0] = 0.435*m;
  fBeamPipeZPosition[0] = fSACResponsibilityRegionZBeginning + fBeamPipeZLength[0]*0.5 - fSACResponsibilityRegionZCenter + volumeTol;
  fBeamPipeInnerRadius[0] = 159.0*mm;
  fBeamPipeOuterRadius[0] = 162.0*mm;  
  fBeamPipeFinZLength[0] = 1.0*mm;
  fBeamPipeFinOuterRadius[0] = 163.0*mm;
  fBeamPipeFinSpacing[0] = 5.0*m;
  fBeamPipeInputDisplacementWRTBeam[0]=-40*mm;
  fBeamPipeOutputDisplacementWRTBeam[0]=-40*mm;
  fBeamPipeZLengthWFins[0] = 0;

  //Flange between pipes
  fSACFlangeZLength = 10*mm;//5*mm;
  fSACFlangeZPosition = fBeamPipeZPosition[0] + fBeamPipeZLength[0]*0.5 + fSACFlangeZLength*0.5 + volumeTol; // previously was 255.522 now is 255.4965
  fSACFlangeInnerRadius = fBeamPipeOuterRadius[0];
  fSACFlangeOuterRadius = 299*mm;
  fSACFlangeDisplacement = fBeamPipeOutputDisplacementWRTBeam[0];

  //Second beam pipe should enclose the SACDetector volume
  G4double totalSACDetectorLength = fNLayers*(fAbsorberLayerZLength+fScintillatorLayerZLength) + 2*fAluminiumLayerZLength;
  G4double SACDetectorZEnd = fSACDetectorFrontZPosition + totalSACDetectorLength - fSACResponsibilityRegionZCenter; // Z position relative to the RR center
  G4double BeamPipe1ZLength = SACDetectorZEnd - (fSACFlangeZPosition - 0.5*fSACFlangeZLength);
  if (BeamPipe1ZLength < 5.496*m) BeamPipe1ZLength = 5.496*m; // preserve original measurement if the SACDetector moves upstream

  fBeamPipeZLength[1] = BeamPipe1ZLength;
  fBeamPipeZPosition[1] = fSACFlangeZPosition + 0.5*fSACFlangeZLength + fBeamPipeZLength[1]*0.5 + volumeTol; // was 258.252*m - fSACResponsibilityRegionZCenter;
  fBeamPipeInnerRadius[1] = fSACFlangeOuterRadius;
  fBeamPipeOuterRadius[1] = 304.0*mm;  
  fBeamPipeFinZLength[1] = 1.0*mm;
  fBeamPipeFinOuterRadius[1] = 305.0*mm;
  fBeamPipeFinSpacing[1] = 5.0*m;
  fBeamPipeInputDisplacementWRTBeam[1]=-40*mm;
  fBeamPipeOutputDisplacementWRTBeam[1]=-40*mm;
  fBeamPipeZLengthWFins[1] = 0;   
}

SACGeometryParameters* SACGeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new SACGeometryParameters();
  return fInstance;
}

TObjArray SACGeometryParameters::GetHashTable() {
  return 0;
}

void SACGeometryParameters::Print(){
  G4cout << "fSACResponsibilityRegionBeginning= "<< fSACResponsibilityRegionZBeginning << G4endl
	 << "fSACResponsibilityRegionEnd= "<< fSACResponsibilityRegionZEnd << G4endl
	 << "fSACDetectorFrontZPosition= "<< fSACDetectorFrontZPosition << G4endl
	 << "fSACDetectorXLength= "<< fSACDetectorXLength << G4endl
	 << "fSACDetectorYLength= "<< fSACDetectorYLength << G4endl;
}
