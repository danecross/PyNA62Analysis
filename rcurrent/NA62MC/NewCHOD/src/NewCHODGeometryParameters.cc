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
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-22
//
// --------------------------------------------------------------------

#include "TVector.h"
#include "NewCHODGeometryParameters.hh"
#include "DetectorParameter.hh"

NewCHODGeometryParameters* NewCHODGeometryParameters::fInstance = 0;

NewCHODGeometryParameters::NewCHODGeometryParameters() :
  NA62VGeometryParameters(G4String("NewCHOD"))  {

  //////////////////////////////////////////////////////////////////////////
  // NewCHOD responsibility region: between RICH RR and LAV RR4 (i.e. LAV12)

  fRespRegionZStart  = 237.588*m;// 237.700*m;
  fRespRegionZEnd    = 238.198*m;
  fRespRegionXLength = 4.00*m; // 2.14*m;
  fRespRegionYLength = 4.00*m; // 2.14*m;
  fRespRegionZCentre = 0.5*(fRespRegionZStart+fRespRegionZEnd);
  fRespRegionZLength = fRespRegionZEnd-fRespRegionZStart;

  fResponsibilityRegion.push_back
    (new ResponsibilityRegion(fRespRegionZStart, fRespRegionZEnd));

  ///////////////////////
  // Beam pipe parameters

  fBeamPipeZLength[0] = fRespRegionZLength;
  fBeamPipeZPosition[0] =
    0.5*(fRespRegionZEnd + fRespRegionZStart) - fRespRegionZCentre;
  fBeamPipeInnerRadius[0]    = 84*mm;
  fBeamPipeOuterRadius[0]    = 85*mm;
  fBeamPipeFinZLength[0]     =  5*mm;
  fBeamPipeFinOuterRadius[0] = 89*mm;
  fBeamPipeFinSpacing[0]     = 40*mm;
  fBeamPipeZLengthWFins[0]   = fBeamPipeZLength[0];
  fBeamPipeInputDisplacementWRTBeam[0]  = 0.0;
  fBeamPipeOutputDisplacementWRTBeam[0] = 0.0;

  /////////////////////////////////////////////////////////////////////////////
  // Rows of counters attached to both sides of an octagonal fiber glass plane.
  // Z position according to the BEATCH file dated 08/03/2016.

  fFiberglassThickness    =  3.0*mm;
  fScintThickness         = 30.0*mm;
  fAlWindowThickness      =  0.6*mm;  // Al windows on both sides of scintillators
  fHoneycombThickness     = 30.0*mm;  // The whole honeycomb layer, it is mostly empty
  fHoneycombSkinThickness =  0.15*mm; // Area density = 300 g/m^2: two skins
  fHoneycombAlThickness   =  0.62*mm; // Equivalent thickness of 30mm of Al at 56 kg/m^3

  fInnerRadius           =  140*mm;
  fOuterRadius           = 1070*mm;
  fHoneycombInnerRadius  = 1120*mm;
  fHoneycombOuterRadius  = 1440*mm; // apothem of a regular octagon
  fFiberglassOuterRadius = 1500*mm; // apothem of a regular octagon

  // Positions of the elements relative to the centre of the RR
  fZPosition          = 238131.5*mm - fRespRegionZCentre; // detector centre = G10 plate centre
  fScintZPosition1    = fZPosition -0.5*(fFiberglassThickness+fScintThickness);
  fScintZPosition2    = fZPosition +0.5*(fFiberglassThickness+fScintThickness);
  fAlWindowZPosition1 = fZPosition -0.5*fFiberglassThickness-fScintThickness-0.5*fAlWindowThickness;
  fAlWindowZPosition2 = fZPosition +0.5*fFiberglassThickness+fScintThickness+0.5*fAlWindowThickness;
  fHoneycombZPosition = fZPosition +0.5*(fFiberglassThickness+fHoneycombThickness);

  fScintSize = G4ThreeVector(134*mm, 108*mm, fScintThickness); // elementary brick size

  ///////////////////////////////////////////////////////////////
  // Positions of the scintillators in a single NewCHOD quadrant.
  // A generic design made of small 134x108 mm^2 counters.
  // Missing counters have fake positions (0,0,0).

  fNCounters = 100;
  for (int i=0; i<fNCounters; i++) {
    fScintPosition[i] = G4ThreeVector (0.0, 0.0, 0.0);
  }

  G4int NCounters[10] = {8, 8, 8, 8, 8, 7, 7, 6, 5, 4}; // counters per row
  G4double Yrow[10], Xcounter[8];
  for (G4int i=0; i<10; i++) Yrow[i]     = 107*mm * (i+0.5);
  for (G4int i=0; i<8;  i++) Xcounter[i] = fScintSize.x() * (i+0.5);

  for (G4int iRow=0; iRow<10; iRow++) {
    G4double Zposition = (iRow%2) ? fScintZPosition2 : fScintZPosition1;
    for (G4int iCounter=0; iCounter<NCounters[iRow]; iCounter++) {
      fScintPosition [10*iRow+iCounter] =
	G4ThreeVector(Xcounter[iCounter], Yrow[iRow], Zposition);
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////
  // Scintillator map in a single quadrant: the elementary counters (0-99)
  // introduced for simplicity of the simulation are mapped to tile geometric IDs (1-38).
  // Depending on the quadrant, 100*N is added to both the counter ID and the geometric ID.

  G4double Map[100] = {01,  1,  2,  3,  4,  4,  5,  5,  0,  0,
		       06,  7,  8,  9, 10, 10, 11, 11,  0,  0,
		       12, 13, 14, 14, 15, 15, 16, 16,  0,  0,
		       17, 18, 19, 19, 20, 20, 21, 21,  0,  0,
		       22, 22, 23, 23, 24, 24, 25, 25,  0,  0,
		       26, 26, 27, 27, 28, 28, 29,  0,  0,  0,
		       30, 30, 31, 31, 32, 32, 29,  0,  0,  0,
		       33, 33, 34, 34, 35, 35,  0,  0,  0,  0,
		       36, 36, 37, 37, 35,  0,  0,  0,  0,  0,
		       38, 38, 37, 37,  0,  0,  0,  0,  0,  0};

  for (G4int i=0; i<fNCounters; i++) fScintMap[i] = Map[i];
}

NewCHODGeometryParameters* NewCHODGeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new NewCHODGeometryParameters();
  return fInstance;
}

TObjArray NewCHODGeometryParameters::GetHashTable() {
  return 0;
}

void NewCHODGeometryParameters::Print() {}
