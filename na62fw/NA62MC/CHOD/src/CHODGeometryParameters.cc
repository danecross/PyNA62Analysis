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
// 2015-03-19 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Fixed Responsibility region to adjust for A12 final design
//
// 2014-03-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - NewCHOD simulation added
//
// 2012-01-30 Giuseppe Ruggiero
//   - Responsibility region and Z positions changed according
//     to the 2011-11-02 beatch file.
//
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2009-02-02
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------
#include "TVector.h"

#include "CHODGeometryParameters.hh"
#include "DetectorParameter.hh"

CHODGeometryParameters* CHODGeometryParameters::fInstance = 0;

CHODGeometryParameters::CHODGeometryParameters() :
  NA62VGeometryParameters(G4String("CHOD")),
  ///////////////////////////////////////////////////////////////////////
  // CHOD responsibility region: between LAV RR4 (i.e. LAV12) and IRC RR.
  fRespRegionZStart (238.898*m),
  fRespRegionZEnd   (239.540*m),
  fRespRegionZCentre(0.5*(fRespRegionZStart+fRespRegionZEnd)),
  fRespRegionXLength(4.00*m),
  fRespRegionYLength(4.00*m),
  fRespRegionZLength(fRespRegionZEnd-fRespRegionZStart),
  ////////////////
  // CHOD geometry
  // Z positions and rotations of CHOD planes
  fInnerRadius         ( 12.8*cm),
  fOuterRadius         (121.0*cm),
  fScintThickness      (  2.0*cm),
  fNPlanes             (2),
  fNQuadrants          (4),
  fTransverseSize      (122.0*cm), // half-size
  fDetectorZPositionVer(239.009*m + 0.5*fScintThickness),
  fDetectorZPositionHor(239.389*m + 0.5*fScintThickness),
  fDetectorZRotationVer( 0.*deg),
  fDetectorZRotationHor(90.*deg),
  fNCounters           (16)
{
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
  fBeamPipeInputDisplacementWRTBeam[0]  = 0;
  fBeamPipeOutputDisplacementWRTBeam[0] = 0;

  // Half-dimensions of the CHOD counters (first quadrant of the vertical plane)
  fScintSize[0]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[1]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[2]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[3]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[4]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[5]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[6]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[7]  = G4ThreeVector( 3.25*cm, 60.50*cm, 0.5*fScintThickness);
  fScintSize[8]  = G4ThreeVector( 3.25*cm, 59.55*cm, 0.5*fScintThickness);
  fScintSize[9]  = G4ThreeVector( 3.25*cm, 56.30*cm, 0.5*fScintThickness);
  fScintSize[10] = G4ThreeVector( 3.25*cm, 53.05*cm, 0.5*fScintThickness);
  fScintSize[11] = G4ThreeVector( 4.95*cm, 49.80*cm, 0.5*fScintThickness);
  fScintSize[12] = G4ThreeVector( 4.95*cm, 44.85*cm, 0.5*fScintThickness);
  fScintSize[13] = G4ThreeVector( 4.95*cm, 39.90*cm, 0.5*fScintThickness);
  fScintSize[14] = G4ThreeVector( 4.95*cm, 34.95*cm, 0.5*fScintThickness);
  fScintSize[15] = G4ThreeVector( 4.95*cm, 30.00*cm, 0.5*fScintThickness);

  // Positions of the CHOD counters (first quadrant of vertical plane) w.r.t. quadrant centre
  fScintPosition[0]  = G4ThreeVector( 57.735*cm, -0.485*cm, 0.0*cm);
  fScintPosition[1]  = G4ThreeVector( 51.205*cm, -0.485*cm, 0.0*cm);
  fScintPosition[2]  = G4ThreeVector( 44.675*cm, -0.485*cm, 0.0*cm);
  fScintPosition[3]  = G4ThreeVector( 38.145*cm, -0.485*cm, 0.0*cm);
  fScintPosition[4]  = G4ThreeVector( 31.615*cm, -0.485*cm, 0.0*cm);
  fScintPosition[5]  = G4ThreeVector( 25.085*cm, -0.485*cm, 0.0*cm);
  fScintPosition[6]  = G4ThreeVector( 18.555*cm, -0.485*cm, 0.0*cm);
  fScintPosition[7]  = G4ThreeVector( 12.025*cm, -0.485*cm, 0.0*cm);
  fScintPosition[8]  = G4ThreeVector(  5.495*cm, -1.435*cm, 0.0*cm);
  fScintPosition[9]  = G4ThreeVector( -1.035*cm, -4.685*cm, 0.0*cm);
  fScintPosition[10] = G4ThreeVector( -7.565*cm, -7.935*cm, 0.0*cm);
  fScintPosition[11] = G4ThreeVector(-15.795*cm,-11.185*cm, 0.0*cm);
  fScintPosition[12] = G4ThreeVector(-25.725*cm,-16.135*cm, 0.0*cm);
  fScintPosition[13] = G4ThreeVector(-35.655*cm,-21.085*cm, 0.0*cm);
  fScintPosition[14] = G4ThreeVector(-45.585*cm,-26.035*cm, 0.0*cm);
  fScintPosition[15] = G4ThreeVector(-55.515*cm,-30.985*cm, 0.0*cm);
}

CHODGeometryParameters::~CHODGeometryParameters() {}

CHODGeometryParameters* CHODGeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new CHODGeometryParameters();
  return fInstance;
}

TObjArray CHODGeometryParameters::GetHashTable() {
  return 0;
}
