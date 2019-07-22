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
// Major update: Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) March 2014
//
// --------------------------------------------------------------------

/// \class MUV3GeometryParameters
/// \Brief
/// MUV3 geometry definition for MC
/// \EndBrief

#include "TVector.h"
#include "MUV3GeometryParameters.hh"
#include "DetectorParameter.hh"

MUV3GeometryParameters* MUV3GeometryParameters::fInstance = 0;

MUV3GeometryParameters::MUV3GeometryParameters() : NA62VGeometryParameters(G4String("MUV3"))  {

  // MUV3 responsibility region
  fResponsibilityRegionXLength  =   5.000*m;
  fResponsibilityRegionYLength  =   5.000*m;
  fResponsibilityRegionZStart   = 245.350*m;
  fResponsibilityRegionZEnd     = 247.800*m;

  // Z positions of objects
  fFiscZStart                   = 245.392*m;
  fFeWallZStart                 = 245.900*m;
  fFrontPlateZStart             = 246.790*m;
  fScintillatorZStart           = 246.800*m;
  fPMTZStart                    = 247.0565*m; // measured by Luigi: 206.5mm downstream of the tile (22/6/2016)
  fBackPlateZStart              = 247.190*m;

  // The module is between the front and back plates
  fModuleZLength = fBackPlateZStart - fScintillatorZStart;

  // Geometry parameters
  fFrontPlateThickness = 10.00*mm;
  fScintillatorZLength = 50.00*mm;
  fBackPlateThickness  = 10.00*mm;

  // EMU 9814B 2-inch PMTs are mainly used: https://my.et-enterprises.com/pdf/9814B.pdf
  // Also, 16 similar Philips XP2262 PMTs are used: https://my.et-enterprises.com/pdf/XP2262.pdf
  // Further details: Luigi di Lella's talk, Feb 2014 MUV WG meeting.

  fPMTWindowThickness  =  2.25*mm;
  fPMTWindowRadius     = 23.00*mm;

  // Inner and outer radii
  fBeamPipeInnerRadius[0] = 92.5*mm;
  fBeamPipeOuterRadius[0] = 98.5*mm;
  fPassiveInnerRadius     = 101.5*mm; // Al semi-cylinders / plates boundary
  fActiveInnerRadius      = 103.0*mm; // scintillator

  // Fe wall parameters
  fFeWallXSize         = 2800*mm;
  fFeWallYSize         = 2800*mm;
  fFeWallThickness     =  800*mm;
  fFeWallInnerRadius   =  101*mm;

  // Width of the vertical space in the middle of MUV3
  fGapWidth            = 0.8*mm;

  // Transverse module sizes
  fNModulesX           = 12;
  fNModulesY           = 12;
  fLargeModuleSize     = 220.00*mm;
  fSmallModuleSize     = (440.0/3.0)*mm;

  // Positions of the PMTs

  // For the large modules: the distance the between the PMT centres is 75mm.
  fPMT1LargeX =  26.5165*mm;
  fPMT1LargeY =  26.5165*mm;
  fPMT2LargeX = -26.5165*mm;
  fPMT2LargeY = -26.5165*mm;

  // For the small top-left corner modules (unchecked)
  fPMT1CornerX = -14.133333*mm;
  fPMT1CornerY = +15.883333*mm;
  fPMT2CornerX = +38.366667*mm;
  fPMT2CornerY = -37.516667*mm;

  // For the small side modules (unchecked)
  fPMTSideX = 0.0*mm;
  fPMTSideY = 15.883333*mm;

  fResponsibilityRegion.push_back
    (new ResponsibilityRegion(fResponsibilityRegionZStart,
			      fResponsibilityRegionZEnd));
}

MUV3GeometryParameters* MUV3GeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new MUV3GeometryParameters();
  return fInstance;
}

TObjArray MUV3GeometryParameters::GetHashTable() {
  return 0;
}

void MUV3GeometryParameters::Print() {
  G4cout << "fMUV3ResponsibilityRegionStart= "<< fResponsibilityRegionZStart << G4endl
	 << "fMUV3ResponsibilityRegionEnd= "<< fResponsibilityRegionZEnd << G4endl
	 << "fMUV3DetectorFrontZPosition= "<< fFrontPlateZStart << G4endl;
}
