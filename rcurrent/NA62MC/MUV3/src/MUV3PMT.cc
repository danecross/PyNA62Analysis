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
//	      Spasimir Balev (Spasimir.Balev@cern.ch)
//
// --------------------------------------------------------------------

/// \class MUV3PMT
/// \Brief
/// Simulation of an EMI 9814B PMT of MUV3
/// \EndBrief
/// \Detailed
/// EMU 9814B 2-inch PMTs are mainly used for MUV3.
/// Their data sheet is available here: https://my.et-enterprises.com/pdf/9814B.pdf
/// Also, 16 Philips XP2262 PMTs with similar characteristics are used
/// (the are 1 cm shorter, so 12 ns faster transit).
/// Further details: Luigi di Lella's talk, Feb 2014 MUV WG meeting.
/// Only the PMT window is simulated: this is essential to simulate the response to
/// the "early" Cherenkov photons produced in the window.
/// \EndDetailed

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "G4PVReplica.hh"
#include "globals.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV3PMT.hh"
#include "MUV3GeometryParameters.hh"
#include "MUV3MaterialParameters.hh"

#include "MUV3SD.hh"
#include "G4SDManager.hh"

MUV3PMT::MUV3PMT(G4Material* Material, G4LogicalVolume* MotherVolume,
		 G4double X, G4double Y, G4int ID) :
NA62VComponent(Material,MotherVolume) {

  ReadGeometryParameters();
  fX    = X;
  fY    = Y;
  fID   = ID;
  fName = Form("MUV3PMT%03d", fID);

  MUV3MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV3PMT::ReadGeometryParameters() {
  MUV3GeometryParameters* GeoPars = MUV3GeometryParameters::GetInstance();
  fModuleZStart       = GeoPars->GetScintillatorZStart();
  fModuleZLength      = GeoPars->GetModuleZLength();
  fPMTZStart          = GeoPars->GetPMTZStart();
  fPMTWindowRadius    = GeoPars->GetPMTWindowRadius();
  fPMTWindowThickness = GeoPars->GetPMTWindowThickness();
}

void MUV3PMT::CreateGeometry() {

  G4Tubs* PMTWindowSolid = new G4Tubs
    (fName, 0.0, fPMTWindowRadius, 0.5*fPMTWindowThickness, 0.0, 360*deg);

  fLogicalVolume = new G4LogicalVolume (PMTWindowSolid, fMaterial, fName);

  G4ThreeVector PMTPosition = G4ThreeVector
    (fX, fY, fPMTZStart + 0.5*fPMTWindowThickness - fModuleZStart - 0.5*fModuleZLength);

  fPhysicalVolume = new G4PVPlacement
    (0, PMTPosition, fLogicalVolume, fName, fMotherVolume, false, fID);

  // Make PMT windows are sensitive detectors,
  // this allows simulation of Cherenkov light production in windows.

  G4SDManager* SDManager = G4SDManager::GetSDMpointer();
  G4String MUV3SDname = "/MUV3";
  G4VSensitiveDetector* Muv3SD = SDManager->FindSensitiveDetector(MUV3SDname);
  fLogicalVolume->SetSensitiveDetector(Muv3SD);
}
