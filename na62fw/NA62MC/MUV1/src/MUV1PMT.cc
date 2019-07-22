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
//---------------------------------------------------------------------
//
//
// Created by Mario Vormstein (mario.vormstein@cern.ch)  2011-02-01
// based on MUV1Fiber.cc
//
// Creates a box used as PMT
//---------------------------------------------------------------------

#include "G4Box.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"

#include "MUV1PMT.hh"
#include "MUV1PMTSD.hh"
#include "G4SDManager.hh"

/// \class MUV1PMT
/// \Brief
/// MUV1PMT class.
/// \EndBrief
///
/// \Detailed
///	Creates a PMT. constisting of a box with a glass window
/// \EndDetailed


MUV1PMT::MUV1PMT(G4Material * Material,
		     G4LogicalVolume * MotherVolume,
		     G4ThreeVector PMTPosition,
                     G4ThreeVector PMTSize,
		     G4int iCopy) :
  NA62VComponent(Material,MotherVolume),
  fiCopy        (iCopy),
  fPMTSize    (PMTSize),
  fPMTPosition(PMTPosition)
{
  ReadGeometryParameters();
   // Mandatory here to Find or Build the needed materials
  MUV1MaterialParameters::GetInstance();
  CreateGeometry();
  SetProperties();
}

void MUV1PMT::ReadGeometryParameters()
{  
  MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();
  fAirGap = GeoPars->GetPMTAirGap();
  fGlassGap = GeoPars->GetPMTGlassGap();
  

}

void MUV1PMT::CreateGeometry()
{
 G4Box* pmt_box = new G4Box("MUV1scintillator_box",
			  fPMTSize.x(),
			  fPMTSize.y(),
			  fPMTSize.z());
  
  fLogicalVolume = new G4LogicalVolume(pmt_box,
				       fMaterial,
				       "MUV1PMT",
				       0,
				       0,
				       0 );

  
  fPhysicalVolume = new G4PVPlacement(0,
				      fPMTPosition,
				      fLogicalVolume,
				      "MUV1PMT",
				      fMotherVolume,
				      false,
				      fiCopy);

  G4Box* pmt_sensitive_box = new G4Box("pmt_sensitive_box",
 			  fPMTSize.x(),
 			  fPMTSize.y()-2*fAirGap-2*fGlassGap,
 			  fPMTSize.z());

  G4LogicalVolume* sensitive_volume = new G4LogicalVolume(pmt_sensitive_box,
		   G4Material::GetMaterial("Air"),
  				       "MUV1PMTAir",
  				       0,
  				       0,
  				       0 );

  new G4PVPlacement(0, G4ThreeVector(0,0,0),
		    sensitive_volume, "MUV1PMTSensitive",
		    fLogicalVolume, false, fiCopy);

  G4Box* air_box = new G4Box("MUV1scintillator_box",
  			  fPMTSize.x(),
  			  fAirGap,
  			  fPMTSize.z());

     G4LogicalVolume* air_volume = new G4LogicalVolume(air_box,
  		   G4Material::GetMaterial("Air"),
     				       "MUV1PMTAir",
     				       0,
     				       0,
     				       0 );

     new G4PVPlacement(0, G4ThreeVector(0,(fPMTSize.x()-fAirGap),0),
		       air_volume, "MUV1PMTAir",
		       fLogicalVolume, false, fiCopy);

     new G4PVPlacement(0, G4ThreeVector(0,-(fPMTSize.x()-fAirGap),0),
		       air_volume, "MUV1PMTAir",
		       fLogicalVolume, false, fiCopy);

  G4Box* glas_box = new G4Box("MUV1scintillator_box",
 			  fPMTSize.x(),
 			  fGlassGap,
 			  fPMTSize.z());

    G4LogicalVolume* glas_volume = new
      G4LogicalVolume(glas_box,
		      G4Material::GetMaterial("Glass"),
		      "MUV1PMTGlass", 0, 0, 0);

    new G4PVPlacement(0, G4ThreeVector(0,(fPMTSize.y()-2*fAirGap-fGlassGap),0),
		      glas_volume, "MUV1PMTGlass",
		      fLogicalVolume, false, fiCopy);

    new G4PVPlacement(0, G4ThreeVector(0,-(fPMTSize.y()-2*fAirGap-fGlassGap),0),
		      glas_volume, "MUV1PMTGlass",
		      fLogicalVolume, false, fiCopy);

  G4SDManager* SDman = G4SDManager::GetSDMpointer();
  G4String MUV1PMTSDname = "/MUV1/PMT";
  G4VSensitiveDetector * Muv1PMTSD = SDman->FindSensitiveDetector(MUV1PMTSDname);
  sensitive_volume->SetSensitiveDetector(Muv1PMTSD);
}

void MUV1PMT::SetProperties() {
  // Set visualization properties
  fVisAtt= new G4VisAttributes(G4Colour(0.0,0.5,1.0));
  fVisAtt -> SetVisibility(true);
  fLogicalVolume ->SetVisAttributes(fVisAtt);
}
